# s1d's two outputs are ALL-OR-NONE.
#
# .s1d_worker writes `file_raw`, then spends minutes on imputation, IPW
# estimation and weight truncation over a multi-GB panel, then writes
# `file_imp`. Before this, a crash in that window committed `file_raw` and
# left `file_imp` absent, and nothing downstream could tell.
#
# The fix moves s1d onto batchit's declared-output commit engine with
# `style = "staged_writer"`: the worker resolves each destination with
# `.batch_where_to_write_output("raw" / "imp")` -- attempt-scoped staging
# files in the FINAL directories -- and batchit renames both into place only
# once the item has returned.
#
# `style = "return"` would be INCORRECT here, not merely slower: TTEEnrollment
# is R6 wrapping a data.table and `$s2_ipw()` mutates it BY REFERENCE, so
# `list(raw = trial, imp = trial)` would be two references to the same
# post-mutation object and `file_raw` would silently hold the imputed panel.
# The happy-path test below asserts `file_raw` has no `ipw` column, which is
# what catches that regression.
#
# This file drives the REAL dispatch -- real subprocess, real batchit commit
# engine, no mocked binding. A captured-dispatch test cannot see whether the
# commit is actually atomic, which is the entire property under test.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("processx")

# s1a moved onto batchit's declared-output commit engine with
# `style = "staged_writer"`: it resolves each of its `2 x n_enrollments`
# destinations by NAME through .batch_where_to_write_output(), takes no
# `work_dir`, and is therefore NOT callable outside a
# run_and_write_files_atomically() dispatch. So this fixture issues the REAL
# s1a dispatch instead of calling the worker in-process.
s1a_run_real <- function(skel_path, es_list, spec, work_dir) {
  bn <- basename(skel_path)
  id <- paste0("s1a_", bn)
  items <- list(list(
    file_path = skel_path,
    enrollment_specs = es_list,
    spec = spec
  ))
  names(items) <- id
  eids <- unlist(lapply(es_list, function(e) e$enrollment_id))
  outputs <- list(swereg:::.s1a_outputs_for_skeleton(work_dir, eids, bn))
  names(outputs) <- id
  invisible(utils::capture.output(
    swereg:::.batch_run_and_write(
      target = swereg:::.batch_target("swereg", ".s1a_worker_multi"),
      items = items,
      outputs = outputs,
      style = "staged_writer",
      n_workers = 1L,
      dev_path = swereg:::.swereg_dev_path(),
      label = "s1a"
    ),
    type = "output"
  ))
}

# Build a small real plan and run s1a + s1b + s1c in-process, so the s1c panel
# chunk s1d reads is genuinely on disk. Returns everything the dispatch needs
# plus the two FINAL output paths (deliberately outside the plan's own naming,
# so nothing but this test can write them).
s1d_fixture <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  sk <- ttm_skeleton(
    scenario = "A",
    n_persons = 40L,
    date_min = "2018-01-01",
    date_max = "2019-06-30",
    n_init_bands = 8L,
    seed = 4242L
  )
  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    "s1datomic",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)]
  )

  work_dir <- swereg:::.s1_work_dir(plan, ensure_exists = FALSE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  es <- plan$enrollment_spec(1)
  es$n_threads <- 1L
  bn <- basename(skel_path)

  s1a_run_real(skel_path, list(es), plan$spec, work_dir)
  # s1b and s1c RETURN their outputs; batchit commits them in a real run, so
  # the fixture writes them itself.
  s1b <- swereg:::.s1b_worker(
    enrollment_spec = es,
    spec = plan$spec,
    work_dir = work_dir,
    skel_basenames = bn
  )
  qs2::qs_save(
    s1b$enrolled_ids,
    swereg:::.s1b_enrolled_ids_path(work_dir, es$enrollment_id)
  )
  s1c <- swereg:::.s1c_worker(
    enrollment_spec = es,
    file_path = skel_path,
    spec = plan$spec,
    work_dir = work_dir
  )
  qs2::qs_save(
    s1c$panel,
    swereg:::.s1c_panel_path(work_dir, es$enrollment_id, bn)
  )

  # A dedicated output directory: the preservation assertions must not be able
  # to confuse a final path with a fixture input.
  dir_out <- file.path(root, "out")
  dir.create(dir_out, showWarnings = FALSE)

  list(
    plan = plan,
    spec = plan$spec,
    es = es,
    skel_basenames = bn,
    work_dir = work_dir,
    dir_out = dir_out,
    file_raw = file.path(dir_out, "enr_raw.qs2"),
    file_imp = file.path(dir_out, "enr_imp.qs2")
  )
}

# The REAL dispatch, exactly as the s1d call site issues it.
s1d_dispatch <- function(fx, impute_fn = NULL, id = "s1d_probe") {
  items <- list(list(
    enrollment_spec = fx$es,
    spec = fx$spec,
    work_dir = fx$work_dir,
    skel_basenames = fx$skel_basenames,
    impute_fn = impute_fn,
    stabilize = TRUE
  ))
  names(items) <- id
  outputs <- list(c(raw = fx$file_raw, imp = fx$file_imp))
  names(outputs) <- id

  # dev_path is what decides WHICH swereg the subprocess loads, so it is
  # asserted, not decorated: if it resolved to the wrong thing the whole file
  # would be testing some other build. It has exactly two legal values, one
  # per environment, and both are correct:
  #
  #   * under devtools::load_all(), a single existing directory -- THIS source
  #     tree -- so the child runs the code under test rather than a stale
  #     install;
  #   * under R CMD check the package is INSTALLED, .swereg_dev_path() returns
  #     NULL by design, and NULL is batchit's documented "load the installed
  #     package" value. Passing it through is what makes this file drive the
  #     real dispatch against the installed build, which is the build under
  #     test there.
  #
  # NULL is only acceptable BECAUSE swereg is installed, so that is checked
  # against R's own installed-package marker -- a dev tree has no
  # Meta/package.rds, so a regression in dev-path detection under load_all()
  # still fails here instead of silently loading someone else's swereg.
  dev_path <- swereg:::.swereg_dev_path()
  expect_identical(dev_path, swereg:::.swereg_dev_path())
  expect_true(is.null(dev_path) || (is.character(dev_path) && length(dev_path) == 1L))
  if (is.null(dev_path)) {
    expect_true(file.exists(file.path(
      system.file(package = "swereg"),
      "Meta",
      "package.rds"
    )))
  } else {
    expect_true(dir.exists(dev_path))
    expect_true(file.exists(file.path(dev_path, "DESCRIPTION")))
  }

  invisible(utils::capture.output(
    res <- swereg:::.batch_run_and_write(
      target = swereg:::.batch_target("swereg", ".s1d_worker"),
      items = items,
      outputs = outputs,
      style = "staged_writer",
      n_workers = 1L,
      dev_path = dev_path,
      label = "s1d"
    ),
    type = "output"
  ))
  res
}

test_that(".s1d_worker is handed no output paths", {
  need <- names(formals(swereg:::.s1d_worker))
  expect_setequal(
    need,
    c(
      "enrollment_spec",
      "spec",
      "work_dir",
      "skel_basenames",
      "impute_fn",
      "stabilize"
    )
  )
  expect_false("file_raw_path" %in% need)
  expect_false("file_imp_path" %in% need)
})

test_that("a REAL s1d dispatch commits both outputs, and file_raw is the RAW panel", {
  skip_on_cran()
  fx <- s1d_fixture()

  expect_false(file.exists(fx$file_raw))
  expect_false(file.exists(fx$file_imp))

  res <- s1d_dispatch(fx)

  expect_true(file.exists(fx$file_raw))
  expect_true(file.exists(fx$file_imp))
  expect_identical(
    unname(res[["s1d_probe"]]$committed[["raw"]]),
    fx$file_raw
  )
  expect_identical(
    unname(res[["s1d_probe"]]$committed[["imp"]]),
    fx$file_imp
  )

  raw <- swereg:::qs2_read(fx$file_raw)
  imp <- swereg:::qs2_read(fx$file_imp)
  expect_s3_class(raw, "TTEEnrollment")
  expect_s3_class(imp, "TTEEnrollment")
  expect_true(nrow(raw$data) > 0L)

  # THE regression guard against a future `style = "return"` "simplification":
  # $s2_ipw() mutates the panel by reference, so a returned
  # list(raw = trial, imp = trial) would put the IPW'd panel in BOTH files.
  expect_false("ipw" %in% names(raw$data))
  expect_true("ipw" %in% names(imp$data))
  expect_equal(nrow(raw$data), nrow(imp$data))

  # No staging litter survived the successful commit.
  expect_length(
    grep("\\.stage", list.files(fx$dir_out, all.files = TRUE), value = TRUE),
    0L
  )
})

test_that("a failure BETWEEN the two writes leaves both pre-existing finals byte-identical", {
  skip_on_cran()
  fx <- s1d_fixture()

  # The production RERUN case: both finals already exist. Absence-only would
  # be the empty-directory case and is strictly weaker.
  writeLines("SENTINEL-RAW-DO-NOT-TOUCH", fx$file_raw)
  writeLines("SENTINEL-IMP-DO-NOT-TOUCH", fx$file_imp)
  raw_before <- readBin(fx$file_raw, "raw", file.size(fx$file_raw))
  imp_before <- readBin(fx$file_imp, "raw", file.size(fx$file_imp))

  # Fails EXACTLY between the two writes: impute_fn is the first thing called
  # after the raw write. It records the output directory's contents first, so
  # the test can prove the raw STAGE file had already been written -- i.e.
  # what was prevented was the COMMIT, not the write.
  probe_path <- file.path(fx$dir_out, "..", "probe_listing.txt")
  boom <- local({
    d <- fx$dir_out
    pf <- probe_path
    function(x, cv) {
      writeLines(list.files(d, all.files = TRUE), pf)
      stop("__BOOM__")
    }
  })

  err <- tryCatch(s1d_dispatch(fx, impute_fn = boom), error = function(e) e)

  # 1. The worker actually RAN and failed inside the intended window. Without
  #    this the test could pass for the wrong reason: an item rejected at
  #    batchit's parent-side validation never runs impute_fn at all, and both
  #    finals would be untouched anyway.
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "__BOOM__", fixed = TRUE)

  # 2. The raw STAGE file existed at the moment of failure -- so the first
  #    write had happened, and only its commit was prevented.
  expect_true(file.exists(probe_path))
  listing <- readLines(probe_path)
  raw_stage <- grep(
    paste0("^", basename(fx$file_raw), "\\..*\\.stage"),
    listing,
    value = TRUE
  )
  expect_length(raw_stage, 1L)
  # The imp side never got that far.
  expect_length(
    grep(
      paste0("^", basename(fx$file_imp), "\\..*\\.stage"),
      listing,
      value = TRUE
    ),
    0L
  )

  # 3. PRESERVATION: both finals still hold their exact previous contents.
  expect_true(file.exists(fx$file_raw))
  expect_true(file.exists(fx$file_imp))
  expect_identical(
    readBin(fx$file_raw, "raw", file.size(fx$file_raw)),
    raw_before
  )
  expect_identical(
    readBin(fx$file_imp, "raw", file.size(fx$file_imp)),
    imp_before
  )

  # And no staging file was orphaned in the output directory.
  expect_length(
    grep("\\.stage", list.files(fx$dir_out, all.files = TRUE), value = TRUE),
    0L
  )
})

test_that("the s1d call site dispatches via run_and_write(style = 'staged_writer')", {
  fx <- s1d_fixture()

  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .batch_run = function(...) invisible(NULL),
    .batch_run_and_write = function(target, ...) {
      if (!identical(target$symbol, ".s1d_worker")) {
        return(invisible(NULL))
      }
      cap$target <- target
      cap$args <- list(...)
      stop("__CAPTURED__")
    },
    .package = "swereg"
  )

  expect_error(
    suppressWarnings(utils::capture.output(
      fx$plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = "/tmp/fake_dev_path"
      ),
      type = "output"
    )),
    "__CAPTURED__"
  )

  a <- cap$args
  expect_identical(cap$target$symbol, ".s1d_worker")
  expect_identical(cap$target$package, "swereg")
  expect_identical(a$style, "staged_writer")
  # s1d dispatches inside a per-enrollment loop: one item per call, one
  # subworker.
  expect_identical(a$n_workers, 1L)
  expect_identical(a$dev_path, "/tmp/fake_dev_path")
  expect_identical(a$label, "s1d")
  expect_true(is.function(a$p))

  expect_length(a$items, 1L)
  expect_length(a$outputs, 1L)
  expect_identical(names(a$outputs), names(a$items))
  expect_true(all(grepl("^s1d_", names(a$items))))

  # No output path may reach the worker as an argument.
  expect_setequal(names(a$items[[1L]]), names(formals(swereg:::.s1d_worker)))
  expect_false("file_raw_path" %in% names(a$items[[1L]]))
  expect_false("file_imp_path" %in% names(a$items[[1L]]))

  o <- a$outputs[[1L]]
  expect_true(is.character(o))
  expect_named(o, c("raw", "imp"))
  expect_match(o[["raw"]], "^/")
  expect_match(o[["imp"]], "^/")
  expect_false(identical(o[["raw"]], o[["imp"]]))
})
