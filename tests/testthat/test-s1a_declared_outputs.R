# s1a on batchit's declared-output commit engine, `style = "staged_writer"`.
#
# s1a is the widest fan-out in Loop 1: ONE item writes `2 x n_enrollments`
# files (a projected skeleton cache and a (tuples, attrition) chunk per
# enrollment). Before this, the worker built all of those paths itself from a
# `work_dir` argument and streamed them out as it went, so a crash left a
# partial set behind and nothing downstream could tell.
#
# THE RISK THIS FILE EXISTS FOR is not the crash, it is the DRIFT. s1c reads
# the s1a cache back through `.s1a_cache_path()`; if the path s1a writes and
# the path s1c reads ever disagree by one character, `.s1c_worker_impl()` used
# to take its recompute fallback -- no error, no warning, ~10x slower, and a
# DIFFERENT column set in the committed panel. A silent production-output
# change hiding behind a performance optimisation. Two things close it, and
# both are asserted below:
#
#   1. the parent declares every path (`.s1a_outputs_for_skeleton()`) and the
#      worker resolves each by NAME (`.batch_where_to_write_output()`), so a
#      name drift is a loud child failure; and
#   2. `.s1c_worker()` passes `require_cache = TRUE`, so a missing cache stops
#      the item instead of recomputing.
#
# A captured-dispatch test alone CANNOT prove point 1: capturing the parent's
# `outputs` map sees only one end of the handshake, and a parent-only rename
# leaves the captured map perfectly self-consistent. Only real child execution
# rejects it. So this file drives a REAL one-item dispatch -- real subprocess,
# real batchit commit engine, no mocked binding -- as well as capturing the
# call site.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("processx")

# Two enrollments, so `2 x n_enrollments` is 4 and not 2: with a single
# enrollment the cache/pre distinction and the per-enrollment suffix cannot be
# told apart from each other.
s1a_write_spec <- function(path, project_prefix) {
  ttm_write_spec(path, project_prefix, "rd_age_continuous")
  spec <- yaml::read_yaml(path)
  e2 <- spec$enrollments[[1L]]
  e2$id <- "02"
  e2$name <- "Treated vs control (second enrollment)"
  e2$treatment$implementation$seed <- 2L
  spec$enrollments[[2L]] <- e2
  yaml::write_yaml(spec, path)
  invisible(path)
}

s1a_fixture <- function(env = parent.frame(), n_skel = 2L) {
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
  skel_paths <- file.path(
    dir_tteplan,
    paste0("skel_", letters[seq_len(n_skel)], ".qs2")
  )
  for (p in skel_paths) {
    qs2::qs_save(sk, p)
  }
  s1a_write_spec(file.path(dir_spec, "spec_v001.yaml"), "s1adecl")

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_paths, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)]
  )

  work_dir <- swereg:::.s1_work_dir(plan, ensure_exists = FALSE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  eids <- unique(plan$ett$enrollment_id)
  all_es <- lapply(seq_along(eids), function(i) {
    es <- plan$enrollment_spec(i)
    es$n_threads <- 1L
    es
  })

  list(
    plan = plan,
    spec = plan$spec,
    all_es = all_es,
    eids = eids,
    skel_paths = skel_paths,
    skel_basenames = basename(skel_paths),
    work_dir = work_dir
  )
}

# The REAL dispatch, exactly as the s1a call site issues it.
s1a_dispatch <- function(fx, j = 1L, outputs = NULL) {
  bn <- fx$skel_basenames[j]
  id <- paste0("s1a_", bn)
  items <- list(list(
    file_path = fx$skel_paths[j],
    enrollment_specs = fx$all_es,
    spec = fx$spec
  ))
  names(items) <- id
  if (is.null(outputs)) {
    outputs <- list(swereg:::.s1a_outputs_for_skeleton(fx$work_dir, fx$eids, bn))
  }
  names(outputs) <- id

  # dev_path = .swereg_dev_path() is what makes the subprocess load THIS
  # source tree rather than an installed swereg -- i.e. what makes the child
  # run the code under test.
  dev_path <- swereg:::.swereg_dev_path()
  expect_true(is.character(dev_path) && length(dev_path) == 1L)
  expect_true(file.exists(file.path(dev_path, "DESCRIPTION")))

  invisible(utils::capture.output(
    res <- swereg:::.batch_run_and_write(
      target = swereg:::.batch_target("swereg", ".s1a_worker_multi"),
      items = items,
      outputs = outputs,
      style = "staged_writer",
      n_workers = 1L,
      dev_path = dev_path,
      label = "s1a"
    ),
    type = "output"
  ))
  res
}

test_that(".s1a_worker_multi is handed no work_dir and no output path", {
  need <- names(formals(swereg:::.s1a_worker_multi))
  expect_setequal(need, c("file_path", "enrollment_specs", "spec"))
  expect_false("work_dir" %in% need)
  expect_false(any(grepl("path$", setdiff(need, "file_path"))))
})

test_that("the s1a output enumeration is the single source of the 2N name set", {
  eids <- c("01", "02")
  o <- swereg:::.s1a_outputs_for_skeleton("/wd", eids, "skel_a.qs2")

  expect_true(is.character(o))
  expect_length(o, 4L)
  expect_identical(names(o), c("cache_01", "pre_01", "cache_02", "pre_02"))
  # Reconstructed independently of the constructors, so a change to either
  # end of the naming contract shows up here.
  expect_identical(
    unname(o),
    c(
      "/wd/s1a_cache_enr01_skel_a.qs2",
      "/wd/s1a_pre_enr01_skel_a.qs2",
      "/wd/s1a_cache_enr02_skel_a.qs2",
      "/wd/s1a_pre_enr02_skel_a.qs2"
    )
  )
  expect_identical(swereg:::.s1a_cache_name("01"), "cache_01")
  expect_identical(swereg:::.s1a_pre_name("01"), "pre_01")
  expect_identical(
    swereg:::.s1a_outputs_for_skeleton("/wd", character(0), "b"),
    character(0)
  )
})

test_that("the s1a call site declares 2 x n_enrollments outputs per skeleton", {
  fx <- s1a_fixture(n_skel = 2L)

  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .batch_run = function(...) invisible(NULL),
    .batch_run_and_write = function(target, ...) {
      if (!identical(target$symbol, ".s1a_worker_multi")) {
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

  expect_identical(cap$target$symbol, ".s1a_worker_multi")
  expect_identical(cap$target$package, "swereg")

  a <- cap$args
  expect_identical(a$style, "staged_writer")
  expect_identical(a$n_workers, 1L)
  expect_identical(a$dev_path, "/tmp/fake_dev_path")
  expect_identical(a$label, "s1a")
  expect_true(is.function(a$p))
  # `collect` belonged to the old .batch_run() dispatch and is not a formal of
  # run_and_write_files_atomically(); passing it would error in batchit.
  expect_false("collect" %in% names(a))

  items <- a$items
  outputs <- a$outputs
  expect_length(items, length(fx$skel_paths))
  expect_length(outputs, length(fx$skel_paths))
  expect_identical(names(items), paste0("s1a_", fx$skel_basenames))
  expect_identical(names(outputs), names(items))

  # No output path, and no work_dir, may reach the worker as an argument.
  for (it in items) {
    expect_setequal(names(it), names(formals(swereg:::.s1a_worker_multi)))
    expect_false("work_dir" %in% names(it))
  }

  # THE FULL 2N MAP, every skeleton and every enrollment, reconstructed from
  # the literal on-disk naming contract rather than from the constructors.
  eids <- fx$eids
  expect_length(eids, 2L)
  for (j in seq_along(fx$skel_basenames)) {
    bn <- fx$skel_basenames[j]
    o <- outputs[[j]]
    expect_true(is.character(o))
    expect_length(o, 2L * length(eids))
    expect_identical(
      names(o),
      as.vector(rbind(paste0("cache_", eids), paste0("pre_", eids)))
    )
    expect_identical(
      unname(o),
      as.vector(rbind(
        file.path(fx$work_dir, sprintf("s1a_cache_enr%s_%s", eids, bn)),
        file.path(fx$work_dir, sprintf("s1a_pre_enr%s_%s", eids, bn))
      ))
    )
    # batchit rejects a relative declared output.
    expect_true(all(grepl("^/", o)))
  }
  expect_identical(anyDuplicated(unlist(unname(outputs), use.names = FALSE)), 0L)

  # THE DRIFT ASSERTION: what the PARENT declares must be, character for
  # character, what .s1c_worker() later recomputes with .s1a_cache_path() in
  # order to read the cache back. This is the pairing whose failure used to be
  # silent.
  for (j in seq_along(fx$skel_basenames)) {
    for (eid in eids) {
      expect_identical(
        unname(outputs[[j]][[swereg:::.s1a_cache_name(eid)]]),
        swereg:::.s1a_cache_path(fx$work_dir, eid, fx$skel_basenames[j])
      )
      expect_identical(
        unname(outputs[[j]][[swereg:::.s1a_pre_name(eid)]]),
        swereg:::.s1a_pre_path(fx$work_dir, eid, fx$skel_basenames[j])
      )
    }
  }
})

test_that("a REAL s1a dispatch commits all 2 x n_enrollments declared outputs", {
  skip_on_cran()
  fx <- s1a_fixture(n_skel = 1L)
  bn <- fx$skel_basenames[1L]
  eids <- fx$eids

  declared <- swereg:::.s1a_outputs_for_skeleton(fx$work_dir, eids, bn)
  expect_length(declared, 4L)
  expect_false(any(file.exists(declared)))

  res <- s1a_dispatch(fx, j = 1L)
  id <- paste0("s1a_", bn)

  # Every declared file is on disk, at exactly the declared path.
  expect_true(all(file.exists(declared)))
  committed <- res[[id]]$committed
  for (nm in names(declared)) {
    expect_identical(unname(committed[[nm]]), unname(declared[[nm]]))
  }
  expect_setequal(names(committed), names(declared))

  # The worker wrote NOTHING else into the work directory.
  expect_setequal(list.files(fx$work_dir), basename(unname(declared)))
  expect_length(
    grep("\\.stage", list.files(fx$work_dir, all.files = TRUE), value = TRUE),
    0L
  )

  # The committed files are the real artefacts, per enrollment.
  for (eid in eids) {
    cache <- swereg:::qs2_read(swereg:::.s1a_cache_path(fx$work_dir, eid, bn))
    expect_s3_class(cache, "data.table")
    expect_true(nrow(cache) > 0L)

    pre <- swereg:::qs2_read(swereg:::.s1a_pre_path(fx$work_dir, eid, bn))
    expect_setequal(names(pre), c("tuples", "attrition"))
    expect_true(nrow(pre$attrition) > 0L)
  }
})

test_that("a PARENT-ONLY output-name drift fails the real dispatch loudly", {
  skip_on_cran()
  fx <- s1a_fixture(n_skel = 1L)
  bn <- fx$skel_basenames[1L]

  # Exactly the defect a captured-map assertion cannot see: the parent
  # declares `cache01`/`pre01` while the worker still asks for
  # `cache_01`/`pre_01`. The map is self-consistent; only the CHILD notices.
  drifted <- swereg:::.s1a_outputs_for_skeleton(fx$work_dir, fx$eids, bn)
  names(drifted) <- sub("_", "", names(drifted), fixed = TRUE)
  expect_false(any(names(drifted) %in% c("cache_01", "pre_01")))

  err <- tryCatch(
    s1a_dispatch(fx, j = 1L, outputs = list(drifted)),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "declared outputs")

  # And nothing was committed.
  expect_length(list.files(fx$work_dir), 0L)
})

test_that(".s1c_worker requires the s1a cache: a missing one is loud, not silent", {
  skip_on_cran()
  fx <- s1a_fixture(n_skel = 1L)
  bn <- fx$skel_basenames[1L]
  es <- fx$all_es[[1L]]
  eid <- es$enrollment_id

  s1a_dispatch(fx, j = 1L)
  s1b <- swereg:::.s1b_worker(
    enrollment_spec = es,
    spec = fx$spec,
    work_dir = fx$work_dir,
    skel_basenames = bn
  )
  qs2::qs_save(
    s1b$enrolled_ids,
    swereg:::.s1b_enrolled_ids_path(fx$work_dir, eid)
  )

  # 1. With the cache present the production worker is happy.
  cache_path <- swereg:::.s1a_cache_path(fx$work_dir, eid, bn)
  expect_true(file.exists(cache_path))
  ok <- swereg:::.s1c_worker(
    enrollment_spec = es,
    file_path = fx$skel_paths[1L],
    spec = fx$spec,
    work_dir = fx$work_dir
  )
  expect_s3_class(ok$panel, "TTEEnrollment")

  # 2. Remove the cache -- i.e. simulate the drift, where s1c looks somewhere
  #    s1a did not write. The PRODUCTION path must stop, not recompute.
  unlink(cache_path)
  expect_false(file.exists(cache_path))
  err <- tryCatch(
    swereg:::.s1c_worker(
      enrollment_spec = es,
      file_path = fx$skel_paths[1L],
      spec = fx$spec,
      work_dir = fx$work_dir
    ),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "s1a skeleton cache is required")
  expect_match(conditionMessage(err), basename(cache_path), fixed = TRUE)

  # 3. A NULL cache_path is refused the same way -- the guard is on the branch
  #    taken, not merely on file.exists().
  err_null <- tryCatch(
    swereg:::.s1c_worker_impl(
      es,
      fx$skel_paths[1L],
      fx$spec,
      s1b$enrolled_ids,
      cache_path = NULL,
      require_cache = TRUE
    ),
    error = function(e) e
  )
  expect_s3_class(err_null, "error")
  expect_match(conditionMessage(err_null), "s1a skeleton cache is required")

  # 4. The fallback SURVIVES for dev/direct callers: require_cache is FALSE by
  #    default and still recomputes from the skeleton file.
  expect_identical(formals(swereg:::.s1c_worker_impl)$require_cache, FALSE)
  recomputed <- swereg:::.s1c_worker_impl(
    es,
    fx$skel_paths[1L],
    fx$spec,
    s1b$enrolled_ids,
    cache_path = cache_path
  )
  expect_s3_class(recomputed, "TTEEnrollment")
  expect_true(nrow(recomputed$data) > 0L)
})
