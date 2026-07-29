# s1c on batchit's declared-output commit engine. Two independent halves, both
# needed:
#   * the WORKER returns list(panel = ) and writes nothing -- a
#     captured-dispatch test never runs the worker, so it cannot see this;
#   * the DISPATCH goes through .batch_run_and_write(style = "return") with one
#     absolute declared output per (enrollment, skeleton) item -- a direct
#     worker test never touches the call site.
#
# s1c keeps its `work_dir` formal: it READS the s1a cache and the s1b
# enrolled-ids file from there, so work_dir is an input, not an output-path
# source.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")

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

# Build a small real plan (spec YAML + one skeleton .qs2) plus its s1 work
# directory, and run s1a + s1b in-process so the s1c inputs exist on disk.
s1c_fixture <- function(env = parent.frame(), run_s1ab = TRUE) {
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
    "s1cdecl",
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

  if (run_s1ab) {
    s1a_run_real(skel_path, list(es), plan$spec, work_dir)
    # .s1b_worker RETURNS its outputs (phase 4); batchit commits them in the
    # real run, so the fixture writes the enrolled-ids file itself -- s1c
    # reads it from work_dir.
    s1b <- swereg:::.s1b_worker(
      enrollment_spec = es,
      spec = plan$spec,
      work_dir = work_dir,
      skel_basenames = basename(skel_path)
    )
    qs2::qs_save(
      s1b$enrolled_ids,
      swereg:::.s1b_enrolled_ids_path(work_dir, es$enrollment_id)
    )
  }

  list(
    plan = plan,
    spec = plan$spec,
    es = es,
    skel_path = skel_path,
    work_dir = work_dir,
    dir_tteplan = dir_tteplan
  )
}

test_that(".s1c_worker returns list(panel=) and writes no panel file", {
  fx <- s1c_fixture()

  # work_dir stays: s1c READS the s1b enrolled-ids file from it.
  expect_true("work_dir" %in% names(formals(swereg:::.s1c_worker)))

  panel_path <- swereg:::.s1c_panel_path(
    fx$work_dir,
    fx$es$enrollment_id,
    basename(fx$skel_path)
  )
  expect_false(file.exists(panel_path))
  before <- sort(list.files(fx$work_dir))

  res <- swereg:::.s1c_worker(
    enrollment_spec = fx$es,
    file_path = fx$skel_path,
    spec = fx$spec,
    work_dir = fx$work_dir
  )

  expect_named(res, "panel")
  expect_s3_class(res$panel, "TTEEnrollment")
  expect_true(nrow(res$panel$data) > 0L)
  # The old code wrote the panel itself; the new one must not.
  expect_false(file.exists(panel_path))
  expect_identical(sort(list.files(fx$work_dir)), before)
})

test_that("s1c dispatch declares one absolute output per item via run_and_write", {
  fx <- s1c_fixture(run_s1ab = FALSE)

  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    # All four sub-steps now dispatch through .batch_run_and_write(), so the
    # symbol guard below is what selects s1c; s1a and s1b return NULL and run
    # no subprocess. `.batch_run` is stubbed only because nothing should reach
    # it any more -- if something does, it must not fork.
    .batch_run = function(...) invisible(NULL),
    .batch_run_and_write = function(target, ...) {
      if (!identical(target$symbol, ".s1c_worker")) {
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

  expect_identical(cap$target$symbol, ".s1c_worker")
  expect_identical(cap$target$package, "swereg")

  a <- cap$args
  expect_identical(a$style, "return")
  expect_identical(a$n_workers, 1L)
  expect_identical(a$dev_path, "/tmp/fake_dev_path")
  expect_identical(a$label, "s1c")
  expect_true(is.function(a$p))

  items <- a$items
  outputs <- a$outputs
  n_expected <- length(unique(fx$plan$ett$enrollment_id)) *
    length(fx$plan$skeleton_files)
  expect_length(items, n_expected)
  expect_length(outputs, n_expected)
  expect_identical(names(outputs), names(items))
  expect_true(all(grepl("^s1c_", names(items))))
  expect_identical(anyDuplicated(names(items)), 0L)

  # Item formals: complete for .s1c_worker (work_dir included), and no
  # output path among them.
  need <- names(formals(swereg:::.s1c_worker))
  expect_setequal(need, c("enrollment_spec", "file_path", "spec", "work_dir"))
  for (it in items) {
    expect_setequal(names(it), need)
    expect_false(any(grepl("panel", names(it))))
  }

  # One declared output per item, named `panel`, absolute, matching the
  # canonical s1c panel path.
  for (o in outputs) {
    expect_named(o, "panel")
    expect_true(is.character(o))
    expect_match(o[["panel"]], "^/")
  }
  expect_identical(
    unname(vapply(outputs, function(o) o[["panel"]], character(1))),
    unname(vapply(
      names(items),
      function(id) {
        it <- items[[id]]
        swereg:::.s1c_panel_path(
          it$work_dir,
          it$enrollment_spec$enrollment_id,
          basename(it$file_path)
        )
      },
      character(1)
    ))
  )
})
