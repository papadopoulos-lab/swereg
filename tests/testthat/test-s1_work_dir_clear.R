# .clear_s1_work_dir() removes the s1 work directory before a run starts.
#
# The count and the duration are the point. A run that inherits thousands of
# leftover chunk files spends minutes in unlink() before the first sub-step,
# and the log said nothing about it.

test_that(".clear_s1_work_dir() reports the file count and removes the directory", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  dir.create(file.path(work_dir, "sub"), recursive = TRUE)
  writeLines("a", file.path(work_dir, "s1a_pre_enr1.qs2"))
  writeLines("b", file.path(work_dir, "s1b_enrolled_ids_enr1.qs2"))
  writeLines("c", file.path(work_dir, "sub", "s1c_panel_enr1.qs2"))

  out <- utils::capture.output(n <- .clear_s1_work_dir(work_dir))

  # Three files across two levels. The subdirectory itself is not counted.
  expect_identical(n, 3L)
  expect_length(out, 1L)
  expect_match(out, "^Cleared s1 work directory: 3 files in [0-9]+\\.[0-9] s$")
  expect_false(dir.exists(work_dir))
})

test_that(".clear_s1_work_dir() returns 0 and prints nothing when the directory is absent", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  expect_false(dir.exists(work_dir))

  out <- utils::capture.output(n <- .clear_s1_work_dir(work_dir))

  expect_identical(n, 0L)
  expect_identical(out, character(0))
})

test_that(".clear_s1_work_dir() prints the label it is given", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  dir.create(work_dir, recursive = TRUE)
  writeLines("a", file.path(work_dir, "s1a_pre_enr1.qs2"))

  out <- utils::capture.output(
    .clear_s1_work_dir(work_dir, label = "Removed s1 work directory")
  )

  expect_match(
    out,
    "^Removed s1 work directory: 1 files in [0-9]+\\.[0-9] s$"
  )
})


# The REAL boundary. The success path is the one that ran for minutes with no
# line in the log, and only a completed s1 executes it. A unit test of the
# helper cannot see which call site the pipeline uses.
test_that("a completed s1 reports the work directory it removed", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )

  sk <- ttm_skeleton("A", n_persons = 400L, seed = 2026L)
  root <- withr::local_tempdir()
  dirs <- list(
    spec = file.path(root, "spec"),
    tteplan = file.path(root, "tteplan"),
    results = file.path(root, "results"),
    meta = file.path(root, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "ttms1clear",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)]
  )

  out <- utils::capture.output(
    plan$s1_generate_enrollments_and_ipw(
      n_workers = 1L,
      swereg_dev_path = ttm_dev_path()
    ),
    type = "output"
  )

  hit <- grep("^Removed s1 work directory: ", out, value = TRUE)
  expect_length(hit, 1L)
  # The count and the duration are the point, so assert both fields.
  expect_match(hit, "^Removed s1 work directory: [0-9]+ files in [0-9]+\\.[0-9] s$")
  # `hit[1]` is NA when the line is absent, so this reports a failure rather
  # than erroring inside expect_gt().
  n_files <- as.integer(sub("^.*directory: ([0-9]+) files.*$", "\\1", hit[1]))
  expect_gt(n_files, 0L)
  expect_false(dir.exists(.s1_work_dir(plan, ensure_exists = FALSE)))
})
