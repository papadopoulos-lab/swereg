# Production-boundary proof (Phase 2 enrollment loop; Phase 3 ETT loop): drive
# BOTH of s3_analyze's loops through the REAL path -- .batch_run -> the generic
# inst/batch_worker.R subprocess -> .s3_enrollment_worker / .s3_ett_worker ->
# result envelope -> collection -> the plan's results_enrollment / results_ett --
# and assert both a real success AND a real failure. Fixtures and mocked
# dispatchers cannot substitute for this: they were exactly what the Phase-1
# sign-off carry-forward said was insufficient.
#
# Reuses the truth-matrix DGP helpers (ttm_skeleton / ttm_write_spec /
# ttm_dev_path from helper-tteplan_truth.R). Slow (a full s1->s2->s3 on a small
# synthetic cohort), so it is opt-out on CRAN like the truth matrix itself.

.batch_prod_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "inst", "batch_worker.R")),
    "package source tree not available"
  )
}

.batch_prod_plan <- function() {
  sk <- ttm_skeleton("A", n_persons = 2500L, seed = 2026L)
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dirs <- list(
    spec = file.path(root, "spec"),
    tteplan = file.path(root, "tteplan"),
    results = file.path(root, "results"),
    meta = file.path(root, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  global_max <- sk[, max(isoyearweek, na.rm = TRUE)]
  ttm_write_spec(file.path(dirs$spec, "spec_v001.yaml"), "ttmprod",
    "rd_age_continuous")

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = global_max
  )
  dev_path <- ttm_dev_path()
  invisible(utils::capture.output(
    {
      plan$s1_generate_enrollments_and_ipw(n_workers = 1L, swereg_dev_path = dev_path)
      plan$s2_generate_analysis_files_and_ipcw_pp(n_workers = 1L, swereg_dev_path = dev_path)
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path)
    },
    type = "output"
  ))
  list(plan = plan, dev_path = dev_path)
}

test_that("s3_analyze enrollment loop produces REAL baseline results and forwards arm_labels through the production boundary", {
  .batch_prod_skip()
  built <- .batch_prod_plan()
  plan <- built$plan

  # The enrollment loop ran through .batch_run and stored a real, well-formed
  # result -- not NULL, not garbage. (The old truth-matrix tests only checked
  # results_ett / IRRs, so the enrollment loop could have returned NULL and
  # stayed green; this closes that gap.)
  expect_gt(length(plan$results_enrollment), 0L)
  eid <- names(plan$results_enrollment)[[1]]
  r <- plan$results_enrollment[[eid]]
  expect_false(is.null(r$table1_unweighted))
  expect_gt(r$n_baseline, 0L)

  # arm_labels FORWARDED end to end through the generic worker. This is the
  # historic bug (worker_s3_enrollment.R silently dropped arm_labels): under it,
  # r$arm_labels would be NULL despite the spec defining arms. The spec sets
  # arms = list(intervention = "Treated", comparator = "Control").
  expect_identical(r$arm_labels,
    c(comparator = "Control", intervention = "Treated"))

  # The ETT loop (Phase 3 migration) ran through the SAME real path --
  # .batch_run -> generic worker -> .s3_ett_worker -> collected results -- and
  # stored real, well-formed per-ETT results. `summary` and `irr_pp_trunc` are
  # the keys results_summary() itself reads, so their presence is what "the
  # pipeline can use this" actually means. Had the migrated item builder
  # dropped an optional formal (subgroup_var) the parent validation would have
  # stopped the whole run before any of this existed.
  expect_gt(length(plan$results_ett), 0L)
  r_ett <- plan$results_ett[[1L]]
  expect_false(is.null(r_ett$summary))
  expect_false(is.null(r_ett$irr_pp_trunc))
})

test_that("a REAL worker failure in the enrollment loop propagates out of s3_analyze (no silent swallow)", {
  .batch_prod_skip()
  built <- .batch_prod_plan()
  plan <- built$plan

  # Corrupt every analysis file the enrollment loop could read, then force a
  # re-run. .s3_enrollment_worker's qs2_read() will throw in the subprocess; that
  # error must travel back through the generic worker's error envelope and
  # .batch_run and make s3_analyze RAISE -- not return quietly with a NULL result.
  afiles <- file.path(plan$dir_tteplan, unique(plan$ett$file_analysis))
  afiles <- afiles[file.exists(afiles)]
  expect_gt(length(afiles), 0L)
  for (f in afiles) writeLines("this is not a qs2 file", f)

  expect_error(
    suppressWarnings(utils::capture.output(
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = built$dev_path, force = TRUE),
      type = "output"
    )),
    "item|returned an error|unreadable|no result"
  )
})
