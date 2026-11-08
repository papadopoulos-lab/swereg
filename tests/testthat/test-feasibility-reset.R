# A rerun of s1 or s2 clears the stored result of `$s3_feasibility()`.
#
# `$s3_feasibility()` reads the s2 analysis files, and s1 and s2 rewrite them.
# So each of the two stages sets `plan$feasibility` to NULL, as it drops the
# stored s3 results. The fixture is the s1 -> s2 -> s3 run of
# `spr_report_pipeline()` in helper-sparse_report.R.

skip_on_cran()
skip_if_not_installed("openxlsx")
skip_if_not_installed("qs2")
skip_if_not_installed("survey")

.fr_cache <- new.env()
.fr_fixture <- function() {
  if (is.null(.fr_cache$run)) {
    .fr_cache$run <- spr_report_pipeline()
    withr::defer(
      unlink(.fr_cache$run$root, recursive = TRUE),
      envir = testthat::teardown_env()
    )
  }
  return(.fr_cache$run)
}

.fr_feasibility <- function(plan) {
  invisible(utils::capture.output(plan$s3_feasibility(
    n_workers = 1L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  return(invisible(plan))
}

test_that("a rerun of s2 after s3_feasibility leaves plan$feasibility NULL", {
  plan <- .fr_fixture()$plan
  .fr_feasibility(plan)
  expect_false(is.null(plan$feasibility))
  suppressWarnings(invisible(utils::capture.output(
    plan$s2_generate_analysis_files_and_ipcw_pp(
      n_workers = 1L,
      swereg_dev_path = swereg:::.swereg_dev_path()
    )
  )))
  expect_null(plan$feasibility)
})

test_that("a rerun of s1 after s3_feasibility leaves plan$feasibility NULL", {
  plan <- .fr_fixture()$plan
  .fr_feasibility(plan)
  expect_false(is.null(plan$feasibility))
  suppressWarnings(invisible(utils::capture.output(
    plan$s1_generate_enrollments_and_ipw(
      n_workers = 1L,
      swereg_dev_path = swereg:::.swereg_dev_path(),
      check_skeletons = FALSE
    )
  )))
  expect_null(plan$feasibility)
})
