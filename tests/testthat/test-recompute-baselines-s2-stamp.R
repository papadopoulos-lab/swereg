# `$recompute_baselines()` reads the analysis files of the ETTs of each named
# enrollment. It MUST refuse them when an ETT's s2 stamp names another s1 run
# than the plan's current one, as `$s3_analyze()` does.
#
# The plan is spr_report_fast_plan() of helper-sparse_report.R with one stored
# enrollment result that holds no panel. Such a result is not stale, so
# without the refusal the method skips it and reads no file.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

rbs_plan <- function(env = parent.frame()) {
  plan <- spr_report_fast_plan(env = env)
  plan$results_enrollment <- list(`01` = list(n_baseline = 100L))
  return(tte_stamp_stages(plan))
}


test_that("recompute_baselines refuses an ETT whose s2 stamp names an earlier s1 run", {
  plan <- rbs_plan()
  ett <- data.table::copy(plan$ett)
  data.table::set(
    ett,
    j = "s1_run_id",
    value = rep("an-earlier-s1-run", nrow(ett))
  )
  plan$ett <- ett
  expect_error(
    plan$recompute_baselines(output_dir = withr::local_tempdir()),
    paste0(
      "recompute_baselines(): s2 did not write the analysis files of 1 ",
      "ETT(s) after the current s1 run: ",
      plan$ett$ett_id[plan$ett$enrollment_id == "01"],
      ". Run $s2_generate_analysis_files_and_ipcw_pp(), then run this stage."
    ),
    fixed = TRUE
  )
})


test_that("recompute_baselines refuses an ETT that s2 is still writing", {
  plan <- rbs_plan()
  swereg:::.tte_s2_stamp(plan, NA_character_, NA_character_)
  expect_error(
    plan$recompute_baselines(output_dir = withr::local_tempdir()),
    "s2 did not write the analysis files",
    fixed = TRUE
  )
})


test_that("recompute_baselines runs when every stamp is current", {
  plan <- rbs_plan()
  expect_message(
    plan$recompute_baselines(output_dir = withr::local_tempdir()),
    "Baseline panels: recomputed 0, skipped 1 already current.",
    fixed = TRUE
  )
})
