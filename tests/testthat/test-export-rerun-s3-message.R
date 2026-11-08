# A stored result without a baseline panel names the stage that writes one.
#
# s3 stores the baseline panels. The message of `$export_tables()` names s3,
# and not `$recompute_baselines()`. The plan fixture is `.xp_plan()` from
# helper-export_parity.R.

skip_if_not_installed("openxlsx")
skip_if_not_installed("data.table")
skip_if_not_installed("withr")

test_that("export_tables names s3 when the Table 1 enrollment has no IPW-truncated panel", {
  plan <- .xp_plan("new")
  plan$results_enrollment[["02"]]$table1_ipw_trunc <- NULL
  plan$results_enrollment[["02"]]$table1_ipw_trunc_main <- NULL
  path <- file.path(withr::local_tempdir(), "tables.xlsx")
  err <- expect_error(suppressMessages(suppressWarnings(
    plan$export_tables(path = path, table1_enrollment = "02")
  )))
  expect_match(
    conditionMessage(err),
    paste0(
      "Enrollment 02 has no stored IPW-truncated baseline panel, so Table 1 ",
      "cannot be drawn. Re-run s3 ($s3_analyze()), or name another ",
      "enrollment in `table1_enrollment`."
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    "recompute_baselines",
    conditionMessage(err),
    fixed = TRUE
  ))
})
