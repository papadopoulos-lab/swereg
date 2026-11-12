# tables.xlsx is feasibility.xlsx plus the results.
#
# `$export_tables()` writes both workbooks from one plan, and
# `.write_feasibility_sheets()` writes the feasibility sheets of both. After
# Provenance and Summary, the sheets of tables.xlsx MUST begin with the
# sheets of feasibility.xlsx, in the same order. The results sheets follow.
# test-sparse-report-e2e.R checks the same order on a plan with a
# rare_condition_rule.

skip_if_not_installed("openxlsx")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

.wp_dir <- withr::local_tempdir(.local_envir = teardown_env())
.wp_tables <- file.path(.wp_dir, "tables.xlsx")
.wp_feas <- file.path(.wp_dir, "feasibility.xlsx")
suppressMessages(suppressWarnings(
  .xp_plan("new")$export_tables(path = .wp_tables)
))

test_that("tables.xlsx after Provenance and Summary begins with the sheets of feasibility.xlsx, in order", {
  tables <- openxlsx::getSheetNames(.wp_tables)
  feas <- openxlsx::getSheetNames(.wp_feas)
  expect_identical(tables[1:2], c("Provenance", "Summary"))
  rest <- tables[-(1:2)]
  expect_gt(length(feas), 0L)
  expect_gt(length(rest), length(feas))
  expect_identical(rest[seq_along(feas)], feas)
  # The fixture stores an adherence table, so the optional last sheet is
  # there, and the results sheets start after it.
  expect_identical(feas[length(feas)], "Protocol adherence")
  expect_identical(rest[length(feas) + 1L], "Target trial protocol")
})

test_that("each feasibility sheet of tables.xlsx holds the cells of feasibility.xlsx", {
  for (sh in openxlsx::getSheetNames(.wp_feas)) {
    expect_identical(
      .ff_cells(.wp_tables, sh),
      .ff_cells(.wp_feas, sh),
      info = sh
    )
  }
})
