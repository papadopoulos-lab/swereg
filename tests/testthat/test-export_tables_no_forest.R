# =============================================================================
# `$export_tables()` writes no forest plot, and the protocol sheet is named
# =============================================================================
#
# The supplement workbook reports every emulated trial on the `PP results` and
# `ITT results` sheets. A forest image repeated a subset of those numbers, so
# `$export_tables()` no longer draws one. `$export()` still does, for a
# manuscript.
#
# Removing the images removed the only consumer of `featured_etts`, except one:
# it also decided which trial the `Target trial protocol` sheet documents. That
# pick is now the explicit `protocol_ett_id` argument.
#
# The plan fixture is `.xp_plan()` from `helper-export_parity.R`. It holds
# eight emulated trials over two enrollments.

skip_if_not_installed("openxlsx")
skip_if_not_installed("ggplot2")
skip_if_not_installed("patchwork")
skip_if_not_installed("data.table")
skip_if_not_installed("withr")

#' The table of contents, read off the Provenance sheet as a two-column frame
#' of sheet name and description. It occupies columns 4 to 6 of that sheet, and
#' `openxlsx::read.xlsx()` drops the empty leading columns unless it is told
#' not to, so pass `skipEmptyCols = FALSE`. Row 1 is the header.
.nf_read_toc <- function(path) {
  d <- openxlsx::read.xlsx(
    path,
    sheet = "Provenance",
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  d <- d[stats::complete.cases(d[, 5:6]), 5:6]
  d <- d[d[[1]] != "Name", , drop = FALSE]
  names(d) <- c("sheet", "description")
  rownames(d) <- NULL
  d
}

# One export, shared by the first two tests below. The other three each run
# their own export, because each needs a different `protocol_ett_id`.
.nf_dir <- withr::local_tempdir(.local_envir = teardown_env())
.nf_path <- file.path(.nf_dir, "tables.xlsx")
.nf_plan <- .xp_plan("new", subgroups = TRUE)
suppressMessages(suppressWarnings(
  .nf_plan$export_tables(path = .nf_path, protocol_ett_id = "ETT00003")
))
.nf_toc <- .nf_read_toc(.nf_path)


test_that("export_tables writes no forest sheet and no forest image", {
  sheets <- openxlsx::getSheetNames(.nf_path)
  expect_false(any(grepl("forest", sheets, ignore.case = TRUE)))

  files <- list.files(.nf_dir)
  expect_false(any(grepl("forest", files, ignore.case = TRUE)))
  # The workbook and the Love plot and CONSORT sidecars are still written, so
  # an empty directory cannot pass this test by accident.
  expect_true("tables.xlsx" %in% files)
  expect_true(any(grepl("love_plot\\.png$", files)))
  expect_true(any(grepl("consort_.*\\.png$", files)))

  # The table of contents lives on the Provenance sheet. It MUST NOT advertise
  # a sheet the workbook does not hold, so it names every sheet in workbook
  # order and then one extra row for the CONSORT sidecar files.
  expect_false(any(grepl("forest", .nf_toc$sheet, ignore.case = TRUE)))
  expect_identical(.nf_toc$sheet[seq_along(sheets)], sheets)
  expect_identical(
    .nf_toc$sheet[-seq_along(sheets)],
    "CONSORT sidecars (standalone files)"
  )
  # The same rule, stated as the rule and not as a position. Every name in the
  # table of contents is a sheet of this workbook, or the one sidecar row.
  expect_identical(
    setdiff(.nf_toc$sheet, c(sheets, "CONSORT sidecars (standalone files)")),
    character(0)
  )

  # And an enrollment the export SKIPS is what makes that rule bite. The
  # fixture's enrollment 02 carries a legacy attrition table, and one criterion
  # has no global row. `.attrition_overall()` therefore returns NULL. The
  # export writes no attrition sheet and renders no CONSORT sidecar.
  expect_identical(grep("^Attrition_", sheets, value = TRUE), "Attrition_01")
  expect_false(any(grepl("consort_02\\.png$", files)))
  expect_true(any(grepl("consort_01\\.png$", files)))
})


test_that("the protocol sheet documents the requested trial", {
  toc <- .nf_toc
  row <- toc[toc[[1]] == "Target trial protocol", ]
  expect_equal(nrow(row), 1L)
  expect_match(row[[2]], "ETT00003", fixed = TRUE)

  # ETT00003 belongs to enrollment 02, and the Table 1 enrollment is 01, so
  # this id cannot have come from the fallback.
  expect_equal(.nf_plan$ett$enrollment_id[.nf_plan$ett$ett_id == "ETT00003"], "02")
})


test_that("an absent protocol_ett_id falls back to the Table 1 enrollment", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "tables.xlsx")
  plan <- .xp_plan("new", subgroups = FALSE)
  suppressMessages(suppressWarnings(plan$export_tables(path = path)))

  toc <- .nf_read_toc(path)
  row <- toc[toc[[1]] == "Target trial protocol", ]
  # Enrollment 01 carries the most baseline observations, and ETT00001 is its
  # first trial in the grid.
  expect_match(row[[2]], "ETT00001", fixed = TRUE)
})


test_that("an unknown protocol_ett_id warns and falls back", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "tables.xlsx")
  plan <- .xp_plan("new", subgroups = FALSE)
  expect_warning(
    suppressMessages(plan$export_tables(path = path, protocol_ett_id = "ETT99999")),
    "protocol_ett_id is not an ETT id"
  )

  toc <- .nf_read_toc(path)
  row <- toc[toc[[1]] == "Target trial protocol", ]
  expect_match(row[[2]], "ETT00001", fixed = TRUE)
})


test_that("export_tables no longer accepts the forest arguments", {
  args <- names(formals(swereg::TTEPlan$public_methods$export_tables))
  expect_identical(
    args,
    c("path", "table1_enrollment", "protocol_ett_id", "output_dir")
  )
})
