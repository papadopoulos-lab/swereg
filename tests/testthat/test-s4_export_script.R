# =============================================================================
# A committed export script runs top to bottom, and the suite runs it
# =============================================================================
# Every other check in this suite drives one internal function. That leaves one
# defect class open: an entry-point script that omits a call, or that passes the
# wrong argument. The suite stays green and the artefact never appears.
#
# `fixtures/s4_export_shape.R` is that entry point. This file evaluates it once
# against the `helper-export_parity.R` plan, then reads the files it wrote.
#
# The fixture is a SCRIPT, and this file never repeats what the script does. A
# copy of the export sequence here could not detect a script that omits a call,
# which is the whole reason the fixture exists.
#
# The script writes into `s4_dir_out`, which this file supplies.
# =============================================================================

skip_if_not_installed("openxlsx")
skip_if_not_installed("ggplot2")
skip_if_not_installed("patchwork")
skip_if_not_installed("data.table")
skip_if_not_installed("withr")
# The participant-flow exhibit and the workbook sidecars need the optional
# diagram stack. Without it the script writes no such image, and the inventory
# check below would fail for a reason the script does not own.
skip_if_not_installed("DiagrammeR")
skip_if_not_installed("DiagrammeRsvg")
skip_if_not_installed("rsvg")

# One run of the script, read by every test below.
.s4_dir <- withr::local_tempdir(.local_envir = teardown_env())
.s4_env <- new.env(parent = environment())
.s4_env$s4_dir_out <- .s4_dir
suppressMessages(suppressWarnings(
  source(testthat::test_path("fixtures", "s4_export_shape.R"), local = .s4_env)
))


# Bytes on disk, and 0 for a path that holds no file. `file.size()` returns `NA`
# for an absent file. `expect_gt(NA, 0)` then fails on the `NA`, and not on the
# size it names. This helper gives one expectation for both states.
.s4_wb_bytes <- function(p) if (file.exists(p)) file.size(p) else 0L


test_that("the script writes the supplement workbook", {
  # `$export_tables()` is the second of the two export calls. Remove that call
  # from `fixtures/s4_export_shape.R` and this expectation is the one that
  # fails. A workbook that exists and holds 0 bytes fails it too.
  expect_gt(.s4_wb_bytes(file.path(.s4_dir, "tables.xlsx")), 0L)
})


test_that("the script writes every numbered manuscript exhibit", {
  # `$export()` numbers each exhibit by its position in the manifest. The names
  # below are the four the manifest declares, in that order.
  want <- c(
    "01_fig1_consort.png",
    "02_table1_01.csv",
    "03_fig3_forest_itt.png",
    "04_fig4_forest_pp.png"
  )
  # One expectation per exhibit, through the same helper the workbook uses. It
  # covers the absent file and the 0-byte file, and it never compares against
  # `NA`. `expect_gt()` takes no `info` argument, so `label` names the exhibit.
  for (f in want) {
    expect_gt(.s4_wb_bytes(file.path(.s4_dir, f)), 0L, label = f)
  }
})


test_that("both risk-difference slots hold one finite number after the run", {
  # The risk difference moved into `$s3_analyze()`, so the export path formats
  # it and never computes it. An empty slot means the export path lost the
  # stored result. Presence alone does not show that: a skip envelope is a list
  # and is present.
  plan <- .s4_env$plan
  for (slot in c("rd_pp_trunc", "rd_itt")) {
    rd <- plan$results_ett[["ETT00001"]][[slot]]
    expect_true(data.table::is.data.table(rd), info = slot)
    expect_identical(nrow(rd), 1L, info = slot)
    expect_true(is.numeric(rd$rd), info = slot)
    expect_length(rd$rd, 1L)
    expect_true(is.finite(rd$rd), info = slot)
  }
})
