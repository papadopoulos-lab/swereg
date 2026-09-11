# `$recompute_baselines()` keeps the fields the s3 worker does not return.
#
# The method replaces `results_enrollment[[eid]]` with what
# `.s3_enrollment_worker()` returns. The worker builds the baseline tables and
# nothing else, so every other field of the stored result would go with the
# replacement. `fill_summary` is one of those: s1 measures it, s3 never sees
# it, and `$export_tables()` reads it for the "Table S1 Missing data" sheet.
#
# A refresh that drops it costs the supplement a table and reports no missing
# data where the fill supplied thousands of values.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")

# One analysis file per emulated trial of the enrollment, plus the raw file.
# The method reads them only to choose the smallest, and the worker is mocked,
# so an empty file is enough.
.rbc_touch_inputs <- function(plan, dir) {
  files <- c(plan$ett$file_analysis, plan$ett$file_raw)
  for (f in unique(files)) {
    file.create(file.path(dir, f))
  }
  invisible(dir)
}


test_that("a recompute keeps fill_summary when the worker does not return it", {
  plan <- .xp_plan("new", fill_summary = TRUE)
  dir <- withr::local_tempdir()
  .rbc_touch_inputs(plan, dir)

  before <- plan$results_enrollment[["01"]]$fill_summary
  expect_s3_class(before, "data.table")
  expect_gt(nrow(before), 0L)

  # What the s3 worker returns: baseline tables and counts, no fill summary.
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(...) {
      list(
        table1_unweighted = data.table::data.table(x = "recomputed"),
        n_baseline = 4242L
      )
    },
    .package = "swereg"
  )

  plan$recompute_baselines(output_dir = dir, enrollment_ids = "01")

  after <- plan$results_enrollment[["01"]]

  # The recompute happened: the worker's own fields are the ones now stored.
  expect_identical(after$n_baseline, 4242L)
  expect_identical(after$table1_unweighted$x, "recomputed")

  # And the field the worker never saw survived it, unchanged.
  expect_identical(after$fill_summary, before)
})


test_that("a recompute keeps the other enrollment's fill_summary untouched", {
  # `enrollment_ids = "01"` must not reach enrollment 02 at all.
  plan <- .xp_plan("new", fill_summary = TRUE)
  dir <- withr::local_tempdir()
  .rbc_touch_inputs(plan, dir)

  before_02 <- plan$results_enrollment[["02"]]$fill_summary

  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(...) list(n_baseline = 4242L),
    .package = "swereg"
  )

  plan$recompute_baselines(output_dir = dir, enrollment_ids = "01")

  expect_identical(plan$results_enrollment[["02"]]$fill_summary, before_02)
  expect_identical(plan$results_enrollment[["02"]]$n_baseline, 640L)
})
