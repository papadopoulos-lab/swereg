# `$recompute_baselines()` skips an enrollment whose cached panels are current.
#
# The method used to call `.s3_enrollment_worker()` for every enrollment it was
# handed. An export script documents the call as a no-op once the panels carry
# an SMD, and it was not one. A study with 42 enrollments paid 42 worker calls,
# and each call reads an analysis file off the registry share.
#
# swereg already owns the rule. `.baseline_panel_is_stale()` tests the SCHEMA of
# every present panel, and `$export_tables()` calls it. This file pins that
# `$recompute_baselines()` calls the same rule.
#
# Why schema and not values. A fresh `swereg_table1` carries `NA` in
# `smd_numeric` for N rows, sum-of-weights rows, continuation levels and missing
# rows. A predicate such as "the SMD column is wholly non-NA" would therefore
# call every valid panel stale and recompute it forever.
#
# Nothing here reads a registry file. The worker is mocked in every test. The
# analysis files exist as empty files, so a call count of 0 proves the skip and
# never a missing input.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")

# The five panel names `.baseline_panel_is_stale()` inspects.
.RBS_PANELS <- c(
  "table1_ipw_trunc",
  "table1_ipw_trunc_main",
  "table1_unweighted",
  "table1_ipw",
  "table1_raw"
)

# One empty file per analysis file and raw file the plan names. The method reads
# them to choose the smallest, and the worker is mocked, so empty is enough.
.rbs_touch_inputs <- function(plan, dir) {
  for (f in unique(c(plan$ett$file_analysis, plan$ett$file_raw))) {
    file.create(file.path(dir, f))
  }
  invisible(dir)
}

# A counter for the mocked worker. `n` is the call count and `ids` records which
# enrollment each call carried.
.rbs_calls <- function() {
  e <- new.env(parent = emptyenv())
  e$n <- 0L
  e$ids <- character(0)
  e
}

# Age one panel of one enrollment. The panel keeps the `swereg_table1` class and
# loses `smd_numeric`, which is the cache generation the helper was written for.
.rbs_age_one_panel <- function(plan, eid, panel = "table1_ipw") {
  p <- data.table::copy(plan$results_enrollment[[eid]][[panel]])
  p[, smd_numeric := NULL]
  plan$results_enrollment[[eid]][[panel]] <- p
  invisible(plan)
}

# Empty every panel of one enrollment, and keep the names. This is the result of
# an enrollment the worker never produced a panel for.
.rbs_empty_panels <- function(plan, eid) {
  r <- plan$results_enrollment[[eid]]
  for (nm in .RBS_PANELS) {
    r[nm] <- list(NULL)
  }
  plan$results_enrollment[[eid]] <- r
  invisible(plan)
}


test_that("a current result reaches the worker for no enrollment", {
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  .rbs_touch_inputs(plan, dir)

  # The fixture states the case. Both enrollments are current.
  expect_false(swereg:::.baseline_panel_is_stale(plan$results_enrollment[["01"]]))
  expect_false(swereg:::.baseline_panel_is_stale(plan$results_enrollment[["02"]]))

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(
        table1_unweighted = data.table::data.table(x = "recomputed"),
        n_baseline = 4242L
      )
    },
    .package = "swereg"
  )

  expect_message(
    plan$recompute_baselines(output_dir = dir),
    "recomputed 0, skipped 2 already current",
    fixed = TRUE
  )

  expect_identical(calls$n, 0L)
  expect_identical(calls$ids, character(0))

  # The cache is the one the fixture built, and not the worker's.
  expect_identical(plan$results_enrollment[["01"]]$n_baseline, 1000L)
  expect_identical(plan$results_enrollment[["02"]]$n_baseline, 640L)
})


test_that("one stale panel reaches the worker for that enrollment only", {
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  .rbs_touch_inputs(plan, dir)
  .rbs_age_one_panel(plan, "01")

  # One panel of enrollment 01 lost `smd_numeric`. The other four still carry
  # it, and every panel of enrollment 02 does.
  expect_true(swereg:::.baseline_panel_is_stale(plan$results_enrollment[["01"]]))
  expect_false(swereg:::.baseline_panel_is_stale(plan$results_enrollment[["02"]]))

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(
        table1_unweighted = data.table::data.table(x = "recomputed"),
        n_baseline = 4242L
      )
    },
    .package = "swereg"
  )

  suppressMessages(plan$recompute_baselines(output_dir = dir))

  expect_identical(calls$n, 1L)
  expect_identical(calls$ids, "01")

  # The worker's result replaced enrollment 01, and enrollment 02 is untouched.
  expect_identical(plan$results_enrollment[["01"]]$n_baseline, 4242L)
  expect_identical(plan$results_enrollment[["01"]]$table1_unweighted$x, "recomputed")
  expect_identical(plan$results_enrollment[["02"]]$n_baseline, 640L)
})


test_that("a result whose panels are all NULL reaches the worker", {
  # Absence is not staleness. A result with no panel has nothing to refresh, so
  # the method MUST NOT open an analysis file for it.
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  .rbs_touch_inputs(plan, dir)
  .rbs_empty_panels(plan, "01")
  .rbs_empty_panels(plan, "02")

  r1 <- plan$results_enrollment[["01"]]
  expect_true(all(vapply(r1[.RBS_PANELS], is.null, logical(1))))
  expect_false(swereg:::.baseline_panel_is_stale(r1))

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(n_baseline = 4242L)
    },
    .package = "swereg"
  )

  suppressMessages(plan$recompute_baselines(output_dir = dir))

  expect_identical(calls$n, 0L)
  expect_identical(calls$ids, character(0))
})


test_that("force = TRUE reaches the worker for a current result", {
  # The escape hatch. Use it when the worker changed and the schema did not.
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  .rbs_touch_inputs(plan, dir)

  expect_false(swereg:::.baseline_panel_is_stale(plan$results_enrollment[["01"]]))
  expect_false(swereg:::.baseline_panel_is_stale(plan$results_enrollment[["02"]]))

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(
        table1_unweighted = data.table::data.table(x = "recomputed"),
        n_baseline = 4242L
      )
    },
    .package = "swereg"
  )

  expect_message(
    plan$recompute_baselines(output_dir = dir, force = TRUE),
    "recomputed 2, skipped 0 already current",
    fixed = TRUE
  )

  expect_identical(calls$n, 2L)
  expect_identical(calls$ids, c("01", "02"))
  expect_identical(plan$results_enrollment[["01"]]$n_baseline, 4242L)
  expect_identical(plan$results_enrollment[["02"]]$n_baseline, 4242L)
})


test_that("enrollment_ids still names the enrollments, under the same rule", {
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  .rbs_touch_inputs(plan, dir)
  .rbs_age_one_panel(plan, "01")
  .rbs_age_one_panel(plan, "02")

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(n_baseline = 4242L)
    },
    .package = "swereg"
  )

  # Both enrollments are stale, and the caller asked for one of them.
  suppressMessages(
    plan$recompute_baselines(output_dir = dir, enrollment_ids = "01")
  )

  expect_identical(calls$n, 1L)
  expect_identical(calls$ids, "01")
  expect_identical(plan$results_enrollment[["02"]]$n_baseline, 640L)
})


test_that("enrollment_ids does not force a current enrollment to recompute", {
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  .rbs_touch_inputs(plan, dir)

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(n_baseline = 4242L)
    },
    .package = "swereg"
  )

  expect_message(
    plan$recompute_baselines(output_dir = dir, enrollment_ids = "01"),
    "recomputed 0, skipped 1 already current",
    fixed = TRUE
  )

  expect_identical(calls$n, 0L)

  # And `force` reaches the named enrollment, and only that one.
  suppressMessages(
    plan$recompute_baselines(
      output_dir = dir,
      enrollment_ids = "01",
      force = TRUE
    )
  )

  expect_identical(calls$n, 1L)
  expect_identical(calls$ids, "01")
  expect_identical(plan$results_enrollment[["02"]]$n_baseline, 640L)
})


test_that("the report names both counts on one line", {
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  .rbs_touch_inputs(plan, dir)
  .rbs_age_one_panel(plan, "01")

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(n_baseline = 4242L)
    },
    .package = "swereg"
  )

  msg <- testthat::capture_messages(plan$recompute_baselines(output_dir = dir))

  expect_length(msg, 1L)
  expect_match(
    msg[[1L]],
    "recomputed 1, skipped 1 already current",
    fixed = TRUE
  )
})


test_that("a current enrollment raises no missing-file warning", {
  # The warning belongs to the recompute path. A skipped enrollment never
  # reaches it. `dir` holds no analysis file at all, so the warning would fire
  # for every enrollment the method decided to recompute.
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()

  calls <- .rbs_calls()
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(enrollment_id, ...) {
      calls$n <- calls$n + 1L
      calls$ids <- c(calls$ids, enrollment_id)
      list(n_baseline = 4242L)
    },
    .package = "swereg"
  )

  expect_no_warning(suppressMessages(plan$recompute_baselines(output_dir = dir)))
  expect_identical(calls$n, 0L)

  # A stale enrollment does reach the check, so it warns. It counts as neither
  # recomputed nor skipped, and enrollment 02 is the one skipped enrollment.
  .rbs_age_one_panel(plan, "01")
  msg <- NULL
  expect_warning(
    msg <- testthat::capture_messages(plan$recompute_baselines(output_dir = dir)),
    "No analysis files found on disk for enrollment 01",
    fixed = TRUE
  )
  expect_identical(calls$n, 0L)
  expect_match(
    msg[[1L]],
    "recomputed 0, skipped 1 already current",
    fixed = TRUE
  )
})
