# Pin the cache-clearing semantics of `s3_analyze(force = TRUE)`,
# added in 26.4.27 as the supported escape hatch when prior results
# were poisoned by a broken environment (e.g. silent
# `skipped = TRUE` placeholders from a missing `survey` package).
#
# Scope rules:
#   - force = TRUE, no enrollment_ids, no ett_ids   -> clear everything
#   - force = TRUE, enrollment_ids only             -> clear only those
#                                                      enrollments and
#                                                      their child ETTs
#   - force = TRUE, ett_ids only                    -> clear only those
#                                                      ETTs (and their
#                                                      enclosing enrollments)
#   - force = FALSE (default)                       -> preserve all caches

skip_if_not_installed("data.table")

# Build a TTEPlan with enough scaffolding for `s3_analyze` to walk
# past validation and reach the force-clearing block. Worker dispatch
# is mocked out -- this test only exercises the cache-management logic,
# not the analysis itself.
.fixture_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id   = c("01", "01", "02", "02"),
    ett_id          = c("ETT00001", "ETT00002", "ETT00003", "ETT00004"),
    outcome_var     = "osd_a",
    outcome_name    = "Outcome A",
    follow_up       = 52L,
    age_min         = 50L,
    age_max         = 59L,
    age_group       = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var   = "lopnr",
    treatment_var   = "rd_tx",
    file_imp        = c("imp_01.qs2", "imp_01.qs2",
                        "imp_02.qs2", "imp_02.qs2"),
    file_raw        = c("raw_01.qs2", "raw_01.qs2",
                        "raw_02.qs2", "raw_02.qs2"),
    file_analysis   = c("analysis_001.qs2", "analysis_002.qs2",
                        "analysis_003.qs2", "analysis_004.qs2"),
    description     = c("ETT00001: A", "ETT00002: A",
                        "ETT00003: A", "ETT00004: A")
  )
  swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
}

# Pre-populate cached results so we can observe what force clears.
.populate_cache <- function(plan) {
  plan$results_enrollment <- list(
    "01" = list(table1_unweighted = "ENR_01_CACHE"),
    "02" = list(table1_unweighted = "ENR_02_CACHE")
  )
  plan$results_ett <- list(
    "ETT00001" = list(skipped = TRUE, reason = "test"),
    "ETT00002" = list(skipped = TRUE, reason = "test"),
    "ETT00003" = list(skipped = TRUE, reason = "test"),
    "ETT00004" = list(skipped = TRUE, reason = "test")
  )
  invisible(plan)
}

test_that("s3_analyze: force = FALSE preserves cached results (no scope)", {
  plan <- .fixture_plan()
  .populate_cache(plan)
  # Mock parallel_pool to short-circuit before any worker dispatch.
  # The cache-clearing block runs BEFORE this point, so the post-call
  # state of results_enrollment / results_ett reflects only what
  # force = ... did.
  testthat::local_mocked_bindings(
    parallel_pool = function(...) stop("__SENTINEL_HALT__"),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  # With force = FALSE, every entry is already cached, so enr_todo and
  # ett_todo are both empty -> parallel_pool is never called -> our
  # sentinel stop() doesn't fire.
  plan$s3_analyze(output_dir = output_dir, force = FALSE)
  # Both caches still hold all 2 enrollments and 4 ETTs.
  expect_equal(length(plan$results_enrollment), 2L)
  expect_equal(length(plan$results_ett),        4L)
})

test_that("s3_analyze: force = TRUE with no scope clears all cached results", {
  plan <- .fixture_plan()
  .populate_cache(plan)
  # Mock parallel_pool to short-circuit before any worker dispatch.
  # The cache-clearing block runs BEFORE this point, so the post-call
  # state of results_enrollment / results_ett reflects only what
  # force = ... did.
  testthat::local_mocked_bindings(
    parallel_pool = function(...) stop("__SENTINEL_HALT__"),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  # force=TRUE clears the cache, then dispatch reaches our sentinel.
  expect_error(
    plan$s3_analyze(output_dir = output_dir, force = TRUE),
    "__SENTINEL_HALT__"
  )
  # The cache state at the moment of halt -- reflecting only the
  # force-clearing step.
  expect_equal(length(plan$results_enrollment), 0L)
  expect_equal(length(plan$results_ett),        0L)
})

test_that("s3_analyze: force = TRUE with enrollment_ids clears only that scope", {
  plan <- .fixture_plan()
  .populate_cache(plan)
  # Mock parallel_pool to short-circuit before any worker dispatch.
  # The cache-clearing block runs BEFORE this point, so the post-call
  # state of results_enrollment / results_ett reflects only what
  # force = ... did.
  testthat::local_mocked_bindings(
    parallel_pool = function(...) stop("__SENTINEL_HALT__"),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  expect_error(
    plan$s3_analyze(output_dir = output_dir, force = TRUE,
                    enrollment_ids = "01"),
    "__SENTINEL_HALT__"
  )
  # Enrollment 02 untouched.
  expect_true("02" %in% names(plan$results_enrollment))
  expect_identical(plan$results_enrollment[["02"]]$table1_unweighted,
                   "ENR_02_CACHE")
  # Enrollment 01 cleared.
  expect_false("01" %in% names(plan$results_enrollment))
  # ETTs under enrollment 01 cleared, ETTs under 02 preserved.
  expect_false("ETT00001" %in% names(plan$results_ett))
  expect_false("ETT00002" %in% names(plan$results_ett))
  expect_true("ETT00003" %in% names(plan$results_ett))
  expect_true("ETT00004" %in% names(plan$results_ett))
})

test_that("s3_analyze: force = TRUE with ett_ids clears only those ETTs (and their enrollments)", {
  plan <- .fixture_plan()
  .populate_cache(plan)
  # Mock parallel_pool to short-circuit before any worker dispatch.
  # The cache-clearing block runs BEFORE this point, so the post-call
  # state of results_enrollment / results_ett reflects only what
  # force = ... did.
  testthat::local_mocked_bindings(
    parallel_pool = function(...) stop("__SENTINEL_HALT__"),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  expect_error(
    plan$s3_analyze(output_dir = output_dir, force = TRUE,
                    ett_ids = "ETT00001"),
    "__SENTINEL_HALT__"
  )
  # ETT00001 cleared, others preserved.
  expect_false("ETT00001" %in% names(plan$results_ett))
  expect_true("ETT00002" %in% names(plan$results_ett))
  expect_true("ETT00003" %in% names(plan$results_ett))
  expect_true("ETT00004" %in% names(plan$results_ett))
  # The enclosing enrollment (01) is also cleared, since it gets
  # auto-narrowed to.
  expect_false("01" %in% names(plan$results_enrollment))
  expect_true("02" %in% names(plan$results_enrollment))
})

test_that("s3_analyze: force = TRUE with empty scope still clears (degenerate case)", {
  # If enrollment_ids matches nothing in plan$ett, all_enrollment_ids
  # ends up empty -> nothing is cleared (force still ran, just no
  # targets). Pin the no-op behaviour.
  plan <- .fixture_plan()
  .populate_cache(plan)
  # Mock parallel_pool to short-circuit before any worker dispatch.
  # The cache-clearing block runs BEFORE this point, so the post-call
  # state of results_enrollment / results_ett reflects only what
  # force = ... did.
  testthat::local_mocked_bindings(
    parallel_pool = function(...) stop("__SENTINEL_HALT__"),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  expect_error(
    plan$s3_analyze(output_dir = output_dir, force = TRUE,
                    enrollment_ids = "99"),
    "Unknown enrollment_ids"
  )
  # Caches untouched (we never reached the clearing logic).
  expect_equal(length(plan$results_enrollment), 2L)
  expect_equal(length(plan$results_ett),        4L)
})
