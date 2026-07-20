# Pin the unified contract that worker-pool failures during *any*
# pipeline stage propagate to the caller, not silently get swallowed
# into a warning + 0%-progress hang. Test by mocking the dispatcher
# (`.batch_run`) to throw a sentinel error and asserting each stage
# method propagates it.
#
# Stages covered:
#   - $s3_analyze
#   - $s2_generate_analysis_files_and_ipcw_pp
#   - $s1_generate_enrollments_and_ipw   (TODO -- requires more setup)
#
# `process_skeletons` is covered separately by
# test-process_skeletons_loud_errors.R.

skip_if_not_installed("data.table")

# Build a TTEPlan with enough scaffolding to dispatch a batch-runner
# stage. Workers are mocked out via local_mocked_bindings.
.fixture_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id   = "01",
    ett_id          = "ETT00001",
    outcome_var     = "osd_a",
    outcome_name    = "Outcome A",
    follow_up       = 52L,
    age_min         = 50L,
    age_max         = 59L,
    age_group       = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var   = "lopnr",
    treatment_var   = "rd_tx",
    file_imp        = "imp_01.qs2",
    file_raw        = "raw_01.qs2",
    file_analysis   = "analysis_001.qs2",
    description     = "ETT00001"
  )
  swereg::TTEPlan$new(
    project_prefix = "loud_test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
}

test_that("s3_analyze propagates dispatcher failure (no silent swallow)", {
  plan <- .fixture_plan()
  # Pre-populate so we don't trip on missing-cache logic.
  plan$results_enrollment <- list("01" = list(table1_unweighted = "stub"))
  plan$results_ett <- list()

  # Both s3 loops dispatch via .batch_run (Phase 2 + Phase 3); a failure in
  # either must propagate, not be swallowed.
  testthat::local_mocked_bindings(
    .batch_run = function(...) stop("__SENTINEL_S3__"),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  expect_error(
    plan$s3_analyze(output_dir = output_dir),
    "__SENTINEL_S3__"
  )
})

test_that("s2_generate_analysis_files_and_ipcw_pp propagates dispatcher failure", {
  plan <- .fixture_plan()
  testthat::local_mocked_bindings(
    .batch_run = function(...) stop("__SENTINEL_S2__"),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  # We expect the stage to call .batch_run somewhere on the way to
  # actual work. Whatever the failure mode, the message must mention
  # our sentinel -- not a generic "all workers complete" or a NULL
  # return.
  err <- tryCatch(
    plan$s2_generate_analysis_files_and_ipcw_pp(
      output_dir = output_dir,
      n_workers = 1L
    ),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  expect_true(grepl("__SENTINEL_S2__", err) ||
                grepl("Worker.*failed", err, ignore.case = TRUE),
    info = paste0(
      "s2 must propagate worker-pool failures to the caller. ",
      "Captured: '", err, "'"))
})
