# `$get_estimates()` reports, per arm, the person-trials that a protocol
# deviation at time zero removed from the per-protocol analysis set.
#
# The fixture runs `$s3_analyze()` through the real batch runner, so the count
# reaches the plan through `.s3_ett_worker()` and the parent merge. The
# per-protocol analysis file carries a hand-built `$time_zero_deviation`
# record. In it, 3 intervention and 5 comparator person-trials deviate at time
# zero. The two arm counts differ, and neither equals the pooled 8.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("survey")

# One trial per person, 13 intervals of 4 weeks to week 52.
.tzd_panel <- function(n = 40L, seed = 11L) {
  set.seed(seed)
  rows <- lapply(seq_len(n), function(k) {
    arm <- k %% 2L == 1L
    ev <- stats::rbinom(13L, 1L, if (arm) 0.02 else 0.05)
    keep <- if (any(ev == 1L)) seq_len(which(ev == 1L)[1L]) else seq_len(13L)
    return(data.table::data.table(
      id = sprintf("p%04d", k),
      enrollment_person_trial_id = sprintf("p%04d_t1", k),
      rd_intervention = arm,
      tstart = (keep - 1L) * 4L,
      tstop = keep * 4L,
      event = ev[keep],
      person_weeks = 4,
      rd_age_continuous = 50 + k %% 10L
    ))
  })
  d <- data.table::rbindlist(rows)
  d[, analysis_weight_pp_trunc := 1]
  d[, analysis_weight_pp := 1]
  d[, ipw_trunc := 1]
  d[, ipw := 1]
  return(d)
}

# The record `s5_prepare_outcome()` writes: one row per person-trial at risk
# at time zero. The panel persons did not deviate. The extra person-trials
# deviated and so hold no row in the panel.
.tzd_record <- function(d) {
  base <- unique(d[, .(enrollment_person_trial_id, rd_intervention)])
  base[, rd_age_continuous := 55]
  base[, deviation_time_zero := 0L]
  extra <- data.table::data.table(
    enrollment_person_trial_id = sprintf("x%04d_t1", 1:8),
    rd_intervention = c(rep(TRUE, 3L), rep(FALSE, 5L)),
    rd_age_continuous = 55,
    deviation_time_zero = 1L
  )
  return(data.table::rbindlist(list(base, extra), use.names = TRUE))
}

.tzd_build <- function(dir) {
  d <- .tzd_panel()
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 52L
  )
  pp <- swereg::TTEEnrollment$new(d, design, data_level = "trial")
  pp$time_zero_deviation <- .tzd_record(d)
  qs2::qs_save(pp, file.path(dir, "analysis_52.qs2"))
  itt <- swereg::TTEEnrollment$new(d, design, data_level = "trial")
  qs2::qs_save(itt, file.path(dir, "analysis_itt_52.qs2"))
  qs2::qs_save(itt, file.path(dir, "raw_01.qs2"))
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_52.qs2",
    file_analysis_itt = "analysis_itt_52.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "tzd",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir
  plan$spec <- list(
    study = list(
      title = "Fixture",
      implementation = list(project_prefix = "tzd")
    ),
    enrollments = list(list(
      id = "01",
      name = "Enrollment one",
      treatment = list(
        arms = list(intervention = "Treated", comparator = "Untreated")
      )
    )),
    min_expected_events_per_arm_per_year = 0,
    period_width = 4L,
    admin_censoring_isoyearweek = "2020-52"
  )
  tte_stamp_stages(plan)
  return(plan)
}

# s3 runs once per file. Every block below reads this run.
.tzd_cache <- new.env()
.tzd_fixture <- function() {
  if (!is.null(.tzd_cache$fx)) {
    return(.tzd_cache$fx)
  }
  dir <- tempfile("tzd_s3_")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .tzd_build(dir)
  utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 2L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  record <- qs2::qs_read(file.path(dir, "analysis_52.qs2"))$time_zero_deviation
  .tzd_cache$fx <- list(plan = plan, record = record)
  return(.tzd_cache$fx)
}

.tzd_cols <- c(
  "n_trials_deviated_time_zero_int",
  "n_trials_deviated_time_zero_cmp"
)


test_that("per-protocol rows carry the time-zero deviation count of each arm", {
  fx <- .tzd_fixture()
  est <- fx$plan$get_estimates()
  pp <- est[estimand == "pp"]
  expect_identical(sort(pp$weights), c("truncated", "untruncated"))
  rec <- fx$record
  want_int <- sum(rec$deviation_time_zero[rec$rd_intervention %in% TRUE])
  want_cmp <- sum(rec$deviation_time_zero[rec$rd_intervention %in% FALSE])
  expect_identical(c(want_int, want_cmp), c(3L, 5L))
  expect_identical(pp$n_trials_deviated_time_zero_int, rep(3, 2L))
  expect_identical(pp$n_trials_deviated_time_zero_cmp, rep(5, 2L))
})

test_that("each arm counts its own deviations, not the pooled total", {
  fx <- .tzd_fixture()
  pp <- fx$plan$get_estimates()[estimand == "pp"]
  pooled <- sum(fx$record$deviation_time_zero)
  expect_identical(pooled, 8L)
  expect_true(all(pp$n_trials_deviated_time_zero_int < pooled))
  expect_true(all(pp$n_trials_deviated_time_zero_cmp < pooled))
  expect_identical(
    pp$n_trials_deviated_time_zero_int + pp$n_trials_deviated_time_zero_cmp,
    rep(8, 2L)
  )
})

test_that("intention-to-treat rows give NA for both counts", {
  fx <- .tzd_fixture()
  itt <- fx$plan$get_estimates()[estimand == "itt"]
  expect_identical(sort(itt$weights), c("truncated", "untruncated"))
  expect_identical(itt$n_trials_deviated_time_zero_int, rep(NA_real_, 2L))
  expect_identical(itt$n_trials_deviated_time_zero_cmp, rep(NA_real_, 2L))
})

test_that("a result stored before the count existed gives NA on every row", {
  fx <- .tzd_fixture()
  plan <- fx$plan$clone()
  res <- plan$results_ett
  expect_true("n_trials_deviated_time_zero" %in% names(res[["ETT00001"]]))
  res[["ETT00001"]][["n_trials_deviated_time_zero"]] <- NULL
  plan$results_ett <- res
  est <- plan$get_estimates()
  expect_identical(nrow(est), 4L)
  expect_true(all(is.na(est$n_trials_deviated_time_zero_int)))
  expect_true(all(is.na(est$n_trials_deviated_time_zero_cmp)))
})

test_that("the counts change no other estimates column", {
  fx <- .tzd_fixture()
  now <- fx$plan$get_estimates()
  plan <- fx$plan$clone()
  res <- plan$results_ett
  res[["ETT00001"]][["n_trials_deviated_time_zero"]] <- NULL
  plan$results_ett <- res
  before <- plan$get_estimates()
  other <- setdiff(names(now), .tzd_cols)
  expect_identical(now[, ..other], before[, ..other])
})
