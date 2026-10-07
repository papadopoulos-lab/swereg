# An outcome event in the SAME follow-up interval as the protocol deviation.
#
# Per-protocol follow-up stops at the start of the deviation interval, so the
# deviation interval is not adherent follow-up. An event in it does not count,
# and the interval before it carries the censoring. This is the rule of
# TrialEmulation and of Danaei et al. (2013). Releases 26.7.3 to 26.15.0
# counted that event instead.
#
# These panels are built outside `enroll()`, so `s5_prepare_outcome()` reads
# the deviation from the collapsed treatment value at `tstart`.

test_that("PP does not count an event in the deviation follow-up interval", {
  n_interval <- 4L
  ids <- 1:40
  long <- data.table::CJ(
    enrollment_person_trial_id = ids,
    tstop = seq_len(n_interval)
  )
  long[, tstart := tstop - 1L]
  long[, treatment_baseline := enrollment_person_trial_id <= 20L]
  long[, time_treatment := treatment_baseline]
  long[, event := 0L]
  long[, person_weeks := 1L]
  long[, baseline_L0 := (enrollment_person_trial_id %% 5L) - 2]

  # id 1 (intervention): deviates at follow-up interval 3 AND has the event at
  # follow-up interval 3
  long[
    enrollment_person_trial_id == 1L & tstop == 3L,
    `:=`(
      time_treatment = FALSE,
      event = 1L
    )
  ]
  long[enrollment_person_trial_id == 1L & tstop == 4L, time_treatment := FALSE]
  # id 2 (intervention): clean event at follow-up interval 2, no deviation
  long[enrollment_person_trial_id == 2L & tstop == 2L, event := 1L]
  # id 3 (intervention): deviates at follow-up interval 4, no event
  long[enrollment_person_trial_id == 3L & tstop == 4L, time_treatment := FALSE]
  # id 21 (comparator): clean event at follow-up interval 3, no deviation
  long[enrollment_person_trial_id == 21L & tstop == 3L, event := 1L]
  # id 22 (comparator): deviates (starts treatment) at follow-up interval 4, no
  # event
  long[enrollment_person_trial_id == 22L & tstop == 4L, time_treatment := TRUE]

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = "baseline_L0",
    follow_up_time = n_interval
  )
  trial <- TTEEnrollment$new(long, design)
  # toy deterministic data: the tiny censoring glm separates -> benign warning
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = n_interval,
      estimate_ipcw_pp_with_gam = FALSE
    )
  })

  d <- trial$data
  # id 1 stops at the start of follow-up interval 3, so her event in it is not
  # counted and follow-up interval 2 carries her censoring.
  id1 <- d[enrollment_person_trial_id == 1L][order(tstop)]
  expect_identical(id1$tstop, c(1L, 2L))
  expect_identical(id1$event, c(0L, 0L))
  expect_identical(id1$censor_this_period, c(0L, 1L))
  # only the two events without a deviation are retained (ids 2, 21)
  expect_identical(sum(d$event), 2L)
  # censoring rows are retained, and no censoring row is an event row
  expect_gt(sum(d$censor_this_period), 0L)
  expect_identical(sum(d[censor_this_period == 1L]$event), 0L)
  # id 3 deviates in follow-up interval 4 with no event, so follow-up interval
  # 3 carries her censoring and follow-up interval 4 is gone
  id3 <- d[enrollment_person_trial_id == 3L][order(tstop)]
  expect_identical(id3$tstop, c(1L, 2L, 3L))
  expect_identical(id3$censor_this_period, c(0L, 0L, 1L))
})

test_that("ITT counts an event in a deviation follow-up interval", {
  n_interval <- 4L
  long <- data.table::CJ(
    enrollment_person_trial_id = 1:40,
    tstop = seq_len(n_interval)
  )
  long[, tstart := tstop - 1L]
  long[, treatment_baseline := enrollment_person_trial_id <= 20L]
  long[, time_treatment := treatment_baseline]
  long[, event := 0L]
  long[, person_weeks := 1L]
  long[, baseline_L0 := (enrollment_person_trial_id %% 5L) - 2]
  long[
    enrollment_person_trial_id == 1L & tstop == 3L,
    `:=`(
      time_treatment = FALSE,
      event = 1L
    )
  ]
  long[enrollment_person_trial_id == 2L & tstop == 2L, event := 1L]

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = "baseline_L0",
    follow_up_time = n_interval
  )
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = n_interval,
    estimand = "itt"
  )
  expect_identical(sum(trial$data$event), 2L)
})
