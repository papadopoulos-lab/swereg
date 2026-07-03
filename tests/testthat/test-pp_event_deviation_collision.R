# Regression: an outcome event that falls in the SAME band as the protocol
# deviation must count as an event, not be dropped as a censoring. Before the
# fix, s5_prepare_outcome flagged the collision row censor_this_period == 1 and
# s4_prepare_for_analysis deleted it, silently losing ~10% of PP events in
# switching-heavy data (probe: 368/3849 events at persist_coef = 2).

test_that("PP keeps an event that collides with protocol deviation in the same band", {
  n_band <- 4L
  ids <- 1:40
  long <- data.table::CJ(
    enrollment_person_trial_id = ids,
    tstop = seq_len(n_band)
  )
  long[, tstart := tstop - 1L]
  long[, treatment_baseline := enrollment_person_trial_id <= 20L]
  long[, time_treatment := treatment_baseline]
  long[, event := 0L]
  long[, person_weeks := 1L]
  long[, baseline_L0 := (enrollment_person_trial_id %% 5L) - 2]

  # id 1 (intervention): deviates at band 3 AND has the event at band 3
  long[
    enrollment_person_trial_id == 1L & tstop == 3L,
    `:=`(
      time_treatment = FALSE,
      event = 1L
    )
  ]
  long[enrollment_person_trial_id == 1L & tstop == 4L, time_treatment := FALSE]
  # id 2 (intervention): clean event at band 2, no deviation
  long[enrollment_person_trial_id == 2L & tstop == 2L, event := 1L]
  # id 3 (intervention): deviates at band 4, no event
  long[enrollment_person_trial_id == 3L & tstop == 4L, time_treatment := FALSE]
  # id 21 (comparator): clean event at band 3, no deviation
  long[enrollment_person_trial_id == 21L & tstop == 3L, event := 1L]
  # id 22 (comparator): deviates (starts treatment) at band 4, no event
  long[enrollment_person_trial_id == 22L & tstop == 4L, time_treatment := TRUE]

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = "baseline_L0",
    follow_up_time = n_band
  )
  trial <- TTEEnrollment$new(long, design)
  # toy deterministic data: the tiny censoring glm separates -> benign warning
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = n_band,
      estimate_ipcw_pp_with_gam = FALSE
    )
  })

  d <- trial$data
  # the collision event row survives and is an event
  expect_identical(
    d[enrollment_person_trial_id == 1L & tstop == 3L, event],
    1L
  )
  # all three events are retained (ids 1, 2, 21)
  expect_identical(sum(d$event), 3L)
  # censoring rows are gone, but never at the price of an event row
  expect_identical(sum(d$censor_this_period), 0L)
})

test_that("ITT is unaffected by the collision rule (no deviation censoring)", {
  n_band <- 4L
  long <- data.table::CJ(
    enrollment_person_trial_id = 1:40,
    tstop = seq_len(n_band)
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
    follow_up_time = n_band
  )
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = n_band,
    estimand = "itt"
  )
  expect_identical(sum(trial$data$event), 2L)
})
