# Per-protocol follow-up stops at the left edge of the deviation week.
#
# For tolerance `k`, follow-up stops at the start of the `(k + 1)`th
# consecutive discordant week. That week is not adherent follow-up, so an
# outcome in it or later does not count. TrialEmulation (`expand_until_switch()`
# keeps rows to `first_switch - 1`) and Danaei et al. (2013) use this rule, and
# swereg uses it for an observation gap.
#
# Releases 26.9.0 to 26.15.0 stopped at the right edge of the deviation week.
# Releases 26.7.3 to 26.15.0 counted an event later in the same follow-up
# interval. This file pins the left edge, the end of that override, the
# fallback read, and the rule for a tie with every other boundary.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

.le_pw <- 4L
.le_n_fu <- 12L

# Consecutive ISO year-weeks that start on a follow-up interval boundary. Under
# `period_width = 4` the first four make the enrollment period, and the rest
# are follow-up weeks.
.le_weeks <- function(n_weeks = 16L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .le_pw == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# One person, one row per week. `discordant_fu`, `event_fu` and `absent_fu`
# name 1-indexed follow-up weeks. Follow-up week `f` is `[f - 1, f)` on the
# analysis scale, so its left edge is `f - 1`.
.le_person <- function(
  id,
  weeks,
  arm,
  discordant_fu = integer(0),
  event_fu = integer(0),
  absent_fu = integer(0)
) {
  n <- length(weeks)
  fu <- seq_len(n) - .le_pw
  on_tx <- rep(arm, n)
  on_tx[fu %in% discordant_fu] <- !arm
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = rep(arm, n),
    eligible = seq_len(n) <= .le_pw,
    died = fu %in% event_fu,
    on_tx = on_tx,
    age = 50 + seq_len(n)
  )
  d[!(fu %in% absent_fu)]
}

# Concordant fillers for the propensity model. A ratio of 2 requests more
# comparators than the fixture holds, so every comparator is drawn.
.le_fillers <- function(weeks) {
  data.table::rbindlist(c(
    lapply(1:8, function(i) .le_person(paste0("FI", i), weeks, arm = TRUE)),
    lapply(1:12, function(i) .le_person(paste0("FC", i), weeks, arm = FALSE))
  ))
}

.le_design <- function(intervention_k = 0L) {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = .le_n_fu,
    period_width = .le_pw,
    intervention_tolerance_weeks = intervention_k,
    comparator_tolerance_weeks = 3L
  )
}

.le_enroll <- function(d, design) {
  TTEEnrollment$new(
    data = data.table::copy(d),
    design = design,
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
}

# The small fixtures make the censoring model separate and warn. The warning
# is about the toy data, and not about the boundary.
.le_prepare <- function(trial, follow_up = .le_n_fu) {
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = follow_up,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  trial$data
}

.le_rows <- function(d, who) {
  d[id == who][order(tstart)]
}

# A trial-level panel built outside `enroll()`. It has no weekly sequence, so
# `s5_prepare_outcome()` reads the deviation from the collapsed treatment
# value. Person-trials 1 to 20 are initiators and 21 to 40 are comparators.
.le_trial_panel <- function(width, n_interval) {
  d <- data.table::CJ(
    enrollment_person_trial_id = 1:40,
    tstop = seq_len(n_interval) * width
  )
  d[, tstart := tstop - width]
  d[, exposed := enrollment_person_trial_id <= 20L]
  d[, on_tx := exposed]
  d[, died := 0L]
  d[, person_weeks := width]
  d[, age := (enrollment_person_trial_id %% 5L) - 2]
  d[]
}

.le_trial_prepare <- function(d, follow_up) {
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = follow_up
  )
  trial <- TTEEnrollment$new(d, design)
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = follow_up,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  trial$data
}


test_that("a deviation in week 6 stops follow-up at week 5, the left edge", {
  weeks <- .le_weeks()
  d <- data.table::rbindlist(list(
    .le_person("DEV6", weeks, arm = TRUE, discordant_fu = 6L),
    .le_fillers(weeks)
  ))
  trial <- .le_enroll(d, .le_design(intervention_k = 0L))

  # `enroll()` writes the boundary. Follow-up week 6 is `[5, 6)`.
  expect_identical(
    unique(trial$data[id == "DEV6"]$weeks_to_protocol_deviation),
    5L
  )

  got <- .le_rows(.le_prepare(trial), "DEV6")
  expect_identical(got$tstop, c(4L, 5L))
  expect_identical(got$person_weeks, c(4L, 1L))
  expect_identical(got$censor_this_period, c(0L, 1L))
})

test_that("an event in the deviation week or later in its interval does not count", {
  weeks <- .le_weeks()
  # Each woman is discordant in follow-up week 6, so follow-up stops at week 5.
  # Follow-up interval 2 is `[4, 8)`, so weeks 6 and 7 are in the same
  # follow-up interval as the deviation.
  d <- data.table::rbindlist(list(
    .le_person("EV6", weeks, arm = TRUE, discordant_fu = 6L, event_fu = 6L),
    .le_person("EV7", weeks, arm = TRUE, discordant_fu = 6L, event_fu = 7L),
    .le_fillers(weeks)
  ))
  out <- .le_prepare(.le_enroll(d, .le_design(intervention_k = 0L)))

  for (who in c("EV6", "EV7")) {
    got <- .le_rows(out, who)
    expect_identical(got$tstop, c(4L, 5L), info = who)
    expect_identical(got$event, c(0L, 0L), info = who)
    expect_identical(got$censor_this_period, c(0L, 1L), info = who)
  }
  expect_identical(unique(.le_rows(out, "EV6")$weeks_to_event), 6L)
  expect_identical(unique(.le_rows(out, "EV7")$weeks_to_event), 7L)
})

test_that("an event in the week before the deviation counts, and is not censored", {
  weeks <- .le_weeks()
  # EV5 has the outcome in follow-up week 5, `[4, 5)`, and deviates in week 6.
  # The event and the deviation give the same boundary, week 5. The event
  # labels the stop.
  d <- data.table::rbindlist(list(
    .le_person("EV5", weeks, arm = TRUE, discordant_fu = 6L, event_fu = 5L),
    .le_fillers(weeks)
  ))
  got <- .le_rows(
    .le_prepare(.le_enroll(d, .le_design(intervention_k = 0L))),
    "EV5"
  )

  expect_identical(unique(got$weeks_to_event), 5L)
  expect_identical(unique(got$weeks_to_protocol_deviation), 5L)
  expect_identical(got$tstop, c(4L, 5L))
  expect_identical(got$event, c(0L, 1L))
  expect_identical(got$censor_this_period, c(0L, 0L))
})

test_that("tolerance 1 stops follow-up at the left edge of the second discordant week", {
  weeks <- .le_weeks()
  # RUN2 is discordant in follow-up weeks 6 and 7. Tolerance 1 allows the
  # first, so follow-up stops at the left edge of week 7, which is week 6.
  # ONE is discordant in week 6 only, which tolerance 1 allows.
  d <- data.table::rbindlist(list(
    .le_person("RUN2", weeks, arm = TRUE, discordant_fu = 6:7),
    .le_person("ONE", weeks, arm = TRUE, discordant_fu = 6L),
    .le_fillers(weeks)
  ))
  out <- .le_prepare(.le_enroll(d, .le_design(intervention_k = 1L)))

  run2 <- .le_rows(out, "RUN2")
  expect_identical(unique(run2$weeks_to_protocol_deviation), 6L)
  expect_identical(run2$tstop, c(4L, 6L))
  expect_identical(run2$censor_this_period, c(0L, 1L))

  one <- .le_rows(out, "ONE")
  expect_identical(unique(one$weeks_to_protocol_deviation), NA_integer_)
  expect_identical(one$tstop, c(4L, 8L, 12L))
})

test_that("a deviation at the planned end does not censor the last row", {
  # Follow-up week 7 is `[6, 7)`. A deviation in it stops at week 6, which is
  # also the requested end. The discordant week falls after follow-up, so the
  # last row is complete follow-up.
  weeks <- .le_weeks()
  d <- data.table::rbindlist(list(
    .le_person("AFTER6", weeks, arm = TRUE, discordant_fu = 7L),
    .le_fillers(weeks)
  ))
  got <- .le_rows(
    .le_prepare(.le_enroll(d, .le_design(intervention_k = 0L)), follow_up = 6L),
    "AFTER6"
  )
  expect_identical(unique(got$weeks_to_protocol_deviation), 6L)
  expect_identical(got$tstop, c(4L, 6L))
  expect_identical(got$censor_this_period, c(0L, 0L))

  # The same holds at the end of the panel. Follow-up week 13 is `[12, 13)`,
  # the first week after 12 weeks of follow-up.
  weeks17 <- .le_weeks(17L)
  d17 <- data.table::rbindlist(list(
    .le_person("AFTER12", weeks17, arm = TRUE, discordant_fu = 13L),
    .le_fillers(weeks17)
  ))
  got17 <- .le_rows(
    .le_prepare(.le_enroll(d17, .le_design(intervention_k = 0L))),
    "AFTER12"
  )
  expect_identical(unique(got17$weeks_to_protocol_deviation), 12L)
  expect_identical(got17$tstop, c(4L, 8L, 12L))
  expect_identical(got17$censor_this_period, c(0L, 0L, 0L))
})

test_that("a deviation in the first follow-up week gives zero rows and no error", {
  weeks <- .le_weeks()
  # ZERO is discordant in follow-up week 1, `[0, 1)`. Follow-up stops at
  # time zero, so she contributes no person-time.
  d <- data.table::rbindlist(list(
    .le_person("ZERO", weeks, arm = TRUE, discordant_fu = 1L),
    .le_fillers(weeks)
  ))
  trial <- .le_enroll(d, .le_design(intervention_k = 0L))
  expect_identical(
    unique(trial$data[id == "ZERO"]$weeks_to_protocol_deviation),
    0L
  )

  out <- NULL
  expect_no_error(out <- .le_prepare(trial))
  expect_identical(nrow(out[id == "ZERO"]), 0L)
  expect_true(all(out$person_weeks > 0L))
})

test_that("a gap and a deviation at the same boundary are labelled loss", {
  # Person-trial 1 is discordant in follow-up interval 3, `[2, 3)`, and her
  # `weeks_to_observation_gap` is also week 2. Person-trial 3 has the same
  # deviation and no gap.
  d <- .le_trial_panel(width = 1L, n_interval = 4L)
  d[enrollment_person_trial_id %in% c(1L, 3L) & tstop == 3L, on_tx := FALSE]
  d[, weeks_to_observation_gap := NA_integer_]
  d[enrollment_person_trial_id == 1L, weeks_to_observation_gap := 2L]
  out <- .le_trial_prepare(d, follow_up = 4L)

  both <- out[enrollment_person_trial_id == 1L][order(tstop)]
  expect_identical(unique(both$weeks_to_protocol_deviation), 2L)
  expect_identical(unique(both$weeks_to_loss), 2L)
  expect_identical(both$tstop, c(1L, 2L))
  expect_identical(both$censor_this_period, c(0L, 1L))

  dev_only <- out[enrollment_person_trial_id == 3L][order(tstop)]
  expect_identical(unique(dev_only$weeks_to_protocol_deviation), 2L)
  expect_identical(unique(dev_only$weeks_to_loss), NA_integer_)
  expect_identical(dev_only$censor_this_period, c(0L, 1L))
})

test_that("the fallback read stops at the start of the first discordant interval", {
  # Three four-week follow-up intervals: `[0, 4)`, `[4, 8)` and `[8, 12)`.
  # Person-trial 1 is discordant in the second, so follow-up stops at its
  # `tstart`, week 4, and the first interval carries the censoring.
  d <- .le_trial_panel(width = 4L, n_interval = 3L)
  d[enrollment_person_trial_id == 1L & tstart == 4L, on_tx := FALSE]
  expect_false("weeks_to_protocol_deviation" %in% names(d))
  out <- .le_trial_prepare(d, follow_up = 12L)

  got <- out[enrollment_person_trial_id == 1L][order(tstop)]
  expect_identical(unique(got$weeks_to_protocol_deviation), 4L)
  expect_identical(got$tstop, 4L)
  expect_identical(got$censor_this_period, 1L)
})
