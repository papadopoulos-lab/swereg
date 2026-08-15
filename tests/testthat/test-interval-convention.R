# The interval convention, pinned.
#
# Every interval in swereg is `[tstart, tstop)`. The stop is exclusive, so the
# person holds no part of the week the stop names. Every duration is
# `tstop - tstart`, and it never adds one.
#
# Three complete four-week bands span `[0, 12)`. That is 12 person-weeks, and
# the bands bill 4, 4 and 4. The inclusive convention bills 5, 5 and 5.
#
# The `+ 1` belongs to the inclusive convention, where weeks 1 through 4 is
# `4 - 1 + 1 = 4`. Both are correct arithmetic. The two differ in whether the
# stop belongs to the interval. A mix of them makes a silently wrong
# denominator, so swereg MUST read every stop as exclusive.
#
# This file holds one test per boundary quantity.
#
# 1. `weeks_to_event`
# 2. `weeks_to_protocol_deviation`
# 3. `weeks_to_loss`
# 4. `weeks_to_admin_end`
# 5. `weeks_to_record_end`
#
# Each fixture places its boundary where the two conventions disagree. Each
# test pins both readings. It pins the number the code produces, and it pins
# the number the inclusive rule produces from the same rows.
#
# `weeks_to_loss` and `weeks_to_record_end` are coupled by construction.
# `enroll()` writes the exact record-end week, and `s5_prepare_outcome()`
# reads it to place the loss. Test 3 and test 5 therefore share a code path,
# and each names the half of it that it owns.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

.ic_pw <- 4L
.ic_n_fu <- 12L

# Sixteen consecutive ISO year-weeks, starting on a band boundary. Under
# `period_width = 4` they make one entry band and three follow-up bands.
.ic_weeks <- function(n_weeks = 16L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .ic_pw == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# One person, one row per week.
#
# `arm` is her assigned arm. `exposed` holds that arm in every week, and
# `eligible` holds `TRUE` only inside the entry band. `on_tx` is the weekly
# assessment, and it holds her assigned arm by default.
#
# Three arguments move the assessment, and each one names 1-indexed FOLLOW-UP
# weeks rather than rows of `weeks`. Follow-up week `f` is the interval
# `[f - 1, f)` on the analysis scale, so its right edge is `f`.
#
# * `discordant_fu` sets the opposite arm, which is discordant.
# * `event_fu` sets the outcome to `TRUE`.
# * `absent_fu` deletes the row, which ends the record under the
#   `row_presence` sentinel.
.ic_person <- function(
  id,
  weeks,
  arm,
  discordant_fu = integer(0),
  event_fu = integer(0),
  absent_fu = integer(0)
) {
  n <- length(weeks)
  fu <- seq_len(n) - .ic_pw
  on_tx <- rep(arm, n)
  on_tx[fu %in% discordant_fu] <- !arm
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = rep(arm, n),
    eligible = seq_len(n) <= .ic_pw,
    died = fu %in% event_fu,
    on_tx = on_tx,
    age = 50 + seq_len(n)
  )
  d[!(fu %in% absent_fu)]
}

# Concordant fillers, so the propensity model has something to fit. A ratio of
# 2 requests more comparators than the fixture holds, so every comparator is
# drawn and no test depends on the seeded draw.
.ic_fillers <- function(weeks, n_intervention = 8L, n_comparator = 12L) {
  data.table::rbindlist(list(
    data.table::rbindlist(lapply(
      seq_len(n_intervention),
      function(i) .ic_person(paste0("FI", i), weeks, arm = TRUE)
    )),
    data.table::rbindlist(lapply(
      seq_len(n_comparator),
      function(i) .ic_person(paste0("FC", i), weeks, arm = FALSE)
    ))
  ))
}

.ic_design <- function(
  intervention_k = 0L,
  comparator_k = 3L,
  admin_isoyearweek = NULL
) {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = .ic_n_fu,
    period_width = .ic_pw,
    intervention_tolerance_weeks = intervention_k,
    comparator_tolerance_weeks = comparator_k,
    admin_censor_isoyearweek = admin_isoyearweek
  )
}

.ic_enroll <- function(d, design, ratio = 2, seed = 4) {
  TTEEnrollment$new(
    data = data.table::copy(d),
    design = design,
    ratio = ratio,
    seed = seed,
    extra_cols = "isoyearweek"
  )
}

# The public route. `$s5_prepare_outcome()` and `$s6_ipcw_pp()` are private, so
# every test drives `$s4_prepare_for_analysis()` instead.
#
# The fixtures are small and deterministic, so the censoring model separates
# and warns. That warning is about the toy data and not about the boundary.
.ic_prepare <- function(trial, follow_up = .ic_n_fu, estimand = "pp") {
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = follow_up,
      estimand = estimand,
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  trial$data
}

# The rows of one woman, in follow-up order.
.ic_rows <- function(d, who) {
  d[id == who][order(tstart)]
}

# What the inclusive convention bills over the same rows. Each row is one week
# longer, because the inclusive rule counts both endpoints.
.ic_inclusive_weeks <- function(rows) {
  sum(rows$tstop - rows$tstart + 1L)
}


# ---------------------------------------------------------------------------
# PROOF 1 -- weeks_to_event
# ---------------------------------------------------------------------------

test_that("weeks_to_event is an exclusive stop, and the rows bill its weeks", {
  weeks <- .ic_weeks()
  # EVENT7 has the outcome in follow-up week 7. That week is the interval
  # `[6, 7)`, so its right edge is week 7 and the boundary is week 7. Week 7
  # falls inside band 2, which opens at week 4 and closes at week 8, so her
  # terminal row is clipped at week 7 and bills three weeks.
  #
  # WHOLE has no outcome and no boundary of any kind. She is the plain
  # statement of the duration rule.
  d <- data.table::rbindlist(list(
    .ic_person("EVENT7", weeks, arm = TRUE, event_fu = 7L),
    .ic_person("WHOLE", weeks, arm = TRUE),
    .ic_fillers(weeks)
  ))

  out <- .ic_prepare(.ic_enroll(d, .ic_design(intervention_k = 0L)))
  got <- .ic_rows(out, "EVENT7")

  # The boundary is the week the outcome falls in, and never the stop of the
  # band that holds it. That band stops at 8, and reading it there would make
  # every number below one higher.
  expect_identical(unique(got$weeks_to_event), 7L)
  expect_identical(nrow(got), 2L)
  expect_identical(got$tstart, c(0L, 4L))
  expect_identical(got$tstop, c(4L, 7L))
  expect_identical(got$event, c(0L, 1L))

  # The stop is exclusive, so no row opens at or after it.
  expect_identical(nrow(got[tstart >= 7L]), 0L)

  # One whole band and one clipped band bill 7 person-weeks, and the total
  # equals the boundary. The inclusive rule would bill 9 over the same two
  # rows.
  expect_identical(got$person_weeks, c(4L, 3L))
  expect_identical(sum(got$person_weeks), 7L)
  expect_identical(sum(got$person_weeks), unique(got$weeks_to_event))
  expect_identical(.ic_inclusive_weeks(got), 9L)

  # Three complete four-week bands span `[0, 12)`, which is 12 person-weeks.
  # The inclusive rule would bill 15.
  whole <- .ic_rows(out, "WHOLE")
  expect_identical(whole$tstart, c(0L, 4L, 8L))
  expect_identical(whole$tstop, c(4L, 8L, 12L))
  expect_identical(whole$person_weeks, c(4L, 4L, 4L))
  expect_identical(sum(whole$person_weeks), 12L)
  expect_identical(.ic_inclusive_weeks(whole), 15L)

  # The duration rule holds on every row of the analysis dataset.
  expect_identical(out$person_weeks, out$tstop - out$tstart)
})


# ---------------------------------------------------------------------------
# PROOF 2 -- weeks_to_protocol_deviation
# ---------------------------------------------------------------------------

test_that("weeks_to_protocol_deviation is an exclusive stop at the right edge", {
  weeks <- .ic_weeks()
  # DEV6 is discordant in follow-up week 6, under a tolerance of 0. Follow-up
  # week 6 is the interval `[5, 6)`, so its right edge is week 6.
  #
  # The boundary is that right edge. She holds the whole of week 6, and she
  # holds nothing after it.
  d <- data.table::rbindlist(list(
    .ic_person("DEV6", weeks, arm = TRUE, discordant_fu = 6L),
    .ic_fillers(weeks)
  ))

  out <- .ic_prepare(.ic_enroll(d, .ic_design(intervention_k = 0L)))
  got <- .ic_rows(out, "DEV6")

  # The right edge of follow-up week 6 is week 6. The inclusive reading names
  # week 5, which is the left edge of the same week.
  expect_identical(unique(got$weeks_to_protocol_deviation), 6L)
  expect_identical(nrow(got), 2L)
  expect_identical(got$tstart, c(0L, 4L))
  expect_identical(got$tstop, c(4L, 6L))
  expect_identical(got$censor_this_period, c(0L, 1L))

  # The stop is exclusive, so no row opens at or after it.
  expect_identical(nrow(got[tstart >= 6L]), 0L)

  # The terminal band is clipped to 2 of its 4 weeks, and the total equals the
  # boundary. The inclusive rule would bill 8 over the same two rows.
  expect_identical(got$person_weeks, c(4L, 2L))
  expect_identical(sum(got$person_weeks), 6L)
  expect_identical(
    sum(got$person_weeks),
    unique(got$weeks_to_protocol_deviation)
  )
  expect_identical(.ic_inclusive_weeks(got), 8L)

  expect_identical(out$person_weeks, out$tstop - out$tstart)
})


# ---------------------------------------------------------------------------
# PROOF 3 -- weeks_to_loss
# ---------------------------------------------------------------------------

test_that("weeks_to_loss is an exclusive stop, and it yields to a planned end", {
  weeks <- .ic_weeks()
  # LOSS6 has no row for follow-up weeks 7 to 12, so her record stops at the
  # end of follow-up week 6. The exclusive stop is week 6.
  #
  # PLANNED reaches follow-up week 10 and then stops. Her record end is week
  # 10, which is after the requested end of week 6, so she is not lost.
  d <- data.table::rbindlist(list(
    .ic_person("LOSS6", weeks, arm = TRUE, absent_fu = 7L:12L),
    .ic_person("PLANNED", weeks, arm = TRUE, absent_fu = 11L:12L),
    .ic_fillers(weeks)
  ))
  # Six observed follow-up weeks, counted on the fixture and not on the panel.
  expect_identical(nrow(d[id == "LOSS6"]) - .ic_pw, 6L)

  out <- .ic_prepare(.ic_enroll(d, .ic_design(intervention_k = 0L)))
  got <- .ic_rows(out, "LOSS6")

  # The record holds follow-up weeks 1 to 6, so the exclusive stop is week 6.
  # The inclusive reading names week 5.
  expect_identical(unique(got$weeks_to_loss), 6L)
  expect_identical(nrow(got), 2L)
  expect_identical(got$tstart, c(0L, 4L))
  expect_identical(got$tstop, c(4L, 6L))
  expect_identical(got$censor_this_period, c(0L, 1L))

  # The stop is exclusive, so no row opens at or after it.
  expect_identical(nrow(got[tstart >= 6L]), 0L)

  # She bills the six follow-up weeks she was observed for, and no more. The
  # inclusive rule would bill 8 over the same two rows.
  expect_identical(got$person_weeks, c(4L, 2L))
  expect_identical(sum(got$person_weeks), 6L)
  expect_identical(sum(got$person_weeks), unique(got$weeks_to_loss))
  expect_identical(sum(got$person_weeks), nrow(d[id == "LOSS6"]) - .ic_pw)
  expect_identical(.ic_inclusive_weeks(got), 8L)

  # `weeks_to_loss` reports a record that stops BEFORE every planned end. A
  # six-week requested follow-up ends first for PLANNED, so she reads `NA`
  # even though her record also stops early.
  out_short <- .ic_prepare(
    .ic_enroll(d, .ic_design(intervention_k = 0L)),
    follow_up = 6L
  )
  planned <- .ic_rows(out_short, "PLANNED")
  expect_identical(unique(planned$weeks_to_loss), NA_integer_)
  expect_identical(unique(planned$weeks_to_record_end), 10L)
  expect_identical(planned$tstop, c(4L, 6L))
  expect_identical(sum(planned$person_weeks), 6L)

  expect_identical(out$person_weeks, out$tstop - out$tstart)
})


# ---------------------------------------------------------------------------
# PROOF 4 -- weeks_to_admin_end
# ---------------------------------------------------------------------------

test_that("weeks_to_admin_end is an exclusive stop one week past the last week", {
  weeks <- .ic_weeks()
  # ADMIN6 is under study to the END of the administrative week, and follow-up
  # opens at the START of the landmark week. The exclusive stop therefore sits
  # one week after the calendar difference between the two weeks.
  d <- data.table::rbindlist(list(
    .ic_person("ADMIN6", weeks, arm = TRUE),
    .ic_fillers(weeks)
  ))

  # `weeks[10]` is follow-up week 6. `weeks[.ic_pw + 1L]` is the landmark
  # week, which is follow-up week 1.
  admin_week <- weeks[10]
  landmark_week <- weeks[.ic_pw + 1L]
  # The woman is under study in 6 calendar weeks, counting both endpoints.
  n_calendar_weeks <- 10L - (.ic_pw + 1L) + 1L
  expect_identical(n_calendar_weeks, 6L)

  out <- .ic_prepare(
    .ic_enroll(
      d,
      .ic_design(intervention_k = 0L, admin_isoyearweek = admin_week)
    )
  )
  got <- .ic_rows(out, "ADMIN6")

  # `difftime()` measures last date to last date, so it returns the whole
  # weeks BETWEEN the two weeks. Reading THAT as the stop is the inclusive
  # error, because it drops the administrative week itself.
  calendar_difference <- as.integer(difftime(
    cstime::isoyearweek_to_last_date(admin_week),
    cstime::isoyearweek_to_last_date(landmark_week),
    units = "weeks"
  ))
  expect_identical(calendar_difference, 5L)

  # The exclusive stop equals the inclusive count of calendar weeks.
  expect_identical(unique(got$weeks_to_admin_end), 6L)
  expect_identical(unique(got$weeks_to_admin_end), n_calendar_weeks)
  expect_identical(unique(got$weeks_to_admin_end), calendar_difference + 1L)
  expect_identical(nrow(got), 2L)
  expect_identical(got$tstart, c(0L, 4L))
  expect_identical(got$tstop, c(4L, 6L))

  # The stop is exclusive, so no row opens at or after it.
  expect_identical(nrow(got[tstart >= 6L]), 0L)

  # Six weeks under study bill 6 person-weeks. The inclusive rule would bill 8
  # over the same two rows.
  expect_identical(got$person_weeks, c(4L, 2L))
  expect_identical(sum(got$person_weeks), 6L)
  expect_identical(sum(got$person_weeks), unique(got$weeks_to_admin_end))
  expect_identical(.ic_inclusive_weeks(got), 8L)

  # The end binds the whole cohort, so no row anywhere reaches past it.
  expect_identical(max(out$tstop), 6L)
  expect_identical(out$person_weeks, out$tstop - out$tstart)
})


# ---------------------------------------------------------------------------
# PROOF 5 -- weeks_to_record_end
# ---------------------------------------------------------------------------

test_that("weeks_to_record_end is an exclusive stop written by enroll()", {
  weeks <- .ic_weeks()
  # REC9 has no row for follow-up weeks 10 to 12, so her last observed week is
  # follow-up week 9. Week 9 is the interval `[8, 9)`, so the exclusive stop
  # is week 9.
  #
  # Band 3 opens at week 8 and closes at week 12, so her record ends after 1
  # of its 4 weeks. The inclusive reading names week 8, which would empty that
  # band and drop the row.
  d <- data.table::rbindlist(list(
    .ic_person("REC9", weeks, arm = TRUE, absent_fu = 10L:12L),
    .ic_fillers(weeks)
  ))
  # Nine observed follow-up weeks, counted on the fixture and not on the panel.
  expect_identical(nrow(d[id == "REC9"]) - .ic_pw, 9L)

  trial <- .ic_enroll(d, .ic_design(intervention_k = 0L))

  # `enroll()` writes the boundary, so it is readable before any outcome
  # preparation runs.
  panel <- trial$data[id == "REC9"]
  expect_identical(unique(panel$weeks_to_record_end), 9L)

  out <- .ic_prepare(trial)
  got <- .ic_rows(out, "REC9")

  expect_identical(unique(got$weeks_to_record_end), 9L)
  expect_identical(nrow(got), 3L)
  expect_identical(got$tstart, c(0L, 4L, 8L))
  expect_identical(got$tstop, c(4L, 8L, 9L))
  expect_identical(got$censor_this_period, c(0L, 0L, 1L))

  # The stop is exclusive, so no row opens at or after it.
  expect_identical(nrow(got[tstart >= 9L]), 0L)

  # The terminal band bills the single week it holds. The inclusive rule would
  # bill 12 over the same three rows.
  expect_identical(got$person_weeks, c(4L, 4L, 1L))
  expect_identical(sum(got$person_weeks), 9L)
  expect_identical(sum(got$person_weeks), unique(got$weeks_to_record_end))
  expect_identical(sum(got$person_weeks), nrow(d[id == "REC9"]) - .ic_pw)
  expect_identical(.ic_inclusive_weeks(got), 12L)

  expect_identical(out$person_weeks, out$tstop - out$tstart)
})
