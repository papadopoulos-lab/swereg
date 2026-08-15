# Person-time is the exposure a woman contributed, and never the width of the
# band she was censored in.
#
# `enroll()` used to write `person_weeks` from the count of source weeks in the
# band, and `$s4_prepare_for_analysis()` used to delete the censoring row. A
# woman who deviated in week 2 of a four-week terminal band was billed for four
# weeks, and then lost all four. Every rate and every Poisson offset read that
# number.
#
# `s5_prepare_outcome()` now clips the terminal row at the exact boundary, and
# sets `person_weeks` to the clipped width. The row stays. It carries the
# exposure before the boundary and nothing after it.
#
# This file pins four properties.
#
# 1. `person_weeks` is the clipped duration, and not the band width.
# 2. The terminal censoring row is retained, and carries only pre-censor
#    exposure.
# 3. An administrative or requested end is exact, and never rounded to a band.
# 4. A zero-duration row never reaches the offset.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

.lb_pw <- 4L
.lb_n_fu <- 12L

# Sixteen consecutive ISO year-weeks, starting on a band boundary. Under
# `period_width = 4` they make one entry band and three follow-up bands.
.lb_weeks <- function(n_weeks = 16L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .lb_pw == 0L
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
# weeks rather than rows of `weeks`.
#
# * `discordant_fu` sets the opposite arm, which is discordant.
# * `event_fu` sets the outcome to `TRUE`.
# * `absent_fu` deletes the row, which is loss of observation under the
#   `row_presence` sentinel.
.lb_person <- function(
  id,
  weeks,
  arm,
  discordant_fu = integer(0),
  event_fu = integer(0),
  absent_fu = integer(0)
) {
  n <- length(weeks)
  fu <- seq_len(n) - .lb_pw
  on_tx <- rep(arm, n)
  on_tx[fu %in% discordant_fu] <- !arm
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = rep(arm, n),
    eligible = seq_len(n) <= .lb_pw,
    died = fu %in% event_fu,
    on_tx = on_tx,
    age = 50 + seq_len(n)
  )
  d[!(fu %in% absent_fu)]
}

# Concordant fillers, so the propensity model has something to fit. A ratio of
# 2 requests more comparators than the fixture holds, so every comparator is
# drawn and no test depends on the seeded draw.
.lb_fillers <- function(weeks, n_intervention = 8L, n_comparator = 12L) {
  data.table::rbindlist(list(
    data.table::rbindlist(lapply(
      seq_len(n_intervention),
      function(i) .lb_person(paste0("FI", i), weeks, arm = TRUE)
    )),
    data.table::rbindlist(lapply(
      seq_len(n_comparator),
      function(i) .lb_person(paste0("FC", i), weeks, arm = FALSE)
    ))
  ))
}

.lb_design <- function(
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
    follow_up_time = .lb_n_fu,
    period_width = .lb_pw,
    intervention_tolerance_weeks = intervention_k,
    comparator_tolerance_weeks = comparator_k,
    admin_censor_isoyearweek = admin_isoyearweek
  )
}

.lb_enroll <- function(d, design, ratio = 2, seed = 4) {
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
.lb_prepare <- function(trial, follow_up = .lb_n_fu, estimand = "pp") {
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
.lb_rows <- function(d, who) {
  d[id == who][order(tstart)]
}


# ---------------------------------------------------------------------------
# PROOF 1
# ---------------------------------------------------------------------------

test_that("person_weeks is the clipped duration, not the band width", {
  weeks <- .lb_weeks()
  # MIDBAND is discordant in follow-up week 6, under a tolerance of 0. The
  # boundary is the right edge of that week, which is week 6.
  #
  # Follow-up band 2 opens at week 4 and closes at week 8, so week 6 falls
  # squarely inside it. The boundary is week 2 of a four-week terminal band.
  #
  # WHOLE is never discordant. She shows what an unclipped band costs.
  d <- data.table::rbindlist(list(
    .lb_person("MIDBAND", weeks, arm = TRUE, discordant_fu = 6L),
    .lb_person("WHOLE", weeks, arm = TRUE),
    .lb_fillers(weeks)
  ))

  out <- .lb_prepare(.lb_enroll(d, .lb_design(intervention_k = 0L)))
  got <- .lb_rows(out, "MIDBAND")

  # The boundary is exact to the week, and it falls inside band 2.
  expect_identical(unique(got$weeks_to_protocol_deviation), 6L)
  expect_identical(got$tstart, c(0L, 4L))
  expect_identical(got$tstop, c(4L, 6L))

  # The terminal row is billed for the two weeks it holds, and not for four.
  expect_identical(got$person_weeks, c(4L, 2L))
  expect_identical(sum(got$person_weeks), 6L)

  # `person_weeks` is the width of every retained row, hers and everyone's.
  expect_identical(out$person_weeks, out$tstop - out$tstart)

  # WHOLE keeps three complete bands, so the fixture clips MIDBAND alone.
  expect_identical(.lb_rows(out, "WHOLE")$person_weeks, c(4L, 4L, 4L))
})


# ---------------------------------------------------------------------------
# PROOF 2
# ---------------------------------------------------------------------------

test_that("the terminal censor row is retained and carries only pre-censor exposure", {
  weeks <- .lb_weeks()
  # RETAINED is discordant in follow-up week 6, so her boundary is week 6 and
  # band 2 carries the censoring.
  d <- data.table::rbindlist(list(
    .lb_person("RETAINED", weeks, arm = TRUE, discordant_fu = 6L),
    .lb_fillers(weeks)
  ))

  out <- .lb_prepare(.lb_enroll(d, .lb_design(intervention_k = 0L)))
  got <- .lb_rows(out, "RETAINED")

  # The row survives the whole of `$s4_prepare_for_analysis()`.
  expect_identical(nrow(got), 2L)
  expect_identical(got$censor_this_period, c(0L, 1L))

  # It carries the exposure before the boundary, and nothing after it.
  expect_identical(got[censor_this_period == 1L]$tstart, 4L)
  expect_identical(got[censor_this_period == 1L]$tstop, 6L)
  expect_identical(got[censor_this_period == 1L]$person_weeks, 2L)

  # The deviated regime contributes no outcome, so the retained row cannot
  # attribute one to the baseline treatment.
  expect_identical(sum(out[censor_this_period == 1L]$event), 0L)

  # Censoring rows exist across the whole panel, and not only for RETAINED.
  expect_gt(sum(out$censor_this_period), 0L)
})


# ---------------------------------------------------------------------------
# PROOF 3
# ---------------------------------------------------------------------------

test_that("an administrative or requested end is exact, not rounded to a band", {
  weeks <- .lb_weeks()
  # Nobody in this fixture ever deviates, so the requested end and the
  # administrative end are the only boundaries in play.
  d <- data.table::rbindlist(list(
    .lb_person("SIXWEEK", weeks, arm = TRUE),
    .lb_fillers(weeks)
  ))

  # A six-week requested follow-up stops at week six. Bands close at weeks 4,
  # 8 and 12, so week 6 falls inside band 2.
  out <- .lb_prepare(
    .lb_enroll(d, .lb_design(intervention_k = 0L)),
    follow_up = 6L
  )
  got <- .lb_rows(out, "SIXWEEK")
  expect_identical(got$tstop, c(4L, 6L))
  expect_identical(got$person_weeks, c(4L, 2L))
  expect_identical(sum(got$person_weeks), 6L)
  # Rounding up to the band would give 8, and rounding down would give 4.
  expect_identical(max(out$tstop), 6L)

  # The administrative end is exact on the same scale. `weeks[10]` is
  # follow-up week 6, and the woman is under study to the end of it.
  out_admin <- .lb_prepare(
    .lb_enroll(
      d,
      .lb_design(intervention_k = 0L, admin_isoyearweek = weeks[10])
    )
  )
  got_admin <- .lb_rows(out_admin, "SIXWEEK")
  expect_identical(unique(got_admin$weeks_to_admin_end), 6L)
  expect_identical(got_admin$tstop, c(4L, 6L))
  expect_identical(got_admin$person_weeks, c(4L, 2L))
  expect_identical(max(out_admin$tstop), 6L)
})


# ---------------------------------------------------------------------------
# PROOF 4
# ---------------------------------------------------------------------------

test_that("a zero-duration row never reaches the offset", {
  weeks <- .lb_weeks()
  # EDGE is discordant in follow-up week 8, under a tolerance of 0, so her
  # boundary is week 8. That is exactly where band 2 closes and band 3 opens.
  # Band 3 would clip to `tstop == tstart` if it were retained.
  d <- data.table::rbindlist(list(
    .lb_person("EDGE", weeks, arm = TRUE, discordant_fu = 8L),
    .lb_fillers(weeks)
  ))

  out <- .lb_prepare(.lb_enroll(d, .lb_design(intervention_k = 0L)))
  got <- .lb_rows(out, "EDGE")

  expect_identical(unique(got$weeks_to_protocol_deviation), 8L)
  expect_identical(nrow(got), 2L)
  expect_identical(got$tstop, c(4L, 8L))
  expect_identical(got$person_weeks, c(4L, 4L))

  # No row of the analysis dataset has zero duration.
  expect_identical(nrow(out[tstop <= tstart]), 0L)
  expect_true(all(out$person_weeks > 0L))

  # `log(person_weeks)` is the Poisson offset, and `log(0)` is `-Inf`.
  expect_true(all(is.finite(log(out$person_weeks))))
})


# ---------------------------------------------------------------------------
# PROOF 5
# ---------------------------------------------------------------------------

test_that("a record that ends mid-band bills only the weeks present", {
  weeks <- .lb_weeks()
  # TAILCUT has no row for follow-up weeks 11 and 12, so her record stops at
  # the end of follow-up week 10. Band 3 opens at week 8 and closes at week
  # 12, so her record ends inside it and holds 2 of its 4 weeks.
  #
  # A record that ends carries no internal gap, because no observed week
  # follows it. The band stop alone would bill her for all four.
  #
  # FULLOBS is the same woman with every week present. She shows that a
  # complete record is not censored by this boundary.
  d <- data.table::rbindlist(list(
    .lb_person("TAILCUT", weeks, arm = TRUE, absent_fu = 11L:12L),
    .lb_person("FULLOBS", weeks, arm = TRUE),
    .lb_fillers(weeks)
  ))
  # Ten follow-up weeks, counted on the fixture and not on the panel.
  expect_identical(nrow(d[id == "TAILCUT"]) - .lb_pw, 10L)

  out <- .lb_prepare(.lb_enroll(d, .lb_design(intervention_k = 0L)))
  got <- .lb_rows(out, "TAILCUT")

  expect_identical(unique(got$weeks_to_record_end), 10L)
  expect_identical(got$tstop, c(4L, 8L, 10L))
  expect_identical(got$person_weeks, c(4L, 4L, 2L))

  # She bills exactly the follow-up weeks she was observed for.
  expect_identical(sum(got$person_weeks), 10L)
  expect_identical(sum(got$person_weeks), nrow(d[id == "TAILCUT"]) - .lb_pw)

  # A record that ends before the planned end is loss to follow-up.
  expect_identical(got$censor_this_period, c(0L, 0L, 1L))
  expect_identical(unique(got$weeks_to_loss), 10L)

  # FULLOBS reaches the end of the panel, so this boundary never binds on her.
  full <- .lb_rows(out, "FULLOBS")
  expect_identical(full$person_weeks, c(4L, 4L, 4L))
  expect_identical(sum(full$person_weeks), 12L)
  expect_identical(full$censor_this_period, c(0L, 0L, 0L))
  expect_identical(unique(full$weeks_to_record_end), NA_integer_)
})


# ---------------------------------------------------------------------------
# Supporting behaviour, tested and not mutation-proven
# ---------------------------------------------------------------------------

test_that("an event in the deviation band wins, and the row stops at the event week", {
  weeks <- .lb_weeks()
  # COLLIDE deviates in follow-up week 6 and has the outcome in follow-up
  # week 7. Both fall in band 2, which closes at week 8.
  #
  # The event wins the band. The deviation in week 6 does not clip her, and
  # her row is not flagged as censored. She stops at her own event week, which
  # is week 7, so the terminal row bills three weeks and not four.
  d <- data.table::rbindlist(list(
    .lb_person(
      "COLLIDE",
      weeks,
      arm = TRUE,
      discordant_fu = 6L,
      event_fu = 7L
    ),
    .lb_fillers(weeks)
  ))

  out <- .lb_prepare(.lb_enroll(d, .lb_design(intervention_k = 0L)))
  got <- .lb_rows(out, "COLLIDE")

  expect_identical(unique(got$weeks_to_protocol_deviation), 6L)
  expect_identical(unique(got$weeks_to_event), 7L)
  expect_identical(got$tstop, c(4L, 7L))
  expect_identical(got$person_weeks, c(4L, 3L))
  expect_identical(sum(got$person_weeks), 7L)
  expect_identical(got$event, c(0L, 1L))
  expect_identical(got$censor_this_period, c(0L, 0L))
})

test_that("a whole missing follow-up band is censored before it can renumber", {
  # `enroll()` numbers the follow-up bands by position, so a person-trial that
  # loses a whole middle band gets its later bands numbered too early. The
  # exposure of those rows would then be measured from the wrong week.
  #
  # A design that declares `observed_var` cannot reach that state. The missing
  # band is an observation gap, and follow-up stops at the first absent week.
  weeks <- .lb_weeks()
  d <- data.table::rbindlist(list(
    .lb_person("MIDGAP", weeks, arm = TRUE, absent_fu = 5L:8L),
    .lb_fillers(weeks)
  ))

  trial <- .lb_enroll(d, .lb_design(intervention_k = 0L))

  # The renumbering is real, and this reads it off the enrolled panel. Her two
  # rows are two bands apart on the calendar and one band apart in `tstop`.
  panel <- trial$data[id == "MIDGAP"][order(trial_id)]
  expect_identical(nrow(panel), 2L)
  expect_identical(as.integer(diff(panel$trial_id)), 2L)
  expect_identical(panel$tstop, c(4L, 8L))

  # The gap opens at follow-up week 5, so the boundary is week 4.
  out <- .lb_prepare(trial)
  got <- .lb_rows(out, "MIDGAP")
  expect_identical(unique(got$weeks_to_protocol_deviation), 4L)

  # Only the band before the gap survives, and the misnumbered row is gone.
  expect_identical(nrow(got), 1L)
  expect_identical(got$tstop, 4L)
  expect_identical(got$person_weeks, 4L)
})

test_that("ITT retains its loss row with the width it holds", {
  weeks <- .lb_weeks()
  # ITT never censors at a switch, so SWITCH keeps all three bands.
  d <- data.table::rbindlist(list(
    .lb_person("SWITCH", weeks, arm = TRUE, discordant_fu = 6L),
    .lb_fillers(weeks)
  ))

  out <- .lb_prepare(
    .lb_enroll(d, .lb_design(intervention_k = 0L)),
    estimand = "itt"
  )
  got <- .lb_rows(out, "SWITCH")

  expect_identical(got$person_weeks, c(4L, 4L, 4L))
  expect_identical(sum(got$person_weeks), 12L)
  expect_identical(out$person_weeks, out$tstop - out$tstart)
})
