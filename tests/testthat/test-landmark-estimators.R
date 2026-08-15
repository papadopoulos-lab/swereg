# A survival risk set spans the time point. A rate uses exact exposure.
#
# `s5_prepare_outcome()` clips the terminal row at the exact censoring week, so
# a stop no longer has to sit on the band grid. Two rows can then close inside
# one nominal band: one at week 6 and one at week 8 of the band that runs from
# week 4 to week 8.
#
# The risk set at week 6 must hold every row that COVERS week 6, and not only
# the row that stops there. Before the clipping every stop sat on the grid, the
# two sets agreed, and the difference was unobservable. It is observable now.
#
# The rules this file pins:
#
#   Y_a(t) = sum_i w_i * I(A_i = a, tstart_i < t <= tstop_i)
#   d_a(t) = sum_i w_i * I(A_i = a, event_i = 1, tstop_i = t)
#   N_a(t) = distinct persons i with tstart_i < t <= tstop_i
#   S_a(t) = prod_{u <= t} (1 - d_a(u) / Y_a(u))
#
# Note the asymmetry: the risk set SPANS the time and the event LANDS at the
# stop of its own row.
#
# `Y_a(t)` is a weighted COUNT of the person-trials at risk. It is not a sum of
# person-time. `$rates()` owns the person-time quantity, forms it as
# `sum(person_weeks * w)`, and is untouched here.
#
# FIXTURE 1, hand computed. Five person-trials held by three people, and two
# event boundaries inside the band that runs from week 4 to week 8.
#
#   trial  person  arm    rows                        w
#   i1     p1      TRUE   (0,4] ; (4,6] event         1
#   i2     p1      TRUE   (0,4] ; (4,8] event         1
#   i3     p2      TRUE   (0,4] ; (4,8] ; (8,12]      0.5
#   c1     p3      FALSE  (0,4] ; (4,8] event         2
#   c2     p3      FALSE  (0,4] ; (4,8]               2
#
# The reporting times are 4, 6, 8 and 12. Spanning risk sets give
#
#   arm     t=4    t=6    t=8    t=12
#   TRUE    2.5    2.5    1.5    0.5     Y(t)
#   TRUE      2      2      2      1     N(t)
#   TRUE      1    0.6    0.2    0.2     S(t)
#   FALSE     4      4      4      0     Y(t)
#   FALSE     1      1      1      0     N(t)
#   FALSE     1      1    0.5    0.5     S(t)
#
# and the risk difference S_FALSE - S_TRUE is 0, 0.4, 0.3, 0.3.
#
# The band-grouped risk set that stops at the row's own `tstop` gives
# S_TRUE = 1, 0, 0, 0 instead, because at week 6 it holds i1 alone and her
# hazard is then 1. The fixture is therefore not decorative: the two
# definitions disagree in every value after week 4.

skip_if_not_installed("data.table")

# --- fixture 1: a hand-built analysis panel with clipped rows ---------------

.le_panel <- function() {
  dt <- data.table::data.table(
    id = c("p1", "p1", "p1", "p1", "p2", "p2", "p2", "p3", "p3", "p3", "p3"),
    enrollment_person_trial_id = c(
      "i1",
      "i1",
      "i2",
      "i2",
      "i3",
      "i3",
      "i3",
      "c1",
      "c1",
      "c2",
      "c2"
    ),
    exposed = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),
    tstart = c(0L, 4L, 0L, 4L, 0L, 4L, 8L, 0L, 4L, 0L, 4L),
    tstop = c(4L, 6L, 4L, 8L, 4L, 8L, 12L, 4L, 8L, 4L, 8L),
    event = c(0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L),
    w = c(1, 1, 1, 1, 0.5, 0.5, 0.5, 2, 2, 2, 2),
    age = 50,
    death = 0L
  )
  dt[, person_weeks := tstop - tstart]
  dt[]
}

.le_design <- function() {
  swereg::TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
}

# data_level is auto-detected as "trial" from the two id columns.
.le_trial <- function(dt = .le_panel()) {
  swereg::TTEEnrollment$new(dt, .le_design())
}

.le_times <- c(4L, 6L, 8L, 12L)

# --- the definition the code replaced, written out -------------------------
#
# The band-grouped risk set: a row counts at the time it STOPS at, and nowhere
# else. Every value it returns is a value the spanning definition must not
# return, so this function is what makes the fixture non-decorative.
.le_band_grouped_surv <- function(dt, arm_value, times) {
  sub <- dt[exposed == arm_value]
  haz <- vapply(
    times,
    function(t) {
      rows <- sub[tstop == t]
      if (nrow(rows) == 0L) {
        return(0)
      }
      sum(rows$w * rows$event) / sum(rows$w)
    },
    numeric(1)
  )
  cumprod(1 - haz)
}

# --- the estimand, transcribed for the bootstrap reference ------------------
#
# One numerator and one denominator matrix per arm, person-trial by reporting
# time, built row by row from the two rules. It is deliberately a literal loop
# and shares no code with the estimator.
.le_ref_mats <- function(dt, arm_value, pt_levels, times) {
  num <- matrix(0, nrow = length(pt_levels), ncol = length(times))
  den <- matrix(0, nrow = length(pt_levels), ncol = length(times))
  sub <- dt[exposed == arm_value]
  for (r in seq_len(nrow(sub))) {
    i <- match(sub$enrollment_person_trial_id[r], pt_levels)
    for (j in seq_along(times)) {
      if (sub$tstart[r] < times[j] && times[j] <= sub$tstop[r]) {
        den[i, j] <- den[i, j] + sub$w[r]
      }
      if (sub$event[r] == 1L && times[j] == sub$tstop[r]) {
        num[i, j] <- num[i, j] + sub$w[r]
      }
    }
  }
  list(num = num, den = den)
}

# Survival of one arm for a batch of multiplicity rows, from those matrices.
# A time the ARM ITSELF cannot reach carries the survival forward. A time only
# THIS replicate emptied stays missing, which is what the percentile step drops.
.le_ref_surv <- function(mult, mats) {
  numerator <- mult %*% mats$num
  denominator <- mult %*% mats$den
  s <- 1 - numerator / denominator
  s[!is.finite(denominator) | denominator <= 0] <- NA_real_
  s[, colSums(mats$den) <= 0] <- 1
  t(apply(s, 1L, cumprod))
}


# ---------------------------------------------------------------------------
# PROOF 1
# ---------------------------------------------------------------------------

test_that("a row spanning a time point is in the risk set at that point", {
  curve <- .le_trial()$survival_curve(weight_col = "w")

  # One row per arm and reporting time, comparator first.
  expect_identical(curve$tstop, rep(.le_times, 2L))
  expect_identical(curve$exposed, rep(c(FALSE, TRUE), each = 4L))

  int <- curve[exposed == TRUE]

  # Week 6 is the discriminating time. Three intervention rows cover it: i1
  # stops there, and i2 and i3 stop at week 8 and are plainly still at risk.
  expect_equal(int$at_risk, c(2.5, 2.5, 1.5, 0.5))
  expect_identical(int$n_persons_at_risk, c(2L, 2L, 2L, 1L))

  # The two rows that only SPAN week 6 carry 1.5 of that 2.5.
  spanning_only <- .le_panel()[
    exposed == TRUE & tstart < 6L & 6L <= tstop & tstop != 6L
  ]
  expect_equal(sum(spanning_only$w), 1.5)

  # The events land at their own stops, week 6 and week 8.
  expect_equal(int$events, c(0, 1, 1, 0))
  expect_equal(int$surv, c(1, 0.6, 0.2, 0.2))

  # The band-grouped definition disagrees at every time after week 4, and it
  # reports certain death from week 6 onwards.
  band <- .le_band_grouped_surv(.le_panel(), TRUE, .le_times)
  expect_equal(band, c(1, 0, 0, 0))
  expect_false(isTRUE(all.equal(int$surv, band)))
})


# ---------------------------------------------------------------------------
# PROOF 2
# ---------------------------------------------------------------------------

test_that("rates() and the person count are unchanged", {
  trial <- .le_trial()
  dt <- .le_panel()

  # `$rates()` reports EXACT exposure duration: sum(person_weeks * w) / 52.25.
  # The intervention arm holds 20 weighted weeks and the comparator arm 32.
  r <- trial$rates(weight_col = "w")
  data.table::setorder(r, exposed)
  expect_equal(r$py_weighted, c(32 / 52.25, 20 / 52.25))
  expect_equal(r$events_weighted, c(2, 2))
  expect_identical(r$n_persons, c(1L, 2L))
  expect_identical(r$n_trials, c(2L, 3L))

  # A weighted COUNT of the rows would give 8 and 5.5 weighted weeks instead,
  # so these numbers can only come from the person-time.
  expect_false(isTRUE(all.equal(r$py_weighted, c(8, 5.5) / 52.25)))

  # The survival risk set is the other quantity, and it stays a weighted COUNT
  # of the person-trials at risk.
  curve <- trial$survival_curve(weight_col = "w")
  int <- curve[exposed == TRUE]
  expect_equal(int$at_risk, c(2.5, 2.5, 1.5, 0.5))

  # Person-time over the same spanning rows would give 10, 8, 6 and 2.
  person_time <- vapply(
    .le_times,
    function(t) {
      rows <- dt[exposed == TRUE & tstart < t & t <= tstop]
      sum(rows$w * rows$person_weeks)
    },
    numeric(1)
  )
  expect_equal(person_time, c(10, 8, 6, 2))
  expect_false(isTRUE(all.equal(int$at_risk, person_time)))

  # The head count is an integer count of people, and it is not the weighted
  # risk set in any cell of either arm.
  expect_type(curve$n_persons_at_risk, "integer")
  expect_false(isTRUE(all.equal(
    as.numeric(curve$n_persons_at_risk),
    curve$at_risk
  )))
  expect_identical(curve[exposed == FALSE]$n_persons_at_risk, c(1L, 1L, 1L, 0L))
})


# ---------------------------------------------------------------------------
# PROOF 3
# ---------------------------------------------------------------------------

test_that("the risk-difference bootstrap uses the same risk sets as the point estimate", {
  dt <- .le_panel()
  curve <- .le_trial()$survival_curve(weight_col = "w")

  set.seed(11L)
  n_boot <- 40L
  out <- swereg:::.tte_rd_curve(
    data = dt,
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_var = "tstop",
    weight_col = "w",
    n_boot = n_boot,
    keep_mult = TRUE
  )

  # The point estimate is the curve the figure draws, arm for arm and time for
  # time. Nothing here re-derives it.
  expect_identical(out$tstop, .le_times)
  expect_equal(out$surv_comparator, curve[exposed == FALSE]$surv)
  expect_equal(out$surv_intervention, curve[exposed == TRUE]$surv)
  expect_equal(out$surv_intervention, c(1, 0.6, 0.2, 0.2))
  expect_equal(out$rd, c(0, 0.4, 0.3, 0.3))

  # Every replicate, cell for cell, against the spanning definition written out
  # by hand. The recorded multiplicity vectors are the ones the estimator
  # applied, so this compares the resampling the estimator really did.
  pt_levels <- levels(factor(dt$enrollment_person_trial_id))
  m_int <- .le_ref_mats(dt, TRUE, pt_levels, .le_times)
  m_cmp <- .le_ref_mats(dt, FALSE, pt_levels, .le_times)
  mult <- attr(out, "mult_intervention")
  expect_identical(dim(mult), c(n_boot, length(pt_levels)))
  expect_identical(mult, attr(out, "mult_comparator"))

  ref <- .le_ref_surv(mult, m_cmp) - .le_ref_surv(mult, m_int)
  expect_equal(attr(out, "rd_boot"), ref, tolerance = 1e-12)

  # The point estimate is that same arithmetic at multiplicity one, so the
  # replicates and the point estimate cannot rest on different risk sets.
  one <- matrix(1L, nrow = 1L, ncol = length(pt_levels))
  expect_equal(as.vector(.le_ref_surv(one, m_int)), out$surv_intervention)
  expect_equal(as.vector(.le_ref_surv(one, m_cmp)), out$surv_comparator)

  # The head counts beside the risk difference are the curve's counts.
  expect_identical(
    out$n_persons_at_risk_intervention,
    curve[exposed == TRUE]$n_persons_at_risk
  )
  expect_identical(
    out$n_persons_at_risk_comparator,
    curve[exposed == FALSE]$n_persons_at_risk
  )
})


# ---------------------------------------------------------------------------
# PROOF 4
# ---------------------------------------------------------------------------

test_that("survival carries forward between event boundaries", {
  curve <- .le_trial()$survival_curve(weight_col = "w")
  cmp <- curve[exposed == FALSE]

  # Week 6 is a reporting endpoint that falls between two boundaries of THIS
  # arm: its rows stop at week 4 and week 8, and none stops at week 6.
  expect_equal(cmp$events, c(0, 0, 2, 0))
  expect_equal(cmp$at_risk, c(4, 4, 4, 0))

  # The reported value there is the latest EXACT survival, carried forward.
  expect_equal(cmp$surv[2], cmp$surv[1])
  expect_equal(cmp$surv[2], 1)

  # It is neither of the two plausible wrong answers. A hazard recomputed over
  # the whole band from week 4 to week 8 gives 2/4, and so 0.5. Interpolating
  # between the neighbouring exact values gives 0.75.
  expect_false(isTRUE(all.equal(cmp$surv[2], 0.5)))
  expect_false(isTRUE(all.equal(cmp$surv[2], 0.75)))

  # Week 12 is past every comparator row, so nobody is at risk. The hazard is
  # undefined and reads NA, and the survival still carries forward.
  expect_identical(cmp$n_persons_at_risk[4], 0L)
  expect_equal(cmp$at_risk[4], 0)
  expect_true(is.na(cmp$hazard[4]))
  expect_false(is.na(cmp$surv[4]))
  expect_equal(cmp$surv[4], cmp$surv[3])
  expect_equal(cmp$surv, c(1, 1, 0.5, 0.5))

  # The intervention arm reaches week 12 and keeps its own exact value, so the
  # carry-forward is not a property of the whole curve.
  expect_equal(curve[exposed == TRUE]$surv, c(1, 0.6, 0.2, 0.2))
})


# ---------------------------------------------------------------------------
# The same shape, reached through the public pipeline
# ---------------------------------------------------------------------------
#
# Fixture 1 is hand built. This block builds the panel the way production does,
# through `$s4_prepare_for_analysis()`, and shows that it holds stops off the
# band grid and that the risk set at such a stop spans. `s5_prepare_outcome()`
# and `s6_ipcw_pp()` are private, so the public method is what runs.
#
# The person builder is the one `test-landmark-boundaries.R` uses, kept in step
# with it: each test file carries its own copy, because testthat gives a file
# no access to another file's definitions.

.le_pw <- 4L
.le_n_fu <- 12L

.le_weeks <- function(n_weeks = 16L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .le_pw == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

.le_person <- function(
  id,
  weeks,
  arm,
  event_fu = integer(0),
  absent_fu = integer(0)
) {
  n <- length(weeks)
  fu <- seq_len(n) - .le_pw
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = rep(arm, n),
    eligible = seq_len(n) <= .le_pw,
    died = fu %in% event_fu,
    on_tx = rep(arm, n),
    age = 50 + seq_len(n)
  )
  # A deleted week is an unobserved week under the `row_presence` sentinel.
  d[!(fu %in% absent_fu)]
}

.le_fillers <- function(weeks, n_intervention = 8L, n_comparator = 12L) {
  data.table::rbindlist(list(
    data.table::rbindlist(lapply(
      seq_len(n_intervention),
      function(i) .le_person(paste0("FI", i), weeks, arm = TRUE)
    )),
    data.table::rbindlist(lapply(
      seq_len(n_comparator),
      function(i) .le_person(paste0("FC", i), weeks, arm = FALSE)
    ))
  ))
}

.le_pipeline_design <- function() {
  swereg::TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = .le_n_fu,
    period_width = .le_pw
  )
}

# The fixtures are small and deterministic, so the censoring model separates
# and warns. That warning is about the toy data and not about the risk set.
.le_prepare <- function(trial) {
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = .le_n_fu,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  trial
}

test_that("the pipeline panel holds off-grid stops and the risk set spans them", {
  skip_if_not_installed("cstime")
  weeks <- .le_weeks()
  # OFFGRID has no row for follow-up weeks 11 and 12, so her record ends at
  # week 10. Week 10 is week 2 of the band that runs from week 8 to week 12,
  # so her terminal row is clipped there and week 10 joins the reporting grid.
  # ONGRID has the outcome in follow-up week 12, at the band boundary.
  #
  # The off-grid boundary here is a record end. `weeks_to_event` is exact to
  # the week too, so an event also lands off the grid, and the block after
  # this one pins that.
  d <- data.table::rbindlist(list(
    .le_person("OFFGRID", weeks, arm = TRUE, absent_fu = 11:12),
    .le_person("ONGRID", weeks, arm = TRUE, event_fu = 12L),
    .le_fillers(weeks)
  ))
  trial <- .le_prepare(swereg::TTEEnrollment$new(
    data = d,
    design = .le_pipeline_design(),
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  ))
  panel <- trial$data

  # The clipping is real: one stop off the four-week grid, and it is hers.
  offgrid <- panel[id == "OFFGRID"][order(tstart)]
  expect_identical(offgrid$tstop, c(4L, 8L, 10L))
  expect_identical(offgrid$censor_this_period, c(0L, 0L, 1L))
  expect_true(any(panel$tstop %% .le_pw != 0L))
  expect_identical(sort(unique(panel$tstop)), c(4L, 8L, 10L, 12L))
  expect_identical(data.table::uniqueN(panel[tstop == 10L]$id), 1L)
  expect_identical(nrow(panel[exposed == FALSE & tstop == 10L]), 0L)

  curve <- trial$survival_curve(weight_col = "ipw")
  at10 <- curve[exposed == TRUE & tstop == 10L]
  spans10 <- panel[exposed == TRUE & tstart < 10L & 10L <= tstop]

  expect_equal(at10$at_risk, sum(spans10$ipw))
  expect_identical(at10$n_persons_at_risk, data.table::uniqueN(spans10$id))
  # Every intervention woman still under follow-up, and not the clipped one
  # alone. The band-grouped risk set at week 10 holds her row and nothing else,
  # so a risk table read off it would print one person out of ten.
  expect_gt(at10$n_persons_at_risk, 1L)
  expect_gt(at10$at_risk, sum(panel[exposed == TRUE & tstop == 10L]$ipw))

  # The comparator arm holds no row that STOPS at week 10, and it is plainly
  # still at risk there. Its survival carries forward, and the risk difference
  # stays estimable at week 10 and after it.
  rd <- trial$risk_difference(weight_col = "ipw", n_boot = 20L, seed = 1L)
  expect_identical(rd$tstop, c(4L, 8L, 10L, 12L))
  expect_false(any(is.na(rd$surv_comparator)))
  expect_false(any(is.na(rd$rd)))
  expect_equal(rd$surv_comparator[3], rd$surv_comparator[2])
  expect_equal(rd$surv_comparator, curve[exposed == FALSE]$surv)
  expect_equal(rd$surv_intervention, curve[exposed == TRUE]$surv)
})


# ---------------------------------------------------------------------------
# PROOF 5
# ---------------------------------------------------------------------------
#
# `weeks_to_event` is exact to the week, like the other four boundaries.
#
# It was the stop of the BAND the outcome fell in. Every stop was a band stop
# before phase 11 clipped the terminal row, so `tstop == weeks_to_event` always
# held and the event indicator was safe. A clipped row compares week 10 against
# band stop 12, the indicator reads 0, and the woman leaves as a loss.
#
# The production skeleton deletes every person-week after death and keeps the
# death week, so a death IS a record end inside a partial band. That is this
# case, and it removed real events from the numerator.
#
# The priority rule is (1) outcome event, (2) protocol deviation or observed
# loss, (3) administrative or requested end. The event and the record end share
# one weekly boundary here, and the event wins.

test_that("an outcome in a partial terminal band is an event, not a loss", {
  skip_if_not_installed("cstime")
  weeks <- .le_weeks()
  # DIED_LATE has the outcome in follow-up week 10 and no row for follow-up
  # weeks 11 and 12. Her record ends at week 10 and her event lands at week 10.
  # Week 10 is week 2 of the band that runs from week 8 to week 12.
  d <- data.table::rbindlist(list(
    .le_person(
      "DIED_LATE",
      weeks,
      arm = TRUE,
      event_fu = 10L,
      absent_fu = 11:12
    ),
    .le_fillers(weeks)
  ))
  trial <- .le_prepare(swereg::TTEEnrollment$new(
    data = d,
    design = .le_pipeline_design(),
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  ))
  got <- trial$data[id == "DIED_LATE"][order(tstart)]

  # The boundary is the week, and not the stop of the band that holds it.
  expect_identical(unique(got$weeks_to_event), 10L)
  expect_identical(got$tstop, c(4L, 8L, 10L))
  expect_identical(got$person_weeks, c(4L, 4L, 2L))

  # She leaves as an event. The record end shares the boundary and loses it,
  # so no loss is recorded and the row is not flagged as censored.
  expect_identical(got$event, c(0L, 0L, 1L))
  expect_identical(got$censor_this_period, c(0L, 0L, 0L))
  expect_true(all(is.na(got$weeks_to_loss)))

  # The event reaches the estimator, at its own exact boundary.
  curve <- trial$survival_curve(weight_col = "ipw")
  at10 <- curve[exposed == TRUE & tstop == 10L]
  expect_gt(at10$events, 0)
  expect_equal(at10$events, sum(got[tstop == 10L]$ipw))
  expect_lt(curve[exposed == TRUE & tstop == 12L]$surv, 1)
})
