# The per-protocol censoring weight, pinned.
#
# The invariant: the weight on the row of band k is the stabilised probability
# of remaining uncensored through the START of band k. It is not the
# probability through the end of band k.
#
# Three properties follow, and this file holds one proof of each.
#
# 1. The censoring model is complementary log-log with a person-time offset,
#    so one linear predictor gives `q(4) = q(1)^4`. Unequal band widths are
#    then comparable.
# 2. The cumulative product is LAGGED. It stops at band `k - 1`, so the first
#    row of every person-trial weighs exactly 1.
# 3. The numerator is a second fitted model, and not the empirical mean of the
#    denominator predictions.
#
# The fourth proof covers what happens when the model cannot be estimated. A
# stratum with no uncensored row stops. swereg substitutes no marginal
# censoring rate.
#
# `$s5_prepare_outcome()` and `$s6_ipcw_pp()` are private, so every test drives
# the public `$s4_prepare_for_analysis()`.

skip_if_not_installed("data.table")

.ipcw_follow_up <- 12L

# One person-trial, band by band. `edges` names the band boundaries, so
# `edges = c(0, 4, 8, 12)` makes the bands `[0, 4)`, `[4, 8)` and `[8, 12)`.
#
# `deviate_band` names the 1-indexed band the person switches arm in. The
# switch censors that band, and `s5_prepare_outcome()` then deletes every band
# after it. `NA` keeps the person on the assigned arm throughout.
.ipcw_trial <- function(id, edges, arm, age, deviate_band = NA_integer_) {
  n <- length(edges) - 1L
  on_tx <- rep(arm, n)
  if (!is.na(deviate_band)) {
    on_tx[deviate_band:n] <- !arm
  }
  data.table::data.table(
    enrollment_person_trial_id = id,
    tstart = as.integer(edges[seq_len(n)]),
    tstop = as.integer(edges[seq_len(n) + 1L]),
    exposed = arm,
    on_tx = on_tx,
    died = FALSE,
    age = as.numeric(age)
  )
}

.ipcw_design <- function(follow_up = .ipcw_follow_up) {
  TTEDesign$new(
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = follow_up
  )
}

# The public route into the censoring model. The fixtures are deterministic and
# small, so `glm()` reports its own convergence warnings; they are about the
# fixture and not about the weight.
.ipcw_run <- function(d, follow_up = .ipcw_follow_up, use_gam = FALSE, ...) {
  trial <- TTEEnrollment$new(data.table::copy(d), .ipcw_design(follow_up))
  suppressWarnings({
    trial$s2_ipw()
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = follow_up,
      estimand = "pp",
      estimate_ipcw_pp_with_gam = use_gam,
      ...
    )
  })
  data.table::setorderv(
    trial$data,
    c("enrollment_person_trial_id", "tstart")
  )
  trial$data
}

# The cohort every proof but the fourth reads.
#
# `PROBE1W` and `PROBE4W` are the pair the offset is measured on. They carry
# the same arm, the same confounder value and the same calendar position, and
# they differ only in the width of their first band: one week against four.
# Neither is censored, so each has a second row to carry the first row's
# contribution.
#
# Censoring rises with age in both arms, so the denominator model, which reads
# age, departs from the numerator model, which does not. The probes sit at the
# old end of the intervention arm, where that departure is largest.
.ipcw_cohort <- function(probe_age = 66) {
  full <- c(0, 4, 8, 12)
  trials <- list(
    .ipcw_trial("PROBE1W", c(0, 1, 5, 9, 12), arm = TRUE, age = probe_age),
    .ipcw_trial("PROBE4W", full, arm = TRUE, age = probe_age)
  )
  # Intervention arm, ages 51 to 70. `switch_band` names the band each person
  # switches in, and `NA` names a person who never switches. Switching rises
  # with age, and it falls in every band, so no band start is free of
  # censoring. A band start with no censored row at all would drive both models
  # to the same floor, and the ratio would be exactly 1 under any link.
  switch_band <- c(
    NA, 1L, NA, NA, 2L, NA, NA, NA, NA, 3L,
    3L, 3L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L
  )
  for (i in 1:20) {
    trials[[length(trials) + 1L]] <- .ipcw_trial(
      paste0("I", i),
      full,
      arm = TRUE,
      age = 50 + i,
      deviate_band = switch_band[i]
    )
  }
  # Comparator arm. Ages 46 to 65, and the switches fall in every band, so no
  # band start is free of censoring.
  for (i in 1:20) {
    band <- if (i >= 16L) 1L else if (i >= 13L) 2L else if (i == 3L) 3L else {
      NA_integer_
    }
    trials[[length(trials) + 1L]] <- .ipcw_trial(
      paste0("C", i),
      full,
      arm = FALSE,
      age = 45 + i,
      deviate_band = band
    )
  }
  data.table::rbindlist(trials)
}

# The censoring model, written out again from its own description: complementary
# log-log, a person-time offset, a natural cubic spline of the interval START,
# and the confounder in the denominator only. It is the ground truth the fitted
# weights are compared against.
#
# The caller MUST check that the arm holds four or more distinct band starts,
# because that is what selects `splines::ns(tstart, df = 3)`.
.ipcw_reference <- function(out, arm) {
  d <- data.table::copy(out[exposed == arm])
  data.table::setorderv(d, c("enrollment_person_trial_id", "tstart"))
  fit <- function(rhs) {
    suppressWarnings(stats::glm(
      stats::as.formula(paste0(
        "censor_this_period ~ ",
        rhs,
        " + offset(log(person_weeks))"
      )),
      data = d,
      family = stats::binomial(link = "cloglog")
    ))
  }
  time_term <- "splines::ns(tstart, df = 3)"
  d[,
    q_denominator := 1 -
      as.numeric(suppressWarnings(stats::predict(
        fit(paste(time_term, "+ age")),
        newdata = d,
        type = "response"
      )))
  ]
  d[,
    q_numerator := 1 -
      as.numeric(suppressWarnings(stats::predict(
        fit(time_term),
        newdata = d,
        type = "response"
      )))
  ]
  d[,
    reference_ipcw := cumprod(data.table::shift(q_numerator, 1L, fill = 1)) /
      cumprod(data.table::shift(q_denominator, 1L, fill = 1)),
    by = "enrollment_person_trial_id"
  ]
  d[]
}


# ---------------------------------------------------------------------------
# PROOF 1 -- the offset makes unequal band widths comparable
# ---------------------------------------------------------------------------

test_that("a four-week row's uncensoring probability is a one-week row's to the fourth power", {
  out <- .ipcw_run(.ipcw_cohort())

  one <- out[enrollment_person_trial_id == "PROBE1W"]
  four <- out[enrollment_person_trial_id == "PROBE4W"]

  # The two first rows differ only in width. Everything the linear predictor
  # reads is identical.
  expect_identical(one$person_weeks[1], 1L)
  expect_identical(four$person_weeks[1], 4L)
  expect_identical(one$tstart[1], four$tstart[1])
  expect_identical(one$age[1], four$age[1])
  expect_identical(one$exposed[1], four$exposed[1])
  expect_identical(one$censor_this_period[1], 0L)
  expect_identical(four$censor_this_period[1], 0L)

  # The weight of the second row is the first row's numerator-to-denominator
  # ratio, because the first row's own weight is 1. Under
  # `q = exp(-exp(eta) * w)` that ratio raised to the fourth power is the
  # ratio a four-week row carries.
  ratio_one <- one$ipcw_pp[2]
  ratio_four <- four$ipcw_pp[2]
  expect_equal(ratio_four, ratio_one^4, tolerance = 1e-8)

  # The identity is not the trivial one. A ratio of exactly 1 satisfies
  # `x^4 == x` for any link, so the fixture MUST move the ratio away from 1.
  expect_gt(abs(ratio_one - 1), 0.005)
})


# ---------------------------------------------------------------------------
# PROOF 2 -- the cumulative product is lagged
# ---------------------------------------------------------------------------

test_that("the weight is through the start of the row, and the first row's weight is one", {
  out <- .ipcw_run(.ipcw_cohort())

  first <- out[, .SD[1], by = "enrollment_person_trial_id"]
  expect_gt(nrow(first), 40L)
  expect_identical(first$ipcw_pp, rep(1, nrow(first)))

  # C16 switches arm in her first band, so she keeps exactly one row and that
  # row is censored. No follow-up precedes it, so her weight is 1.
  alone <- out[enrollment_person_trial_id == "C16"]
  expect_identical(nrow(alone), 1L)
  expect_identical(alone$censor_this_period, 1L)
  expect_identical(alone$ipcw_pp, 1)

  # Every later row carries the rows before it, and nothing else. The
  # reference builds the same lagged product from its own fits.
  ref <- .ipcw_reference(out, arm = TRUE)
  expect_identical(data.table::uniqueN(ref$tstart), 6L)
  expect_equal(ref$ipcw_pp, ref$reference_ipcw, tolerance = 1e-8)

  # The lagged product is a strict prefix, so the last row of a censored
  # person-trial never carries its own censoring probability.
  censored <- out[enrollment_person_trial_id == "I14"]
  expect_identical(censored$censor_this_period, c(0L, 1L))
  ref_i14 <- ref[enrollment_person_trial_id == "I14"]
  expect_equal(
    censored$ipcw_pp[2],
    ref_i14$q_numerator[1] / ref_i14$q_denominator[1],
    tolerance = 1e-8
  )
})


# ---------------------------------------------------------------------------
# PROOF 3 -- the numerator is a fitted model
# ---------------------------------------------------------------------------

test_that("the numerator is a fitted model, not an empirical mean", {
  out <- .ipcw_run(.ipcw_cohort())

  # PROBE1W stops her bands at weeks 1, 5 and 9. No other intervention row
  # stops there, so each of those band stops holds exactly one row of the arm.
  # An empirical mean of that one row returns the row's own denominator, which
  # makes every one of her weights exactly 1.
  intervention <- out[exposed == TRUE]
  for (stop_week in c(1L, 5L, 9L)) {
    expect_identical(sum(intervention$tstop == stop_week), 1L)
  }

  probe <- out[enrollment_person_trial_id == "PROBE1W"]
  expect_identical(nrow(probe), 4L)
  expect_gt(max(abs(probe$ipcw_pp[-1] - 1)), 0.005)

  # A fitted numerator reads the band start and the offset, so it takes a
  # different value from the denominator on the same row.
  ref <- .ipcw_reference(out, arm = TRUE)
  expect_identical(data.table::uniqueN(ref$tstart), 6L)
  ref_probe <- ref[enrollment_person_trial_id == "PROBE1W"]
  expect_equal(probe$ipcw_pp, ref_probe$reference_ipcw, tolerance = 1e-8)
  expect_gt(max(abs(ref_probe$q_numerator - ref_probe$q_denominator)), 1e-4)
})


# ---------------------------------------------------------------------------
# PROOF 4 -- a stratum that cannot be estimated stops
# ---------------------------------------------------------------------------

test_that("a non-estimable stratum fails loudly", {
  trials <- list()
  for (i in 1:20) {
    trials[[length(trials) + 1L]] <- .ipcw_trial(
      paste0("I", i),
      c(0, 4, 8, 12),
      arm = TRUE,
      age = 50 + i,
      deviate_band = if (i >= 17L) 2L else NA_integer_
    )
  }
  # Every comparator switches arm in her first band, so the comparator arm
  # holds one row per person-trial and every one of them is censored. The
  # model has no uncensored row to contrast them with.
  for (i in 1:20) {
    trials[[length(trials) + 1L]] <- .ipcw_trial(
      paste0("C", i),
      c(0, 4, 8, 12),
      arm = FALSE,
      age = 45 + i,
      deviate_band = 1L
    )
  }
  d <- data.table::rbindlist(trials)

  expect_error(
    .ipcw_run(d),
    "Every one of its 20 rows is censored"
  )
  expect_error(.ipcw_run(d), "the comparator arm")
})


# ---------------------------------------------------------------------------
# Supporting behaviour. These are tested and not mutation-proven.
# ---------------------------------------------------------------------------

test_that("a stratum with no censoring takes a weight of one on every row", {
  trials <- list()
  for (i in 1:20) {
    trials[[length(trials) + 1L]] <- .ipcw_trial(
      paste0("I", i),
      c(0, 4, 8, 12),
      arm = TRUE,
      age = 50 + i,
      deviate_band = if (i >= 17L) 2L else NA_integer_
    )
  }
  # No comparator switches arm, and every comparator panel reaches the end of
  # follow-up, so the comparator arm holds no censored row at all.
  for (i in 1:20) {
    trials[[length(trials) + 1L]] <- .ipcw_trial(
      paste0("C", i),
      c(0, 4, 8, 12),
      arm = FALSE,
      age = 45 + i
    )
  }
  out <- .ipcw_run(data.table::rbindlist(trials))

  comparator <- out[exposed == FALSE]
  expect_identical(sum(comparator$censor_this_period), 0L)
  expect_identical(comparator$ipcw_pp, rep(1, nrow(comparator)))
})

test_that("a zero-width row stays out of the offset and weighs one", {
  d <- .ipcw_cohort()
  # A degenerate band. `log(0)` is `-Inf`, so this row MUST NOT reach the
  # offset. It holds no person-time, so it cannot be censored either.
  d <- data.table::rbindlist(list(
    d,
    .ipcw_trial("ZERO", c(0, 4, 4, 8, 12), arm = TRUE, age = 58)
  ))
  out <- .ipcw_run(d)

  zero <- out[enrollment_person_trial_id == "ZERO"]
  expect_identical(zero$person_weeks, c(4L, 0L, 4L, 4L))
  expect_true(all(is.finite(zero$ipcw_pp)))
  # The empty band contributes nothing, so the weight does not move across it.
  expect_identical(zero$ipcw_pp[2], zero$ipcw_pp[3])
  expect_true(all(is.finite(out$ipcw_pp)))
})

test_that("the GAM path fits the same cloglog model with the same offset", {
  skip_if_not_installed("mgcv")
  out <- .ipcw_run(.ipcw_cohort(), use_gam = TRUE)

  expect_true(all(is.finite(out$ipcw_pp)))
  first <- out[, .SD[1], by = "enrollment_person_trial_id"]
  expect_identical(first$ipcw_pp, rep(1, nrow(first)))

  one <- out[enrollment_person_trial_id == "PROBE1W"]
  four <- out[enrollment_person_trial_id == "PROBE4W"]
  expect_equal(four$ipcw_pp[2], one$ipcw_pp[2]^4, tolerance = 1e-8)
})

test_that("a pooled fit weighs the first row of every person-trial at one", {
  out <- .ipcw_run(
    .ipcw_cohort(),
    estimate_ipcw_pp_separately_by_treatment = FALSE
  )

  first <- out[, .SD[1], by = "enrollment_person_trial_id"]
  expect_identical(first$ipcw_pp, rep(1, nrow(first)))
  expect_true(all(is.finite(out$ipcw_pp)))
})
