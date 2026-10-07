# The per-protocol censoring weight is one model per cause, fitted in sequence.
#
#   loss        fitted on the rows without an outcome
#   deviation   fitted on the rows without an outcome that were not lost
#   time zero   logistic, fitted on every person-trial under follow-up at
#               time zero, including those that deviate there and keep no row
#
#   ipcw_pp = ipcw_pp_time_zero * ipcw_pp_loss * ipcw_pp_deviation
#
# The deviation model conditions on not being lost, so the product of the
# three uncensoring probabilities is the joint probability of remaining
# uncensored. This file pins:
#
# 1. the risk set of each model, read off the data each `stats::glm()` fit
#    received;
# 2. on s4, the fitted weights against the oracle weights;
# 3. the joint uncensored probability on a hand-built panel of two person-trial
#    types with known non-rare probabilities;
# 4. the per-cause formula records, including the not-fitted sentinel;
# 5. that ITT fits no censoring weight.
#
# `$s5_prepare_outcome()` and `$s6_ipcw_pp()` are private, so every test drives
# the public `$s4_prepare_for_analysis()`.

skip_if_not_installed("data.table")

# --- the hand-built panel -----------------------------------------------------
#
# Width 1 and follow-up 2, so each person-trial has rows at `tstart` 0 and 1.
# The row at `tstart = 1` ends at the planned end, so it can be neither lost nor
# followed by a deviation. Every censoring therefore falls on the row at
# `tstart = 0`.
#
# Each arm holds 100 person-trials with `x = 0` and 100 with `x = 1`:
#
#                                     x = 0   x = 1
#   deviate at time zero, no row         10      30
#   outcome on the first row             10      10
#   lost after the first row             16      24
#   deviate at the start of row 2        16      18
#   complete, two rows                   48      18
#
# So for x = 0 and x = 1:
#   P(deviation at time zero)                       10/100 = 0.1  30/100 = 0.3
#   P(loss | no outcome on the row)                 16/80  = 0.2  24/60  = 0.4
#   P(deviation | no outcome, not lost)             16/64  = 0.25 18/36  = 0.5
#   P(uncensored through the start of row 2)        0.9 * 0.8 * 0.75 = 0.54
#                                                   0.7 * 0.6 * 0.5  = 0.21
#
# The second row of a complete person-trial is the row of that joint
# probability: 48 of the 80 event-free x = 0 person-trials that passed time zero
# reach it, which is 0.8 * 0.75.
.ipc_counts <- list(
  "0" = c(zero = 10L, event = 10L, lost = 16L, dev = 16L, complete = 48L),
  "1" = c(zero = 30L, event = 10L, lost = 24L, dev = 18L, complete = 18L)
)

.ipc_panel <- function(counts = .ipc_counts, arms = c(TRUE, FALSE)) {
  out <- list()
  for (arm in arms) {
    for (x in names(counts)) {
      n <- counts[[x]]
      kind <- rep(names(n), times = n)
      for (i in seq_along(kind)) {
        id <- sprintf("%s-x%s-%s-%02d", if (arm) "I" else "C", x, kind[i], i)
        on_tx <- switch(
          kind[i],
          zero = c(!arm, !arm),
          dev = c(arm, !arm),
          c(arm, arm)
        )
        d <- data.table::data.table(
          enrollment_person_trial_id = id,
          tstart = 0:1,
          tstop = 1:2,
          exposed = arm,
          on_tx = on_tx,
          died = c(kind[i] == "event", FALSE),
          x = as.numeric(x)
        )
        # A lost person-trial has no second row, so its record ends at week 1.
        if (kind[i] == "lost") {
          d <- d[1L]
        }
        out[[id]] <- d
      }
    }
  }
  data.table::rbindlist(out)
}

.ipc_design <- function(follow_up = 2L) {
  TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    outcome_vars = "died",
    confounder_vars = "x",
    follow_up_time = follow_up
  )
}

# The GLM route, so every censoring fit goes through `stats::glm()`. The
# row-level models separate at `tstart = 1`, which holds no censoring, and
# warn. The warning is about the fixture.
.ipc_run <- function(d, follow_up = 2L, estimand = "pp", ...) {
  trial <- TTEEnrollment$new(data.table::copy(d), .ipc_design(follow_up))
  suppressWarnings({
    trial$s2_ipw()
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = follow_up,
      estimand = estimand,
      estimate_ipcw_pp_with_gam = FALSE,
      ...
    )
  })
  trial
}

# Run `fn` with `stats::glm()` traced. Each fit records the left-hand side of
# its formula, its arm, and the rows it received with three column sums. The
# propensity fit of `$s2_ipw()` is recorded too, under `exposed`.
.ipc_capture <- function(fn) {
  cap <- new.env(parent = emptyenv())
  cap$fits <- list()
  suppressMessages(trace(
    "glm",
    where = asNamespace("stats"),
    print = FALSE,
    tracer = bquote({
      .ipc_d <- data
      .ipc_sum <- function(col) {
        if (col %in% names(.ipc_d)) sum(.ipc_d[[col]]) else NA_integer_
      }
      assign(
        "fits",
        c(
          get("fits", envir = .(cap)),
          list(list(
            lhs = all.vars(formula)[1],
            arm = paste(sort(unique(.ipc_d[["exposed"]])), collapse = "/"),
            n = nrow(.ipc_d),
            n_event = .ipc_sum("event"),
            n_lost = .ipc_sum("censor_loss"),
            n_zero = .ipc_sum("deviation_time_zero")
          ))
        ),
        envir = .(cap)
      )
    })
  ))
  on.exit(suppressMessages(untrace("glm", where = asNamespace("stats"))))
  trial <- fn()
  fits <- data.table::rbindlist(cap$fits)
  return(list(trial = trial, fits = fits))
}


# ---------------------------------------------------------------------------
# (1) the risk set of each model
# ---------------------------------------------------------------------------

test_that("each censoring model fits on its own risk set", {
  got <- .ipc_capture(function() .ipc_run(.ipc_panel()))
  fits <- got$fits
  trial <- got$trial

  # Rows per arm after s5: one row for each person-trial with an outcome, a
  # loss or a deviation, and two for each complete one. x = 0 gives
  # 10 + 16 + 16 + 2 * 48 = 138 and x = 1 gives 10 + 24 + 18 + 2 * 18 = 88.
  rows <- trial$data[, .N, keyby = exposed]
  expect_identical(rows$N, c(226L, 226L))

  for (a in c("TRUE", "FALSE")) {
    f <- fits[fits$arm == a]
    loss <- f[f$lhs == "censor_loss"]
    dev <- f[f$lhs == "censor_deviation"]
    zero <- f[f$lhs == "deviation_time_zero"]

    # Loss: the denominator and the numerator each see the 226 rows less the
    # 20 rows with an outcome. No row with an outcome is in it.
    expect_identical(loss$n, c(206L, 206L), label = paste("loss rows", a))
    expect_identical(loss$n_event, c(0L, 0L), label = paste("loss events", a))

    # Deviation: the loss rows less the 40 lost rows. No lost row is in it.
    expect_identical(dev$n, c(166L, 166L), label = paste("deviation rows", a))
    expect_identical(dev$n_lost, c(0L, 0L), label = paste("deviation lost", a))
    expect_identical(dev$n_event, c(0L, 0L), label = paste("deviation events", a))

    # Time zero: all 200 person-trials of the arm, with the 40 that deviate at
    # time zero and keep no row.
    expect_identical(zero$n, 200L, label = paste("time-zero rows", a))
    expect_identical(zero$n_zero, 40L, label = paste("time-zero deviators", a))
  }

  # The 40 person-trials of each arm that deviate at time zero keep no row.
  expect_false(any(grepl("-zero-", trial$data$enrollment_person_trial_id)))
  expect_identical(nrow(trial$time_zero_deviation), 400L)
  expect_identical(sum(trial$time_zero_deviation$deviation_time_zero), 80L)
})

test_that("the cause columns split censor_this_period, and a gap at the deviation week is loss", {
  out <- .ipc_run(.ipc_panel())$data
  expect_identical(
    out$censor_this_period,
    out$censor_loss + out$censor_deviation
  )
  expect_false(any(out$censor_loss == 1L & out$censor_deviation == 1L))
  expect_identical(sum(out$censor_loss), 80L)
  expect_identical(sum(out$censor_deviation), 68L)
  expect_identical(sum(out$event == 1L & out$censor_this_period == 1L), 0L)

  # A gap and a deviation at the same boundary. Person-trial TIE is
  # discordant from `tstart = 2`, and her first absent week is also week 2.
  # The tie rule labels the stop loss, so `censor_deviation` stays 0.
  # Person-trial DEV has the same deviation and no gap.
  d <- data.table::CJ(enrollment_person_trial_id = sprintf("P%02d", 1:40), tstart = 0:3)
  d[, tstop := tstart + 1L]
  d[, exposed := enrollment_person_trial_id <= "P20"]
  d[, on_tx := exposed]
  d[, died := FALSE]
  d[, x := as.numeric(as.integer(substr(enrollment_person_trial_id, 2, 3)) %% 5L)]
  d[enrollment_person_trial_id %in% c("P01", "P03") & tstart >= 2L, on_tx := FALSE]
  d[, weeks_to_observation_gap := NA_integer_]
  d[enrollment_person_trial_id == "P01", weeks_to_observation_gap := 2L]
  out <- .ipc_run(d, follow_up = 4L)$data

  tie <- out[enrollment_person_trial_id == "P01"][order(tstart)]
  expect_identical(tie$tstop, c(1L, 2L))
  expect_identical(tie$censor_loss, c(0L, 1L))
  expect_identical(tie$censor_deviation, c(0L, 0L))

  dev <- out[enrollment_person_trial_id == "P03"][order(tstart)]
  expect_identical(dev$censor_loss, c(0L, 0L))
  expect_identical(dev$censor_deviation, c(0L, 1L))
})


# ---------------------------------------------------------------------------
# (2) s4: the fitted weights against the oracle weights
# ---------------------------------------------------------------------------
#
# s4 (helper-tte_scenarios.R) has loss driven by L1, deviation and time-zero
# discordance driven by L0, and switching from the first follow-up week. The
# oracle weight of the row of interval k is
#
#   1 / ((1 - p0) * ((1 - hl) * (1 - hd))^k)
#
# from the true probabilities. swereg's weight is stabilised, and its
# numerators depend only on the arm and `tstart`. Dividing each weight by its
# mean in the (arm, tstart) cell removes the numerators, so the two normalised
# weights MUST agree row by row.
#
# N = 20,000 and seed 2101, the default GAM route in each arm. Measured on
# 2026-10-07 over seeds 2101 to 2105, with the time-zero factor and then
# without it:
#
#                                         with factor       without factor
#   mean absolute difference, all rows    0.014 to 0.018    0.087 to 0.099
#   mean absolute difference, tstart = 0  0.0046 to 0.0073  0.083 to 0.086
#   correlation, all rows                 0.969 to 0.994    0.910 to 0.967
#   correlation of the time-zero factor   0.996 to 0.998    n/a
#
# The mean absolute difference carries the test. Its thresholds, 0.03 and
# 0.02, are 1.7 and 2.7 times the largest error measured with the factor. They
# are 0.34 and 0.24 times the smallest error measured without it. The
# correlation over all
# rows moves with a few large weights at late `tstart`, and it hardly moves
# without the factor, so its threshold of 0.95 only checks the direction.

.ipc_s4 <- new.env(parent = emptyenv())
.ipc_s4_rows <- function() {
  if (is.null(.ipc_s4$dt)) {
    d <- scen_simulate_s4(N = 20000L, seed = 2101L)
    trial <- suppressWarnings(scen_prepare_s4(scen_long_s4(d)))
    dt <- data.table::copy(trial$data)
    or <- scen_oracle_weights_s4(dt)
    dt[, w_oracle := or$w_time_zero * or$w_loss * or$w_deviation]
    dt[, o_time_zero := or$w_time_zero]
    dt[,
      `:=`(
        r_fit = ipcw_pp / mean(ipcw_pp),
        r_oracle = w_oracle / mean(w_oracle)
      ),
      by = c("treatment_baseline", "tstart")
    ]
    .ipc_s4$dt <- dt
    .ipc_s4$trial <- trial
  }
  return(.ipc_s4$dt)
}

test_that("on s4 the fitted censoring weights reproduce the oracle weights", {
  dt <- .ipc_s4_rows()

  # The scenario censors at a rate that is not rare.
  tz <- .ipc_s4$trial$time_zero_deviation
  expect_identical(nrow(tz), 20000L)
  expect_gt(mean(tz$deviation_time_zero), 0.10)
  expect_gt(dt[event == 0L, mean(censor_loss)], 0.03)
  expect_gt(dt[event == 0L & censor_loss == 0L, mean(censor_deviation)], 0.02)

  expect_gt(stats::cor(dt$r_fit, dt$r_oracle), 0.95)
  expect_lt(mean(abs(dt$r_fit - dt$r_oracle)), 0.03)

  # At time zero only the time-zero factor varies within an arm.
  at_zero <- dt[tstart == 0L, list(mad = mean(abs(r_fit - r_oracle))), keyby = treatment_baseline]
  expect_true(all(at_zero$mad < 0.02), label = paste(at_zero$mad, collapse = ", "))
  expect_gt(dt[tstart == 0L, stats::cor(ipcw_pp_time_zero, o_time_zero)], 0.99)
})


# ---------------------------------------------------------------------------
# (3) the joint uncensored probability
# ---------------------------------------------------------------------------

test_that("the three factors give the joint probability of remaining uncensored", {
  out <- .ipc_run(.ipc_panel())$data
  arm <- out[exposed == TRUE]
  r0 <- arm[tstart == 0L, list(w = ipcw_pp[1], tz = ipcw_pp_time_zero[1]), keyby = x]
  r1 <- arm[
    tstart == 1L,
    list(
      w = ipcw_pp[1],
      loss = ipcw_pp_loss[1],
      dev = ipcw_pp_deviation[1],
      tz = ipcw_pp_time_zero[1],
      n = .N
    ),
    keyby = x
  ]
  expect_identical(r1$n, c(48L, 18L))

  # The numerators are the marginal proportions of the arm: 40 of 200 deviate
  # at time zero, 40 of 140 event-free rows at `tstart = 0` are lost, and 34
  # of 100 that were not lost deviate.
  tol <- 1e-6
  expect_equal(r0$tz, (1 - 40 / 200) / (1 - c(0.1, 0.3)), tolerance = tol)
  expect_equal(r1$loss, (1 - 40 / 140) / (1 - c(0.2, 0.4)), tolerance = tol)
  expect_equal(r1$dev, (1 - 34 / 100) / (1 - c(0.25, 0.5)), tolerance = tol)

  # The weight of the second row is the inverse of the joint probability of
  # remaining uncensored through its start, times the arm's numerator. The
  # numerator is the same for both person-trial types, so the ratio of their
  # weights is the inverse ratio of their joint probabilities: 0.54 / 0.21.
  expect_equal(r1$w[2] / r1$w[1], 0.54 / 0.21, tolerance = tol)
  expect_equal(r0$w[2] / r0$w[1], 0.9 / 0.7, tolerance = tol)
  expect_equal(r1$w, r1$tz * r1$loss * r1$dev, tolerance = 1e-12)
})


# ---------------------------------------------------------------------------
# (4) the per-cause formula records
# ---------------------------------------------------------------------------

test_that("each stratum records one fitted model per cause", {
  forms <- .ipc_run(.ipc_panel())$ipcw_formulas
  expect_setequal(names(forms), c("the intervention arm", "the comparator arm"))
  for (label in names(forms)) {
    rec <- forms[[label]]
    expect_identical(names(rec), c("loss", "deviation", "time_zero"))
    expect_true(isTRUE(rec$loss$fitted), label = label)
    expect_true(isTRUE(rec$deviation$fitted), label = label)
    expect_true(isTRUE(rec$time_zero$fitted), label = label)
    expect_identical(all.vars(rec$loss$denominator)[1], "censor_loss")
    expect_identical(all.vars(rec$loss$numerator)[1], "censor_loss")
    expect_identical(all.vars(rec$deviation$denominator)[1], "censor_deviation")
    expect_identical(all.vars(rec$deviation$numerator)[1], "censor_deviation")
    expect_identical(
      deparse1(rec$loss$denominator),
      "censor_loss ~ factor(tstart) + x + offset(log(person_weeks))"
    )
    expect_identical(
      deparse1(rec$deviation$numerator),
      "censor_deviation ~ factor(tstart) + offset(log(person_weeks))"
    )
    expect_identical(deparse1(rec$time_zero$denominator), "deviation_time_zero ~ x")
    expect_identical(deparse1(rec$time_zero$numerator), "deviation_time_zero ~ 1")
  }
})

test_that("a cause with no censoring records the not-fitted sentinel and weighs one", {
  # No loss and no deviation at time zero: deviation is the only cause.
  counts <- list(
    "0" = c(zero = 0L, event = 10L, lost = 0L, dev = 16L, complete = 48L),
    "1" = c(zero = 0L, event = 10L, lost = 0L, dev = 18L, complete = 18L)
  )
  trial <- .ipc_run(.ipc_panel(counts))
  for (label in names(trial$ipcw_formulas)) {
    rec <- trial$ipcw_formulas[[label]]
    expect_identical(rec$loss$fitted, FALSE)
    expect_match(rec$loss$reason, "is censored by loss", fixed = TRUE)
    expect_identical(rec$time_zero$fitted, FALSE)
    expect_match(rec$time_zero$reason, "deviates at time zero", fixed = TRUE)
    expect_identical(names(rec$loss), c("fitted", "reason"))
    expect_true(isTRUE(rec$deviation$fitted))
  }
  expect_identical(trial$data$ipcw_pp_loss, rep(1, nrow(trial$data)))
  expect_identical(trial$data$ipcw_pp_time_zero, rep(1, nrow(trial$data)))
  expect_true(all(is.finite(trial$data$ipcw_pp)))
})

test_that("an arm in which every person-trial deviates at time zero fits nothing and does not stop", {
  # Every comparator deviates at time zero, so the comparator arm keeps no row.
  zero_arm <- .ipc_panel(
    list(
      "0" = c(zero = 30L, event = 0L, lost = 0L, dev = 0L, complete = 0L),
      "1" = c(zero = 30L, event = 0L, lost = 0L, dev = 0L, complete = 0L)
    ),
    arms = FALSE
  )
  d <- data.table::rbindlist(list(.ipc_panel(arms = TRUE), zero_arm))
  trial <- NULL
  expect_no_error(trial <- .ipc_run(d))
  expect_identical(nrow(trial$data[exposed == FALSE]), 0L)

  cmp <- trial$ipcw_formulas[["the comparator arm"]]
  expect_identical(cmp$loss$fitted, FALSE)
  expect_match(cmp$loss$reason, "has no follow-up row at risk of loss", fixed = TRUE)
  expect_identical(cmp$deviation$fitted, FALSE)
  expect_match(cmp$deviation$reason, "has no follow-up row at risk of deviation", fixed = TRUE)
  expect_identical(cmp$time_zero$fitted, FALSE)
  expect_match(
    cmp$time_zero$reason,
    "Every one of the 60 person-trials of the comparator arm deviates at time zero",
    fixed = TRUE
  )
  # The 60 comparators are in the record, all deviating at time zero.
  rec <- trial$time_zero_deviation[exposed == FALSE]
  expect_identical(nrow(rec), 60L)
  expect_identical(sum(rec$deviation_time_zero), 60L)

  # The intervention arm is fitted as before.
  tx <- trial$ipcw_formulas[["the intervention arm"]]
  expect_true(isTRUE(tx$loss$fitted) && isTRUE(tx$deviation$fitted) && isTRUE(tx$time_zero$fitted))
  expect_true(all(is.finite(trial$data$ipcw_pp)))
})

test_that("a pooled fit records the three causes under one label", {
  forms <- .ipc_run(
    .ipc_panel(),
    estimate_ipcw_pp_separately_by_treatment = FALSE
  )$ipcw_formulas
  expect_identical(names(forms), "the pooled cohort")
  expect_identical(names(forms[[1]]), c("loss", "deviation", "time_zero"))
  expect_true(all(vapply(forms[[1]], function(r) isTRUE(r$fitted), logical(1))))
})

test_that("a custom censoring_var stops", {
  trial <- TTEEnrollment$new(.ipc_panel(), .ipc_design())
  suppressWarnings(trial$s2_ipw())
  expect_error(
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = 2L,
      censoring_var = "my_censor"
    ),
    "censor_loss and censor_deviation"
  )
})


# ---------------------------------------------------------------------------
# (5) ITT fits no censoring weight
# ---------------------------------------------------------------------------

test_that("the intention-to-treat estimand fits no censoring weight", {
  trial <- .ipc_run(.ipc_panel(), estimand = "itt")
  cols <- c("ipcw_pp", "ipcw_pp_loss", "ipcw_pp_deviation", "ipcw_pp_time_zero")
  expect_false(any(cols %in% names(trial$data)))
  expect_null(trial$ipcw_formulas)
  expect_null(trial$time_zero_deviation)
  # ITT never censors at a deviation, so only loss can censor.
  expect_identical(sum(trial$data$censor_deviation), 0L)
  expect_identical(trial$data$censor_this_period, trial$data$censor_loss)
})
