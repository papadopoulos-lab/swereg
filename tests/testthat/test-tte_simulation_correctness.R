# End-to-end correctness tests for the swereg-TTE PP pipeline using
# simulations with a KNOWN true per-protocol effect.
#
# These tests verify the package recovers the true effect under controlled
# data-generating processes. They use the production analysis path
# (`$s4_prepare_for_analysis()` -> `$combine_weights()` -> `$irr()`) rather
# than rolling their own outcome regression. They are slow (a minute or
# more each) so they are skipped on CRAN. Run locally with:
#   devtools::test(filter = "tte_simulation_correctness")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Generate a longitudinal dataset for TTE simulation testing.
#'
#' @param N integer, number of persons
#' @param T_periods integer, number of follow-up periods
#' @param true_lor numeric, true per-period log-OR of treatment on outcome
#' @param persist_coef numeric, coefficient on prev_A controlling adherence
#'   persistence. Higher => more adherence.
#' @param L0_drives_adherence numeric, coefficient on L0 in the per-period
#'   treatment model. Non-zero => L0 affects adherence (selection bias).
#' @param seed integer
.simulate_pp <- function(N = 5000, T_periods = 20, true_lor = -0.5,
                          persist_coef = 4, L0_drives_adherence = 0.4,
                          seed = 2026) {
  set.seed(seed)
  L0 <- rnorm(N, 0, 1)
  out <- vector("list", T_periods)
  prev_A <- integer(N)
  for (t in 0:(T_periods - 1)) {
    logit_A <- if (t == 0) {
      -0.3 + 0.6 * L0
    } else {
      -3.0 + L0_drives_adherence * L0 + persist_coef * prev_A
    }
    At <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Yt <- stats::rbinom(N, 1,
                        stats::plogis(-3.5 + true_lor * At + 0.4 * L0))
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N), period = t, L0 = L0, A_t = At, Y_t = Yt
    )
    prev_A <- At
  }
  dt <- data.table::rbindlist(out)
  data.table::setorder(dt, id, period)
  attr(dt, "params") <- list(N = N, T = T_periods, true_lor = true_lor,
                              persist_coef = persist_coef,
                              L0_drives_adherence = L0_drives_adherence,
                              seed = seed)
  dt
}

#' Compute the TRUE log incidence-rate-ratio for PP via potential outcomes.
#'
#' This is the quantity `$irr()` estimates: a Poisson rate ratio of events
#' per person-period under always-treated vs never-treated regimes.
.true_pp_log_irr <- function(params) {
  events <- numeric(2); person_periods <- numeric(2)
  for (i in seq_along(c(0L, 1L))) {
    force_A <- c(0L, 1L)[i]
    set.seed(999)
    L0_po <- rnorm(params$N, 0, 1)
    n_ev <- 0L; n_pp <- 0L
    for (t in 0:(params$T - 1)) {
      logit_Y <- -3.5 + params$true_lor * force_A + 0.4 * L0_po
      Y <- stats::rbinom(params$N, 1, stats::plogis(logit_Y))
      n_ev <- n_ev + sum(Y)
      n_pp <- n_pp + params$N
    }
    events[i] <- n_ev
    person_periods[i] <- n_pp
  }
  rate0 <- events[1] / person_periods[1]
  rate1 <- events[2] / person_periods[2]
  log(rate1 / rate0)
}

#' Convert a simulated person-period data frame into trial-long format
#' that swereg-TTE consumes.
.build_long <- function(dt) {
  sw <- data.table::copy(dt)
  sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
  sw[, baseline_L0 := L0]
  sw[, tstart := period]
  sw[, tstop := period + 1L]
  sw[, time_treatment := as.logical(A_t)]
  sw[, treatment_baseline := as.logical(baseline_treatment)]
  sw[, person_weeks := 1L]    # required by $irr() / $rates()
  data.table::setnames(sw, "id", "enrollment_person_trial_id")
  data.table::setnames(sw, "Y_t", "event")
  sw[, list(enrollment_person_trial_id, tstart, tstop,
            treatment_baseline, time_treatment, event,
            person_weeks, baseline_L0)]
}

#' Run the full PP pipeline and return $irr() output.
.run_irr <- function(long) {
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = c("baseline_L0"),
    follow_up_time = max(long$tstop)
  )
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = "event", follow_up = max(long$tstop),
    estimate_ipcw_pp_with_gam = TRUE,
    estimate_ipcw_pp_separately_by_treatment = TRUE
  )
  # combine_weights is private; multiply directly into data
  trial$data[, analysis_weight_pp_trunc := ipw_trunc * ipcw_pp]
  trial$irr(weight_col = "analysis_weight_pp_trunc")
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("$irr() recovers truth when there's no protocol deviation", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  # Force perfect adherence: A_t = A_0 throughout.
  set.seed(2026)
  N <- 5000; T_periods <- 20
  L0 <- rnorm(N, 0, 1)
  A_0 <- rbinom(N, 1, plogis(-0.3 + 0.6 * L0))
  out <- vector("list", T_periods)
  for (t in 0:(T_periods - 1)) {
    Yt <- rbinom(N, 1, plogis(-3.5 + (-0.5) * A_0 + 0.4 * L0))
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N), period = t, L0 = L0, A_t = A_0, Y_t = Yt)
  }
  dt <- data.table::rbindlist(out)
  data.table::setorder(dt, id, period)
  params <- list(N = N, T = T_periods, true_lor = -0.5)
  true_log_irr <- .true_pp_log_irr(params)

  long <- .build_long(dt)
  irr_result <- .run_irr(long)
  est_log_irr <- log(irr_result$IRR)

  expect_lt(abs(est_log_irr - true_log_irr), 0.10)
})

test_that("$irr() recovers truth with baseline-L0-driven deviation", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  dt <- .simulate_pp(N = 5000, T_periods = 20, true_lor = -0.5,
                     persist_coef = 6, L0_drives_adherence = 0.4,
                     seed = 2026)
  true_log_irr <- .true_pp_log_irr(attr(dt, "params"))

  long <- .build_long(dt)
  irr_result <- .run_irr(long)
  est_log_irr <- log(irr_result$IRR)

  expect_lt(abs(est_log_irr - true_log_irr), 0.10)
})

test_that("s4_prepare_for_analysis drops all censoring-event rows", {
  skip_on_cran()
  skip_if_not_installed("mgcv")

  dt <- .simulate_pp(N = 2000, T_periods = 15, true_lor = -0.5,
                     persist_coef = 3, L0_drives_adherence = 0.4,
                     seed = 2026)
  long <- .build_long(dt)

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = c("baseline_L0"),
    follow_up_time = max(long$tstop)
  )
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = "event", follow_up = max(long$tstop)
  )

  expect_equal(sum(trial$data$censor_this_period == 1, na.rm = TRUE), 0L)
})
