# Cross-package comparison: swereg-TTE $irr() vs CRAN TrialEmulation.
#
# swereg's $irr() returns a Poisson incidence-rate ratio.
# TrialEmulation's initiators() returns a logistic odds ratio.
# For rare events (~2-3% per period in these simulations) log-OR ~ log-IRR,
# so they should agree closely. Tests use a slightly wider tolerance than
# the within-package correctness tests to account for the OR-vs-RR scale
# mismatch.
#
# Skipped on CRAN and when TrialEmulation isn't installed. Each test takes
# a minute or more because TrialEmulation's data_preparation is slow.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.simulate_for_comparison <- function(N = 5000, T_periods = 15, true_lor = -0.5,
                                      persist_coef = 6, L0_coef = 0.4,
                                      seed = 2026) {
  set.seed(seed)
  L0 <- rnorm(N, 0, 1)
  out <- vector("list", T_periods)
  prev_A <- integer(N)
  for (t in 0:(T_periods - 1)) {
    logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -3.0 + L0_coef * L0 + persist_coef * prev_A
    At <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Yt <- stats::rbinom(N, 1, stats::plogis(-3.5 + true_lor * At + 0.4 * L0))
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N), period = t, L0 = L0, A_t = At, Y_t = Yt)
    prev_A <- At
  }
  dt <- data.table::rbindlist(out)
  data.table::setorder(dt, id, period)
  dt
}

.true_pp_log_irr <- function(N, T_periods, true_lor) {
  events <- numeric(2); person_periods <- numeric(2)
  for (i in seq_along(c(0L, 1L))) {
    force_A <- c(0L, 1L)[i]
    set.seed(999)
    L0_po <- rnorm(N, 0, 1)
    n_ev <- 0L; n_pp <- 0L
    for (t in 0:(T_periods - 1)) {
      Y <- stats::rbinom(N, 1,
                          stats::plogis(-3.5 + true_lor * force_A + 0.4 * L0_po))
      n_ev <- n_ev + sum(Y); n_pp <- n_pp + N
    }
    events[i] <- n_ev; person_periods[i] <- n_pp
  }
  log((events[2] / person_periods[2]) / (events[1] / person_periods[1]))
}

.fit_swereg_log_irr <- function(dt) {
  sw <- data.table::copy(dt)
  sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
  sw[, baseline_L0 := L0]
  sw[, tstart := period]; sw[, tstop := period + 1L]
  sw[, time_treatment := as.logical(A_t)]
  sw[, treatment_baseline := as.logical(baseline_treatment)]
  sw[, person_weeks := 1L]
  data.table::setnames(sw, "id", "enrollment_person_trial_id")
  data.table::setnames(sw, "Y_t", "event")
  long <- sw[, list(enrollment_person_trial_id, tstart, tstop,
                    treatment_baseline, time_treatment, event,
                    person_weeks, baseline_L0)]

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
  res <- trial$irr(weight_col = "analysis_weight_pp_trunc")
  list(est = log(res$IRR),
       lower = log(res$IRR_lower), upper = log(res$IRR_upper))
}

.fit_te_log_or <- function(dt) {
  te_in <- data.table::copy(dt)
  data.table::setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
  te_in[, eligible := as.integer(period == 0)]
  res <- TrialEmulation::initiators(
    data = te_in, id = "id", period = "period", eligible = "eligible",
    treatment = "treatment", estimand_type = "PP", outcome = "outcome",
    model_var = "assigned_treatment", outcome_cov = c("L0"),
    switch_n_cov = ~ L0, switch_d_cov = ~ L0,
    use_censor_weights = FALSE, quiet = TRUE
  )
  row <- res$robust$summary[
    res$robust$summary$names == "assigned_treatment", ]
  list(est = unname(row$estimate),
       lower = unname(row$`2.5%`), upper = unname(row$`97.5%`))
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("swereg $irr() and TrialEmulation PP agree on rare-event simulated data", {
  skip_on_cran()
  skip_if_not_installed("TrialEmulation")
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  dt <- .simulate_for_comparison(
    N = 5000, T_periods = 15, true_lor = -0.5,
    persist_coef = 6, L0_coef = 0.4, seed = 2026
  )
  true_log_irr <- .true_pp_log_irr(N = 5000, T_periods = 15, true_lor = -0.5)

  sw <- .fit_swereg_log_irr(dt)
  te <- .fit_te_log_or(dt)

  # 1) Point estimates close (rare-event scale matching log-OR vs log-IRR)
  expect_lt(abs(sw$est - te$est), 0.20)
  # 2) Each package's 95% CI covers the true PP effect
  expect_gte(true_log_irr, sw$lower); expect_lte(true_log_irr, sw$upper)
  expect_gte(true_log_irr, te$lower); expect_lte(true_log_irr, te$upper)
  # 3) The two packages' CIs overlap (consistent inference)
  expect_gt(min(sw$upper, te$upper), max(sw$lower, te$lower))
})

test_that("agreement holds in a low-deviation scenario", {
  skip_on_cran()
  skip_if_not_installed("TrialEmulation")
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  # Very strong persistence -> low deviation -> tight agreement expected.
  dt <- .simulate_for_comparison(
    N = 5000, T_periods = 15, true_lor = -0.5,
    persist_coef = 8, L0_coef = 0.2, seed = 2026
  )
  true_log_irr <- .true_pp_log_irr(N = 5000, T_periods = 15, true_lor = -0.5)

  sw <- .fit_swereg_log_irr(dt)
  te <- .fit_te_log_or(dt)

  expect_lt(abs(sw$est - te$est), 0.15)
  expect_gte(true_log_irr, sw$lower); expect_lte(true_log_irr, sw$upper)
  expect_gte(true_log_irr, te$lower); expect_lte(true_log_irr, te$upper)
  expect_gt(min(sw$upper, te$upper), max(sw$lower, te$lower))
})
