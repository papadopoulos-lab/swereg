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
  log(res$IRR)
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
  unname(res$robust$summary$estimate[
    res$robust$summary$names == "assigned_treatment"])
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

  est_swereg_log_irr <- .fit_swereg_log_irr(dt)
  est_te_log_or      <- .fit_te_log_or(dt)

  # log-OR ~ log-IRR for rare events; tolerance accounts for both
  # finite-sample noise and the OR-vs-RR scale gap.
  expect_lt(abs(est_swereg_log_irr - est_te_log_or), 0.20)
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

  est_swereg_log_irr <- .fit_swereg_log_irr(dt)
  est_te_log_or      <- .fit_te_log_or(dt)

  expect_lt(abs(est_swereg_log_irr - est_te_log_or), 0.15)
})
