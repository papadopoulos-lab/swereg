# Shared synthetic-data + known-truth helpers for the ITT/PP estimand tests.
#
# Correctness work needs a data-generating process with KNOWN truth, not the
# static fake_* package data (which has no ground truth to recover). The same
# DGP is pushed through swereg (ITT + PP) and TrialEmulation (ITT + PP), so the
# generators live here and are shared across the correctness, cross-package,
# and coverage test files.
#
# DGP: contemporaneous effect of A_t on Y_t, baseline confounder L0, treatment
# persistence/switching governed by persist_coef (higher => more adherence =>
# ITT truth closer to PP truth). Verified separation (N=2e5, lor=-0.7):
#   persist=8 -> PP -0.68, ITT -0.44 (36% attenuation)  <- test sweet spot
#   persist=2 -> PP -0.68, ITT -0.04 (near-null ITT)

tte_simulate <- function(
  N = 10000,
  T_periods = 20,
  true_lor = -0.7,
  persist_coef = 8,
  L0_drives_adherence = 0.4,
  seed = 2026
) {
  set.seed(seed)
  L0 <- stats::rnorm(N)
  out <- vector("list", T_periods)
  prev_A <- integer(N)
  for (t in 0:(T_periods - 1)) {
    logit_A <- if (t == 0) {
      -0.3 + 0.6 * L0
    } else {
      -3.0 + L0_drives_adherence * L0 + persist_coef * prev_A
    }
    At <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Yt <- stats::rbinom(N, 1, stats::plogis(-3.5 + true_lor * At + 0.4 * L0))
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N),
      period = t,
      L0 = L0,
      A_t = At,
      Y_t = Yt
    )
    prev_A <- At
  }
  dt <- data.table::rbindlist(out)
  data.table::setorder(dt, id, period)
  attr(dt, "params") <- list(
    N = N,
    T_periods = T_periods,
    true_lor = true_lor,
    persist_coef = persist_coef,
    L0_drives_adherence = L0_drives_adherence,
    seed = seed
  )
  dt
}

# PP truth: sustained-treatment contrast (force A_t = a every period),
# standardised over L0. Computed as a FIRST-EVENT incidence rate to match
# swereg's estimand exactly: each person is censored at their first event, the
# numerator is first events and the denominator is person-periods AT RISK (not
# the fixed N x T). Summing recurrent events over N x T would target a
# different quantity than the system under test.
tte_true_pp_log_irr <- function(params, N_truth = 200000, seed = 999) {
  rate <- numeric(2)
  for (i in 1:2) {
    a <- c(0L, 1L)[i]
    set.seed(seed)
    L0 <- stats::rnorm(N_truth)
    at_risk <- rep(TRUE, N_truth)
    ev <- 0L
    pt <- 0L
    for (t in 0:(params$T_periods - 1)) {
      haz <- stats::plogis(-3.5 + params$true_lor * a + 0.4 * L0)
      Y <- stats::rbinom(N_truth, 1, haz)
      pt <- pt + sum(at_risk) # person-time at risk this period
      new_ev <- at_risk & (Y == 1L) # first events only
      ev <- ev + sum(new_ev)
      at_risk <- at_risk & !new_ev # leave the risk set after first event
    }
    rate[i] <- ev / pt
  }
  out <- log(rate[2] / rate[1])
  attr(out, "p0") <- rate[1] # reference-arm per-period hazard (for OR->IRR)
  out
}

# ITT truth: force baseline A_0 = a0, then let A_t switch NATURALLY (real
# per-period model), contrast by baseline arm, standardised over L0 -- the
# do(A_0=1) vs do(A_0=0) estimand that IPW-only ITT estimates. Same FIRST-EVENT
# incidence-rate construction as the PP truth (censor at first event,
# person-time-at-risk denominator).
tte_true_itt_log_irr <- function(params, N_truth = 200000, seed = 999) {
  rate <- numeric(2)
  for (i in 1:2) {
    a0 <- c(0L, 1L)[i]
    set.seed(seed)
    L0 <- stats::rnorm(N_truth)
    at_risk <- rep(TRUE, N_truth)
    prev_A <- rep(a0, N_truth)
    ev <- 0L
    pt <- 0L
    for (t in 0:(params$T_periods - 1)) {
      At <- if (t == 0) {
        rep(a0, N_truth)
      } else {
        stats::rbinom(
          N_truth,
          1,
          stats::plogis(
            -3.0 +
              params$L0_drives_adherence * L0 +
              params$persist_coef * prev_A
          )
        )
      }
      haz <- stats::plogis(-3.5 + params$true_lor * At + 0.4 * L0)
      Y <- stats::rbinom(N_truth, 1, haz)
      pt <- pt + sum(at_risk) # person-time at risk this period
      new_ev <- at_risk & (Y == 1L) # first events only
      ev <- ev + sum(new_ev)
      at_risk <- at_risk & !new_ev # leave the risk set after first event
      prev_A <- At
    }
    rate[i] <- ev / pt
  }
  out <- log(rate[2] / rate[1])
  attr(out, "p0") <- rate[1] # reference-arm per-period hazard (for OR->IRR)
  out
}

# Convert a logistic log-OR to a log rate/risk ratio on a common scale, so a
# Poisson-IRR estimate (swereg, truth) and a pooled-logistic-OR estimate
# (TrialEmulation) can be compared apples-to-apples instead of leaning on the
# rare-event approximation. Zhang & Yu (1998): RR = OR / (1 - p0 + p0 * OR),
# where p0 is the reference-arm per-period event risk.
# NOTE: removes the OR-vs-IRR *scale* gap only; it does NOT remove the
# conditional-vs-marginal (OR non-collapsibility) gap when one estimator
# adjusts for a covariate and the other marginalises (matters for ITT).
tte_log_or_to_log_irr <- function(log_or, p0) {
  or <- exp(log_or)
  log(or / (1 - p0 + p0 * or))
}

# Apply INDEPENDENT loss to follow-up: each person drops out at a random
# period drawn independently of treatment, outcome and L0 (geometric hazard),
# losing all later periods. Because the censoring is independent, the true
# incidence rate ratio is unchanged -- so ITT (which uses no IPCW for loss)
# should still recover the same truth. Used to stress-test the
# independent-loss assumption that the complete-panel DGP never exercises.
tte_apply_independent_loss <- function(dt, hazard = 0.05, seed = 7) {
  set.seed(seed)
  d <- data.table::copy(dt)
  ids <- unique(d$id)
  drop_at <- stats::rgeom(length(ids), hazard) # 0..Inf, independent of all
  d[, .drop := drop_at[match(id, ids)]]
  d <- d[period <= .drop] # keep periods 0..drop_at
  d[, .drop := NULL]
  d[]
}

# Convert simulated person-period data into swereg-TTE trial-long format.
# The SAME long table feeds both estimands; only the s4 call differs.
tte_build_long <- function(dt) {
  sw <- data.table::copy(dt)
  sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
  sw[, baseline_L0 := L0]
  sw[, tstart := period]
  sw[, tstop := period + 1L]
  sw[, time_treatment := as.logical(A_t)]
  sw[, treatment_baseline := as.logical(baseline_treatment)]
  sw[, person_weeks := 1L]
  data.table::setnames(sw, "id", "enrollment_person_trial_id")
  data.table::setnames(sw, "Y_t", "event")
  sw[, list(
    enrollment_person_trial_id,
    tstart,
    tstop,
    treatment_baseline,
    time_treatment,
    event,
    person_weeks,
    baseline_L0
  )]
}

tte_make_design <- function(long) {
  TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = c("baseline_L0"),
    follow_up_time = max(long$tstop)
  )
}

# Run the full pipeline for one estimand and return the $irr() one-row table.
#   estimand = "pp"  -> per-protocol censoring + IPCW; weight analysis_weight_pp_trunc
#   estimand = "itt" -> no switch censoring, no IPCW;  weight ipw_trunc
# NOTE: the estimand = "itt" branch pins the API that Phase 1 implements; it
# fails until s4_prepare_for_analysis() gains estimand= and irr()'s guard is
# relaxed for ITT-tagged datasets.
tte_run_irr <- function(long, estimand = c("pp", "itt")) {
  estimand <- match.arg(estimand)
  trial <- TTEEnrollment$new(long, tte_make_design(long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  if (estimand == "pp") {
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = max(long$tstop),
      estimate_ipcw_pp_with_gam = TRUE,
      estimate_ipcw_pp_separately_by_treatment = TRUE
    )
    trial$data[, analysis_weight_pp_trunc := ipw_trunc * ipcw_pp]
    trial$irr(weight_col = "analysis_weight_pp_trunc")
  } else {
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = max(long$tstop),
      estimand = "itt"
    )
    trial$irr(weight_col = "ipw_trunc")
  }
}
