# Adversarial STRESS scenarios for the swereg TTE estimators (test-tte_stress_matrix.R).
#
# Where helper-tte_scenarios.R fixes the true effect and varies only the
# nuisance (confounding, loss) across three canned scenarios, this helper
# exposes EVERY knob of the same person-period DGP so a single generator can
# drive the full stress battery -- null / harmful / rare outcomes, near
# positivity violations, heavy informative attrition, arm imbalance -- plus a
# separate time-varying-confounding generator with treatment-confounder
# feedback. The estimand and the truth target are identical to the validation
# matrix (first-event IRR, standardised over the baseline confounder), so the
# fit wrappers reuse scen_fit_swereg()/scen_fit_te()/tte_build_long() verbatim.
#
# Reference numbers observed for each cell live next to the assertions in
# test-tte_stress_matrix.R; the tolerances are set from them.

# Generalized person-period DGP (id, period, L0, A_t, Y_t). Superset of
# scen_simulate(): a0_int/a0_L0 set the baseline treatment logit; sw_int/sw_L0/
# persist the per-period switching logit; out_int/lor/L0_Y the outcome logit;
# loss = none|independent|informative applies geometric drop-out (informative
# hazard rises with the confounder L0, so drop-out selects on L0).
stress_sim <- function(
  N,
  T_periods,
  lor,
  out_int = -3.5,
  L0_Y = 0.4,
  a0_int = -0.3,
  a0_L0 = 0.6,
  sw_int = -3.0,
  sw_L0 = 0.4,
  persist = 8,
  loss = "none",
  loss_haz = 0.06,
  loss_int = -2.4,
  loss_L0 = 0.9,
  seed = 2026
) {
  set.seed(seed)
  L0 <- stats::rnorm(N)
  out <- vector("list", T_periods)
  prev_A <- integer(N)
  for (t in 0:(T_periods - 1L)) {
    logit_A <- if (t == 0) {
      a0_int + a0_L0 * L0
    } else {
      sw_int + sw_L0 * L0 + persist * prev_A
    }
    A <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Y <- stats::rbinom(N, 1, stats::plogis(out_int + lor * A + L0_Y * L0))
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N),
      period = t,
      L0 = L0,
      A_t = A,
      Y_t = Y
    )
    prev_A <- A
  }
  d <- data.table::rbindlist(out)
  data.table::setorder(d, id, period)
  if (loss != "none") {
    set.seed(seed + 99L)
    ids <- unique(d$id)
    L0i <- d[period == 0L, L0]
    haz <- if (loss == "independent") {
      rep(loss_haz, length(ids))
    } else {
      stats::plogis(loss_int + loss_L0 * L0i)
    }
    drop_at <- stats::qgeom(stats::runif(length(ids)), haz)
    d[, .drop := drop_at[match(id, ids)]]
    d <- d[period <= .drop]
    d[, .drop := NULL]
  }
  d[]
}

# Matching truth: first-event log-IRR, standardised over L0, with NO loss (loss
# is a nuisance the estimator must be robust to, not part of the estimand). PP =
# sustained treatment; ITT = do(A_0) then natural switching. Returns log-IRR
# with reference-arm per-period hazard as attr "p0" (for the OR->IRR conversion).
stress_truth <- function(
  estimand,
  T_periods,
  lor,
  out_int = -3.5,
  L0_Y = 0.4,
  sw_int = -3.0,
  sw_L0 = 0.4,
  persist = 8,
  N_truth = 200000L,
  seed = 999
) {
  rate <- numeric(2)
  for (i in 1:2) {
    a <- c(0L, 1L)[i]
    set.seed(seed)
    L0 <- stats::rnorm(N_truth)
    at_risk <- rep(TRUE, N_truth)
    prev_A <- rep(a, N_truth)
    ev <- 0L
    pt <- 0
    for (t in 0:(T_periods - 1L)) {
      At <- if (estimand == "pp" || t == 0) {
        rep(a, N_truth)
      } else {
        stats::rbinom(
          N_truth,
          1,
          stats::plogis(sw_int + sw_L0 * L0 + persist * prev_A)
        )
      }
      Y <- stats::rbinom(
        N_truth,
        1,
        stats::plogis(out_int + lor * At + L0_Y * L0)
      )
      pt <- pt + sum(at_risk)
      new_ev <- at_risk & (Y == 1L)
      ev <- ev + sum(new_ev)
      at_risk <- at_risk & !new_ev
      prev_A <- At
    }
    rate[i] <- ev / pt
  }
  out <- log(rate[2] / rate[1])
  attr(out, "p0") <- rate[1]
  out
}

# swereg ITT fit at a chosen weight-truncation window; returns the log-IRR
# point estimate plus the maximum RAW (untruncated) stabilised weight -- the
# latter documents how close to a positivity violation the design is.
stress_fit_itt_trunc <- function(d, lower, upper) {
  long <- tte_build_long(d)
  trial <- TTEEnrollment$new(long, tte_make_design(long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = lower, upper = upper)
  trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = max(long$tstop),
    estimand = "itt"
  )
  r <- trial$irr("ipw_trunc")
  c(est = log(r$IRR), wmax = max(trial$data$ipw, na.rm = TRUE))
}

# TIME-VARYING CONFOUNDING with treatment-confounder feedback. L_t is an AR(1)
# risk factor that treatment improves (feedback), and L_t drives BOTH switching
# and the outcome. The correct analysis lets the censoring model see the
# per-row TIME-UPDATED confounder; freezing it at baseline throws that
# information away. tv_build_long() emits either version so the two can be
# compared head-to-head.
tv_sim <- function(N, T_periods, lor = -0.7, seed = 2026) {
  set.seed(seed)
  L <- stats::rnorm(N)
  out <- vector("list", T_periods)
  prev_A <- integer(N)
  for (t in 0:(T_periods - 1L)) {
    if (t > 0) {
      L <- 0.7 * L - 0.4 * prev_A + stats::rnorm(N, 0, 0.5)
    }
    logit_A <- if (t == 0) -0.3 + 0.6 * L else -3.0 + 0.8 * L + 6 * prev_A
    A <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Y <- stats::rbinom(N, 1, stats::plogis(-3.5 + lor * A + 0.5 * L))
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N),
      period = t,
      L_t = L,
      A_t = A,
      Y_t = Y
    )
    prev_A <- A
  }
  d <- data.table::rbindlist(out)
  data.table::setorder(d, id, period)
  d[]
}

tv_truth <- function(
  estimand,
  T_periods,
  lor = -0.7,
  N_truth = 200000L,
  seed = 999
) {
  rate <- numeric(2)
  for (i in 1:2) {
    a <- c(0L, 1L)[i]
    set.seed(seed)
    L <- stats::rnorm(N_truth)
    at_risk <- rep(TRUE, N_truth)
    prev_A <- rep(a, N_truth)
    ev <- 0L
    pt <- 0
    for (t in 0:(T_periods - 1L)) {
      if (t > 0) {
        L <- 0.7 * L - 0.4 * prev_A + stats::rnorm(N_truth, 0, 0.5)
      }
      At <- if (estimand == "pp" || t == 0) {
        rep(a, N_truth)
      } else {
        stats::rbinom(N_truth, 1, stats::plogis(-3.0 + 0.8 * L + 6 * prev_A))
      }
      Y <- stats::rbinom(N_truth, 1, stats::plogis(-3.5 + lor * At + 0.5 * L))
      pt <- pt + sum(at_risk)
      new_ev <- at_risk & (Y == 1L)
      ev <- ev + sum(new_ev)
      at_risk <- at_risk & !new_ev
      prev_A <- At
    }
    rate[i] <- ev / pt
  }
  out <- log(rate[2] / rate[1])
  attr(out, "p0") <- rate[1]
  out
}

# Long format with the confounder column either time-updated or frozen at
# baseline. Same table feeds PP and ITT; only the s4 call differs.
tv_build_long <- function(dt, confounder = c("updated", "frozen")) {
  confounder <- match.arg(confounder)
  sw <- data.table::copy(dt)
  sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
  sw[,
    conf_L := if (confounder == "updated") L_t else L_t[period == 0L][1L],
    by = id
  ]
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
    conf_L
  )]
}

tv_fit <- function(long, estimand) {
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = "conf_L",
    follow_up_time = max(long$tstop)
  )
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  if (estimand == "pp") {
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = max(long$tstop),
      estimate_ipcw_pp_with_gam = TRUE,
      estimate_ipcw_pp_separately_by_treatment = TRUE
    )
    r <- trial$irr("analysis_weight_pp_trunc")
  } else {
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = max(long$tstop),
      estimand = "itt"
    )
    r <- trial$irr("ipw_trunc")
  }
  c(est = log(r$IRR), lo = log(r$IRR_lower), hi = log(r$IRR_upper))
}
