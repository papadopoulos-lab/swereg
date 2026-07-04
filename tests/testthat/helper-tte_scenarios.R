# Scenario generators for the swereg-vs-TrialEmulation validation MATRIX
# (test-tte_validation_matrix.R). Three escalating scenarios, each fed through
# the full triangle: known truth, swereg, and TrialEmulation, for BOTH
# per-protocol and intention-to-treat.
#
#   s1: no confounding, no loss to follow-up
#       -> swereg and TE are IDENTICAL (no confounder => no OR non-collapsibility)
#   s2: confounding, INDEPENDENT loss to follow-up
#       -> both estimands recover truth; small finite-sample + non-collapsibility
#   s3: confounding, INFORMATIVE loss to follow-up (loss depends on the confounder)
#       -> PP (IPCW models loss) stays close; ITT (no loss weight, BY DESIGN) is
#          biased ~0.09 in BOTH packages, which AGREE with each other -- the
#          point being that "swereg == TE" does not imply "correct".
#
# Reuses tte_build_long() and tte_log_or_to_log_irr() from helper-tte_itt.R.

# Shared data-generating constants (fixed across scenarios so the TRUE treatment
# effect is identical; only the nuisances -- confounding, loss -- change).
.SCEN_T <- 20L
.SCEN_LOR <- -0.7
.SCEN_PERSIST <- 8

# scenario -> nuisance configuration
scen_cfg <- function(scenario) {
  switch(
    scenario,
    s1 = list(a0_L0 = 0.0, sw_L0 = 0.0, L0_Y = 0.0, loss = "none"),
    s2 = list(a0_L0 = 0.6, sw_L0 = 0.4, L0_Y = 0.4, loss = "independent"),
    s3 = list(a0_L0 = 0.6, sw_L0 = 0.4, L0_Y = 0.4, loss = "informative"),
    stop("unknown scenario: ", scenario)
  )
}

# Person-period data: baseline confounder L0, baseline treatment, per-period
# switching (persistence), contemporaneous rare outcome. Then apply loss.
scen_simulate <- function(scenario, N = 20000L, seed = 2026) {
  p <- scen_cfg(scenario)
  set.seed(seed)
  L0 <- stats::rnorm(N)
  out <- vector("list", .SCEN_T)
  prev_A <- integer(N)
  for (t in 0:(.SCEN_T - 1L)) {
    logit_A <- if (t == 0) {
      -0.3 + p$a0_L0 * L0
    } else {
      -3.0 + p$sw_L0 * L0 + .SCEN_PERSIST * prev_A
    }
    A <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Y <- stats::rbinom(N, 1, stats::plogis(-3.5 + .SCEN_LOR * A + p$L0_Y * L0))
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

  # Loss to follow-up: each person drops out at a random period, removing later
  # periods. Independent: same hazard for all. Informative: hazard rises with
  # the confounder L0 (so dropout is non-random selection on L0).
  if (p$loss != "none") {
    set.seed(seed + 99L)
    ids <- unique(d$id)
    L0i <- d[period == 0L, L0]
    haz <- if (p$loss == "independent") {
      rep(0.06, length(ids))
    } else {
      stats::plogis(-2.4 + 0.9 * L0i)
    }
    drop_at <- stats::qgeom(stats::runif(length(ids)), haz)
    d[, .drop := drop_at[match(id, ids)]]
    d <- d[period <= .drop]
    d[, .drop := NULL]
  }
  d[]
}

# Marginal first-event incidence-rate-ratio truth (NO loss -- loss is a nuisance
# we want estimators to be robust to, not part of the estimand). PP = sustained
# (force A every period); ITT = do(A_0) then natural switching. Standardised
# over L0. Returns log-IRR with reference-arm per-period hazard as attr "p0".
scen_truth <- function(scenario, estimand, N_truth = 200000L, seed = 999) {
  p <- scen_cfg(scenario)
  rate <- numeric(2)
  for (i in 1:2) {
    a <- c(0L, 1L)[i]
    set.seed(seed)
    L0 <- stats::rnorm(N_truth)
    at_risk <- rep(TRUE, N_truth)
    prev_A <- rep(a, N_truth)
    ev <- 0L
    pt <- 0L
    for (t in 0:(.SCEN_T - 1L)) {
      At <- if (estimand == "pp") {
        a
      } else if (t == 0) {
        a
      } else {
        stats::rbinom(
          N_truth,
          1,
          stats::plogis(
            -3.0 + p$sw_L0 * L0 + .SCEN_PERSIST * prev_A
          )
        )
      }
      Y <- stats::rbinom(
        N_truth,
        1,
        stats::plogis(
          -3.5 + .SCEN_LOR * At + p$L0_Y * L0
        )
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

# swereg estimate (log-IRR + log-scale CI width) for one estimand. Always
# IPT-weights on L0 (in s1, A independent of L0 -> weights ~ 1 -> marginal).
scen_fit_swereg <- function(d, estimand) {
  long <- tte_build_long(d)
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = "baseline_L0",
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
  c(
    est = log(r$IRR),
    lo = log(r$IRR_lower),
    hi = log(r$IRR_upper),
    width = log(r$IRR_upper) - log(r$IRR_lower)
  )
}

# Monte Carlo coverage: over M replicates (each a fresh draw), what fraction of
# 95% CIs cover the population truth? Validates the SE is calibrated, not just
# that swereg and TE agree. Returns the empirical coverage.
scen_coverage <- function(
  scenario,
  estimand,
  M = 200L,
  N = 3000L,
  seed0 = 1000L
) {
  truth <- as.numeric(scen_truth(scenario, estimand))
  covered <- logical(M)
  for (m in seq_len(M)) {
    d <- scen_simulate(scenario, N = N, seed = seed0 + m)
    fit <- tryCatch(scen_fit_swereg(d, estimand), error = function(e) NULL)
    covered[m] <- if (is.null(fit)) {
      NA
    } else {
      truth >= fit[["lo"]] && truth <= fit[["hi"]]
    }
  }
  mean(covered, na.rm = TRUE)
}

# TrialEmulation estimate, OR converted to the IRR scale (Zhang & Yu) with the
# reference-arm hazard p0. Always adjusts for L0 in the outcome model (in s1,
# L0 is inert so this is harmless and matches swereg's marginal estimate).
scen_fit_te <- function(d, estimand, p0) {
  ti <- data.table::copy(d)
  data.table::setnames(ti, c("A_t", "Y_t"), c("treatment", "outcome"))
  ti[, eligible := as.integer(period == 0L)]
  res <- TrialEmulation::initiators(
    data = ti,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment",
    estimand_type = toupper(estimand),
    outcome = "outcome",
    model_var = "assigned_treatment",
    outcome_cov = c("L0"),
    switch_n_cov = ~1,
    switch_d_cov = ~1,
    use_censor_weights = FALSE,
    quiet = TRUE
  )
  row <- res$robust$summary[res$robust$summary$names == "assigned_treatment", ]
  lo <- tte_log_or_to_log_irr(unname(row$`2.5%`), p0)
  hi <- tte_log_or_to_log_irr(unname(row$`97.5%`), p0)
  c(
    est = tte_log_or_to_log_irr(unname(row$estimate), p0),
    lo = lo,
    hi = hi,
    width = hi - lo
  )
}
