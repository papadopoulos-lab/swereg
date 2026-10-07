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
#   s4: two baseline covariates, one per censoring cause (defined further down)
#       -> L0 drives treatment, time-zero discordance and deviation; L1 drives
#          loss. Both drive the outcome, so both causes are informative. It is
#          the scenario of the per-cause censoring weights, with exact truths
#          and oracle weights. It is not in the TrialEmulation matrix.
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
    s4 = .SCEN_S4,
    stop("unknown scenario: ", scenario)
  )
}

# Person-period data: baseline confounder L0, baseline treatment, per-period
# switching (persistence), contemporaneous rare outcome. Then apply loss.
scen_simulate <- function(scenario, N = 20000L, seed = 2026) {
  if (identical(scenario, "s4")) {
    return(scen_simulate_s4(N = N, seed = seed))
  }
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
    ru <- trial$irr("analysis_weight_pp") # untruncated sensitivity fit
  } else {
    trial$s4_prepare_for_analysis(
      outcome = "event",
      follow_up = max(long$tstop),
      estimand = "itt"
    )
    r <- trial$irr("ipw_trunc")
    ru <- trial$irr("ipw") # untruncated sensitivity fit
  }
  c(
    est = log(r$IRR),
    lo = log(r$IRR_lower),
    hi = log(r$IRR_upper),
    width = log(r$IRR_upper) - log(r$IRR_lower),
    est_untrunc = log(ru$IRR),
    lo_untrunc = log(ru$IRR_lower),
    hi_untrunc = log(ru$IRR_upper)
  )
}

# Monte Carlo coverage: over M replicates (each a fresh draw), what fraction of
# 95% CIs cover the population truth? Validates the SE is calibrated, not just
# that swereg and TE agree. Returns the empirical coverage. The truth is the
# exact log-IRR of swereg's own outcome model, scen_truth_irr_exact(). The
# attributes `truth` and `est` hold that truth and the estimates of the
# replicates that fitted.
scen_coverage <- function(
  scenario,
  estimand,
  M = 200L,
  N = 3000L,
  seed0 = 1000L
) {
  truth <- as.numeric(scen_truth_irr_exact(scenario, estimand))
  covered <- logical(M)
  est <- rep(NA_real_, M)
  for (m in seq_len(M)) {
    d <- scen_simulate(scenario, N = N, seed = seed0 + m)
    fit <- tryCatch(scen_fit_swereg(d, estimand), error = function(e) NULL)
    covered[m] <- if (is.null(fit)) {
      NA
    } else {
      truth >= fit[["lo"]] && truth <= fit[["hi"]]
    }
    if (!is.null(fit)) {
      est[m] <- fit[["est"]]
    }
  }
  out <- mean(covered, na.rm = TRUE)
  attr(out, "truth") <- truth
  attr(out, "est") <- est[!is.na(est)]
  out
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

# --- s4: one baseline covariate per censoring cause ---------------------------
#
# L0 and L1 are independent standard normal. For each person:
#
#   arm          logit P(arm = 1)           = a0_c + a0_L0 * L0
#   week 0       logit P(A_0 = 1 | arm)     = tz_c + tz_arm * arm + tz_L0 * L0
#   week t > 0   logit P(A_t = 1 | A_t-1)   = sw_c + sw_L0 * L0 + sw_persist * A_t-1
#   outcome      logit P(Y_t = 1 | A_t)     = -3.5 + .SCEN_LOR * A_t
#                                               + Y_L0 * L0 + Y_L1 * L1
#   loss         logit P(lost after week t) = loss_c + loss_L1 * L1
#
# The arm is fixed at time zero, and the treatment of week 0 can already differ
# from it. Per-protocol follow-up stops at the start of the first week with
# A_t != arm, so a person discordant in week 0 keeps no row. Loss after week t
# removes every later week, and it is independent of the outcome given L1.
#
# L0 drives the arm, the time-zero discordance and the deviation. L1 drives the
# loss. Both drive the outcome, so each censoring cause is informative and each
# model needs its own covariate. The probabilities are not rare. At N = 20,000
# and seed 2101, 12% of person-trials deviate at time zero, 4.7% of the rows
# without an outcome are lost, and 3.0% of those not lost deviate.
.SCEN_S4 <- list(
  kind = "s4",
  a0_c = -0.3,
  a0_L0 = 0.6,
  tz_c = -2.0,
  tz_arm = 4.0,
  tz_L0 = 1.0,
  sw_c = -3.3,
  sw_L0 = 0.8,
  sw_persist = 6.6,
  loss_c = -3.0,
  loss_L1 = 0.8,
  Y_L0 = 0.4,
  Y_L1 = 0.6
)

# Person-period data for s4: id, period, L0, L1, arm, A_t, Y_t. The loss is
# applied, so a person lost after week t has no later row.
scen_simulate_s4 <- function(N = 20000L, seed = 2026) {
  p <- .SCEN_S4
  set.seed(seed)
  L0 <- stats::rnorm(N)
  L1 <- stats::rnorm(N)
  arm <- stats::rbinom(N, 1, stats::plogis(p$a0_c + p$a0_L0 * L0))
  out <- vector("list", .SCEN_T)
  prev_A <- integer(N)
  for (t in 0:(.SCEN_T - 1L)) {
    logit_A <- if (t == 0) {
      p$tz_c + p$tz_arm * arm + p$tz_L0 * L0
    } else {
      p$sw_c + p$sw_L0 * L0 + p$sw_persist * prev_A
    }
    A <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Y <- stats::rbinom(
      N,
      1,
      stats::plogis(-3.5 + .SCEN_LOR * A + p$Y_L0 * L0 + p$Y_L1 * L1)
    )
    out[[t + 1L]] <- data.table::data.table(
      id = seq_len(N),
      period = t,
      L0 = L0,
      L1 = L1,
      arm = arm,
      A_t = A,
      Y_t = Y
    )
    prev_A <- A
  }
  d <- data.table::rbindlist(out)
  data.table::setorder(d, id, period)
  set.seed(seed + 99L)
  drop_at <- stats::qgeom(
    stats::runif(N),
    stats::plogis(p$loss_c + p$loss_L1 * L1)
  )
  d <- d[period <= drop_at[id]]
  d[]
}

# The s4 person-period data in swereg trial-long format. The arm comes from
# `arm` and not from week 0, which is what makes a time-zero deviation
# possible.
scen_long_s4 <- function(d) {
  sw <- data.table::copy(d)
  sw[, tstart := period]
  sw[, tstop := period + 1L]
  sw[, treatment_baseline := as.logical(arm)]
  sw[, time_treatment := as.logical(A_t)]
  sw[, person_weeks := 1L]
  sw[, baseline_L0 := L0]
  sw[, baseline_L1 := L1]
  data.table::setnames(sw, c("id", "Y_t"), c("enrollment_person_trial_id", "event"))
  sw[, list(
    enrollment_person_trial_id,
    tstart,
    tstop,
    treatment_baseline,
    time_treatment,
    event,
    person_weeks,
    baseline_L0,
    baseline_L1
  )]
}

# swereg's per-protocol preparation of s4: treatment weights on L0 and L1, and
# the censoring weights in each arm. Returns the enrollment.
scen_prepare_s4 <- function(long, use_gam = TRUE) {
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = c("baseline_L0", "baseline_L1"),
    follow_up_time = .SCEN_T
  )
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = .SCEN_T,
    estimate_ipcw_pp_with_gam = use_gam,
    estimate_ipcw_pp_separately_by_treatment = TRUE
  )
  trial
}

# The oracle censoring probabilities and unstabilised weights of s4, from the
# true parameters. `dt` holds swereg panel rows: `tstart`, `treatment_baseline`,
# `baseline_L0` and `baseline_L1`. The row of interval k weighs the inverse of
# the probability of remaining uncensored through its start:
#
#   (1 - p0) * ((1 - hl) * (1 - hd))^k
#
# `p0` is the time-zero discordance, `hl` the loss and `hd` the deviation of
# one row. None depends on time, so the lagged product is a power.
scen_oracle_weights_s4 <- function(dt) {
  p <- .SCEN_S4
  a <- as.integer(dt$treatment_baseline)
  L0 <- dt$baseline_L0
  k <- dt$tstart
  on_0 <- stats::plogis(p$tz_c + p$tz_arm * a + p$tz_L0 * L0)
  p0 <- ifelse(a == 1L, 1 - on_0, on_0)
  on_t <- stats::plogis(p$sw_c + p$sw_L0 * L0 + p$sw_persist * a)
  hd <- ifelse(a == 1L, 1 - on_t, on_t)
  hl <- stats::plogis(p$loss_c + p$loss_L1 * dt$baseline_L1)
  data.table::data.table(
    p0 = p0,
    hd = hd,
    hl = hl,
    w_time_zero = 1 / (1 - p0),
    w_loss = (1 - hl)^(-k),
    w_deviation = (1 - hd)^(-k)
  )
}

# Marginal cumulative-risk truth, exact: quadrature over the baseline
# covariates and a forward recursion over the 2-state treatment chain (ITT)
# or a fixed treatment (PP). No Monte Carlo error. Returns a data.table of
# h, risk0, risk1 and rd = risk1 - risk0, for h = 1 to .SCEN_T. Loss is not
# part of either estimand. Ported from the 2026-10-07 probe
# /tmp/rd-probe/work/truth.R for s1 to s3, and extended to s4.
scen_truth_risk_exact <- function(scenario, estimand, n_grid = 4001L) {
  if (identical(scenario, "s4")) {
    return(scen_truth_risk_exact_s4(estimand))
  }
  p <- scen_cfg(scenario)
  x <- seq(-8, 8, length.out = n_grid)
  wq <- stats::dnorm(x)
  wq <- wq / sum(wq)
  risk <- matrix(NA_real_, .SCEN_T, 2)
  for (i in 1:2) {
    a <- c(0L, 1L)[i]
    v0 <- if (a == 0L) rep(1, n_grid) else rep(0, n_grid) # P(alive, A_t = 0)
    v1 <- 1 - v0 # P(alive, A_t = 1)
    h0 <- stats::plogis(-3.5 + p$L0_Y * x)
    h1 <- stats::plogis(-3.5 + .SCEN_LOR + p$L0_Y * x)
    s0 <- stats::plogis(-3.0 + p$sw_L0 * x) # P(A = 1 | prev 0)
    s1 <- stats::plogis(-3.0 + p$sw_L0 * x + .SCEN_PERSIST) # P(A = 1 | prev 1)
    for (t in 0:(.SCEN_T - 1L)) {
      if (t > 0 && estimand == "itt") {
        n1 <- v0 * s0 + v1 * s1
        n0 <- v0 * (1 - s0) + v1 * (1 - s1)
        v0 <- n0
        v1 <- n1
      }
      v0 <- v0 * (1 - h0)
      v1 <- v1 * (1 - h1)
      risk[t + 1L, i] <- 1 - sum(wq * (v0 + v1))
    }
  }
  data.table::data.table(
    h = seq_len(.SCEN_T),
    risk0 = risk[, 1],
    risk1 = risk[, 2],
    rd = risk[, 2] - risk[, 1]
  )
}

# The s4 exact truth. L0 and L1 both enter the outcome, so the quadrature is
# over their product grid. Under ITT the arm fixes only the assignment: week 0
# follows P(A_0 = 1 | arm), and later weeks follow the switching chain.
scen_truth_risk_exact_s4 <- function(estimand, n_grid = 401L) {
  p <- .SCEN_S4
  x <- seq(-8, 8, length.out = n_grid)
  w1 <- stats::dnorm(x)
  w1 <- w1 / sum(w1)
  L0 <- rep(x, times = n_grid)
  L1 <- rep(x, each = n_grid)
  wq <- rep(w1, times = n_grid) * rep(w1, each = n_grid)
  lp <- -3.5 + p$Y_L0 * L0 + p$Y_L1 * L1
  h0 <- stats::plogis(lp)
  h1 <- stats::plogis(lp + .SCEN_LOR)
  s0 <- stats::plogis(p$sw_c + p$sw_L0 * L0)
  s1 <- stats::plogis(p$sw_c + p$sw_L0 * L0 + p$sw_persist)
  risk <- matrix(NA_real_, .SCEN_T, 2)
  for (i in 1:2) {
    a <- c(0L, 1L)[i]
    for (t in 0:(.SCEN_T - 1L)) {
      if (estimand == "pp") {
        if (t == 0) {
          v1 <- rep(a, length(L0))
          v0 <- 1 - v1
        }
      } else if (t == 0) {
        v1 <- stats::plogis(p$tz_c + p$tz_arm * a + p$tz_L0 * L0)
        v0 <- 1 - v1
      } else {
        n1 <- v0 * s0 + v1 * s1
        n0 <- v0 * (1 - s0) + v1 * (1 - s1)
        v0 <- n0
        v1 <- n1
      }
      v0 <- v0 * (1 - h0)
      v1 <- v1 * (1 - h1)
      risk[t + 1L, i] <- 1 - sum(wq * (v0 + v1))
    }
  }
  data.table::data.table(
    h = seq_len(.SCEN_T),
    risk0 = risk[, 1],
    risk1 = risk[, 2],
    rd = risk[, 2] - risk[, 1]
  )
}

# --- exact log-IRR truth -------------------------------------------------------
#
# swereg's outcome model is a weighted quasi-Poisson fit of
# `event ~ treatment + flex(tstart) + offset(log(person_weeks))`. These panels
# hold one trial, so the `enrollment_period_id` term drops out, and 20 distinct
# `tstart` values give `splines::ns(tstart, df = 3)`. The exact truth is the
# treatment coefficient of that model, fitted to the exact expected events and
# person-time of each (arm, interval) cell. Each cell is computed by the same
# quadrature and forward recursion as the risk truth, so it has no Monte Carlo
# error.
#
# The model has one rate ratio for all of follow-up, and the marginal rate
# ratio of these scenarios changes over follow-up. So the coefficient depends
# on how the cells are weighted. Two weightings are available.
#
#   "stabilised"  each cell is weighted by the marginal probability of
#                 remaining uncensored at the interval start, in its arm. That
#                 is the numerator of the stabilised censoring weights: time
#                 zero, loss and deviation for per-protocol, and loss for ITT.
#                 It is the limit of swereg's fit when the treatment and
#                 censoring weights are correct, so it is the default.
#   "uncensored"  no censoring at all.
#
#
# The two coincide in s1, where the hazard ratio is constant over follow-up.
# In s2 to s4 they differ by 0.006 to 0.064 on the log-IRR scale, the most in
# s3 ITT (measured 2026-10-07). The arm weights are the marginal arm
# proportions, the numerator of the stabilised treatment weight.

# The quadrature grid of one scenario, with the per-point probabilities that
# the recursions read:
#   p_arm    P(arm = 1)
#   p_a0(a)  P(A_0 = 1 | arm a)
#   s0, s1   P(A_t = 1 | A_t-1 = 0), P(A_t = 1 | A_t-1 = 1)
#   h0, h1   the outcome hazard when A_t = 0, when A_t = 1
#   hl       the loss hazard of one row
scen_grid_exact <- function(scenario) {
  if (identical(scenario, "s4")) {
    p <- .SCEN_S4
    n_grid <- 401L
    x <- seq(-8, 8, length.out = n_grid)
    w1 <- stats::dnorm(x)
    w1 <- w1 / sum(w1)
    L0 <- rep(x, times = n_grid)
    L1 <- rep(x, each = n_grid)
    lp <- -3.5 + p$Y_L0 * L0 + p$Y_L1 * L1
    return(list(
      wq = rep(w1, times = n_grid) * rep(w1, each = n_grid),
      p_arm = stats::plogis(p$a0_c + p$a0_L0 * L0),
      p_a0 = function(a) stats::plogis(p$tz_c + p$tz_arm * a + p$tz_L0 * L0),
      s0 = stats::plogis(p$sw_c + p$sw_L0 * L0),
      s1 = stats::plogis(p$sw_c + p$sw_L0 * L0 + p$sw_persist),
      h0 = stats::plogis(lp),
      h1 = stats::plogis(lp + .SCEN_LOR),
      hl = stats::plogis(p$loss_c + p$loss_L1 * L1)
    ))
  }
  p <- scen_cfg(scenario)
  n_grid <- 4001L
  x <- seq(-8, 8, length.out = n_grid)
  wq <- stats::dnorm(x)
  list(
    wq = wq / sum(wq),
    p_arm = stats::plogis(-0.3 + p$a0_L0 * x),
    p_a0 = function(a) rep(a, n_grid),
    s0 = stats::plogis(-3.0 + p$sw_L0 * x),
    s1 = stats::plogis(-3.0 + p$sw_L0 * x + .SCEN_PERSIST),
    h0 = stats::plogis(-3.5 + p$L0_Y * x),
    h1 = stats::plogis(-3.5 + .SCEN_LOR + p$L0_Y * x),
    hl = switch(
      p$loss,
      none = rep(0, n_grid),
      independent = rep(0.06, n_grid),
      informative = stats::plogis(-2.4 + 0.9 * x)
    )
  )
}

# The exact (arm, interval) cells. `pt` and `events` are the expected
# person-time and events of the target population. `rows` is the expected
# number of panel rows per person that the observed data hold in that cell.
# The rows set the spline knots, as the rows of a real panel do. Per-protocol
# rows stop at the first week with A_t != arm and at loss; ITT rows stop at
# loss.
scen_irr_cells_exact <- function(
  scenario,
  estimand,
  person_time = c("stabilised", "uncensored")
) {
  person_time <- match.arg(person_time)
  g <- scen_grid_exact(scenario)
  wq <- g$wq
  out <- vector("list", 2L * .SCEN_T)
  for (a in 0:1) {
    p_a <- if (a == 1L) g$p_arm else 1 - g$p_arm
    pi_a <- sum(wq * p_a)
    v1 <- g$p_a0(a) # P(alive, A_t = 1) under the target treatment
    v0 <- 1 - v1
    if (estimand == "pp") {
      h_a <- if (a == 1L) g$h1 else g$h0
      stay <- if (a == 1L) g$s1 else 1 - g$s0
      surv <- rep(1, length(wq))
      obs <- p_a * (if (a == 1L) v1 else v0) # observed, uncensored, event-free
    } else {
      obs <- p_a
    }
    num <- sum(wq * obs) / pi_a # time-zero numerator (1 unless s4 per-protocol)
    for (t in 0:(.SCEN_T - 1L)) {
      if (estimand == "pp") {
        if (t > 0L) {
          obs <- obs * stay
        }
        pt <- surv
        ev <- surv * h_a
        free <- obs * (1 - h_a)
        surv <- surv * (1 - h_a)
      } else {
        if (t > 0L) {
          n1 <- v0 * g$s0 + v1 * g$s1
          n0 <- v0 * (1 - g$s0) + v1 * (1 - g$s1)
          v0 <- n0
          v1 <- n1
          obs <- p_a * (v0 + v1) * (1 - g$hl)^t
        }
        pt <- v0 + v1
        ev <- v0 * g$h0 + v1 * g$h1
        free <- p_a * (v0 * (1 - g$h0) + v1 * (1 - g$h1)) * (1 - g$hl)^t
        v0 <- v0 * (1 - g$h0)
        v1 <- v1 * (1 - g$h1)
      }
      w_t <- if (person_time == "stabilised") num else 1
      out[[a * .SCEN_T + t + 1L]] <- data.table::data.table(
        arm = a,
        tstart = t,
        pt = pi_a * w_t * sum(wq * pt),
        events = pi_a * w_t * sum(wq * ev),
        rows = sum(wq * obs)
      )
      # The numerator hazards are marginal and sequential: loss among the
      # event-free rows, then deviation among those not lost.
      kept <- free * (1 - g$hl)
      if (estimand == "pp") {
        num <- num * sum(wq * kept * stay) / sum(wq * free)
        obs <- kept
      } else {
        num <- num * sum(wq * kept) / sum(wq * free)
      }
    }
  }
  data.table::rbindlist(out)
}

# The knots `splines::ns(tstart, df = 3)` places on a large panel: the
# 1/3 and 2/3 quantiles of the row distribution of `tstart`. On a panel of
# integer `tstart` the quantile is the smallest value whose cumulative share
# reaches the probability. Measured 2026-10-07 at N = 20,000 and seed 2101,
# the panel knots equal these in all eight scenario and estimand cells. The
# coefficient moves by less than 1e-6 when the knots move by one week.
scen_irr_knots_exact <- function(cells) {
  m <- cells[, list(rows = sum(rows)), by = "tstart"][order(tstart)]
  cdf <- cumsum(m$rows) / sum(m$rows)
  vapply(
    c(1, 2) / 3,
    function(pr) m$tstart[which(cdf >= pr - 1e-12)[1L]],
    numeric(1)
  )
}

# The treatment coefficient of swereg's outcome model on exact cells. Returns
# the log-IRR, with the knots and the cells as attributes.
scen_truth_irr_exact <- function(
  scenario,
  estimand,
  person_time = c("stabilised", "uncensored")
) {
  person_time <- match.arg(person_time)
  cells <- scen_irr_cells_exact(scenario, estimand, person_time)
  knots <- scen_irr_knots_exact(cells)
  fit <- stats::glm(
    events ~ arm +
      splines::ns(tstart, knots = knots, Boundary.knots = c(0, .SCEN_T - 1L)) +
      offset(log(pt)),
    data = cells,
    family = stats::quasipoisson(),
    control = stats::glm.control(epsilon = 1e-14, maxit = 100L)
  )
  out <- unname(stats::coef(fit)[["arm"]])
  attr(out, "knots") <- knots
  attr(out, "cells") <- cells
  out
}
