# Phase 23: implement the fix — replace marginal-mean numerator with
# model-prediction numerator (Option A per Hernan 2008, Danaei 2013).
# Test on the moderate-persistence simulation that previously gave +0.150 bias.
#
# Implementation: bypass swereg's s6_ipcw_pp, build the corrected IPCW manually.
# If it works (bias drops to TE's ~0.04 level), we know the fix is correct
# and can port it into swereg properly.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
library(mgcv)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

# Moderate persistence DGP (where swereg was badly biased)
L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)
for (t in 0:(T - 1)) {
  logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -2.0 + 0.6 * L0 + 3.0 * prev_A
  At_val <- rbinom(N, 1, plogis(logit_A))
  logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0
  Yt_val <- rbinom(N, 1, plogis(logit_Y))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list)
setorder(dt, id, period)

# True PP via potential outcomes
sim_intervention <- function(seed, force_A) {
  set.seed(seed); L0_po <- rnorm(N, 0, 1)
  out <- vector("list", T)
  for (t in 0:(T - 1)) {
    out[[t+1L]] <- data.table(
      Y = rbinom(N, 1, plogis(-3.5 + TRUE_LOR*force_A + 0.4*L0_po)),
      A = force_A)
  }
  rbindlist(out)
}
po_t <- sim_intervention(999, 1L); po_u <- sim_intervention(999, 0L)
TRUE_PP <- coef(glm(Y ~ A, family = binomial, data = rbind(po_t, po_u)))["A"]
cat("True PP:", round(TRUE_PP, 4), "\n")

# Build long format
sw <- copy(dt)
sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
sw[, baseline_L0 := L0]
sw[, tstart := period]; sw[, tstop := period + 1L]
sw[, time_treatment := as.logical(A_t)]
sw[, treatment_baseline := as.logical(baseline_treatment)]
setnames(sw, "id", "enrollment_person_trial_id")
setnames(sw, "Y_t", "event")
long <- sw[, .(enrollment_person_trial_id, tstart, tstop,
               treatment_baseline, time_treatment, event, baseline_L0)]

design <- TTEDesign$new(
  id_var = "enrollment_person_trial_id",
  treatment_var = "treatment_baseline",
  time_treatment_var = "time_treatment",
  outcome_vars = "event",
  confounder_vars = c("baseline_L0"),
  follow_up_time = max(long$tstop)
)

# Run swereg pipeline up to s5 to get censor_this_period set up.
trial <- TTEEnrollment$new(long, design)
trial$data[, ipw := 1.0]; trial$data[, ipw_trunc := 1.0]
trial$weight_cols <- "ipw"; trial$steps_completed <- c("ipw", "truncate")
trial$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)

ad <- copy(trial$data)
# We'll build TWO sets of weights for comparison:
#   - "buggy": swereg's current ipcw_pp (marginal mean numerator)
#   - "fixed": new ipcw_fixed with model-based numerator

# Confounder variables — in this DGP, baseline_L0 is the only confounder.
# Time-varying piece: shifted time_treatment (current adherence history).
ad[, prev_time_treatment := shift(time_treatment, 1L, fill = FALSE),
   by = .(enrollment_person_trial_id)]

# Per-treatment-arm fitted models for numerator (Option A: baseline-only).
# Denominator was already fit in s6 (we trust that part).
fit_corrected_ipcw <- function(d, treatment_value) {
  sub <- d[treatment_baseline == treatment_value]
  if (nrow(sub) < 10 || sum(sub$censor_this_period, na.rm=TRUE) == 0) {
    sub[, ipcw_fixed := 1.0]
    return(sub)
  }

  # NUMERATOR model: baseline covariates only (Option A).
  # Per Hernan 2008 / Danaei 2013: a separate logistic regression on baseline
  # covariates. We use GAM here to match swereg's default smoothness on tstop.
  fit_n <- mgcv::bam(
    censor_this_period ~ s(tstop) + baseline_L0,
    data = sub, family = binomial, discrete = TRUE
  )
  sub[, p_n_uncens := 1 - predict(fit_n, newdata = sub, type = "response")]

  # DENOMINATOR model: baseline + time-varying (current adherence history).
  fit_d <- mgcv::bam(
    censor_this_period ~ s(tstop) + baseline_L0 + prev_time_treatment,
    data = sub, family = binomial, discrete = TRUE
  )
  sub[, p_d_uncens := 1 - predict(fit_d, newdata = sub, type = "response")]

  setorder(sub, enrollment_person_trial_id, tstop)
  sub[, cum_n := cumprod(p_n_uncens), by = enrollment_person_trial_id]
  sub[, cum_d := cumprod(p_d_uncens), by = enrollment_person_trial_id]
  sub[, ipcw_fixed := cum_n / cum_d]
  sub
}

ad_fixed <- rbind(fit_corrected_ipcw(ad, TRUE), fit_corrected_ipcw(ad, FALSE))
ad_fixed <- ad_fixed[!is.na(ipcw_fixed) & ipcw_fixed > 0]

# ---- Compare three weighting strategies on the same data ----
fit_and_report <- function(d, w_col, label) {
  ad2 <- copy(d)
  ad2[, w_use := get(w_col)]
  ad2 <- ad2[!is.na(w_use) & w_use > 0]
  des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_use, data = ad2)
  fit <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des, family = quasibinomial())
  co <- summary(fit)$coefficients["treatment_baselineTRUE", , drop = FALSE]
  cat(sprintf("%-45s  est = %+.4f  se = %.4f  bias = %+.4f  wmax = %.2f\n",
              label, co[, "Estimate"], co[, "Std. Error"],
              co[, "Estimate"] - TRUE_PP, max(ad2$w_use)))
}

cat("\n=========================================================\n")
cat("MODERATE PERSISTENCE N=50k, true PP =", round(TRUE_PP, 4), "\n")
cat("=========================================================\n")

# unweighted baseline
ad_fixed[, w_one := 1]
fit_and_report(ad_fixed, "w_one", "unweighted (no IPCW)")

# swereg's buggy default
fit_and_report(ad, "ipcw_pp", "swereg buggy (marginal mean numerator)")

# the fix
fit_and_report(ad_fixed, "ipcw_fixed", "FIXED: model-prediction numerator")

cat("\n(for reference: TE PP gives bias -0.039)\n")

cat("\n--- Weight distributions ---\n")
cat(sprintf("buggy ipcw_pp:   range [%.3f, %.3f]   sd %.3f\n",
            min(ad$ipcw_pp, na.rm=T), max(ad$ipcw_pp, na.rm=T), sd(ad$ipcw_pp, na.rm=T)))
cat(sprintf("FIXED ipcw:      range [%.3f, %.3f]   sd %.3f\n",
            min(ad_fixed$ipcw_fixed), max(ad_fixed$ipcw_fixed), sd(ad_fixed$ipcw_fixed)))
