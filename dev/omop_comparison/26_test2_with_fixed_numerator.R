# Test 2 + numerator fix: full pipeline with corrected IPCW numerator.
# Same DGP as Test 2 (baseline-L0-driven deviation).

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
library(mgcv)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000; T <- 30; TRUE_LOR <- -0.5

L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)
for (t in 0:(T - 1)) {
  logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -3.0 + 0.4 * L0 + 4.0 * prev_A
  At_val <- rbinom(N, 1, plogis(logit_A))
  Yt_val <- rbinom(N, 1, plogis(-3.5 + TRUE_LOR * At_val + 0.4 * L0))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list); setorder(dt, id, period)

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

# Full pipeline (IPW + s4 = s5 + s6 IPCW with marginal-mean numerator)
trial <- TTEEnrollment$new(long, design)
trial$s2_ipw(stabilize = TRUE)
trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
trial$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad <- copy(trial$data)
ad[, prev_time_treatment := shift(time_treatment, 1L, fill = FALSE),
   by = .(enrollment_person_trial_id)]

# ----- Replace marginal-mean numerator with model-prediction numerator -----
# Refit GAMs separately by treatment_baseline.
fix_ipcw <- function(d, tx_val) {
  sub <- d[treatment_baseline == tx_val]
  if (nrow(sub) < 10 || sum(sub$censor_this_period, na.rm = TRUE) == 0) {
    sub[, ipcw_fixed := 1.0]
    return(sub)
  }
  # NUMERATOR: baseline-only model.
  fit_n <- mgcv::bam(censor_this_period ~ s(tstop) + baseline_L0,
                     data = sub, family = binomial, discrete = TRUE)
  sub[, p_n_uncens := 1 - predict(fit_n, newdata = sub, type = "response")]
  # DENOMINATOR: baseline + time-varying (prev adherence history).
  fit_d <- mgcv::bam(censor_this_period ~ s(tstop) + baseline_L0 + prev_time_treatment,
                     data = sub, family = binomial, discrete = TRUE)
  sub[, p_d_uncens := 1 - predict(fit_d, newdata = sub, type = "response")]
  setorder(sub, enrollment_person_trial_id, tstop)
  sub[, cum_n := cumprod(p_n_uncens), by = enrollment_person_trial_id]
  sub[, cum_d := cumprod(p_d_uncens), by = enrollment_person_trial_id]
  sub[, ipcw_fixed := cum_n / cum_d]
  sub
}
ad_fixed <- rbind(fix_ipcw(ad, TRUE), fix_ipcw(ad, FALSE))
ad_fixed[, w_final_fixed := ipw_trunc * ipcw_fixed]
ad_fixed <- ad_fixed[!is.na(w_final_fixed) & w_final_fixed > 0]

# Old (marginal-mean) for comparison
ad_old <- copy(ad)
ad_old[, w_final_old := ipw_trunc * ipcw_pp]
ad_old <- ad_old[!is.na(w_final_old) & w_final_old > 0]

# Fit outcome model with each weight set
fit_and_report <- function(d, w_col, label) {
  des <- svydesign(ids = ~enrollment_person_trial_id, weights = as.formula(paste0("~", w_col)), data = d)
  fit <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des, family = quasibinomial())
  co <- summary(fit)$coefficients["treatment_baselineTRUE", , drop = FALSE]
  cat(sprintf("%-55s est = %+.4f  se = %.4f  bias = %+.4f  wmax = %.2f\n",
              label, co[, "Estimate"], co[, "Std. Error"],
              co[, "Estimate"] - TRUE_PP, max(d[[w_col]])))
}

# TE PP for reference
te_in <- copy(dt)
setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
te_in[, eligible := as.integer(period == 0)]
res_te <- initiators(
  data = te_in, id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", estimand_type = "PP", outcome = "outcome",
  model_var = "assigned_treatment", outcome_cov = c("L0"),
  switch_n_cov = ~ L0, switch_d_cov = ~ L0,
  use_censor_weights = FALSE, quiet = TRUE
)
te_trt <- res_te$robust$summary[res_te$robust$summary$names == "assigned_treatment", ]

cat("=========================================================\n")
cat("TEST 2 + NUMERATOR FIX (true PP =", round(TRUE_PP, 4), ")\n")
cat("=========================================================\n")
fit_and_report(ad_old,   "w_final_old",   "swereg IPW * IPCW (marginal numerator)")
fit_and_report(ad_fixed, "w_final_fixed", "swereg IPW * IPCW (FIXED numerator)")
cat(sprintf("%-55s est = %+.4f  se = %.4f  bias = %+.4f\n",
            "TE PP (reference)",
            te_trt$estimate, te_trt$robust_se, te_trt$estimate - TRUE_PP))

cat("\n--- weights ---\n")
cat(sprintf("ipcw_pp (old marginal): [%.3f, %.3f]   sd %.3f\n",
            min(ad_old$ipcw_pp), max(ad_old$ipcw_pp), sd(ad_old$ipcw_pp)))
cat(sprintf("ipcw_fixed:             [%.3f, %.3f]   sd %.3f\n",
            min(ad_fixed$ipcw_fixed), max(ad_fixed$ipcw_fixed), sd(ad_fixed$ipcw_fixed)))
