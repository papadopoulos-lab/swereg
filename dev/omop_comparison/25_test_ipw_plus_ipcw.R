# Test 2: IPW + IPCW with deviation, where adherence depends on baseline L0.
#
# DGP:
#   - L0 ~ N(0, 1) -- baseline confounder
#   - A_0 ~ Bernoulli(plogis(-0.3 + 0.6 * L0))
#   - For t > 0: A_t ~ Bernoulli(plogis(-3 + 0.4*L0 + 4*prev_A))
#       Strong persistence (coef 4) moderated by L0.
#       High-L0 treated more likely to stay; high-L0 untreated more likely to switch.
#   - Y_t ~ Bernoulli(plogis(-3.5 + TRUE_LOR * A_t + 0.4 * L0))
#
# Adherence-driving variable is BASELINE L0 only (no time-varying confounder).
# So all confounding is captured by L0.
#
# Expected:
#   - Naive: biased
#   - Oracle: should recover (per-period coef = marginal PP since no A->L0 feedback)
#   - IPW alone: handles baseline confounding for treatment assignment,
#     but adherent subset is selected -> biased
#   - IPW + IPCW: should fix it
#   - TE PP: reference

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)
for (t in 0:(T - 1)) {
  logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -3.0 + 0.4 * L0 + 4.0 * prev_A
  At_val <- rbinom(N, 1, plogis(logit_A))
  logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0
  Yt_val <- rbinom(N, 1, plogis(logit_Y))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list)
setorder(dt, id, period)
cat("rows:", nrow(dt), "  events:", sum(dt$Y_t),
    sprintf(" (%.2f%%)\n", 100*mean(dt$Y_t)))

# adherence statistics
dt[, baseline_A := A_t[period == 0L][1L], by = id]
dt[, deviated_period := A_t != baseline_A]
adh <- dt[, .(any_dev = any(deviated_period),
              first_dev = if (any(deviated_period))
                as.numeric(min(period[deviated_period])) else NA_real_),
          by = id]
cat("% ever deviating:", round(100 * mean(adh$any_dev), 1), "%\n")
cat("median first-deviation period:",
    median(adh$first_dev, na.rm = TRUE), "\n\n")

# True PP
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
cat("True PP:", round(TRUE_PP, 4), "\n\n")

fit_naive  <- glm(Y_t ~ A_t,       family = binomial, data = dt)
fit_oracle <- glm(Y_t ~ A_t + L0,  family = binomial, data = dt)

# --- Build long format for swereg ---
sw <- copy(dt)
sw[, baseline_A := NULL]; sw[, deviated_period := NULL]
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

# --- Variant A: IPW alone (no IPCW). No protocol-deviation censoring. ---
# This represents an ITT-ish analysis: use all rows, weighted by baseline IPW.
trial_A <- TTEEnrollment$new(long, design)
trial_A$s2_ipw(stabilize = TRUE)
trial_A$s3_truncate_weights(lower = 0.01, upper = 0.99)
ad_A <- copy(trial_A$data)
des_A <- svydesign(ids = ~enrollment_person_trial_id, weights = ~ipw_trunc, data = ad_A)
fit_A <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des_A, family = quasibinomial())
co_A <- summary(fit_A)$coefficients["treatment_baselineTRUE", , drop = FALSE]

# --- Variant B: IPW + IPCW (full PP swereg pipeline) ---
trial_B <- TTEEnrollment$new(long, design)
trial_B$s2_ipw(stabilize = TRUE)
trial_B$s3_truncate_weights(lower = 0.01, upper = 0.99)
trial_B$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad_B <- copy(trial_B$data)
ad_B[, w_final := ipw_trunc * ipcw_pp]
ad_B <- ad_B[!is.na(w_final) & w_final > 0]
des_B <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad_B)
fit_B <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des_B, family = quasibinomial())
co_B <- summary(fit_B)$coefficients["treatment_baselineTRUE", , drop = FALSE]

# --- Variant C: TE PP for reference ---
te_in <- copy(dt)
te_in[, baseline_A := NULL]; te_in[, deviated_period := NULL]
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
cat("TEST 2: IPW + IPCW, baseline-L0-driven deviation\n")
cat("True PP =", round(TRUE_PP, 4), "\n")
cat("=========================================================\n")
fmt <- function(est, se, target) sprintf("est = %+.4f  se = %.4f  bias = %+.4f",
                                          est, se, est - target)
cat(sprintf("%-50s %s\n", "naive (no adj)",
            fmt(coef(fit_naive)["A_t"], NA, TRUE_PP)))
cat(sprintf("%-50s %s\n", "oracle (Y ~ A + L0)",
            fmt(coef(fit_oracle)["A_t"], NA, TRUE_PP)))
cat(sprintf("%-50s %s\n", "swereg IPW only (no PP censoring)",
            fmt(co_A[, "Estimate"], co_A[, "Std. Error"], TRUE_PP)))
cat(sprintf("%-50s %s\n", "swereg IPW + IPCW (full PP pipeline)",
            fmt(co_B[, "Estimate"], co_B[, "Std. Error"], TRUE_PP)))
cat(sprintf("%-50s %s\n", "TE PP (reference)",
            fmt(te_trt$estimate, te_trt$robust_se, TRUE_PP)))

cat("\n--- weight diagnostics ---\n")
cat(sprintf("IPW only: ipw_trunc range [%.3f, %.3f]\n",
            min(ad_A$ipw_trunc), max(ad_A$ipw_trunc)))
cat(sprintf("IPW+IPCW: ipcw_pp range [%.3f, %.3f]\n",
            min(ad_B$ipcw_pp, na.rm=T), max(ad_B$ipcw_pp, na.rm=T)))
cat(sprintf("IPW+IPCW: w_final range [%.3f, %.3f]\n",
            min(ad_B$w_final), max(ad_B$w_final)))
