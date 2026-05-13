# Phase 20: swereg-TTE WITHOUT baseline IPW (s2_ipw).
# Instead handle baseline confounding by including baseline covariates
# as regressors in the final svyglm — matching TE's approach (Step e
# of the paper: outcome MSM includes V and L_0).
#
# This isolates the effect of swereg's IPW machinery vs the IPCW.
# Use the Phase 16 setup: easy scenario, N=50k, MODERATE persistence
# (where swereg-TTE was biased by +0.150 with s2_ipw).

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

# Regenerate moderate-persistence DGP
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

# True PP
sim_intervention <- function(seed, force_A) {
  set.seed(seed)
  L0_po <- rnorm(N, 0, 1)
  out <- vector("list", T)
  for (t in 0:(T - 1)) {
    logit_Y <- -3.5 + TRUE_LOR*force_A + 0.4*L0_po
    Yt <- rbinom(N, 1, plogis(logit_Y))
    out[[t + 1L]] <- data.table(Y = Yt, A = force_A)
  }
  rbindlist(out)
}
po_t <- sim_intervention(999, 1L); po_u <- sim_intervention(999, 0L)
TRUE_PP <- coef(glm(Y ~ A, family = binomial, data = rbind(po_t, po_u)))["A"]
cat("True PP:", round(TRUE_PP, 4), "\n")

# Build trial-long
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

# ---- Variant A: WITH s2_ipw (Phase 16's setup) ----
cat("\n=== Variant A: swereg-TTE WITH s2_ipw (baseline IPW) ===\n")
trial_a <- TTEEnrollment$new(long, design)
trial_a$s2_ipw(stabilize = TRUE)
trial_a$s3_truncate_weights(lower = 0.01, upper = 0.99)
trial_a$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad_a <- copy(trial_a$data)
ad_a[, w_final := ipw_trunc * ipcw_pp]
ad_a <- ad_a[!is.na(w_final) & w_final > 0]
des_a <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad_a)
fit_a <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des_a, family = quasibinomial())
co_a <- summary(fit_a)$coefficients["treatment_baselineTRUE", , drop = FALSE]
cat(sprintf("  est = %.4f  se = %.4f\n", co_a[, "Estimate"], co_a[, "Std. Error"]))

# ---- Variant B: NO s2_ipw, baseline covariates in outcome model ----
cat("\n=== Variant B: swereg-TTE WITHOUT s2_ipw, baseline confounding via outcome regressor ===\n")
trial_b <- TTEEnrollment$new(long, design)
# Skip s2_ipw and s3_truncate. Set ipw = ipw_trunc = 1 so s6_ipcw_pp doesn't error.
trial_b$data[, ipw := 1.0]
trial_b$data[, ipw_trunc := 1.0]
trial_b$weight_cols <- "ipw"
trial_b$steps_completed <- c("ipw", "truncate")
trial_b$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad_b <- copy(trial_b$data)
ad_b[, w_final := ipcw_pp]   # IPCW only, no baseline IPW
ad_b <- ad_b[!is.na(w_final) & w_final > 0]
des_b <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad_b)
fit_b <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des_b, family = quasibinomial())
co_b <- summary(fit_b)$coefficients["treatment_baselineTRUE", , drop = FALSE]
cat(sprintf("  est = %.4f  se = %.4f\n", co_b[, "Estimate"], co_b[, "Std. Error"]))

cat("\n========================================================\n")
cat("EASY SCENARIO N=50k, MODERATE PERSISTENCE\n")
cat("True PP =", round(TRUE_PP, 4), "\n")
cat("========================================================\n")
cat(sprintf("%-50s %8s  %6s  %s\n", "Method", "est", "se", "bias"))
cat(sprintf("%-50s %8.4f  %6.4f  %.4f\n",
            "swereg-TTE WITH s2_ipw (previous)",
            co_a[, "Estimate"], co_a[, "Std. Error"],
            co_a[, "Estimate"] - TRUE_PP))
cat(sprintf("%-50s %8.4f  %6.4f  %.4f\n",
            "swereg-TTE WITHOUT s2_ipw (Variant B)",
            co_b[, "Estimate"], co_b[, "Std. Error"],
            co_b[, "Estimate"] - TRUE_PP))
cat(sprintf("%-50s %8.4f  %6.4f  %.4f\n",
            "(for ref) TE PP from Phase 16",
            -0.5335, 0.0295, -0.5335 - TRUE_PP))
