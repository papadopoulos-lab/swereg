# Phase 15: easy scenario — no time-varying confounder.
# W_t set to zero. Only baseline L0 confounds treatment.
# Both methods should easily recover the true effect.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 5000
T <- 30
TRUE_LOR <- -0.5

L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)

for (t in 0:(T - 1)) {
  Wt_val <- rep(0, N)  # NO time-varying confounder
  # Treatment: depends on L0 (baseline confounder) + persistence
  logit_A <- if (t == 0) {
    -0.3 + 0.6 * L0
  } else {
    -2.0 + 0.6 * L0 + 3.0 * prev_A
  }
  At_val <- rbinom(N, 1, plogis(logit_A))
  # Outcome: depends only on A_t and L0 (no W_t)
  logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0
  Yt_val <- rbinom(N, 1, plogis(logit_Y))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t,
                                       L0 = L0, W_t = Wt_val,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list)
setorder(dt, id, period)

cat("=== easy-scenario simulated data ===\n")
cat("rows:", nrow(dt), "  persons:", uniqueN(dt$id), "\n")
cat("treated rows:", sum(dt$A_t), sprintf("(%.1f%%)\n", 100*mean(dt$A_t)))
cat("event rows:  ", sum(dt$Y_t), sprintf("(%.1f%%)\n", 100*mean(dt$Y_t)))

# Compute true PP by potential outcomes
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
po_t <- sim_intervention(999, 1L)
po_u <- sim_intervention(999, 0L)
TRUE_PP <- coef(glm(Y ~ A, family = binomial, data = rbind(po_t, po_u)))["A"]
cat("\nTrue PP effect (no W_t):", round(TRUE_PP, 3),
    "  (should be very close to per-period coef", TRUE_LOR, ")\n")

# Naive and oracle
fit_naive <- glm(Y_t ~ A_t, family = binomial, data = dt)
fit_oracle <- glm(Y_t ~ A_t + L0, family = binomial, data = dt)
cat("\nNaive glm:    ", round(coef(fit_naive)["A_t"], 3), "\n")
cat("Oracle glm:   ", round(coef(fit_oracle)["A_t"], 3),
    "(should be ~", TRUE_LOR, ")\n")

# ---- TE PP ----
te_in <- copy(dt)
setnames(te_in, c("A_t", "Y_t", "W_t"), c("treatment", "outcome", "W"))
te_in[, eligible := as.integer(period == 0)]
res_te <- initiators(
  data = te_in, id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", estimand_type = "PP", outcome = "outcome",
  model_var = "assigned_treatment", outcome_cov = c("L0"),
  switch_n_cov = ~ L0, switch_d_cov = ~ L0,  # no W -- only baseline confounder
  use_censor_weights = FALSE, quiet = TRUE
)
te_trt <- res_te$robust$summary[res_te$robust$summary$names == "assigned_treatment", ]

# ---- swereg-TTE PP ----
sw <- copy(dt)
sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
sw[, baseline_L0 := L0]
sw[, tstart := period]; sw[, tstop := period + 1L]
sw[, time_treatment := as.logical(A_t)]
sw[, treatment_baseline := as.logical(baseline_treatment)]
setnames(sw, "id", "enrollment_person_trial_id")
setnames(sw, "Y_t", "event")
long <- sw[, .(enrollment_person_trial_id, tstart, tstop,
               treatment_baseline, time_treatment, event,
               baseline_L0)]
design <- TTEDesign$new(
  id_var = "enrollment_person_trial_id",
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
ad <- copy(trial$data)
ad[, w_final := ipw_trunc * ipcw_pp]
ad <- ad[!is.na(w_final) & w_final > 0]
des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad)
fit_sw <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                 design = des, family = quasibinomial())
co_sw <- summary(fit_sw)$coefficients["treatment_baselineTRUE", , drop = FALSE]
sw_est <- co_sw[, "Estimate"]
sw_se  <- co_sw[, "Std. Error"]

cat("\n========================================================\n")
cat("EASY SCENARIO (no W_t)\n")
cat("========================================================\n")
cat(sprintf("%-50s %8s  %6s  %s\n", "Estimator", "est", "se", "bias vs PP"))
cat(sprintf("%-50s %8.3f  %6s  %.3f\n", "naive (no adjust)",
            coef(fit_naive)["A_t"], "-",
            coef(fit_naive)["A_t"] - TRUE_PP))
cat(sprintf("%-50s %8.3f  %6s  %.3f\n", "oracle (Y ~ A + L0)",
            coef(fit_oracle)["A_t"], "-",
            coef(fit_oracle)["A_t"] - TRUE_PP))
cat(sprintf("%-50s %8.3f  %6.3f  %.3f\n", "TE PP",
            te_trt$estimate, te_trt$robust_se,
            te_trt$estimate - TRUE_PP))
cat(sprintf("%-50s %8.3f  %6.3f  %.3f\n", "swereg-TTE PP",
            sw_est, sw_se, sw_est - TRUE_PP))
