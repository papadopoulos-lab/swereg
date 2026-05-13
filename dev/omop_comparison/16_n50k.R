# Phase 16: easy scenario at N=50,000.
# If swereg's ~0.1 bias persists, it's systematic.
# If it shrinks toward zero, it was finite-sample noise.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

cat("Generating N =", N, "persons * T =", T, "periods =", N*T, "rows\n")

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
cat("dt rows:", nrow(dt), "  events:", sum(dt$Y_t),
    sprintf(" (%.2f%%)\n", 100*mean(dt$Y_t)))

# True PP via potential outcomes (N=50k, so very precise)
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
cat("True PP effect:", round(TRUE_PP, 4), "\n")

fit_oracle <- glm(Y_t ~ A_t + L0, family = binomial, data = dt)
cat("Oracle glm:    ", round(coef(fit_oracle)["A_t"], 3), "\n")

# ---- TE PP ----
te_in <- copy(dt)
setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
te_in[, eligible := as.integer(period == 0)]
cat("\nrunning TE PP (will take a while)...\n")
t0 <- Sys.time()
res_te <- initiators(
  data = te_in, id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", estimand_type = "PP", outcome = "outcome",
  model_var = "assigned_treatment", outcome_cov = c("L0"),
  switch_n_cov = ~ L0, switch_d_cov = ~ L0,
  use_censor_weights = FALSE, quiet = TRUE
)
cat("TE elapsed:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
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
cat("running swereg-TTE...\n")
t0 <- Sys.time()
trial <- TTEEnrollment$new(long, design)
trial$s2_ipw(stabilize = TRUE)
trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
trial$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
cat("swereg pipeline elapsed:",
    round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

ad <- copy(trial$data)
ad[, w_final := ipw_trunc * ipcw_pp]
ad <- ad[!is.na(w_final) & w_final > 0]
des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad)
fit_sw <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                 design = des, family = quasibinomial())
co_sw <- summary(fit_sw)$coefficients["treatment_baselineTRUE", , drop = FALSE]
sw_est <- co_sw[, "Estimate"]; sw_se <- co_sw[, "Std. Error"]

cat("\n========================================================\n")
cat("EASY SCENARIO, N =", N, "(true PP =", round(TRUE_PP, 4), ")\n")
cat("========================================================\n")
cat(sprintf("%-30s %8s  %6s  %s\n", "Estimator", "est", "se", "bias"))
cat(sprintf("%-30s %8.4f  %6s  %.4f\n", "oracle (Y ~ A + L0)",
            coef(fit_oracle)["A_t"], "-",
            coef(fit_oracle)["A_t"] - TRUE_PP))
cat(sprintf("%-30s %8.4f  %6.4f  %.4f\n", "TE PP",
            te_trt$estimate, te_trt$robust_se,
            te_trt$estimate - TRUE_PP))
cat(sprintf("%-30s %8.4f  %6.4f  %.4f\n", "swereg-TTE PP",
            sw_est, sw_se, sw_est - TRUE_PP))
cat("\n--- N=5k comparison ---\n")
cat("TE bias at N=5k:     -0.047\n")
cat("swereg bias at N=5k: -0.114\n")
