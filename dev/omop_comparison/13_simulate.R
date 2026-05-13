# Phase 13: simulate data with a KNOWN true per-period treatment effect,
# run both methods, see if either/both recover it.
#
# Data-generating process:
#   - N persons, T periods each
#   - Baseline confounder L0 (continuous)
#   - Time-varying confounder W_t evolves: W_t = a*W_{t-1} + b*A_{t-1} + noise
#     (worsening disease activity, partly caused by past treatment status)
#   - Treatment A_t at period t depends on W_t (confounding by indication)
#     and on A_{t-1} (treatment persistence -- mostly people stay on)
#   - Outcome Y_t at period t depends on:
#         intercept + TRUE_LOR * A_t + L0 + W_t
#     so the true per-period log-OR for A_t on Y_t is exactly TRUE_LOR.
#
# By design: time-varying confounder W_t drives BOTH switching and outcome,
# so naive analysis is biased; PP machinery (with W_t in the weight model)
# should correct.

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

# Build dataset by binding period-wise records
period_list <- vector("list", T)
prev_A <- integer(N)
prev_W <- L0  # initial W

for (t in 0:(T - 1)) {
  Wt_val <- 0.8 * prev_W + 0.3 * prev_A + rnorm(N, 0, 0.5)
  logit_A <- if (t == 0) {
    -0.3 + 0.6 * Wt_val
  } else {
    -2.0 + 0.6 * Wt_val + 3.0 * prev_A
  }
  At_val <- rbinom(N, 1, plogis(logit_A))
  logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0 + 0.4 * Wt_val
  Yt_val <- rbinom(N, 1, plogis(logit_Y))
  period_list[[t + 1L]] <- data.table(
    id = seq_len(N),
    period = t,
    L0 = L0,
    W_t = Wt_val,
    A_t = At_val,
    Y_t = Yt_val
  )
  prev_A <- At_val
  prev_W <- Wt_val
}
dt <- rbindlist(period_list)
setorder(dt, id, period)

# ---------------------------------------------------------------
# Compute the TRUE per-protocol effect by simulating potential
# outcomes under "always treated" and "never treated" using the
# same DGP and same L0 / noise sequence.
# ---------------------------------------------------------------
simulate_intervention <- function(N, T, L0, force_A) {
  set.seed(99)  # same noise for both interventions for paired comparison
  prev_W <- L0
  res <- vector("list", T)
  for (t in 0:(T - 1)) {
    Wt_val <- 0.8 * prev_W + 0.3 * force_A + rnorm(N, 0, 0.5)
    At_val <- rep(force_A, N)
    logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0 + 0.4 * Wt_val
    Yt_val <- rbinom(N, 1, plogis(logit_Y))
    res[[t + 1L]] <- data.table(period = t, A_t = At_val, Y_t = Yt_val,
                                 W_t = Wt_val, L0 = L0)
    prev_W <- Wt_val
  }
  rbindlist(res)
}
po_treated   <- simulate_intervention(N, T, L0, 1L)
po_untreated <- simulate_intervention(N, T, L0, 0L)
# Per-period log-OR under always-treated vs never-treated, pooled
po_pool <- rbind(
  data.table(Y = po_treated$Y_t,   A = 1L),
  data.table(Y = po_untreated$Y_t, A = 0L)
)
fit_po <- glm(Y ~ A, data = po_pool, family = binomial)
TRUE_PP_LOR <- coef(fit_po)["A"]
cat("\n=== TRUE PP effect from potential outcomes ===\n")
cat("event rate always-treated:  ", round(mean(po_treated$Y_t),   4), "\n")
cat("event rate never-treated:   ", round(mean(po_untreated$Y_t), 4), "\n")
cat("True marginal log-OR (PP):  ", round(TRUE_PP_LOR, 3), "\n")
cat("(differs from per-period coefficient",  TRUE_LOR,
    "due to A->W->Y feedback)\n")

cat("=== simulated data ===\n")
cat("rows:", nrow(dt), "  persons:", uniqueN(dt$id), "\n")
cat("treated (A_t=1) rows:", sum(dt$A_t), "of", nrow(dt),
    sprintf(" (%.1f%%)", 100*mean(dt$A_t)), "\n")
cat("events (Y_t=1) rows:", sum(dt$Y_t), "of", nrow(dt),
    sprintf(" (%.1f%%)", 100*mean(dt$Y_t)), "\n")
cat("TRUE per-period log-OR for A on Y:", TRUE_LOR, "\n")

# Sanity check: confounded naive estimate (no adjustment)
fit_naive <- glm(Y_t ~ A_t, family = binomial, data = dt)
cat("\nNaive unadjusted glm:    est =", round(coef(fit_naive)["A_t"], 3), "\n")

# Adjusted for confounders (the right model if we had no time-varying treatment):
fit_adj <- glm(Y_t ~ A_t + L0 + W_t, family = binomial, data = dt)
cat("Adjusted glm (oracle):   est =", round(coef(fit_adj)["A_t"], 3),
    "(should be ~", TRUE_LOR, ")\n")

# ---------------- TE PP ----------------
te_input <- copy(dt)
setnames(te_input, c("A_t", "Y_t", "W_t"), c("treatment", "outcome", "W"))
te_input[, eligible := as.integer(period == 0)]   # single trial: enroll at period 0
te_input[, catvar_dummy := factor(1L)]  # TE needs at least one categorical

cat("\n=== TE PP, sequential MSM (single trial via eligible=1 only at period 0) ===\n")
t0 <- Sys.time()
res_te <- initiators(
  data           = te_input,
  id             = "id",
  period         = "period",
  eligible       = "eligible",
  treatment      = "treatment",
  estimand_type  = "PP",
  outcome        = "outcome",
  model_var      = "assigned_treatment",
  # BASELINE covariates only in outcome model (standard MSM practice).
  # Time-varying confounder W goes into the SWITCH weight model, not outcome.
  outcome_cov    = c("L0"),
  switch_n_cov   = ~ W,
  switch_d_cov   = ~ L0 + W,
  use_censor_weights = FALSE,
  quiet = TRUE
)
cat("elapsed:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
te_trt <- res_te$robust$summary[res_te$robust$summary$names == "assigned_treatment", ]
cat(sprintf("TE PP estimate: %.3f  (SE %.3f, 95%%CI %.3f - %.3f)  bias = %.3f\n",
            te_trt$estimate, te_trt$robust_se, te_trt$`2.5%`, te_trt$`97.5%`,
            te_trt$estimate - TRUE_LOR))

# ---------------- swereg-TTE PP ----------------
# Build single-trial-per-person long format. Keep W_t as a time-varying
# confounder so swereg's IPCW model can use it (the previous version
# only had baseline W, which couldn't capture time-varying switching).
sw <- copy(dt)
sw[, baseline_treatment := A_t[period == 0L][1L], by = .(id)]
sw[, baseline_L0 := L0]
sw[, t := period]
sw[, tstart := t]
sw[, tstop  := t + 1L]
sw[, time_treatment := as.logical(A_t)]
sw[, treatment_baseline := as.logical(baseline_treatment)]
setnames(sw, "id", "enrollment_person_trial_id")
setnames(sw, "Y_t", "event")
trial_long <- sw[, .(
  enrollment_person_trial_id,
  tstart, tstop,
  treatment_baseline, time_treatment,
  event,
  baseline_L0,
  W_t  # KEY: keep time-varying confounder for IPCW model
)]

# Confounders include time-varying W_t now.
# Note: s2_ipw uses confounders at baseline (tstart == 0), so for it
# W_t is the period-0 value (=baseline W). For s6_ipcw_pp, confounders
# enter the censoring model at all periods, so W_t is time-varying.
design <- TTEDesign$new(
  id_var             = "enrollment_person_trial_id",
  treatment_var      = "treatment_baseline",
  time_treatment_var = "time_treatment",
  outcome_vars       = "event",
  confounder_vars    = c("baseline_L0", "W_t"),
  follow_up_time     = max(trial_long$tstop)
)
trial <- TTEEnrollment$new(trial_long, design)
trial$s2_ipw(stabilize = TRUE)
trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
trial$s4_prepare_for_analysis(
  outcome = "event",
  follow_up = max(trial_long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad <- copy(trial$data)
ad[, w_final := ipw_trunc * ipcw_pp]
ad <- ad[!is.na(w_final) & w_final > 0]

des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad)
fit_sw <- svyglm(
  # outcome model: baseline covariates + time terms only (standard MSM)
  event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
  design = des, family = quasibinomial()
)
co_sw <- summary(fit_sw)$coefficients["treatment_baselineTRUE", , drop = FALSE]
sw_est <- co_sw[, "Estimate"]
sw_se  <- co_sw[, "Std. Error"]
cat(sprintf("\nswereg-TTE PP estimate: %.3f  (SE %.3f, 95%%CI %.3f - %.3f)  bias = %.3f\n",
            sw_est, sw_se, sw_est - 1.96*sw_se, sw_est + 1.96*sw_se,
            sw_est - TRUE_LOR))

cat("\n========================================================\n")
cat("R=1 SIMULATION RESULT\n")
cat("========================================================\n")
cat(sprintf("True per-period coefficient on A:   %.3f\n", TRUE_LOR))
cat(sprintf("True marginal PP log-OR (computed): %.3f\n", TRUE_PP_LOR))
cat("\n")
cat(sprintf("%-50s %8s  %6s  %s\n", "Estimator", "est", "se", "bias vs PP truth"))
cat(sprintf("%-50s %8.3f  %6s  %.3f\n", "naive (no adjustment)",
            coef(fit_naive)["A_t"], "-", coef(fit_naive)["A_t"] - TRUE_PP_LOR))
cat(sprintf("%-50s %8.3f  %6s  %.3f\n", "oracle glm (Y ~ A + L0 + W)",
            coef(fit_adj)["A_t"], "-", coef(fit_adj)["A_t"] - TRUE_PP_LOR))
cat(sprintf("%-50s %8.3f  %6.3f  %.3f\n", "TE PP",
            te_trt$estimate, te_trt$robust_se, te_trt$estimate - TRUE_PP_LOR))
cat(sprintf("%-50s %8.3f  %6.3f  %.3f\n", "swereg-TTE PP",
            sw_est, sw_se, sw_est - TRUE_PP_LOR))
