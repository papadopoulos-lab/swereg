# Phase 14: R=10 replications of the simulation.
# Is swereg-TTE consistently more biased than TE PP, or was that noise?

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

N <- 5000
T <- 30
TRUE_LOR <- -0.5
R <- 10

simulate_once <- function(seed) {
  set.seed(seed)
  L0 <- rnorm(N, 0, 1)
  period_list <- vector("list", T)
  prev_A <- integer(N); prev_W <- L0
  for (t in 0:(T - 1)) {
    Wt <- 0.8*prev_W + 0.3*prev_A + rnorm(N, 0, 0.5)
    logit_A <- if (t == 0) -0.3 + 0.6*Wt else -2.0 + 0.6*Wt + 3.0*prev_A
    At <- rbinom(N, 1, plogis(logit_A))
    logit_Y <- -3.5 + TRUE_LOR*At + 0.4*L0 + 0.4*Wt
    Yt <- rbinom(N, 1, plogis(logit_Y))
    period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                         W_t = Wt, A_t = At, Y_t = Yt)
    prev_A <- At; prev_W <- Wt
  }
  rbindlist(period_list)
}

# True PP effect via potential outcomes (fixed across reps)
sim_intervention <- function(seed, force_A) {
  set.seed(seed)
  L0 <- rnorm(N, 0, 1)
  prev_W <- L0
  out <- vector("list", T)
  for (t in 0:(T - 1)) {
    Wt <- 0.8*prev_W + 0.3*force_A + rnorm(N, 0, 0.5)
    logit_Y <- -3.5 + TRUE_LOR*force_A + 0.4*L0 + 0.4*Wt
    Yt <- rbinom(N, 1, plogis(logit_Y))
    out[[t + 1L]] <- data.table(Y = Yt, A = force_A)
    prev_W <- Wt
  }
  rbindlist(out)
}

# Compute true PP with a big N for precision (using seed 999)
po_t <- sim_intervention(999, 1L); po_t[, A := 1L]
po_u <- sim_intervention(999, 0L); po_u[, A := 0L]
TRUE_PP_LOR <- coef(glm(Y ~ A, family = binomial, data = rbind(po_t, po_u)))["A"]
cat("Estimated true PP log-OR:", round(TRUE_PP_LOR, 3), "\n\n")

run_te <- function(dt_local) {
  te_in <- copy(dt_local)
  setnames(te_in, c("A_t", "Y_t", "W_t"), c("treatment", "outcome", "W"))
  te_in[, eligible := as.integer(period == 0)]
  res <- initiators(
    data = te_in, id = "id", period = "period", eligible = "eligible",
    treatment = "treatment", estimand_type = "PP", outcome = "outcome",
    model_var = "assigned_treatment", outcome_cov = c("L0"),
    switch_n_cov = ~ W, switch_d_cov = ~ L0 + W,
    use_censor_weights = FALSE, quiet = TRUE
  )
  res$robust$summary$estimate[res$robust$summary$names == "assigned_treatment"]
}

run_swereg <- function(dt_local) {
  sw <- copy(dt_local)
  sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
  sw[, baseline_L0 := L0]
  sw[, tstart := period]; sw[, tstop := period + 1L]
  sw[, time_treatment := as.logical(A_t)]
  sw[, treatment_baseline := as.logical(baseline_treatment)]
  setnames(sw, "id", "enrollment_person_trial_id")
  setnames(sw, "Y_t", "event")
  long <- sw[, .(enrollment_person_trial_id, tstart, tstop,
                 treatment_baseline, time_treatment, event,
                 baseline_L0, W_t)]
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = c("baseline_L0", "W_t"),
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
  fit <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des, family = quasibinomial())
  summary(fit)$coefficients["treatment_baselineTRUE", "Estimate"]
}

results <- data.table(rep = integer(), te = numeric(), sw = numeric())
for (r in 1:R) {
  dt <- simulate_once(seed = 2000 + r)
  te_est <- run_te(dt)
  sw_est <- run_swereg(dt)
  results <- rbind(results, data.table(rep = r, te = te_est, sw = sw_est))
  cat(sprintf("rep %2d:  TE = %+.3f  (bias %+.3f)   swereg = %+.3f  (bias %+.3f)\n",
              r, te_est, te_est - TRUE_PP_LOR, sw_est, sw_est - TRUE_PP_LOR))
}

cat("\n========================================================\n")
cat("R = 10 REP SUMMARY (true PP log-OR =", round(TRUE_PP_LOR, 3), ")\n")
cat("========================================================\n")
cat(sprintf("TE PP:      mean est = %+.3f   mean bias = %+.3f   abs-mean-bias = %.3f   sd = %.3f\n",
            mean(results$te), mean(results$te - TRUE_PP_LOR),
            mean(abs(results$te - TRUE_PP_LOR)), sd(results$te)))
cat(sprintf("swereg-TTE: mean est = %+.3f   mean bias = %+.3f   abs-mean-bias = %.3f   sd = %.3f\n",
            mean(results$sw), mean(results$sw - TRUE_PP_LOR),
            mean(abs(results$sw - TRUE_PP_LOR)), sd(results$sw)))
cat(sprintf("\nPaired diff (swereg - TE): mean = %+.3f   sd = %.3f\n",
            mean(results$sw - results$te), sd(results$sw - results$te)))
cat(sprintf("In how many reps is |swereg bias| > |TE bias|?  %d / %d\n",
            sum(abs(results$sw - TRUE_PP_LOR) > abs(results$te - TRUE_PP_LOR)), R))
