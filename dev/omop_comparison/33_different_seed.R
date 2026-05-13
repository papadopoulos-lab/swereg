# Re-run the N=20k comparison with a different seed. If bias direction
# flips, the earlier 0.045 was noise. If it persists, look more carefully.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

# Truth from previous high-N run
TRUE_LOG_IRR <- -0.4849
TRUE_LOG_OR  <- -0.4973

simulate <- function(N, T_periods, seed) {
  set.seed(seed)
  L0 <- rnorm(N, 0, 1)
  out <- vector("list", T_periods)
  prev_A <- integer(N)
  for (t in 0:(T_periods - 1)) {
    logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -3.0 + 0.4 * L0 + 6.0 * prev_A
    At <- rbinom(N, 1, plogis(logit_A))
    Yt <- rbinom(N, 1, plogis(-3.5 + (-0.5) * At + 0.4 * L0))
    out[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0, A_t = At, Y_t = Yt)
    prev_A <- At
  }
  rbindlist(out)[order(id, period)]
}

run_swereg <- function(dt) {
  sw <- copy(dt)
  sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
  sw[, baseline_L0 := L0]
  sw[, tstart := period]; sw[, tstop := period + 1L]
  sw[, time_treatment := as.logical(A_t)]
  sw[, treatment_baseline := as.logical(baseline_treatment)]
  sw[, person_weeks := 1L]
  setnames(sw, "id", "enrollment_person_trial_id")
  setnames(sw, "Y_t", "event")
  long <- sw[, .(enrollment_person_trial_id, tstart, tstop,
                 treatment_baseline, time_treatment, event,
                 person_weeks, baseline_L0)]
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = c("baseline_L0"),
    follow_up_time = max(long$tstop))
  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = "event", follow_up = max(long$tstop),
    estimate_ipcw_pp_with_gam = TRUE,
    estimate_ipcw_pp_separately_by_treatment = TRUE)
  trial$data[, analysis_weight_pp_trunc := ipw_trunc * ipcw_pp]
  res <- trial$irr(weight_col = "analysis_weight_pp_trunc")
  list(est = log(res$IRR), se = (log(res$IRR_upper) - log(res$IRR_lower)) / (2*1.96))
}
run_te <- function(dt) {
  te_in <- copy(dt)
  setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
  te_in[, eligible := as.integer(period == 0)]
  res <- initiators(
    data = te_in, id = "id", period = "period", eligible = "eligible",
    treatment = "treatment", estimand_type = "PP", outcome = "outcome",
    model_var = "assigned_treatment", outcome_cov = c("L0"),
    switch_n_cov = ~ L0, switch_d_cov = ~ L0,
    use_censor_weights = FALSE, quiet = TRUE)
  row <- res$robust$summary[res$robust$summary$names == "assigned_treatment", ]
  list(est = row$estimate, se = row$robust_se)
}

cat(sprintf("True log-IRR = %.4f   True log-OR = %.4f\n\n",
            TRUE_LOG_IRR, TRUE_LOG_OR))
cat(sprintf("%-8s %-10s %10s %10s %10s\n", "seed", "method", "est", "bias_irr", "bias_or"))
cat(sprintf("%-8s %-10s %10s %10s %10s\n", "----", "------", "----", "--------", "-------"))

for (seed in c(2026L, 7L, 42L, 1337L)) {
  dt <- simulate(N = 20000, T_periods = 15, seed = seed)
  sw <- run_swereg(dt); te <- run_te(dt)
  cat(sprintf("%-8d %-10s %+10.4f %+10.4f %+10.4f\n",
              seed, "swereg", sw$est, sw$est - TRUE_LOG_IRR, sw$est - TRUE_LOG_OR))
  cat(sprintf("%-8d %-10s %+10.4f %+10.4f %+10.4f\n",
              seed, "TE",    te$est, te$est - TRUE_LOG_IRR, te$est - TRUE_LOG_OR))
}
