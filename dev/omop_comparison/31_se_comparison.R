# SE/CI comparison: swereg $irr() vs TE PP after the censor-row fix.
# Run on Test 2 DGP at multiple N to see how SEs scale.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

simulate <- function(N, T_periods = 15, seed = 2026) {
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

true_pp_log_irr <- function(N, T_periods) {
  ev <- numeric(2); pp <- numeric(2)
  for (i in 1:2) {
    force_A <- c(0L, 1L)[i]
    set.seed(999); L0_po <- rnorm(N, 0, 1)
    n_ev <- 0L; n_pp <- 0L
    for (t in 0:(T_periods - 1)) {
      Y <- rbinom(N, 1, plogis(-3.5 + (-0.5)*force_A + 0.4*L0_po))
      n_ev <- n_ev + sum(Y); n_pp <- n_pp + N
    }
    ev[i] <- n_ev; pp[i] <- n_pp
  }
  log((ev[2]/pp[2]) / (ev[1]/pp[1]))
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
  trial$data[, analysis_weight_pp_trunc := ipw_trunc * ipcw_pp]
  res <- trial$irr(weight_col = "analysis_weight_pp_trunc")
  list(est = log(res$IRR), lower = log(res$IRR_lower),
       upper = log(res$IRR_upper), se = (log(res$IRR_upper) - log(res$IRR_lower)) / (2*1.96))
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
    use_censor_weights = FALSE, quiet = TRUE
  )
  row <- res$robust$summary[res$robust$summary$names == "assigned_treatment", ]
  list(est = row$estimate, lower = row$`2.5%`, upper = row$`97.5%`,
       se = row$robust_se)
}

# Run at multiple sample sizes
T_periods <- 15
results <- data.table()
for (N in c(2000, 5000, 20000)) {
  cat(sprintf("=== N = %d ===\n", N))
  dt <- simulate(N = N, T_periods = T_periods)
  true_log_irr <- true_pp_log_irr(N = N, T_periods = T_periods)

  t0 <- Sys.time()
  sw <- run_swereg(dt)
  sw_time <- as.numeric(Sys.time() - t0, units = "secs")

  t0 <- Sys.time()
  te <- run_te(dt)
  te_time <- as.numeric(Sys.time() - t0, units = "secs")

  results <- rbind(results, data.table(
    N = N, true = true_log_irr,
    sw_est = sw$est, sw_se = sw$se, sw_width = sw$upper - sw$lower, sw_time = sw_time,
    te_est = te$est, te_se = te$se, te_width = te$upper - te$lower, te_time = te_time
  ))
}

cat("\n================================================================\n")
cat("SE / CI WIDTH COMPARISON (true log-IRR =", round(results$true[1], 3), "...)\n")
cat("================================================================\n")
cat(sprintf("%-8s %8s %8s %8s %8s %8s %8s %8s %8s\n",
            "N", "sw_est", "te_est", "sw_se", "te_se", "sw_wd", "te_wd", "sw_t", "te_t"))
for (i in seq_len(nrow(results))) {
  r <- results[i]
  cat(sprintf("%-8d %+.3f  %+.3f  %.3f    %.3f    %.3f    %.3f    %.1fs    %.1fs\n",
              r$N, r$sw_est, r$te_est, r$sw_se, r$te_se,
              r$sw_width, r$te_width, r$sw_time, r$te_time))
}
cat("\nse ratio (swereg/te):\n")
print(round(results$sw_se / results$te_se, 2))
cat("\nwidth ratio (swereg/te):\n")
print(round(results$sw_width / results$te_width, 2))
