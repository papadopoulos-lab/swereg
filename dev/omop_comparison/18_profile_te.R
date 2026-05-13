# Phase 18: profile TE PP at N=50k to find the actual bottleneck.
# If most time is in data_preparation's trial expansion, we can replace
# that with a data.table version and call only trial_msm() on the result.
# If most time is in trial_msm's glm fit, we need a different attack.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(profvis)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

# Regenerate the strong-persistence dataset
L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)
for (t in 0:(T - 1)) {
  logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -3.0 + 0.6 * L0 + 6.0 * prev_A
  At_val <- rbinom(N, 1, plogis(logit_A))
  logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0
  Yt_val <- rbinom(N, 1, plogis(logit_Y))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list)
setorder(dt, id, period)

te_in <- copy(dt)
setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
te_in[, eligible := as.integer(period == 0)]

# Time data_preparation() separately from trial_msm()
working_dir <- file.path(tempdir(), "te_profile")
dir.create(working_dir, showWarnings = FALSE)

cat("=== Step 1: data_preparation ===\n")
t0 <- Sys.time()
prep <- data_preparation(
  data = te_in,
  id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", outcome = "outcome",
  estimand_type = "PP",
  outcome_cov = ~ L0,
  switch_n_cov = ~ L0,
  switch_d_cov = ~ L0,
  data_dir = working_dir,
  separate_files = FALSE,    # keep in memory if possible
  quiet = TRUE,
  use_censor_weights = FALSE
)
prep_time <- as.numeric(Sys.time() - t0, units = "secs")
cat("data_preparation elapsed:", round(prep_time, 1), "s\n")

prep_data <- if (is.list(prep) && "data" %in% names(prep)) prep$data else prep
cat("prepared rows:", format(nrow(prep_data), big.mark = ","), "\n")
cat("prepared cols:", paste(names(prep_data), collapse = ", "), "\n")

cat("\n=== Step 2: trial_msm ===\n")
t0 <- Sys.time()
msm <- trial_msm(
  data = prep,
  outcome_cov = c("L0"),
  model_var = "assigned_treatment",
  use_sample_weights = FALSE,
  quiet = TRUE
)
msm_time <- as.numeric(Sys.time() - t0, units = "secs")
cat("trial_msm elapsed:", round(msm_time, 1), "s\n")

cat("\n========================================================\n")
cat("TE PIPELINE TIMING AT N =", N, "\n")
cat("========================================================\n")
cat(sprintf("data_preparation:  %6.1f s  (%4.1f%%)\n",
            prep_time, 100*prep_time/(prep_time+msm_time)))
cat(sprintf("trial_msm:         %6.1f s  (%4.1f%%)\n",
            msm_time, 100*msm_time/(prep_time+msm_time)))
cat(sprintf("total:             %6.1f s\n", prep_time+msm_time))

trt <- msm$robust$summary[msm$robust$summary$names == "assigned_treatment", ]
cat(sprintf("estimate: %.4f  SE %.4f\n", trt$estimate, trt$robust_se))
