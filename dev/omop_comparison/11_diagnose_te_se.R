# Inspect what TE actually fit in single-trial vs sequential mode.
# Why is sequential SE (0.432) > single-trial SE (0.275)?
# More data should give smaller SE under correctly modeled correlation.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)

# Re-load
seq_result <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/05_te_pp_result.rds")

# Re-run single-trial to get the result object
data("trial_example")
dt <- as.data.table(trial_example)
dt[, catvarA := factor(catvarA)]
dt[, catvarB := factor(catvarB)]
first_elig <- dt[eligible == 1, .SD[1], by = .(id)][, .(id, first_elig_period = period)]
dt_single <- merge(dt, first_elig, by = "id")
dt_single[period > first_elig_period, eligible := 0]
dt_single[, first_elig_period := NULL]

single_result <- initiators(
  data         = dt_single,
  id           = "id",
  period       = "period",
  eligible     = "eligible",
  treatment    = "treatment",
  estimand_type = "PP",
  outcome      = "outcome",
  model_var    = "assigned_treatment",
  outcome_cov  = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  switch_n_cov = ~ nvarA + nvarB,
  switch_d_cov = ~ catvarA + catvarB + nvarA + nvarB + nvarC,
  use_censor_weights = FALSE,
  quiet = TRUE
)

cat("=== SEQUENTIAL TE PP (Phase 5) ===\n")
cat("model formula:\n")
print(formula(seq_result$model))
cat("n obs (model):", nrow(seq_result$model$data), "\n")
cat("coefficients:\n")
print(seq_result$model$coefficients)
cat("\nrobust SEs (full table):\n")
print(seq_result$robust$summary)

cat("\n\n=== SINGLE-TRIAL TE PP (Phase 9) ===\n")
cat("model formula:\n")
print(formula(single_result$model))
cat("n obs (model):", nrow(single_result$model$data), "\n")
cat("coefficients:\n")
print(single_result$model$coefficients)
cat("\nrobust SEs (full table):\n")
print(single_result$robust$summary)

cat("\n\n=== Compare data dimensions ===\n")
cat("Sequential model data n rows:    ", nrow(seq_result$model$data), "\n")
cat("Single-trial model data n rows:  ", nrow(single_result$model$data), "\n")

cat("\n=== Weight column stats ===\n")
seq_dat <- as.data.table(seq_result$model$data)
sin_dat <- as.data.table(single_result$model$data)
cat("Sequential weight (cumulative) range:", round(range(seq_dat$weight, na.rm=TRUE), 3), "\n")
cat("Sequential weight sd:                ", round(sd(seq_dat$weight, na.rm=TRUE), 3), "\n")
cat("Sequential weight mean:              ", round(mean(seq_dat$weight, na.rm=TRUE), 3), "\n")
cat("Single-trial weight range:           ", round(range(sin_dat$weight, na.rm=TRUE), 3), "\n")
cat("Single-trial weight sd:              ", round(sd(sin_dat$weight, na.rm=TRUE), 3), "\n")
cat("Single-trial weight mean:            ", round(mean(sin_dat$weight, na.rm=TRUE), 3), "\n")
