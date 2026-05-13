# Phase 9: force TrialEmulation into single-trial-per-person mode.
# TE auto-expands sequentially based on eligible == 1. By zeroing out
# eligibility after each person's FIRST eligible period, we force TE
# to emulate exactly one trial per person — matching swereg-TTE
# single-trial PP. Now the only remaining difference is the methodology
# (switching weights vs IPCW, MSM vs weighted-glm, etc.), not the
# trial-replication design.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)

data("trial_example")
dt <- as.data.table(trial_example)
dt[, catvarA := factor(catvarA)]
dt[, catvarB := factor(catvarB)]

# Force single-trial per person: keep eligible = 1 only at first eligible row
first_elig <- dt[eligible == 1, .SD[1], by = .(id)][, .(id, first_elig_period = period)]
dt_single <- merge(dt, first_elig, by = "id")
dt_single[period > first_elig_period, eligible := 0]
dt_single[, first_elig_period := NULL]

cat("=== sanity check ===\n")
cat("rows in modified data:", nrow(dt_single), "\n")
cat("persons:", uniqueN(dt_single$id), "\n")
cat("total eligible == 1 rows:", sum(dt_single$eligible == 1),
    " (should equal n persons)\n")
cat("max eligible rows per person:",
    max(dt_single[, sum(eligible), by = id]$V1), "\n")

cat("\n=== TE PP, single-trial mode ===\n")
t0 <- Sys.time()
result_pp_single <- initiators(
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
cat("elapsed:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
te_single_trt <- result_pp_single$robust$summary[
  result_pp_single$robust$summary$names == "assigned_treatment", ]
cat("\nTE PP single-trial:\n")
print(te_single_trt)

# All four single-trial-comparable results
te_pp <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/05_te_pp_result.rds")
te_pp_trt <- te_pp$robust$summary[te_pp$robust$summary$names == "assigned_treatment", ]

cat("\n========================================================\n")
cat("APPLES-TO-APPLES: SINGLE-TRIAL PP, BOTH PACKAGES\n")
cat("========================================================\n")
fmt <- function(est, se, lo, hi) sprintf("%8.3f  %6.3f  (%.3f, %.3f)", est, se, lo, hi)
cat(sprintf("%-50s %s\n", "Approach", "est    se    95% CI"))
cat(sprintf("%-50s %s\n", "TE PP single-trial",
            fmt(te_single_trt$estimate, te_single_trt$robust_se,
                te_single_trt$`2.5%`, te_single_trt$`97.5%`)))
cat(sprintf("%-50s %s\n", "swereg-TTE single-trial PP (svyglm)",
            fmt(-0.351, 0.333, -1.005, 0.302)))
cat(sprintf("%-50s %s\n", "(for ref) TE PP sequential",
            fmt(te_pp_trt$estimate, te_pp_trt$robust_se,
                te_pp_trt$`2.5%`, te_pp_trt$`97.5%`)))
