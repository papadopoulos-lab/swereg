# Phase 5: TrialEmulation in PP mode for apples-to-apples comparison.
#
# Correction from before: `cense` in TE is for EXTERNAL censoring
# (loss to follow-up); treatment switching is handled internally
# via switch_n_cov. So PP works without a cense column — just
# don't set use_censor_weights = TRUE.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)

data("trial_example")
te_input <- as.data.table(trial_example)
te_input$catvarA <- as.factor(te_input$catvarA)
te_input$catvarB <- as.factor(te_input$catvarB)

cat("=== TrialEmulation PP (sequential, switch weights) ===\n")
t0 <- Sys.time()
result_pp <- initiators(
  data         = te_input,
  id           = "id",
  period       = "period",
  eligible     = "eligible",
  treatment    = "treatment",
  estimand_type = "PP",
  outcome      = "outcome",
  model_var    = "assigned_treatment",
  outcome_cov  = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  # treatment-switching weights (handled internally by TE)
  switch_n_cov = ~ nvarA + nvarB,
  switch_d_cov = ~ catvarA + catvarB + nvarA + nvarB + nvarC,
  # no external censoring event in this data
  use_censor_weights = FALSE,
  quiet = TRUE
)
cat("elapsed:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

te_pp_trt <- result_pp$robust$summary[result_pp$robust$summary$names == "assigned_treatment", ]
cat("\n=== TE PP treatment effect ===\n")
print(te_pp_trt)

saveRDS(result_pp, "/home/raw996/papadopoulos/swereg/dev/omop_comparison/05_te_pp_result.rds")

# Side-by-side with all results so far
phase1   <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/01_te_result.rds")
te_itt   <- phase1$robust$summary[phase1$robust$summary$names == "assigned_treatment", ]

cat("\n========================================================\n")
cat("ALL FOUR ANALYSES, SAME DATA (trial_example)\n")
cat("========================================================\n")
cat(sprintf("%-50s %8s  %6s  %s\n", "Approach", "est", "se", "95% CI"))
cat(sprintf("%-50s %8.3f  %6.3f  (%.3f, %.3f)\n",
            "TE ITT (sequential MSM)",
            te_itt$estimate, te_itt$robust_se, te_itt$`2.5%`, te_itt$`97.5%`))
cat(sprintf("%-50s %8.3f  %6.3f  (%.3f, %.3f)\n",
            "TE PP (sequential MSM + switch weights)",
            te_pp_trt$estimate, te_pp_trt$robust_se, te_pp_trt$`2.5%`, te_pp_trt$`97.5%`))
# (swereg-TTE PP from Phase 4b, retrieved from saved log values)
cat(sprintf("%-50s %8.3f  %6.3f  (%.3f, %.3f)\n",
            "swereg-TTE PP (single-trial, IPW + IPCW glm)",
            -0.351, 0.259, -0.859, 0.156))
