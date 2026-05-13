# Phase 1: TrialEmulation baseline on its own fake data (trial_example)
# Goal: see what the workflow + output look like before comparing with swereg.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))

library(TrialEmulation)
data("trial_example")

cat("=== trial_example structure ===\n")
str(trial_example)
cat("\n=== first 6 rows ===\n")
print(head(trial_example))
cat("\n=== summary ===\n")
cat("n persons:", length(unique(trial_example$id)), "\n")
cat("n person-periods:", nrow(trial_example), "\n")
cat("period range:", range(trial_example$period), "\n")
cat("eligible rows:", sum(trial_example$eligible == 1), "\n")
cat("outcome events:", sum(trial_example$outcome == 1), "\n")

trial_example$catvarA <- as.factor(trial_example$catvarA)
trial_example$catvarB <- as.factor(trial_example$catvarB)

cat("\n=== running initiators() (ITT) ===\n")
t0 <- Sys.time()
result <- initiators(
  data = trial_example,
  id = "id",
  period = "period",
  eligible = "eligible",
  treatment = "treatment",
  estimand_type = "ITT",
  outcome = "outcome",
  model_var = "assigned_treatment",
  outcome_cov = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  use_censor_weights = FALSE,
  quiet = TRUE
)
cat("elapsed:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

cat("\n=== result object ===\n")
cat("class:", paste(class(result), collapse = " / "), "\n")
cat("names:", paste(names(result), collapse = ", "), "\n")

cat("\n=== robust summary (point estimates + CIs) ===\n")
print(result$robust$summary)

saveRDS(result, "/home/raw996/papadopoulos/swereg/dev/omop_comparison/01_te_result.rds")
cat("\nSaved to dev/omop_comparison/01_te_result.rds\n")
