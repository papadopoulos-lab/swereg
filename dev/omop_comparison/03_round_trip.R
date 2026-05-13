# Phase 3: round-trip test.
# Take trial_example, pretend it's a swereg skeleton (it already has
# the right long-format shape), pass it through the bridge function,
# and verify TrialEmulation produces the same estimate as in Phase 1.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)

source("dev/omop_comparison/02_bridge.R")

data("trial_example")
te_native <- as.data.table(trial_example)

# trial_example already has the swereg-compatible columns; we'll pretend
# the user has a swereg skeleton that happens to use the same names.
# To exercise the bridge non-trivially, rename a couple columns first
# to simulate a real swereg skeleton.
sk <- copy(te_native)
setnames(sk,
         old = c("id",       "period",      "eligible",      "outcome",   "treatment"),
         new = c("id",       "isoyearweek", "rd_eligible",   "f64_diag",  "rd_treat"))
sk[, is_isoyear := FALSE]  # mark all as weekly rows

# Bridge back to TrialEmulation shape
te_via_bridge <- skeleton_to_trialemulation(
  sk,
  id_col         = "id",
  period_col     = "isoyearweek",
  eligible_col   = "rd_eligible",
  outcome_col    = "f64_diag",
  treatment_col  = "rd_treat",
  covariate_cols = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC")
)

# Sanity: same dimensions, same content (modulo integer period)
cat("Native rows:", nrow(te_native), "  Bridge rows:", nrow(te_via_bridge), "\n")
cat("Period range native:", range(te_native$period), "\n")
cat("Period range bridge:", range(te_via_bridge$period), "\n")

# Critically: trial_example periods are not contiguous (range 1:396 but
# only some periods used). The bridge factor-encodes by sorted unique
# levels, which compresses the period domain. Decide whether to fix.
n_unique_native <- length(unique(te_native$period))
n_unique_bridge <- length(unique(te_via_bridge$period))
cat("Unique periods native:", n_unique_native, "  bridge:", n_unique_bridge, "\n")

# Make types match what Phase 1 used
te_via_bridge[, catvarA := factor(catvarA)]
te_via_bridge[, catvarB := factor(catvarB)]

cat("\n=== running initiators() on bridged data ===\n")
t0 <- Sys.time()
result_bridge <- initiators(
  data         = te_via_bridge,
  id           = "id",
  period       = "period",
  eligible     = "eligible",
  treatment    = "treatment",
  estimand_type = "ITT",
  outcome      = "outcome",
  model_var    = "assigned_treatment",
  outcome_cov  = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  use_censor_weights = FALSE,
  quiet = TRUE
)
cat("elapsed:", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

# Compare to Phase 1 result
phase1 <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/01_te_result.rds")

cat("\n=== Phase 1 treatment effect ===\n")
print(phase1$robust$summary[phase1$robust$summary$names == "assigned_treatment", ])
cat("\n=== Phase 3 (via bridge) treatment effect ===\n")
print(result_bridge$robust$summary[result_bridge$robust$summary$names == "assigned_treatment", ])

# Estimate-level identity check
e1 <- phase1$robust$summary[phase1$robust$summary$names == "assigned_treatment", "estimate"]
e3 <- result_bridge$robust$summary[result_bridge$robust$summary$names == "assigned_treatment", "estimate"]
cat("\nestimate diff:", e3 - e1, "  (should be ~0 if bridge is lossless)\n")
