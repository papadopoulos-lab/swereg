# Phase 4: run swereg-TTE on trial_example, compare to TrialEmulation.
#
# IMPORTANT methodological note discovered while reading swereg-TTE:
#   swereg's TTEEnrollment expects "trial-level" data — ONE enrollment
#   per (person, trial_start), with baseline confounders captured at
#   trial_start, treatment assigned at trial_start, and tstart/tstop
#   for follow-up. It computes BASELINE IPW only (propensity for
#   treatment at trial start). No time-varying treatment weights.
#
#   This is the Hernan/Danaei 2013 single-trial approach (per person).
#
#   TrialEmulation does SEQUENTIAL trial emulation: every eligible
#   period is a new trial; person replicated across trials; pooled
#   MSM fit. Hernan's later refinement.
#
# So this comparison is across two DIFFERENT methodological choices
# implemented in two different packages. They will not produce the
# same number, and that's the actual finding.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

data("trial_example")
dt <- as.data.table(trial_example)

# --- Build trial-level data: one enrollment per person at first eligible period ---
# (Single-trial-per-person emulation, matching what swereg-TTE expects.)

dt[, ord := seq_len(.N), by = .(id)]
# Pick first eligible period as enrollment time for each person
baseline <- dt[eligible == 1, .SD[1], by = .(id)]
baseline_keys <- baseline[, .(id, trial_start = period)]

# Attach baseline information back to dt and compute follow-up
dt <- merge(dt, baseline_keys, by = "id")
followup <- dt[period >= trial_start]
followup[, t := period - trial_start]

# Per-person summary: any outcome during follow-up + time to event/censoring
trial_data <- followup[, .(
  treatment    = first(treatment),    # treatment AT baseline
  catvarA      = first(catvarA),
  catvarB      = first(catvarB),
  nvarA        = first(nvarA),
  nvarB        = first(nvarB),
  nvarC        = first(nvarC),
  event        = as.integer(any(outcome == 1)),
  tstop        = ifelse(any(outcome == 1), min(t[outcome == 1]), max(t))
), by = .(id)]
trial_data[, `:=`(
  tstart  = 0L,
  treatment = as.logical(treatment),
  catvarA = factor(catvarA),
  catvarB = factor(catvarB)
)]
setnames(trial_data, "id", "enrollment_person_trial_id")

cat("=== trial-level data for swereg-TTE ===\n")
cat("n enrollments:", nrow(trial_data), "\n")
cat("n treated:    ", sum(trial_data$treatment), "\n")
cat("n events:     ", sum(trial_data$event), "\n")
cat("median tstop: ", median(trial_data$tstop), "\n")

# --- swereg-TTE pipeline ---
design <- TTEDesign$new(
  id_var          = "enrollment_person_trial_id",
  treatment_var   = "treatment",
  outcome_vars    = "event",
  confounder_vars = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  follow_up_time  = max(trial_data$tstop)
)

trial <- TTEEnrollment$new(trial_data, design)
trial$s2_ipw(stabilize = TRUE)

cat("\n=== after s2_ipw() ===\n")
cat("steps_completed:", paste(trial$steps_completed, collapse = ", "), "\n")
cat("ipw range:", round(range(trial$data$ipw), 3), "\n")
cat("ps range:", round(range(trial$data$ps), 3), "\n")
cat("weight sum:", round(sum(trial$data$ipw), 1), "(N =", nrow(trial$data), ")\n")

trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
cat("\n=== after s3_truncate_weights() ===\n")
cat("ipw_trunc range:", round(range(trial$data$ipw_trunc), 3), "\n")

# --- Fit weighted logistic regression on the trial-level data ---
# (swereg-TTE produces analysis-ready data; user fits the model.)
# Comparable to TE's MSM but for the single-trial design.
fit <- glm(
  event ~ treatment + catvarA + catvarB + nvarA + nvarB + nvarC,
  data = trial$data,
  family = binomial(),
  weights = trial$data$ipw_trunc
)
cat("\n=== weighted logistic regression (swereg-TTE single-trial) ===\n")
co <- summary(fit)$coefficients
co_trt <- co["treatmentTRUE", , drop = FALSE]
print(co_trt)

# --- Comparison to TrialEmulation result ---
phase1 <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/01_te_result.rds")
te_trt <- phase1$robust$summary[
  phase1$robust$summary$names == "assigned_treatment", ]

cat("\n========================================================\n")
cat("SIDE-BY-SIDE TREATMENT EFFECT\n")
cat("========================================================\n")
cat(sprintf("TrialEmulation (sequential MSM, ITT):\n"))
cat(sprintf("  estimate = %.3f  se = %.3f  95%%CI = (%.3f, %.3f)  p = %.3f\n",
            te_trt$estimate, te_trt$robust_se, te_trt$`2.5%`, te_trt$`97.5%`, te_trt$p_value))
cat(sprintf("\nswereg-TTE (single-trial baseline IPW + weighted logistic):\n"))
cat(sprintf("  estimate = %.3f  se = %.3f  95%%CI = (%.3f, %.3f)  p = %.3f\n",
            co_trt[, "Estimate"], co_trt[, "Std. Error"],
            co_trt[, "Estimate"] - 1.96 * co_trt[, "Std. Error"],
            co_trt[, "Estimate"] + 1.96 * co_trt[, "Std. Error"],
            co_trt[, "Pr(>|z|)"]))
cat("\nNote: these two estimates target different estimands.\n")
cat("  TE: pooled MSM across many trials per person.\n")
cat("  swereg-TTE: one trial per person at first eligible period.\n")
