# Phase 4b: full swereg-TTE pipeline (s2 + s4 + s5 + s6) vs TrialEmulation PP.
#
# This corrects Phase 4 which stopped at baseline IPW.
# Full swereg-TTE = s2_ipw (baseline) + s5_prepare_outcome (PP censoring) + s6_ipcw_pp
# (time-varying IPCW via GAM). Compare against TrialEmulation in PP mode with
# censor weights enabled, on the SAME data, single-trial-per-person.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

data("trial_example")
dt <- as.data.table(trial_example)
dt[, catvarA_f := factor(catvarA)]
dt[, catvarB_f := factor(catvarB)]

# -----------------------------------------------------------------
# Build trial-level long-format data for swereg-TTE
# -----------------------------------------------------------------
# Single-trial-per-person: trial_start = first eligible period for each id.
baseline_rows <- dt[eligible == 1, .SD[1], by = .(id)]
baseline_treatments <- baseline_rows[, .(id, baseline_treatment = as.logical(treatment), trial_start = period)]

followup <- merge(dt, baseline_treatments, by = "id")
followup <- followup[period >= trial_start]
followup[, t := period - trial_start]
setorder(followup, id, t)

# tstart / tstop columns expected by swereg-TTE
followup[, tstart := t]
followup[, tstop  := t + 1L]

# Treatment columns: baseline_treatment = fixed; time_treatment = per-period current treatment
followup[, treatment_baseline := baseline_treatment]
followup[, time_treatment     := as.logical(treatment)]

# Rename id to enrollment_person_trial_id for swereg-TTE
setnames(followup, "id", "enrollment_person_trial_id")
followup[, event := as.integer(outcome)]

# Subset to columns swereg-TTE needs
trial_long <- followup[, .(
  enrollment_person_trial_id,
  tstart, tstop,
  treatment_baseline, time_treatment,
  event,
  catvarA_f, catvarB_f,
  nvarA, nvarB, nvarC
)]

cat("=== trial-level long data ===\n")
cat("n persons:", uniqueN(trial_long$enrollment_person_trial_id), "\n")
cat("n rows:   ", nrow(trial_long), "\n")
cat("n events: ", sum(trial_long$event), "\n")

# -----------------------------------------------------------------
# swereg-TTE: design + full pipeline
# -----------------------------------------------------------------
design <- TTEDesign$new(
  id_var             = "enrollment_person_trial_id",
  treatment_var      = "treatment_baseline",
  time_treatment_var = "time_treatment",
  outcome_vars       = "event",
  confounder_vars    = c("catvarA_f", "catvarB_f", "nvarA", "nvarB", "nvarC"),
  follow_up_time     = max(trial_long$tstop)
)
trial <- TTEEnrollment$new(trial_long, design)

cat("\n--- s2_ipw() ---\n")
trial$s2_ipw(stabilize = TRUE)
cat("ipw range:", round(range(trial$data$ipw), 3), "\n")

cat("\n--- s3_truncate_weights() ---\n")
trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
cat("ipw_trunc range:", round(range(trial$data$ipw_trunc), 3), "\n")

cat("\n--- s4_prepare_for_analysis() = s5 + s6 ---\n")
trial$s4_prepare_for_analysis(
  outcome = "event",
  follow_up = max(trial_long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
cat("steps_completed:", paste(trial$steps_completed, collapse = ", "), "\n")
cat("ipcw_pp range:", round(range(trial$data$ipcw_pp, na.rm = TRUE), 3), "\n")
cat("rows remaining after PP censoring:", nrow(trial$data), "\n")
cat("events remaining:", sum(trial$data$event, na.rm = TRUE), "\n")

# Final weight = baseline IPW * time-varying IPCW
trial$data[, w_final := ipw_trunc * ipcw_pp]
cat("final weight range:", round(range(trial$data$w_final, na.rm = TRUE), 3), "\n")

# Fit pooled logistic regression on prepared data
# (analog of TrialEmulation's MSM for the single-trial design)
fit_pp <- glm(
  event ~ treatment_baseline + tstop + catvarA_f + catvarB_f + nvarA + nvarB + nvarC,
  data = trial$data[!is.na(w_final) & w_final > 0],
  family = quasibinomial(),
  weights = w_final
)
co_pp <- summary(fit_pp)$coefficients["treatment_baselineTRUE", , drop = FALSE]
cat("\n=== swereg-TTE PP estimate ===\n")
print(co_pp)

# -----------------------------------------------------------------
# Compare against Phase 1 TrialEmulation ITT (already run)
# Note: TE PP would require constructing a `cense` column from
# treatment-switching events; skipped here for scope. The ITT
# comparison is informative even though it targets a different
# estimand than swereg-TTE PP.
# -----------------------------------------------------------------
phase1 <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/01_te_result.rds")
te_trt <- phase1$robust$summary[
  phase1$robust$summary$names == "assigned_treatment", ]
cat("\n=== TrialEmulation ITT (Phase 1, sequential MSM) ===\n")
print(te_trt)

# -----------------------------------------------------------------
# Side-by-side
# -----------------------------------------------------------------
cat("\n========================================================\n")
cat("PER-PROTOCOL TREATMENT EFFECT (log-odds, smaller=less risk)\n")
cat("========================================================\n")
cat(sprintf("TrialEmulation ITT (sequential MSM, baseline-only confounding):\n"))
cat(sprintf("  estimate = %.3f  se = %.3f  95%%CI = (%.3f, %.3f)\n",
            te_trt$estimate, te_trt$robust_se, te_trt$`2.5%`, te_trt$`97.5%`))
cat(sprintf("\nswereg-TTE PP (single-trial baseline IPW + s6 IPCW + weighted glm):\n"))
cat(sprintf("  estimate = %.3f  se = %.3f  95%%CI = (%.3f, %.3f)\n",
            co_pp[, "Estimate"], co_pp[, "Std. Error"],
            co_pp[, "Estimate"] - 1.96 * co_pp[, "Std. Error"],
            co_pp[, "Estimate"] + 1.96 * co_pp[, "Std. Error"]))
cat("\nRemaining methodological difference:\n")
cat("  - TE replicates each person across multiple trial starts (sequential).\n")
cat("  - swereg-TTE here uses one trial per person (first eligible period).\n")
cat("  swereg-TTE's TTEPlan does support multi-trial expansion via trial_id;\n")
cat("  not exercised here because it requires a RegistryStudy + spec setup.\n")
