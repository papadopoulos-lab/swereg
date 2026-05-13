# Phase 10: do they agree in ITT?
#
# If swereg-TTE ITT matches TE ITT, the source of disagreement
# in PP is the PP-specific machinery (switching weights vs IPCW).
# If they disagree even in ITT, the issue is fundamental — different
# data structure, different weighting, different model.
#
# ITT in swereg-TTE = baseline IPW only (s2_ipw), no PP censoring
# (skip s5_prepare_outcome / s6_ipcw_pp).

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

data("trial_example")
dt <- as.data.table(trial_example)
dt[, catvarA_f := factor(catvarA)]
dt[, catvarB_f := factor(catvarB)]

# Build single-trial-per-person long-format data
baseline_rows <- dt[eligible == 1, .SD[1], by = .(id)]
baseline_treatments <- baseline_rows[, .(id, baseline_treatment = as.logical(treatment), trial_start = period)]
followup <- merge(dt, baseline_treatments, by = "id")
followup <- followup[period >= trial_start]
followup[, t := period - trial_start]
setorder(followup, id, t)
followup[, tstart := t]
followup[, tstop  := t + 1L]
followup[, treatment_baseline := baseline_treatment]
followup[, time_treatment     := as.logical(treatment)]
setnames(followup, "id", "enrollment_person_trial_id")
followup[, event := as.integer(outcome)]
trial_long <- followup[, .(
  enrollment_person_trial_id, tstart, tstop,
  treatment_baseline, time_treatment, event,
  catvarA_f, catvarB_f, nvarA, nvarB, nvarC
)]

# --- swereg-TTE ITT: s2_ipw + s3_truncate ONLY (no PP censoring) ---
design <- TTEDesign$new(
  id_var             = "enrollment_person_trial_id",
  treatment_var      = "treatment_baseline",
  time_treatment_var = "time_treatment",
  outcome_vars       = "event",
  confounder_vars    = c("catvarA_f", "catvarB_f", "nvarA", "nvarB", "nvarC"),
  follow_up_time     = max(trial_long$tstop)
)
trial <- TTEEnrollment$new(trial_long, design)
trial$s2_ipw(stabilize = TRUE)
trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
ad_itt <- copy(trial$data)
cat("=== swereg-TTE ITT-equivalent data ===\n")
cat("rows:", nrow(ad_itt), "  unique trials:", uniqueN(ad_itt$enrollment_person_trial_id), "\n")
cat("events:", sum(ad_itt$event), "  ipw_trunc range:", round(range(ad_itt$ipw_trunc), 3), "\n")

# Fit weighted person-period logistic, cluster-robust on person
des <- svydesign(
  ids = ~enrollment_person_trial_id,
  weights = ~ipw_trunc,
  data = ad_itt
)
fit_itt <- svyglm(
  event ~ treatment_baseline + tstop + I(tstop^2) +
          catvarA_f + catvarB_f + nvarA + nvarB + nvarC,
  design = des, family = quasibinomial()
)
co_swereg_itt <- summary(fit_itt)$coefficients["treatment_baselineTRUE", , drop = FALSE]

# --- TE ITT single-trial mode ---
dt_single <- copy(dt)
dt_single$catvarA <- factor(dt_single$catvarA)
dt_single$catvarB <- factor(dt_single$catvarB)
first_elig <- dt_single[eligible == 1, .SD[1], by = .(id)][, .(id, first_elig_period = period)]
dt_single <- merge(dt_single, first_elig, by = "id")
dt_single[period > first_elig_period, eligible := 0]
dt_single[, first_elig_period := NULL]

cat("\n=== TE ITT, single-trial mode ===\n")
t0 <- Sys.time()
result_itt <- initiators(
  data         = dt_single,
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
te_itt_trt <- result_itt$robust$summary[
  result_itt$robust$summary$names == "assigned_treatment", ]

# --- Side-by-side ---
fmt <- function(est, se, lo, hi) sprintf("%8.3f  %6.3f  (%.3f, %.3f)", est, se, lo, hi)
cat("\n========================================================\n")
cat("ITT, BOTH PACKAGES, SINGLE-TRIAL, SAME DATA\n")
cat("========================================================\n")
cat(sprintf("%-50s %s\n", "Method", "est    se    95% CI"))
cat(sprintf("%-50s %s\n", "TE ITT single-trial",
            fmt(te_itt_trt$estimate, te_itt_trt$robust_se,
                te_itt_trt$`2.5%`, te_itt_trt$`97.5%`)))
cat(sprintf("%-50s %s\n", "swereg-TTE ITT (s2_ipw only, svyglm)",
            fmt(co_swereg_itt[, "Estimate"], co_swereg_itt[, "Std. Error"],
                co_swereg_itt[, "Estimate"] - 1.96 * co_swereg_itt[, "Std. Error"],
                co_swereg_itt[, "Estimate"] + 1.96 * co_swereg_itt[, "Std. Error"])))

cat("\nDiagnostic interpretation:\n")
cat("  If close: the disagreement in PP is from the PP machinery.\n")
cat("  If different: the disagreement is fundamental (data shape,\n")
cat("                weighting, model specification).\n")
