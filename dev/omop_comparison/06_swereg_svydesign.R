# Phase 6: refit swereg-TTE PP final model with survey::svydesign for
# cluster-robust SE on enrollment_person_trial_id. This is what swereg-TTE
# does in production (per the source comment in s2_ipw):
#   "Robust standard errors for within-person correlation are handled
#    downstream by survey::svydesign(ids = ~person_id_var) in $irr() and $km()"
#
# Goal: see if SE aligns with TE PP once the variance estimation is comparable.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

data("trial_example")
dt <- as.data.table(trial_example)
dt[, catvarA_f := factor(catvarA)]
dt[, catvarB_f := factor(catvarB)]

# Build trial-level data same way as Phase 4b
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
  enrollment_person_trial_id,
  tstart, tstop,
  treatment_baseline, time_treatment,
  event,
  catvarA_f, catvarB_f,
  nvarA, nvarB, nvarC
)]

# Run swereg-TTE pipeline (s2 + s3 + s4)
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
trial$s4_prepare_for_analysis(
  outcome = "event",
  follow_up = max(trial_long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
analysis_data <- copy(trial$data)
analysis_data[, w_final := ipw_trunc * ipcw_pp]
analysis_data <- analysis_data[!is.na(w_final) & w_final > 0]

cat("=== analysis-ready data ===\n")
cat("rows:", nrow(analysis_data), "  persons:", uniqueN(analysis_data$enrollment_person_trial_id), "\n")

# ---- Fit 1: naive glm (model-based SE) — same as Phase 4b ----
fit_naive <- glm(
  event ~ treatment_baseline + tstop + catvarA_f + catvarB_f + nvarA + nvarB + nvarC,
  data = analysis_data,
  family = quasibinomial(),
  weights = w_final
)
co_naive <- summary(fit_naive)$coefficients["treatment_baselineTRUE", , drop = FALSE]

# ---- Fit 2: svyglm with cluster-robust SE on person ----
des <- svydesign(
  ids = ~enrollment_person_trial_id,
  weights = ~w_final,
  data = analysis_data
)
fit_svy <- svyglm(
  event ~ treatment_baseline + tstop + catvarA_f + catvarB_f + nvarA + nvarB + nvarC,
  design = des,
  family = quasibinomial()
)
co_svy <- summary(fit_svy)$coefficients["treatment_baselineTRUE", , drop = FALSE]

# ---- Fit 3: TE PP estimate (from Phase 5) ----
te_pp <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/05_te_pp_result.rds")
te_pp_trt <- te_pp$robust$summary[
  te_pp$robust$summary$names == "assigned_treatment", ]

# ---- Side-by-side ----
fmt <- function(est, se) sprintf("%8.3f  %6.3f  (%.3f, %.3f)",
                                 est, se, est - 1.96*se, est + 1.96*se)
cat("\n========================================================\n")
cat("PP TREATMENT EFFECT — SE COMPARISON\n")
cat("========================================================\n")
cat(sprintf("%-55s %s\n", "Method", "est    se    95% CI"))
cat(sprintf("%-55s %s\n",
            "TE PP (sequential MSM, cluster-robust SE)",
            fmt(te_pp_trt$estimate, te_pp_trt$robust_se)))
cat(sprintf("%-55s %s\n",
            "swereg-TTE PP, naive glm SE (Phase 4b)",
            fmt(co_naive[, "Estimate"], co_naive[, "Std. Error"])))
cat(sprintf("%-55s %s\n",
            "swereg-TTE PP, svyglm cluster-robust SE",
            fmt(co_svy[, "Estimate"], co_svy[, "Std. Error"])))
