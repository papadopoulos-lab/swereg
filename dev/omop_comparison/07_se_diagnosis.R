# Phase 7: pin down what drives the SE gap between swereg-TTE PP and TE PP.
# Two candidate causes, varied independently:
#   (A) outcome model polynomial time terms (tstop + I(tstop^2))
#   (B) IPCW estimator: GAM (default in swereg-TTE) vs pooled logistic
#
# Fit 4 variants of swereg-TTE PP:
#   1. GAM IPCW + linear tstop                (= Phase 6 baseline)
#   2. GAM IPCW + tstop + I(tstop^2)
#   3. Logistic IPCW + linear tstop
#   4. Logistic IPCW + tstop + I(tstop^2)
# Compare each SE to TE PP's 0.432.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

data("trial_example")
dt <- as.data.table(trial_example)
dt[, catvarA_f := factor(catvarA)]
dt[, catvarB_f := factor(catvarB)]

# Build trial-level data (single-trial-per-person, same as Phase 4b)
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

# Two pipelines: one with GAM IPCW, one with logistic IPCW
run_pipeline <- function(use_gam) {
  d <- TTEDesign$new(
    id_var             = "enrollment_person_trial_id",
    treatment_var      = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars       = "event",
    confounder_vars    = c("catvarA_f", "catvarB_f", "nvarA", "nvarB", "nvarC"),
    follow_up_time     = max(trial_long$tstop)
  )
  tr <- TTEEnrollment$new(trial_long, d)
  tr$s2_ipw(stabilize = TRUE)
  tr$s3_truncate_weights(lower = 0.01, upper = 0.99)
  tr$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = max(trial_long$tstop),
    estimate_ipcw_pp_with_gam = use_gam,
    estimate_ipcw_pp_separately_by_treatment = TRUE
  )
  ad <- copy(tr$data)
  ad[, w_final := ipw_trunc * ipcw_pp]
  ad[!is.na(w_final) & w_final > 0]
}

ad_gam <- run_pipeline(use_gam = TRUE)
ad_log <- run_pipeline(use_gam = FALSE)

cat("=== Weight diagnostics ===\n")
cat(sprintf("GAM IPCW:      ipcw_pp range %.3f - %.3f  sd %.3f\n",
            min(ad_gam$ipcw_pp), max(ad_gam$ipcw_pp), sd(ad_gam$ipcw_pp)))
cat(sprintf("Logistic IPCW: ipcw_pp range %.3f - %.3f  sd %.3f\n",
            min(ad_log$ipcw_pp), max(ad_log$ipcw_pp), sd(ad_log$ipcw_pp)))
cat(sprintf("GAM w_final sd:      %.3f\n", sd(ad_gam$w_final)))
cat(sprintf("Logistic w_final sd: %.3f\n", sd(ad_log$w_final)))

fit_svy <- function(ad, formula_rhs) {
  des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad)
  fit <- svyglm(
    as.formula(paste("event ~", formula_rhs)),
    design = des, family = quasibinomial()
  )
  co <- summary(fit)$coefficients["treatment_baselineTRUE", , drop = FALSE]
  list(est = co[, "Estimate"], se = co[, "Std. Error"])
}

base_rhs <- "treatment_baseline + catvarA_f + catvarB_f + nvarA + nvarB + nvarC"
res <- list(
  gam_lin  = fit_svy(ad_gam, paste(base_rhs, "+ tstop")),
  gam_poly = fit_svy(ad_gam, paste(base_rhs, "+ tstop + I(tstop^2)")),
  log_lin  = fit_svy(ad_log, paste(base_rhs, "+ tstop")),
  log_poly = fit_svy(ad_log, paste(base_rhs, "+ tstop + I(tstop^2)"))
)

te_pp <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/05_te_pp_result.rds")
te_pp_trt <- te_pp$robust$summary[te_pp$robust$summary$names == "assigned_treatment", ]

cat("\n========================================================\n")
cat("SE DIAGNOSIS — swereg-TTE PP variants vs TE PP\n")
cat("========================================================\n")
cat(sprintf("%-50s %8s  %6s\n", "Variant", "est", "se"))
cat(sprintf("%-50s %8.3f  %6.3f\n", "TE PP (target)", te_pp_trt$estimate, te_pp_trt$robust_se))
cat(sprintf("%-50s %8.3f  %6.3f\n", "swereg GAM IPCW + linear tstop",      res$gam_lin$est,  res$gam_lin$se))
cat(sprintf("%-50s %8.3f  %6.3f\n", "swereg GAM IPCW + tstop + tstop^2",   res$gam_poly$est, res$gam_poly$se))
cat(sprintf("%-50s %8.3f  %6.3f\n", "swereg logistic IPCW + linear tstop", res$log_lin$est,  res$log_lin$se))
cat(sprintf("%-50s %8.3f  %6.3f\n", "swereg logistic IPCW + tstop + tstop^2", res$log_poly$est, res$log_poly$se))
