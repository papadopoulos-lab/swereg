# Phase 12: is weight truncation the explanation for the SE gap?
#
# Test:
#   (A) swereg-TTE WITHOUT s3_truncate (untruncated)  -> SE should rise
#   (B) TE WITH analysis_weights = "p99" (1st/99th truncation) -> SE should drop
#
# If both moves bring SEs closer together, weight truncation is the lever.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

data("trial_example")
dt <- as.data.table(trial_example)
dt[, catvarA_f := factor(catvarA)]
dt[, catvarB_f := factor(catvarB)]

# Single-trial-per-person trial-long data
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

# ----------------------------------------------------------
# swereg-TTE PP without truncation
# ----------------------------------------------------------
run_swereg_pp <- function(do_truncate) {
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
  if (do_truncate) {
    tr$s3_truncate_weights(lower = 0.01, upper = 0.99)
    weight_col <- "ipw_trunc"
  } else {
    weight_col <- "ipw"
  }
  tr$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = max(trial_long$tstop),
    estimate_ipcw_pp_with_gam = TRUE,
    estimate_ipcw_pp_separately_by_treatment = TRUE
  )
  ad <- copy(tr$data)
  ad[, w_final := get(weight_col) * ipcw_pp]
  ad <- ad[!is.na(w_final) & w_final > 0]
  des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad)
  fit <- svyglm(
    event ~ treatment_baseline + tstop + I(tstop^2) +
            catvarA_f + catvarB_f + nvarA + nvarB + nvarC,
    design = des, family = quasibinomial()
  )
  co <- summary(fit)$coefficients["treatment_baselineTRUE", , drop = FALSE]
  list(est = co[, "Estimate"], se = co[, "Std. Error"],
       w_range = range(ad$w_final))
}

cat("=== swereg-TTE PP: with vs without s3_truncate ===\n")
sw_trunc   <- run_swereg_pp(do_truncate = TRUE)
cat(sprintf("With truncation:    est = %.3f  se = %.3f  w_range = (%.3f, %.3f)\n",
            sw_trunc$est, sw_trunc$se, sw_trunc$w_range[1], sw_trunc$w_range[2]))
sw_notrunc <- run_swereg_pp(do_truncate = FALSE)
cat(sprintf("Without truncation: est = %.3f  se = %.3f  w_range = (%.3f, %.3f)\n",
            sw_notrunc$est, sw_notrunc$se, sw_notrunc$w_range[1], sw_notrunc$w_range[2]))

# ----------------------------------------------------------
# TE PP single-trial: with vs without truncation
# ----------------------------------------------------------
dt_single <- copy(dt)
dt_single$catvarA <- factor(dt_single$catvarA)
dt_single$catvarB <- factor(dt_single$catvarB)
first_elig <- dt_single[eligible == 1, .SD[1], by = .(id)][, .(id, first_elig_period = period)]
dt_single <- merge(dt_single, first_elig, by = "id")
dt_single[period > first_elig_period, eligible := 0]
dt_single[, first_elig_period := NULL]

cat("\n=== TE PP single-trial: default vs p99 truncation ===\n")
res_te_default <- initiators(
  data = dt_single, id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", estimand_type = "PP", outcome = "outcome",
  model_var = "assigned_treatment",
  outcome_cov = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  switch_n_cov = ~ nvarA + nvarB,
  switch_d_cov = ~ catvarA + catvarB + nvarA + nvarB + nvarC,
  use_censor_weights = FALSE, quiet = TRUE
)
te_def <- res_te_default$robust$summary[
  res_te_default$robust$summary$names == "assigned_treatment", ]
cat(sprintf("Default (no truncation):  est = %.3f  se = %.3f\n",
            te_def$estimate, te_def$robust_se))

res_te_p99 <- initiators(
  data = dt_single, id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", estimand_type = "PP", outcome = "outcome",
  model_var = "assigned_treatment",
  outcome_cov = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  switch_n_cov = ~ nvarA + nvarB,
  switch_d_cov = ~ catvarA + catvarB + nvarA + nvarB + nvarC,
  use_censor_weights = FALSE,
  analysis_weights = "p99",
  quiet = TRUE
)
te_p99 <- res_te_p99$robust$summary[
  res_te_p99$robust$summary$names == "assigned_treatment", ]
cat(sprintf("p99 truncation:           est = %.3f  se = %.3f\n",
            te_p99$estimate, te_p99$robust_se))

# Side-by-side summary
cat("\n========================================================\n")
cat("EFFECT OF WEIGHT TRUNCATION ON BOTH PACKAGES\n")
cat("========================================================\n")
cat(sprintf("%-50s %8s  %6s\n", "Variant", "est", "se"))
cat(sprintf("%-50s %8.3f  %6.3f\n", "swereg-TTE PP, default (with s3_truncate)",
            sw_trunc$est, sw_trunc$se))
cat(sprintf("%-50s %8.3f  %6.3f\n", "swereg-TTE PP, NO truncation",
            sw_notrunc$est, sw_notrunc$se))
cat(sprintf("%-50s %8.3f  %6.3f\n", "TE PP single-trial, default (no truncation)",
            te_def$estimate, te_def$robust_se))
cat(sprintf("%-50s %8.3f  %6.3f\n", "TE PP single-trial, p99 truncation",
            te_p99$estimate, te_p99$robust_se))
