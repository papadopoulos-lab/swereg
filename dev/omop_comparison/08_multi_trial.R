# Phase 8: multi-trial expansion to match TE's sequential structure.
#
# Hypothesis: the SE gap (0.33 vs 0.43) is driven by single-trial vs
# sequential-trial design. Test by manually expanding trial_example
# into multi-trial format (one trial per person-eligible_period),
# running the swereg-TTE pipeline, and clustering SE on person.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

data("trial_example", package = "TrialEmulation")
dt <- as.data.table(trial_example)
dt[, catvarA_f := factor(catvarA)]
dt[, catvarB_f := factor(catvarB)]

# --- Multi-trial expansion ---
# Every (person, eligible_period) row is a new trial baseline.
trial_starts <- dt[eligible == 1, .(
  person_id = id,
  trial_start = period,
  baseline_treatment = as.logical(treatment),
  baseline_catvarA_f = catvarA_f,
  baseline_catvarB_f = catvarB_f,
  baseline_nvarA = nvarA,
  baseline_nvarB = nvarB,
  baseline_nvarC = nvarC
)]
trial_starts[, trial_id := .I]

cat("=== expansion ===\n")
cat("n persons:", uniqueN(trial_starts$person_id), "\n")
cat("n trials: ", nrow(trial_starts), "\n")
cat("avg trials per person:", round(nrow(trial_starts) / uniqueN(trial_starts$person_id), 1), "\n")

# For each trial, build follow-up rows: all periods >= trial_start for that person
# Cross-join trial_starts with original person-period data
setkey(dt, id)
followup <- trial_starts[, {
  pdata <- dt[id == person_id & period >= trial_start]
  data.table(
    period = pdata$period,
    time_treatment = as.logical(pdata$treatment),
    outcome        = as.integer(pdata$outcome),
    catvarA_f      = pdata$catvarA_f,  # could be time-varying but use first for simplicity
    catvarB_f      = pdata$catvarB_f,
    nvarA          = pdata$nvarA,
    nvarB          = pdata$nvarB,
    nvarC          = pdata$nvarC
  )
}, by = .(trial_id, person_id, trial_start, baseline_treatment,
          baseline_catvarA_f, baseline_catvarB_f,
          baseline_nvarA, baseline_nvarB, baseline_nvarC)]

followup[, t := period - trial_start]
setorder(followup, trial_id, t)
followup[, tstart := t]
followup[, tstop  := t + 1L]
followup[, event := outcome]

cat("n rows in expanded data:", format(nrow(followup), big.mark = ","), "\n")
cat("memory (MB):", round(as.numeric(object.size(followup))/1e6, 1), "\n")

# Use baseline-captured confounders as the static confounders, like TE does:
# TE's MSM uses the values at trial start (captured at eligible row).
trial_long <- followup[, .(
  enrollment_person_trial_id = trial_id,
  person_id,
  tstart, tstop,
  treatment_baseline = baseline_treatment,
  time_treatment,
  event,
  catvarA_f = baseline_catvarA_f,
  catvarB_f = baseline_catvarB_f,
  nvarA = baseline_nvarA,
  nvarB = baseline_nvarB,
  nvarC = baseline_nvarC
)]

# --- swereg-TTE pipeline ---
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
ad <- copy(trial$data)
ad[, w_final := ipw_trunc * ipcw_pp]
ad <- ad[!is.na(w_final) & w_final > 0]

# person_id may already be in ad (TTEEnrollment keeps extra columns).
# If so, use it; otherwise merge in from trial_long.
cat("\n--- columns in ad: ---\n")
print(intersect(c("person_id", "person_id.x", "person_id.y"), names(ad)))
cat("ad person_id NA count:", sum(is.na(ad$person_id)), "of", nrow(ad), "\n")
if (!"person_id" %in% names(ad) || all(is.na(ad$person_id))) {
  trial_to_person <- unique(trial_long[, .(enrollment_person_trial_id, person_id_lookup = person_id)])
  ad <- merge(ad, trial_to_person, by = "enrollment_person_trial_id")
  ad[, person_id := person_id_lookup]
  ad[, person_id_lookup := NULL]
}

cat("\n=== analysis-ready (multi-trial) ===\n")
cat("rows:", format(nrow(ad), big.mark = ","), "\n")
cat("trials:", uniqueN(ad$enrollment_person_trial_id), "\n")
cat("persons:", uniqueN(ad$person_id), "\n")
cat("events:", sum(ad$event), "\n")
cat("w_final range:", round(range(ad$w_final), 3), "\n")

# Cluster-robust SE on person (within-person correlation across trials)
des <- svydesign(ids = ~person_id, weights = ~w_final, data = ad)
fit <- svyglm(
  event ~ treatment_baseline + tstop + I(tstop^2) +
          catvarA_f + catvarB_f + nvarA + nvarB + nvarC,
  design = des, family = quasibinomial()
)
co <- summary(fit)$coefficients["treatment_baselineTRUE", , drop = FALSE]
cat("\n=== swereg-TTE multi-trial PP, cluster-robust SE on person ===\n")
cat(sprintf("est = %.3f  se = %.3f  95%%CI = (%.3f, %.3f)\n",
            co[, "Estimate"], co[, "Std. Error"],
            co[, "Estimate"] - 1.96 * co[, "Std. Error"],
            co[, "Estimate"] + 1.96 * co[, "Std. Error"]))

# Compare
te_pp <- readRDS("/home/raw996/papadopoulos/swereg/dev/omop_comparison/05_te_pp_result.rds")
te_pp_trt <- te_pp$robust$summary[te_pp$robust$summary$names == "assigned_treatment", ]
cat("\n=== TE PP (target) ===\n")
cat(sprintf("est = %.3f  se = %.3f  95%%CI = (%.3f, %.3f)\n",
            te_pp_trt$estimate, te_pp_trt$robust_se,
            te_pp_trt$`2.5%`, te_pp_trt$`97.5%`))

cat("\n=== SE convergence check ===\n")
cat(sprintf("TE PP SE:                                0.432\n"))
cat(sprintf("swereg single-trial PP (Phase 6):        0.333\n"))
cat(sprintf("swereg multi-trial PP (this script):     %.3f\n", co[, "Std. Error"]))
cat(sprintf("Gap to TE PP:                            %.3f\n", co[, "Std. Error"] - 0.432))
