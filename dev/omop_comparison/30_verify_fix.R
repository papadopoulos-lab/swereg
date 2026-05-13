# Verify the s4 fix works without the manual workaround.
# Same Test 2 scenario; just call s4_prepare_for_analysis with default arg.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000; T <- 30; TRUE_LOR <- -0.5

L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)
for (t in 0:(T - 1)) {
  logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -3.0 + 0.4 * L0 + 4.0 * prev_A
  At_val <- rbinom(N, 1, plogis(logit_A))
  Yt_val <- rbinom(N, 1, plogis(-3.5 + TRUE_LOR * At_val + 0.4 * L0))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list); setorder(dt, id, period)

sim_intervention <- function(seed, force_A) {
  set.seed(seed); L0_po <- rnorm(N, 0, 1)
  out <- vector("list", T)
  for (t in 0:(T - 1)) {
    out[[t+1L]] <- data.table(Y = rbinom(N, 1, plogis(-3.5 + TRUE_LOR*force_A + 0.4*L0_po)),
                              A = force_A)
  }
  rbindlist(out)
}
po_t <- sim_intervention(999, 1L); po_u <- sim_intervention(999, 0L)
TRUE_PP <- coef(glm(Y ~ A, family = binomial, data = rbind(po_t, po_u)))["A"]
cat("True PP:", round(TRUE_PP, 4), "\n")

sw <- copy(dt)
sw[, baseline_treatment := A_t[period == 0L][1L], by = id]
sw[, baseline_L0 := L0]
sw[, tstart := period]; sw[, tstop := period + 1L]
sw[, time_treatment := as.logical(A_t)]
sw[, treatment_baseline := as.logical(baseline_treatment)]
setnames(sw, "id", "enrollment_person_trial_id")
setnames(sw, "Y_t", "event")
long <- sw[, .(enrollment_person_trial_id, tstart, tstop,
               treatment_baseline, time_treatment, event, baseline_L0)]
design <- TTEDesign$new(
  id_var = "enrollment_person_trial_id",
  treatment_var = "treatment_baseline",
  time_treatment_var = "time_treatment",
  outcome_vars = "event",
  confounder_vars = c("baseline_L0"),
  follow_up_time = max(long$tstop)
)
trial <- TTEEnrollment$new(long, design)
trial$s2_ipw(stabilize = TRUE)
trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
# default drop_censor_rows = TRUE now
trial$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad <- copy(trial$data)
ad[, w_final := ipw_trunc * ipcw_pp]
ad <- ad[!is.na(w_final) & w_final > 0]
cat("rows in ad (after fix):", nrow(ad), "\n")
cat("rows with censor_this_period=1 in ad:",
    sum(ad$censor_this_period == 1, na.rm=T), "(should be 0 except event=1 ties)\n")

des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_final, data = ad)
fit <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
              design = des, family = quasibinomial())
co <- summary(fit)$coefficients["treatment_baselineTRUE", , drop = FALSE]

cat(sprintf("\nswereg with fix:  est = %+.4f  bias = %+.4f\n",
            co[, "Estimate"], co[, "Estimate"] - TRUE_PP))
cat("Expected (from manual filter test):  est = -0.5175  bias = -0.0230\n")
