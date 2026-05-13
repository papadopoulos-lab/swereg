# Phase 22: truncate the final IPCW weight at p99 (a la TE's analysis_weights).
# swereg's default IPCW has weights ranging from 0.024 to 122 — a few extreme
# observations dominate. p99 truncation should pull the estimate toward truth.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)
for (t in 0:(T - 1)) {
  logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -2.0 + 0.6 * L0 + 3.0 * prev_A
  At_val <- rbinom(N, 1, plogis(logit_A))
  logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0
  Yt_val <- rbinom(N, 1, plogis(logit_Y))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list)
setorder(dt, id, period)

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

# Build long format
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
trial$data[, ipw := 1.0]; trial$data[, ipw_trunc := 1.0]
trial$weight_cols <- "ipw"; trial$steps_completed <- c("ipw", "truncate")
trial$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad <- copy(trial$data)
ad <- ad[!is.na(ipcw_pp) & ipcw_pp > 0]

cat("True PP:", round(TRUE_PP, 4), "\n\n")
cat("ipcw_pp distribution:\n")
print(quantile(ad$ipcw_pp, c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)))

# Three truncation variants
fit_with_weights <- function(w, label) {
  ad2 <- copy(ad)
  ad2[, w_use := w]
  des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_use, data = ad2)
  fit <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des, family = quasibinomial())
  co <- summary(fit)$coefficients["treatment_baselineTRUE", , drop = FALSE]
  cat(sprintf("%-40s  est = %.4f  se = %.4f  bias = %+.4f  wmax = %.2f\n",
              label, co[, "Estimate"], co[, "Std. Error"],
              co[, "Estimate"] - TRUE_PP, max(ad2$w_use)))
}

cat("\n=== weight truncation variants ===\n")
fit_with_weights(ad$ipcw_pp, "no truncation")
fit_with_weights(pmin(ad$ipcw_pp, quantile(ad$ipcw_pp, 0.99)), "trunc at p99")
fit_with_weights(pmin(ad$ipcw_pp, quantile(ad$ipcw_pp, 0.95)), "trunc at p95")
fit_with_weights(pmin(pmax(ad$ipcw_pp, quantile(ad$ipcw_pp, 0.01)),
                       quantile(ad$ipcw_pp, 0.99)),
                  "trunc at p1/p99")
fit_with_weights(pmin(pmax(ad$ipcw_pp, 0.1), 10), "clip to [0.1, 10]")
fit_with_weights(pmin(pmax(ad$ipcw_pp, 0.2), 5),  "clip to [0.2, 5]")
fit_with_weights(rep(1, nrow(ad)),               "unweighted (sanity)")

cat("\n(for reference: TE PP at this DGP gives bias -0.039)\n")
