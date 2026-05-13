# Test 1: IPW alone with baseline confounding, no deviation.
#
# DGP:
#   - L0 ~ N(0, 1) -- baseline confounder
#   - A_0 ~ Bernoulli(plogis(intercept + gamma * L0)) -- treatment confounded by L0
#   - A_t = A_0 for ALL t (no deviation, perfect adherence)
#   - Y_t ~ Bernoulli(plogis(intercept + beta * A_t + delta * L0))
#
# Expected:
#   - Naive: biased due to L0
#   - Oracle (Y ~ A + L0): recovers truth
#   - swereg with s2_ipw only: should recover truth
#
# If swereg's IPW works here, we've verified the baseline-confounding piece.
# Then in Test 2 we add deviation and test IPCW on top.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(data.table)
library(survey)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

L0 <- rnorm(N, 0, 1)
# Confounded baseline treatment: stronger L0 -> more likely treated
A_0 <- rbinom(N, 1, plogis(-0.3 + 0.6 * L0))
cat("Treated at baseline:", sum(A_0), sprintf("(%.1f%%)\n", 100*mean(A_0)))

# No deviation: A_t = A_0 throughout
period_list <- vector("list", T)
for (t in 0:(T - 1)) {
  At_val <- A_0  # NO deviation
  Yt_val <- rbinom(N, 1, plogis(-3.5 + TRUE_LOR * At_val + 0.4 * L0))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
}
dt <- rbindlist(period_list)
setorder(dt, id, period)
cat("rows:", nrow(dt), "  events:", sum(dt$Y_t),
    sprintf(" (%.2f%%)\n", 100*mean(dt$Y_t)))

# True PP from potential outcomes
sim_intervention <- function(seed, force_A) {
  set.seed(seed); L0_po <- rnorm(N, 0, 1)
  out <- vector("list", T)
  for (t in 0:(T - 1)) {
    out[[t+1L]] <- data.table(
      Y = rbinom(N, 1, plogis(-3.5 + TRUE_LOR*force_A + 0.4*L0_po)),
      A = force_A)
  }
  rbindlist(out)
}
po_t <- sim_intervention(999, 1L); po_u <- sim_intervention(999, 0L)
TRUE_PP <- coef(glm(Y ~ A, family = binomial, data = rbind(po_t, po_u)))["A"]
cat("True PP:", round(TRUE_PP, 4), "\n\n")

# Sanity-check estimators
fit_naive  <- glm(Y_t ~ A_t,       family = binomial, data = dt)
fit_oracle <- glm(Y_t ~ A_t + L0,  family = binomial, data = dt)

# Build long format for swereg
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

# Run s2_ipw + s3_truncate. Since there's no deviation, skip s4 (PP machinery).
trial$s2_ipw(stabilize = TRUE)
trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
ad <- copy(trial$data)
cat("ad rows:", nrow(ad), "\n")
cat("ipw range:", round(range(ad$ipw), 3), "  ipw_trunc range:", round(range(ad$ipw_trunc), 3), "\n")
cat("weight sd:", round(sd(ad$ipw_trunc), 3), "\n\n")

# Fit weighted outcome model. Include baseline_L0 as covariate or not?
# For pure IPW analysis, baseline_L0 should be EXCLUDED from outcome (weights
# handle it). For doubly-robust, can include it. Let's test both.

des <- svydesign(ids = ~enrollment_person_trial_id, weights = ~ipw_trunc, data = ad)

fit_ipw_no_L0 <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2),
                        design = des, family = quasibinomial())
co_a <- summary(fit_ipw_no_L0)$coefficients["treatment_baselineTRUE", , drop = FALSE]

fit_ipw_with_L0 <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                          design = des, family = quasibinomial())
co_b <- summary(fit_ipw_with_L0)$coefficients["treatment_baselineTRUE", , drop = FALSE]

cat("========================================================\n")
cat("TEST 1: IPW alone, no deviation, N =", N, "\n")
cat("True PP =", round(TRUE_PP, 4), "\n")
cat("========================================================\n")
fmt <- function(est, se, target) sprintf("est = %+.4f  se = %.4f  bias = %+.4f",
                                          est, se, est - target)
cat(sprintf("%-50s %s\n", "naive (no adj)",
            fmt(coef(fit_naive)["A_t"], NA, TRUE_PP)))
cat(sprintf("%-50s %s\n", "oracle (Y ~ A + L0)",
            fmt(coef(fit_oracle)["A_t"], NA, TRUE_PP)))
cat(sprintf("%-50s %s\n", "swereg s2_ipw, outcome ~ A + time (pure IPW)",
            fmt(co_a[, "Estimate"], co_a[, "Std. Error"], TRUE_PP)))
cat(sprintf("%-50s %s\n", "swereg s2_ipw + L0 in outcome (doubly robust)",
            fmt(co_b[, "Estimate"], co_b[, "Std. Error"], TRUE_PP)))
