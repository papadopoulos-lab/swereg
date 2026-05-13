# Phase 21: replace swereg's marginal IPCW stabilization with a
# baseline-conditional one (matching paper Step (b) formula).
#
# Numerator: P(uncensored at j | s(tstop), treatment_baseline, baseline_L0)
#   --- conditions on baseline covariates, NOT time-varying ones
# Denominator: P(uncensored at j | s(tstop), treatment_baseline, baseline_L0,
#                                   time_varying_L_j)
#   --- conditions on time-varying covariates too (the current denominator)
#
# Build the swereg pipeline up to s5_prepare_outcome to get censor_this_period,
# then compute custom IPCW with the new stabilization, then fit svyglm.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(survey)
library(mgcv)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

# Moderate-persistence DGP (where bias was worst: +0.150)
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

# True PP
sim_intervention <- function(seed, force_A) {
  set.seed(seed)
  L0_po <- rnorm(N, 0, 1)
  out <- vector("list", T)
  for (t in 0:(T - 1)) {
    logit_Y <- -3.5 + TRUE_LOR*force_A + 0.4*L0_po
    out[[t + 1L]] <- data.table(Y = rbinom(N, 1, plogis(logit_Y)), A = force_A)
  }
  rbindlist(out)
}
po_t <- sim_intervention(999, 1L); po_u <- sim_intervention(999, 0L)
TRUE_PP <- coef(glm(Y ~ A, family = binomial, data = rbind(po_t, po_u)))["A"]
cat("True PP:", round(TRUE_PP, 4), "\n")

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
               treatment_baseline, time_treatment, event, baseline_L0, L0)]

design <- TTEDesign$new(
  id_var = "enrollment_person_trial_id",
  treatment_var = "treatment_baseline",
  time_treatment_var = "time_treatment",
  outcome_vars = "event",
  confounder_vars = c("baseline_L0"),
  follow_up_time = max(long$tstop)
)

# Run pipeline through s5 (sets up censor_this_period), then we'll
# bypass s6 and compute IPCW with the new stabilization manually.
trial <- TTEEnrollment$new(long, design)
# Skip s2_ipw / s3 since we want to test stabilization, not baseline IPW.
trial$data[, ipw := 1.0]
trial$data[, ipw_trunc := 1.0]
trial$weight_cols <- "ipw"
trial$steps_completed <- c("ipw", "truncate")

# Now run s4 to get censor_this_period AND the default IPCW.
# We'll keep the default IPCW for comparison.
trial$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
ad_default <- copy(trial$data)
ad_default[, w_default := ipcw_pp]
ad_default <- ad_default[!is.na(w_default) & w_default > 0]
des_d <- svydesign(ids = ~enrollment_person_trial_id, weights = ~w_default, data = ad_default)
fit_d <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des_d, family = quasibinomial())
co_d <- summary(fit_d)$coefficients["treatment_baselineTRUE", , drop = FALSE]

# ---- Custom IPCW with baseline-conditional stabilization ----
working <- copy(trial$data)
working <- working[!is.na(get("treatment_baseline"))]

# Denominator: same as swereg uses. Fit per treatment arm.
# Note: trial$data still has censor_this_period from s5.
# We refit the denominator here so we can pair it with a matched numerator model.
fit_p_uncensored <- function(data, tv_confounders, baseline_confounders) {
  data <- copy(data)
  # Numerator model: depends on s(tstop) + baseline confounders only
  num_form <- as.formula(paste0("censor_this_period ~ s(tstop) + ",
                                paste(baseline_confounders, collapse = " + ")))
  # Denominator model: s(tstop) + baseline + time-varying confounders
  denom_form <- as.formula(paste0("censor_this_period ~ s(tstop) + ",
                                  paste(c(baseline_confounders, tv_confounders),
                                        collapse = " + ")))

  predict_p_uncens <- function(formula, d) {
    n_censor <- sum(d$censor_this_period, na.rm = TRUE)
    if (n_censor == 0 || n_censor == nrow(d)) {
      d[, p_uncens := 1 - mean(censor_this_period, na.rm = TRUE)]
      return(d$p_uncens)
    }
    fit <- mgcv::bam(formula, data = d, family = binomial, discrete = TRUE)
    1 - predict(fit, newdata = d, type = "response")
  }

  data[, p_num_uncens := NA_real_]
  data[, p_denom_uncens := NA_real_]

  for (tx in c(TRUE, FALSE)) {
    mask <- data$treatment_baseline == tx
    if (sum(mask) > 0) {
      data[mask, p_num_uncens   := predict_p_uncens(num_form,   .SD), .SDcols = c("censor_this_period", "tstop", baseline_confounders, tv_confounders)]
      data[mask, p_denom_uncens := predict_p_uncens(denom_form, .SD), .SDcols = c("censor_this_period", "tstop", baseline_confounders, tv_confounders)]
    }
  }
  data
}

# Use L0 as the time-varying confounder proxy (current value) — even though
# in the easy scenario L0 is constant, this exercises the formula.
# In our easy scenario there's no W_t, so we'll use treatment status history
# as the time-varying piece. To keep this clean, we'll let the formula match
# what swereg uses: only baseline_L0 in confounder_vars. To make a meaningful
# comparison, let's use baseline_L0 in both numerator and denominator
# (so numerator and denominator only differ in whether they include
# baseline_L0 — and the denominator does, the numerator doesn't either).
# WAIT: that means there's nothing time-varying to differ in. Without any
# time-varying confounder in the data, stabilization choice should be moot.
#
# Reality check: in our DGP there's no W_t and L0 is baseline. So there's
# NOTHING time-varying except history-of-treatment-itself. That means
# numerator-with-baseline-only and denominator-with-baseline+time-varying
# would be the same model in this DGP. The stabilization difference
# doesn't bite here.

# Better experiment: include time_treatment HISTORY as the time-varying
# confounder. (Adherent at j-1 => more likely adherent at j, regardless
# of baseline.)
working[, prev_time_treatment := shift(time_treatment, 1L),
        by = .(enrollment_person_trial_id)]
working[is.na(prev_time_treatment), prev_time_treatment := time_treatment]

# Numerator: s(tstop) + baseline_L0
# Denominator: s(tstop) + baseline_L0 + prev_time_treatment
custom_ipcw <- function(d, treatment_value) {
  sub <- d[treatment_baseline == treatment_value]
  if (nrow(sub) < 10 || sum(sub$censor_this_period, na.rm=TRUE) == 0) {
    sub[, ipcw_new := 1.0]
    return(sub)
  }
  # Denominator (with time-varying)
  fit_d <- mgcv::bam(
    censor_this_period ~ s(tstop) + baseline_L0 + prev_time_treatment,
    data = sub, family = binomial, discrete = TRUE
  )
  sub[, p_d_uncens := 1 - predict(fit_d, newdata = sub, type = "response")]
  # Numerator (no time-varying)
  fit_n <- mgcv::bam(
    censor_this_period ~ s(tstop) + baseline_L0,
    data = sub, family = binomial, discrete = TRUE
  )
  sub[, p_n_uncens := 1 - predict(fit_n, newdata = sub, type = "response")]
  setorder(sub, enrollment_person_trial_id, tstop)
  sub[, cum_d := cumprod(p_d_uncens), by = enrollment_person_trial_id]
  sub[, cum_n := cumprod(p_n_uncens), by = enrollment_person_trial_id]
  sub[, ipcw_new := cum_n / cum_d]
  sub
}

ad_new <- rbind(custom_ipcw(working, TRUE), custom_ipcw(working, FALSE))
ad_new <- ad_new[!is.na(ipcw_new) & ipcw_new > 0]

des_n <- svydesign(ids = ~enrollment_person_trial_id, weights = ~ipcw_new, data = ad_new)
fit_n <- svyglm(event ~ treatment_baseline + tstop + I(tstop^2) + baseline_L0,
                design = des_n, family = quasibinomial())
co_n <- summary(fit_n)$coefficients["treatment_baselineTRUE", , drop = FALSE]

cat("\n========================================================\n")
cat("MODERATE PERSISTENCE N=50k, True PP =", round(TRUE_PP, 4), "\n")
cat("========================================================\n")
cat(sprintf("%-50s %8s  %6s  %s\n", "Method", "est", "se", "bias"))
cat(sprintf("%-50s %8.4f  %6.4f  %.4f\n",
            "swereg default IPCW (marginal num)",
            co_d[, "Estimate"], co_d[, "Std. Error"],
            co_d[, "Estimate"] - TRUE_PP))
cat(sprintf("%-50s %8.4f  %6.4f  %.4f\n",
            "swereg with baseline-conditional num",
            co_n[, "Estimate"], co_n[, "Std. Error"],
            co_n[, "Estimate"] - TRUE_PP))
cat("\nFor reference, TE PP at this DGP was: -0.534 (bias -0.040)\n")
cat("weight range default:", round(range(ad_default$w_default), 3), "\n")
cat("weight range new:    ", round(range(ad_new$ipcw_new), 3), "\n")
