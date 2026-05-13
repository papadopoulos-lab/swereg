# Compare exactly which rows swereg and TE include for one deviator.
# Hypothesis: swereg includes the deviation row (event=0) that TE excludes.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
devtools::load_all("/home/raw996/papadopoulos/swereg", quiet = TRUE)

set.seed(2026)
N <- 5000; T <- 30; TRUE_LOR <- -0.5

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

# Pick one deviator with several adherent periods + then a deviation
dt[, baseline_A := A_t[period == 0L][1L], by = id]
dt[, dev := A_t != baseline_A]
dt[, first_dev := if (any(dev)) min(period[dev]) else NA_integer_, by = id]
candidates <- dt[!is.na(first_dev) & first_dev >= 5 & first_dev <= 10, unique(id)]
pid <- candidates[1]
cat("Using id =", pid, ", baseline_A =", dt[id == pid][1, baseline_A],
    ", first_dev period =", dt[id == pid][1, first_dev], "\n\n")

cat("--- ORIGINAL person-period data for id =", pid, "---\n")
print(dt[id == pid, .(period, baseline_A, A_t, Y_t, dev)])

# swereg pipeline
sw <- copy(dt)
sw[, baseline_A := NULL]; sw[, dev := NULL]; sw[, first_dev := NULL]
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
trial$s4_prepare_for_analysis(
  outcome = "event", follow_up = max(long$tstop),
  estimate_ipcw_pp_with_gam = TRUE,
  estimate_ipcw_pp_separately_by_treatment = TRUE
)
sw_data <- copy(trial$data)
cat("\n--- swereg analysis-ready rows for id =", pid, "---\n")
print(sw_data[enrollment_person_trial_id == pid,
              .(tstop, time_treatment, event, censor_this_period, ipw_trunc, ipcw_pp)])

# TE pipeline
te_in <- copy(dt)
te_in[, baseline_A := NULL]; te_in[, dev := NULL]; te_in[, first_dev := NULL]
setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
te_in[, eligible := as.integer(period == 0)]
working_dir <- file.path(tempdir(), "te_inspect2")
unlink(working_dir, recursive = TRUE); dir.create(working_dir, showWarnings = FALSE)
prep <- data_preparation(
  data = te_in, id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", outcome = "outcome",
  estimand_type = "PP",
  outcome_cov = ~ L0,
  switch_n_cov = ~ L0, switch_d_cov = ~ L0,
  data_dir = working_dir, separate_files = FALSE, quiet = TRUE,
  use_censor_weights = FALSE
)
te_data <- as.data.table(prep$data)
cat("\n--- TE analysis-ready rows for id =", pid, "---\n")
print(te_data[id == pid, .(followup_time, treatment, outcome, weight)])

cat("\n--- SUMMARY ---\n")
cat("swereg rows for this id:", sw_data[enrollment_person_trial_id == pid, .N], "\n")
cat("TE rows for this id:    ", te_data[id == pid, .N], "\n")
cat("first_dev period:", dt[id == pid][1, first_dev], "\n")
