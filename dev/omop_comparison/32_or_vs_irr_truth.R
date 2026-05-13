# Check whether the "swereg more null than TE" pattern is real bias,
# or just the OR-vs-IRR scale mismatch (odds ratios are more extreme than
# rate ratios for events that aren't extremely rare).
#
# Compute BOTH true marginal log-OR and true marginal log-IRR from the same
# potential outcomes. Compare each package's estimate to its OWN target.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(data.table)

N <- 1e6     # huge N so the truth is precise
T_periods <- 15

# Simulate potential outcomes under always-treated and never-treated
sim_intervention <- function(force_A) {
  set.seed(999)
  L0_po <- rnorm(N, 0, 1)
  res <- vector("list", T_periods)
  for (t in 0:(T_periods - 1)) {
    Y <- rbinom(N, 1, plogis(-3.5 + (-0.5)*force_A + 0.4*L0_po))
    res[[t + 1L]] <- data.table(Y = Y, A = force_A, id = seq_len(N), t = t)
  }
  rbindlist(res)
}
po_t <- sim_intervention(1L)
po_u <- sim_intervention(0L)

# 1) True marginal log-IRR: rate ratio
rate_t <- sum(po_t$Y) / nrow(po_t)
rate_u <- sum(po_u$Y) / nrow(po_u)
true_log_irr <- log(rate_t / rate_u)

# 2) True marginal log-OR per period (what a logistic MSM estimates)
pool <- rbind(po_t, po_u)
true_log_or_period <- coef(glm(Y ~ A, family = binomial, data = pool))["A"]

# 3) True marginal log-OR with time terms (what TE's MSM actually fits)
true_log_or_te <- coef(glm(Y ~ A + t + I(t^2), family = binomial,
                            data = pool))["A"]

# 4) Cumulative event status per person: any event over T periods?
po_t_any <- po_t[, .(any_Y = max(Y)), by = id]
po_u_any <- po_u[, .(any_Y = max(Y)), by = id]
p_any_t <- mean(po_t_any$any_Y)
p_any_u <- mean(po_u_any$any_Y)
true_log_or_cumulative <- log((p_any_t / (1 - p_any_t)) / (p_any_u / (1 - p_any_u)))

cat("True marginal effects on different scales:\n")
cat(sprintf("  log-IRR (rate ratio):             %+.4f\n", true_log_irr))
cat(sprintf("  log-OR per period (no time terms): %+.4f\n", true_log_or_period))
cat(sprintf("  log-OR per period (with time^2):   %+.4f\n", true_log_or_te))
cat(sprintf("  log-OR cumulative (any event):     %+.4f\n",
            true_log_or_cumulative))

cat("\nFrom the SE comparison (N=20k):\n")
cat("  swereg log-IRR estimate: -0.439\n")
cat("  TE     log-OR estimate:  -0.453\n")
cat("\nGap between TE and swereg (-0.453 vs -0.439): 0.014\n")
cat("Gap predicted by OR-vs-IRR scale difference:    ",
    round(true_log_or_period - true_log_irr, 4), "\n")
