# Cross-package ITT/PP comparison on a COMMON (rate-ratio) scale.
#
# Two principles, both from vignette("tte-methods"):
#  1. TrialEmulation is a PEER validated against the known simulated truth, NOT
#     an oracle -- we require that IT recovers the planted effect, so that
#     swereg<->TE agreement reflects shared correctness, not shared error.
#  2. TE returns a logistic OR; swereg/truth are on the IRR scale. We convert
#     TE's OR to the IRR scale (tte_log_or_to_log_irr) before comparing, so the
#     only residual is conditional-vs-marginal (OR non-collapsibility), which is
#     ~0 for PP (collapsible after conversion) and a small documented gap for ITT.
#
# Slow (TE's data_preparation is ~1 min/fit); skipped on CRAN. Deterministic
# (fixed seed), so tolerances are set around verified values, not guessed.

.te_log_or <- function(dt, estimand) {
  te_in <- data.table::copy(dt)
  data.table::setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
  te_in[, eligible := as.integer(period == 0)]
  res <- TrialEmulation::initiators(
    data = te_in,
    id = "id",
    period = "period",
    eligible = "eligible",
    treatment = "treatment",
    estimand_type = estimand,
    outcome = "outcome",
    model_var = "assigned_treatment",
    outcome_cov = c("L0"),
    switch_n_cov = ~L0,
    switch_d_cov = ~L0,
    use_censor_weights = FALSE,
    quiet = TRUE
  )
  row <- res$robust$summary[res$robust$summary$names == "assigned_treatment", ]
  list(
    est = unname(row$estimate),
    lo = unname(row$`2.5%`),
    hi = unname(row$`97.5%`)
  )
}

# ---------------------------------------------------------------------------
# Peer validation -- passes today (no swereg-ITT needed).
# ---------------------------------------------------------------------------

test_that("TrialEmulation independently recovers both planted truths (peer, not oracle)", {
  skip_on_cran()
  skip_if_not_installed("TrialEmulation")

  dt <- tte_simulate(
    N = 8000,
    T_periods = 20,
    true_lor = -0.7,
    persist_coef = 8,
    seed = 2026
  )
  params <- attr(dt, "params")
  true_pp <- tte_true_pp_log_irr(params)
  true_itt <- tte_true_itt_log_irr(params)

  te_pp <- .te_log_or(dt, "PP")
  te_itt <- .te_log_or(dt, "ITT")

  # Convert TE's OR to the IRR scale before judging recovery
  pp_irr <- tte_log_or_to_log_irr(te_pp$est, attr(true_pp, "p0"))
  itt_irr <- tte_log_or_to_log_irr(te_itt$est, attr(true_itt, "p0"))

  # PP: collapsible -> conversion makes TE essentially exact
  expect_lt(abs(pp_irr - as.numeric(true_pp)), 0.05)
  # ITT: small residual is conditional(TE)-vs-marginal(truth), not error
  expect_lt(abs(itt_irr - as.numeric(true_itt)), 0.10)
})

# ---------------------------------------------------------------------------
# swereg-ITT vs TE-ITT on a common scale -- FAILS until Phase 1 (estimand="itt").
# Compares both POINT ESTIMATE and CI WIDTH, per the brief.
# ---------------------------------------------------------------------------

test_that("swereg ITT and TrialEmulation ITT agree on point estimate and CI width", {
  skip_on_cran()
  skip_if_not_installed("TrialEmulation")
  skip_if_not_installed("survey")

  dt <- tte_simulate(
    N = 8000,
    T_periods = 20,
    true_lor = -0.7,
    persist_coef = 8,
    seed = 2026
  )
  params <- attr(dt, "params")
  true_itt <- tte_true_itt_log_irr(params)
  p0 <- attr(true_itt, "p0")

  long <- tte_build_long(dt)
  sw <- tte_run_irr(long, estimand = "itt") # FAILS here until Phase 1
  sw_est <- log(sw$IRR)
  sw_width <- log(sw$IRR_upper) - log(sw$IRR_lower)

  te <- .te_log_or(dt, "ITT")
  te_est <- tte_log_or_to_log_irr(te$est, p0)
  te_width <- tte_log_or_to_log_irr(te$hi, p0) -
    tte_log_or_to_log_irr(te$lo, p0)

  # Point estimates agree on the common scale (allow conditional-vs-marginal gap)
  expect_lt(abs(sw_est - te_est), 0.12)
  # swereg (marginal) is the tighter anchor to the marginal truth
  expect_lt(abs(sw_est - as.numeric(true_itt)), 0.15)
  # CI WIDTHS agree within tolerance (the headline requirement)
  expect_lt(abs(sw_width - te_width), 0.10)
})
