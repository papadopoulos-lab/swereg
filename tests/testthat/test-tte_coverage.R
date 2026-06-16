# Monte Carlo COVERAGE study for the ITT standard errors: over M replicate
# draws, what fraction of 95% CIs cover the population truth? This validates
# the SE is *calibrated*, not merely that swereg and TrialEmulation agree on it.
#
# HEAVY and INTENTIONALLY NOT in CI: it refits swereg M times per scenario.
# It is gated three ways:
#   - skip_on_cran()
#   - skip_on_ci()                 -> never runs on GitHub Actions
#   - SWEREG_RUN_COVERAGE=true      -> opt-in even locally
# Run it deliberately with:
#   SWEREG_RUN_COVERAGE=true Rscript -e 'devtools::test(filter="tte_coverage")'
#
# Observed (M=200, N=3000): s1 ~0.96 (clean -> well calibrated),
# s2 ~0.93 (mild undercoverage, typical for IPW), s3 ~0.90 (informative-loss
# bias eats into coverage -- and worsens with N, since the bias is fixed).

test_that("ITT 95% CIs are calibrated where the estimand is valid, and under-cover where it is biased", {
  skip_on_cran()
  testthat::skip_on_ci()
  skip_if_not(
    identical(Sys.getenv("SWEREG_RUN_COVERAGE"), "true"),
    "set SWEREG_RUN_COVERAGE=true to run the (slow) Monte Carlo coverage study"
  )
  skip_if_not_installed("survey")

  M <- 200L
  cov_s1 <- scen_coverage("s1", "itt", M = M, N = 3000L)
  cov_s2 <- scen_coverage("s2", "itt", M = M, N = 3000L)
  cov_s3 <- scen_coverage("s3", "itt", M = M, N = 3000L)
  message(sprintf(
    "ITT coverage (M=%d, N=3000): s1=%.3f  s2=%.3f  s3=%.3f",
    M,
    cov_s1,
    cov_s2,
    cov_s3
  ))

  # s1 (no confounding, no loss): SE is well calibrated -> ~95% coverage.
  expect_gt(cov_s1, 0.90)
  # s2 (confounding + independent loss): mild undercoverage is acceptable for
  # an IPW estimator, but it must stay near nominal.
  expect_gt(cov_s2, 0.87)
  # s3 (informative loss): ITT carries no loss weight, so the bias degrades
  # coverage below nominal -- the SE being "right" cannot rescue a biased point
  # estimate. This is the demonstration, not a defect.
  expect_lt(cov_s3, 0.94)
  expect_lt(cov_s3, cov_s1)
})
