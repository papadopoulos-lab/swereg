# Monte Carlo COVERAGE study for the ITT standard errors: over M replicate
# draws, what fraction of 95% CIs cover the population truth? This validates
# the SE is *calibrated*, not merely that swereg and TrialEmulation agree on it.
#
# It refits swereg M times per scenario, so it is part of the full validation
# tier: it runs only when SWEREG_RUN_VALIDATION=true. The weekly workflow
# .github/workflows/validation.yml sets it. Run it locally with:
#   SWEREG_RUN_VALIDATION=true Rscript -e 'devtools::test(filter="tte_coverage")'
#
# Measured 2026-10-07 (M=200, N=3000) against the exact truth,
# scen_truth_irr_exact(): s1 0.960, s2 0.960, s3 0.945.
#
# s3 used to assert under-coverage (coverage < 0.94, and below s1). That claim
# held only against the older Monte Carlo crude-rate truth, scen_truth(), where
# the same fits give s1 0.965, s2 0.935 and s3 0.885. Against the exact truth
# the s3 design bias is -0.027, small against a replicate SD of 0.093 at
# N=3000, so s3 covers. The test now asserts the design bias itself.

test_that("ITT 95% CIs are calibrated where the estimand is valid, and s3 ITT carries its design bias", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("SWEREG_RUN_VALIDATION"), "true"),
    "set SWEREG_RUN_VALIDATION=true to run the full validation tier"
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
  # s3 (informative loss): ITT carries no loss weight, so its estimate is
  # biased by design. The exact limit of an ITT fit without loss weights is
  # -0.0268 from the exact truth. Measured 2026-10-07 on these 200 replicates:
  # mean bias -0.0275, MC SE 0.0066 (SD 0.0934 / sqrt(200)). The band is that
  # bias plus or minus 3.5 MC SE, rounded outward to 0.001, the method of
  # test-validation-full.R. It excludes 0.
  est_s3 <- attr(cov_s3, "est")
  bias_s3 <- mean(est_s3) - attr(cov_s3, "truth")
  lab <- sprintf(
    "s3 ITT mean log-IRR bias (%.4f, MC SE %.4f, %d fits)",
    bias_s3,
    stats::sd(est_s3) / sqrt(length(est_s3)),
    length(est_s3)
  )
  expect_gte(bias_s3, -0.051, label = lab)
  expect_lte(bias_s3, -0.004, label = lab)
})
