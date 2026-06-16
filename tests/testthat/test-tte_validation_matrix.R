# swereg vs TrialEmulation VALIDATION MATRIX.
#
# For each of three escalating scenarios, every cell of the full triangle is
# checked -- known truth, swereg, TrialEmulation -- for BOTH per-protocol and
# intention-to-treat, on a common rate-ratio scale (TE's OR converted via
# Zhang & Yu), comparing point estimate AND CI width.
#
#   s1  no confounding, no loss          -> swereg == TE == truth
#   s2  confounding, independent loss    -> both estimands recover truth
#   s3  confounding, INFORMATIVE loss    -> PP recovers (IPCW models loss);
#                                           ITT is biased in BOTH packages, which
#                                           agree with each other (consistency is
#                                           not correctness -- this is why the
#                                           estimand distinction matters)
#
# Slow (6 TrialEmulation fits at N=20k); runs in CI (TrialEmulation in Suggests).
# Deterministic (fixed seeds), so tolerances are set ~2x the observed gaps.
# Scenario DGP / truth / fit helpers live in helper-tte_scenarios.R.

.scen_cells <- function(scenario, N = 20000L) {
  d <- scen_simulate(scenario, N = N)
  cells <- list()
  for (est in c("pp", "itt")) {
    tr <- scen_truth(scenario, est)
    cells[[est]] <- list(
      truth = as.numeric(tr),
      sw = scen_fit_swereg(d, est),
      te = scen_fit_te(d, est, attr(tr, "p0"))
    )
  }
  cells
}

test_that("matrix s1 (no confounding, no loss): swereg, TE, truth all agree", {
  skip_on_cran()
  skip_if_not_installed("TrialEmulation")
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  cells <- .scen_cells("s1")
  for (est in c("pp", "itt")) {
    x <- cells[[est]]
    expect_lt(abs(x$sw[["est"]] - x$truth), 0.06)          # swereg recovers truth
    expect_lt(abs(x$te[["est"]] - x$truth), 0.06)          # TE recovers truth
    # No confounder -> no OR non-collapsibility -> the two packages are identical
    expect_lt(abs(x$sw[["est"]] - x$te[["est"]]), 0.03)
    expect_lt(abs(x$sw[["width"]] - x$te[["width"]]), 0.04)
  }
})

test_that("matrix s2 (confounding, independent loss): both estimands recover truth", {
  skip_on_cran()
  skip_if_not_installed("TrialEmulation")
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  cells <- .scen_cells("s2")
  for (est in c("pp", "itt")) {
    x <- cells[[est]]
    expect_lt(abs(x$sw[["est"]] - x$truth), 0.08)          # swereg recovers truth
    expect_lt(abs(x$te[["est"]] - x$truth), 0.08)          # TE recovers truth
    expect_lt(abs(x$sw[["est"]] - x$te[["est"]]), 0.06)    # packages agree
    expect_lt(abs(x$sw[["width"]] - x$te[["width"]]), 0.05)
  }
})

test_that("matrix s3 (confounding, informative loss): PP recovers, ITT biased in both packages", {
  skip_on_cran()
  skip_if_not_installed("TrialEmulation")
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  cells <- .scen_cells("s3")

  # PP: per-protocol IPCW models the (informative) loss -> stays close to truth.
  pp <- cells$pp
  expect_lt(abs(pp$sw[["est"]] - pp$truth), 0.12)
  expect_lt(abs(pp$te[["est"]] - pp$truth), 0.12)

  # ITT: uses NO loss weight by design, so informative loss biases it away from
  # the truth -- in BOTH packages...
  itt <- cells$itt
  expect_gt(abs(itt$sw[["est"]] - itt$truth), 0.04)
  expect_gt(abs(itt$te[["est"]] - itt$truth), 0.04)
  # ...yet the two packages AGREE on the (wrong) answer: matching != correct.
  expect_lt(abs(itt$sw[["est"]] - itt$te[["est"]]), 0.06)
  expect_lt(abs(itt$sw[["width"]] - itt$te[["width"]]), 0.05)
})
