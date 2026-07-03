# Adversarial STRESS MATRIX for the swereg TTE estimators.
#
# Each cell pushes the generalized DGP (helper-tte_stress.R) to an edge the
# canned validation matrix never reaches, then checks the estimate against the
# matching first-event-IRR truth. All cells are deterministic (fixed seeds), so
# tolerances are set directly from the observed single-dataset numbers recorded
# beside each assertion (single-dataset Monte Carlo noise is ~0.03-0.05 on the
# log-IRR scale). PP numbers were re-measured AFTER the same-band event/deviation
# collision fix in R/r6_tteenrollment.R; ITT cells are unaffected by it.
#
# The file has two tiers:
#   ALWAYS-ON  (skip_on_cran + survey/mgcv): a fast subset, ~30s total.
#   OPT-IN     (SWEREG_RUN_STRESS=true): the full battery incl. TrialEmulation
#              cross-checks and the multi-seed replication. Run with:
#     SWEREG_RUN_STRESS=true Rscript -e 'devtools::test(filter="tte_stress")'

.stress_optin <- function() {
  identical(Sys.getenv("SWEREG_RUN_STRESS"), "true")
}

# ===========================================================================
# ALWAYS-ON SUBSET
# ===========================================================================

test_that("stress: rare outcome (~0.25%/period) is recovered for ITT and PP", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  d <- stress_sim(
    N = 40000L,
    T_periods = 20L,
    lor = -0.7,
    out_int = -6.0,
    loss = "none"
  )
  for (est in c("pp", "itt")) {
    tr <- stress_truth(est, 20L, lor = -0.7, out_int = -6.0)
    fit <- scen_fit_swereg(d, est)
    # rare events are the easy regime: observed post-fix bias pp +0.024,
    # itt +0.014. tol 0.08 (~3x observed + finite-sample slack).
    expect_lt(abs(fit[["est"]] - as.numeric(tr)), 0.08)
    # and the 95% CI covers the truth (both covered in the reference run).
    expect_true(as.numeric(tr) >= fit[["lo"]] && as.numeric(tr) <= fit[["hi"]])
  }
})

test_that("stress: null effect (lor=0) ITT is unbiased and covers the truth", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  d <- stress_sim(N = 20000L, T_periods = 20L, lor = 0, loss = "independent")
  tr <- stress_truth("itt", 20L, lor = 0)
  fit <- scen_fit_swereg(d, "itt")
  # no true effect + independent loss: observed bias +0.041 (within one
  # single-dataset MC SD ~0.03-0.05). tol 0.10.
  expect_lt(abs(fit[["est"]] - as.numeric(tr)), 0.10)
  expect_true(as.numeric(tr) >= fit[["lo"]] && as.numeric(tr) <= fit[["hi"]])
})

test_that("stress: PP fit is deterministic (same data fit twice is identical)", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  d <- stress_sim(N = 8000L, T_periods = 20L, lor = -0.7, loss = "independent")
  f1 <- scen_fit_swereg(d, "pp")
  f2 <- scen_fit_swereg(d, "pp")
  # the fit (incl. the GAM IPCW) has no stochastic step -> bit-identical.
  # observed delta = 0. tolerance 0.
  expect_identical(f1[["est"]], f2[["est"]])
  expect_identical(f1[["lo"]], f2[["lo"]])
  expect_identical(f1[["hi"]], f2[["hi"]])
})

test_that("stress: time-varying confounding -- updated confounder beats frozen; ITT sane", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  d <- tv_sim(N = 25000L, T_periods = 20L)

  tr_pp <- as.numeric(tv_truth("pp", 20L))
  b_updated <- abs(tv_fit(tv_build_long(d, "updated"), "pp")[["est"]] - tr_pp)
  b_frozen <- abs(tv_fit(tv_build_long(d, "frozen"), "pp")[["est"]] - tr_pp)
  # treatment-confounder feedback: the PP censoring model that sees the
  # TIME-UPDATED confounder is strictly less biased than the one frozen at
  # baseline. observed post-fix |bias|: updated 0.244 < frozen 0.287 (gap 0.044).
  expect_lt(b_updated, b_frozen)

  tr_itt <- as.numeric(tv_truth("itt", 20L))
  fit_itt <- tv_fit(tv_build_long(d, "updated"), "itt")
  # ITT needs no censoring model against this feedback -> near-unbiased.
  # observed bias -0.025. tol 0.10.
  expect_lt(abs(fit_itt[["est"]] - tr_itt), 0.10)
})

# ===========================================================================
# OPT-IN FULL MATRIX  (SWEREG_RUN_STRESS=true)
# ===========================================================================

test_that("stress [opt-in]: harmful-effect ITT bias is estimand, not defect", {
  skip_on_cran()
  skip_if_not(
    .stress_optin(),
    "set SWEREG_RUN_STRESS=true for the full stress matrix"
  )
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("TrialEmulation")

  tr <- stress_truth("itt", 20L, lor = +0.7)
  biases <- numeric(3)
  sw_vs_te <- numeric(3)
  for (s in 1:3) {
    d <- stress_sim(
      N = 20000L,
      T_periods = 20L,
      lor = +0.7,
      loss = "independent",
      seed = 3000L + s
    )
    fit <- scen_fit_swereg(d, "itt")
    te <- scen_fit_te(d, "itt", attr(tr, "p0"))
    biases[s] <- fit[["est"]] - as.numeric(tr)
    sw_vs_te[s] <- fit[["est"]] - te[["est"]]
  }
  # depletion of susceptibles: the DGP's marginal log-HR declines over
  # follow-up, so the person-time-weighted ITT IRR legitimately runs ABOVE the
  # cumulative-rate truth. observed mean bias 0.075 (seeds 1-3); band (0, 0.14).
  expect_gt(mean(biases), 0)
  expect_lt(mean(biases), 0.14)
  # and this is NOT a swereg defect: swereg and TrialEmulation agree within 0.05
  # (observed max |diff| 0.023) -- both target the same legitimate estimand.
  expect_lt(max(abs(sw_vs_te)), 0.05)
})

test_that("stress [opt-in]: near-positivity violation -- tighter truncation, more attenuation", {
  skip_on_cran()
  skip_if_not(
    .stress_optin(),
    "set SWEREG_RUN_STRESS=true for the full stress matrix"
  )
  skip_if_not_installed("survey")

  d <- stress_sim(
    N = 20000L,
    T_periods = 20L,
    lor = -0.7,
    a0_L0 = 2.5,
    loss = "none"
  )
  tr <- as.numeric(stress_truth("itt", 20L, lor = -0.7))
  fits <- lapply(
    list(c(0.005, 0.995), c(0.01, 0.99), c(0.05, 0.95)),
    function(bd) stress_fit_itt_trunc(d, bd[1], bd[2])
  )
  bias <- vapply(fits, function(f) f[["est"]] - tr, numeric(1))
  # a0_L0=2.5 pushes the propensity score to the boundary -> extreme raw
  # stabilised weights (observed max ~1325).
  expect_gt(fits[[1]][["wmax"]], 100)
  # weight truncation trades variance for bias TOWARD THE NULL; tighter bounds
  # discard more weight mass -> more attenuation. observed bias monotone
  # increasing 0.079 < 0.114 < 0.218. Assert monotonicity, not unbiasedness.
  expect_lt(bias[1], bias[2])
  expect_lt(bias[2], bias[3])
})

test_that("stress [opt-in]: heavy informative attrition -- PP recovers via IPCW, ITT biased by design", {
  skip_on_cran()
  skip_if_not(
    .stress_optin(),
    "set SWEREG_RUN_STRESS=true for the full stress matrix"
  )
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")

  # ~73% of person-periods lost, drop-out hazard driven by the confounder L0.
  d <- stress_sim(
    N = 30000L,
    T_periods = 20L,
    lor = -0.7,
    loss = "informative",
    loss_int = -1.3,
    loss_L0 = 0.9
  )
  # PP: per-protocol IPCW models the (informative) loss -> recovers truth.
  # observed bias +0.010. tol 0.10.
  tr_pp <- as.numeric(stress_truth("pp", 20L, lor = -0.7))
  expect_lt(abs(scen_fit_swereg(d, "pp")[["est"]] - tr_pp), 0.10)
  # ITT: carries NO loss weight by design, so the informative drop-out biases
  # it away from the truth. observed bias -0.126. assert |bias| > 0.05
  # (mirrors the s3 informative-loss cell in test-tte_validation_matrix.R).
  tr_itt <- as.numeric(stress_truth("itt", 20L, lor = -0.7))
  expect_gt(abs(scen_fit_swereg(d, "itt")[["est"]] - tr_itt), 0.05)
})
