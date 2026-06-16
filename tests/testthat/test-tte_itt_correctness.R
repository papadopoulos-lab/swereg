# End-to-end correctness for the ITT estimand, mirroring the PP harness in
# test-tte_simulation_correctness.R. Uses a DGP with a KNOWN true ITT effect
# that is deliberately attenuated relative to the true PP effect (real
# deviation), so the tests prove ITT != PP rather than the two collapsing.
#
# Slow; skipped on CRAN. Run locally with:
#   devtools::test(filter = "tte_itt_correctness")

# ---------------------------------------------------------------------------
# Sanity on the synthetic data itself (no swereg) -- passes today.
# ---------------------------------------------------------------------------

test_that("DGP plants a real, well-separated ITT vs PP gap", {
  skip_on_cran()

  params <- attr(tte_simulate(N = 100, persist_coef = 8), "params")
  pp <- tte_true_pp_log_irr(params)
  itt <- tte_true_itt_log_irr(params)

  # Both effects are solidly non-null (protective)
  expect_lt(pp, -0.4)
  expect_lt(itt, -0.2)
  # ITT is attenuated toward null relative to PP (less negative)
  expect_gt(itt, pp)
  # ...by a meaningful margin, so the estimands are genuinely distinguishable
  expect_gt(itt - pp, 0.1)
})

# ---------------------------------------------------------------------------
# swereg ITT path. Exercises the API:
#   s4_prepare_for_analysis(outcome, follow_up, estimand = "itt")  # no IPCW
#   irr(weight_col = "ipw_trunc")                                  # guard relaxed
# ---------------------------------------------------------------------------

test_that("swereg ITT $irr() recovers the ITT truth, not the PP truth", {
  skip_on_cran()
  skip_if_not_installed("survey")

  dt <- tte_simulate(
    N = 10000,
    T_periods = 20,
    true_lor = -0.7,
    persist_coef = 8,
    L0_drives_adherence = 0.4,
    seed = 2026
  )
  params <- attr(dt, "params")
  true_itt <- tte_true_itt_log_irr(params)
  true_pp <- tte_true_pp_log_irr(params)

  long <- tte_build_long(dt)
  res <- tte_run_irr(long, estimand = "itt")
  est <- log(res$IRR)

  # Recovers the ITT truth within tolerance (tune in the green phase)
  expect_lt(abs(est - true_itt), 0.15)
  # ...and is the ITT estimand, NOT the PP estimand: clearly closer to ITT
  expect_lt(abs(est - true_itt), abs(est - true_pp))
  # 95% CI covers the ITT truth
  expect_gte(true_itt, log(res$IRR_lower))
  expect_lte(true_itt, log(res$IRR_upper))
})

# ---------------------------------------------------------------------------
# Regression: ITT must NOT require a treatment-switch variable. Guards the bug
# where s5's time_treatment_var stop() fired before the ITT bypass.
# ---------------------------------------------------------------------------

test_that("ITT works with a NULL time_treatment_var (no switch variable required)", {
  skip_on_cran()
  skip_if_not_installed("survey")

  dt <- tte_simulate(N = 4000, seed = 5)
  long <- tte_build_long(dt)
  long[, time_treatment := NULL] # study with no observed switching variable

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = NULL,
    outcome_vars = "event",
    confounder_vars = "baseline_L0",
    follow_up_time = max(long$tstop)
  )

  trial <- TTEEnrollment$new(long, design)
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  # Must not stop(): ITT ignores switching, so no switch variable is needed.
  trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = max(long$tstop),
    estimand = "itt"
  )
  res <- trial$irr(weight_col = "ipw_trunc")
  expect_true(is.finite(res$IRR))
})

# ---------------------------------------------------------------------------
# Structural: ITT keeps follow-up through switching; PP censors at it. Guards a
# regression that re-introduced deviation censoring under ITT.
# ---------------------------------------------------------------------------

test_that("ITT retains post-switch person-rows that PP drops", {
  skip_on_cran()
  skip_if_not_installed("mgcv")

  dt <- tte_simulate(N = 3000, persist_coef = 4, seed = 11) # lots of switching
  long <- tte_build_long(dt)
  prep <- function(estd) {
    tr <- TTEEnrollment$new(long, tte_make_design(long))
    tr$s2_ipw(stabilize = TRUE)
    tr$s3_truncate_weights(lower = 0.01, upper = 0.99)
    if (estd == "pp") {
      tr$s4_prepare_for_analysis(outcome = "event", follow_up = max(long$tstop))
    } else {
      tr$s4_prepare_for_analysis(
        outcome = "event",
        follow_up = max(long$tstop),
        estimand = "itt"
      )
    }
    tr$data
  }
  expect_gt(nrow(prep("itt")), nrow(prep("pp")))
})

# ---------------------------------------------------------------------------
# #4: ITT treats loss-to-follow-up as independent (no IPCW). Under genuinely
# independent loss the truth is unchanged, so ITT must still recover it.
# ---------------------------------------------------------------------------

test_that("ITT recovers truth under independent loss to follow-up", {
  skip_on_cran()
  skip_if_not_installed("survey")

  dt <- tte_simulate(N = 12000, true_lor = -0.7, persist_coef = 8, seed = 2026)
  true_itt <- as.numeric(tte_true_itt_log_irr(attr(dt, "params")))

  long <- tte_build_long(tte_apply_independent_loss(
    dt,
    hazard = 0.05,
    seed = 3
  ))
  res <- tte_run_irr(long, estimand = "itt")

  expect_lt(abs(log(res$IRR) - true_itt), 0.15)
  expect_gte(true_itt, log(res$IRR_lower))
  expect_lte(true_itt, log(res$IRR_upper))
})

# ---------------------------------------------------------------------------
# #9: not a single lucky seed -- recover truth across replicates and check the
# CI is roughly calibrated (covers truth in most of them).
# ---------------------------------------------------------------------------

test_that("ITT recovers truth across multiple seeds with rough CI calibration", {
  skip_on_cran()
  skip_if_not_installed("survey")

  true_itt <- as.numeric(tte_true_itt_log_irr(
    attr(tte_simulate(N = 100, true_lor = -0.7, persist_coef = 8), "params")
  ))

  seeds <- c(101, 202, 303, 404, 505)
  ests <- numeric(length(seeds))
  covers <- logical(length(seeds))
  for (i in seq_along(seeds)) {
    dt <- tte_simulate(
      N = 8000,
      true_lor = -0.7,
      persist_coef = 8,
      seed = seeds[i]
    )
    res <- tte_run_irr(tte_build_long(dt), estimand = "itt")
    ests[i] <- log(res$IRR)
    covers[i] <- true_itt >= log(res$IRR_lower) &&
      true_itt <= log(res$IRR_upper)
  }
  # No systematic bias: mean estimate close to truth
  expect_lt(abs(mean(ests) - true_itt), 0.08)
  # Rough calibration: CI covers truth in at least 4 of 5 replicates
  expect_gte(sum(covers), 4L)
})
