# Effect modification (issue #6): irr_by_subgroup() recovers stratum-specific
# IRRs and effect_modification_test() recovers the ratio of stratum IRRs with a
# Wald p-value. DGP plants a binary subgroup Z that modifies the treatment
# effect: IRR ~= 2 in Z=0 and ~= 4 in Z=1 (ratio ~= 2). Treatment is
# randomised (no confounding) so recovery is clean.

.sim_em <- function(N = 10000, T_periods = 15, lor_main = log(2),
                    lor_int = log(2), z_main = 0.3, intercept = -4,
                    seed = 42) {
  set.seed(seed)
  Z <- stats::rbinom(N, 1, 0.5)
  A <- stats::rbinom(N, 1, 0.5)
  out <- vector("list", T_periods)
  for (t in seq_len(T_periods)) {
    haz <- stats::plogis(intercept + lor_main * A + z_main * Z + lor_int * A * Z)
    Y <- stats::rbinom(N, 1, haz)
    out[[t]] <- data.table::data.table(
      id = seq_len(N), tstart = t - 1L, tstop = t,
      treatment = as.logical(A), Z = Z, event = Y, person_weeks = 1L, w = 1
    )
  }
  data.table::rbindlist(out)
}

.em_design <- function() {
  TTEDesign$new(
    id_var = "id", person_id_var = "id", treatment_var = "treatment",
    outcome_vars = "event", confounder_vars = "Z", follow_up_time = 15L
  )
}

test_that("irr_by_subgroup recovers a planted differential effect", {
  skip_on_cran()
  skip_if_not_installed("survey")

  em <- .sim_em(N = 12000, lor_main = log(2), lor_int = log(2))
  trial <- TTEEnrollment$new(em, .em_design(), data_level = "trial")
  res <- trial$irr_by_subgroup(weight_col = "w", subgroup_var = "Z")

  expect_equal(res$level, c("all", "0", "1"))
  irr0 <- res[level == "0", IRR]
  irr1 <- res[level == "1", IRR]
  # Stratum IRRs near the planted 2 and 4 (logit->rate attenuation absorbed)
  expect_lt(abs(log(irr0) - log(2)), 0.25)
  expect_lt(abs(log(irr1) - log(4)), 0.25)
  # Ratio of stratum IRRs ~ 2, with a significant interaction
  expect_lt(abs(log(attr(res, "ratio_of_irrs")) - log(2)), 0.30)
  expect_lt(attr(res, "em_pvalue"), 0.05)
})

test_that("effect_modification_test ratio = IRR(other)/IRR(ref)", {
  skip_on_cran()
  skip_if_not_installed("survey")

  em <- .sim_em(N = 12000, lor_main = log(2), lor_int = log(2))
  trial <- TTEEnrollment$new(em, .em_design(), data_level = "trial")
  emt <- trial$effect_modification_test(weight_col = "w", subgroup_var = "Z")
  sub <- trial$irr_by_subgroup(weight_col = "w", subgroup_var = "Z")
  irr0 <- sub[level == "0", IRR]
  irr1 <- sub[level == "1", IRR]

  expect_equal(emt$n_levels, 2L)
  # exp(interaction coef) == IRR(Z=1) / IRR(Z=0)
  expect_lt(abs(emt$ratio_of_irrs - irr1 / irr0), 0.10 * irr1 / irr0)
  expect_lt(emt$p_value, 0.05)
})

test_that("null case: homogeneous effect -> non-significant interaction", {
  skip_on_cran()
  skip_if_not_installed("survey")

  em <- .sim_em(N = 12000, lor_main = log(2), lor_int = 0)
  trial <- TTEEnrollment$new(em, .em_design(), data_level = "trial")
  emt <- trial$effect_modification_test(weight_col = "w", subgroup_var = "Z")

  expect_gt(emt$p_value, 0.05)
  expect_lt(abs(log(emt$ratio_of_irrs)), 0.20)   # ratio ~ 1
})

test_that("'all' row equals irr() on the full data", {
  skip_on_cran()
  skip_if_not_installed("survey")

  em <- .sim_em(N = 6000)
  trial <- TTEEnrollment$new(em, .em_design(), data_level = "trial")
  sub <- trial$irr_by_subgroup(weight_col = "w", subgroup_var = "Z")
  ref <- trial$irr(weight_col = "w")
  expect_equal(sub[level == "all", IRR], ref$IRR, tolerance = 1e-6)
})

test_that("edge cases: missing column, single level, zero-event stratum", {
  skip_on_cran()
  skip_if_not_installed("survey")

  em <- .sim_em(N = 4000)
  trial <- TTEEnrollment$new(em, .em_design(), data_level = "trial")
  expect_error(trial$irr_by_subgroup("w", "nope"), "not found")

  em1 <- data.table::copy(em)
  em1[, Z := 0L]
  trial1 <- TTEEnrollment$new(em1, .em_design(), data_level = "trial")
  expect_error(trial1$irr_by_subgroup("w", "Z"), ">= 2")

  em2 <- data.table::copy(em)
  em2[Z == 1L, event := 0L]   # stratum Z=1 has no events
  trial2 <- TTEEnrollment$new(em2, .em_design(), data_level = "trial")
  res2 <- suppressWarnings(trial2$irr_by_subgroup("w", "Z"))
  expect_true(is.na(res2[level == "1", IRR]))
  expect_false(is.na(res2[level == "0", IRR]))
})
