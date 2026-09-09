# `$s2_ipw()` records the propensity fit on `$ps_fit` and warns when the fit
# separates. Separation does not stop `stats::glm()`: it returns fitted
# probabilities at the boundary, and those give an inverse probability weight
# near 1e8. Both panels below are hand-built at trial level, so the fit is
# readable off the page.

# One baseline row per person-trial, `tstart == 0`, which is what `$s2_ipw()`
# fits on. `separating = TRUE` sets the confounder equal to the treatment, so
# the logistic fit reproduces the treatment column exactly.
.psd_trial <- function(n, separating) {
  set.seed(11L)
  exposed <- rep(c(TRUE, FALSE), length.out = n)
  x <- if (separating) as.numeric(exposed) else stats::rnorm(n)
  d <- data.table::data.table(
    enrollment_person_trial_id = sprintf("t%04d", seq_len(n)),
    id = sprintf("p%04d", seq_len(n)),
    tstart = 0L,
    tstop = 4L,
    exposed = exposed,
    event = 0L,
    x = x,
    .tte_entry__x = x
  )
  design <- TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    outcome_vars = "event",
    confounder_vars = "x",
    follow_up_time = 4L
  )
  return(TTEEnrollment$new(d, design))
}


test_that("s2_ipw warns and records the boundary rows of a separated fit", {
  trial <- .psd_trial(14L, separating = TRUE)
  expect_null(trial$ps_fit)

  expect_warning(trial$s2_ipw(), "n_boundary")

  expect_s3_class(trial$ps_fit, "data.table")
  expect_identical(nrow(trial$ps_fit), 1L)
  expect_identical(trial$ps_fit$n_fit, 14L)
  expect_identical(trial$ps_fit$rank, 2L)
  expect_gt(trial$ps_fit$n_boundary, 0L)
})

test_that("s2_ipw records a well-conditioned fit and does not warn", {
  trial <- .psd_trial(400L, separating = FALSE)

  expect_no_warning(trial$s2_ipw())

  expect_identical(trial$ps_fit$n_fit, 400L)
  expect_identical(trial$ps_fit$rank, 2L)
  expect_identical(trial$ps_fit$n_boundary, 0L)
  expect_true(trial$ps_fit$converged)
})

# `$ps_fit` must report the model that ran, not a plausible-looking constant.
# This test refits the same propensity model with `stats::glm()` and compares
# every stored number against it.
test_that("ps_fit reports the numbers of an equivalent glm", {
  trial <- .psd_trial(400L, separating = FALSE)
  baseline <- data.table::copy(trial$data[tstart == 0L])
  expect_identical(nrow(baseline), 400L)

  fit <- stats::glm(exposed ~ x, data = baseline, family = stats::binomial)

  expect_no_warning(trial$s2_ipw())

  expect_identical(trial$ps_fit$n_fit, as.integer(stats::nobs(fit)))
  expect_identical(trial$ps_fit$rank, length(stats::coef(fit)))
  expect_true(trial$ps_fit$converged)
  expect_identical(trial$ps_fit$n_boundary, 0L)

  # The fitted probabilities agree too, so the four numbers describe the model
  # the panel was weighted by.
  p <- unname(stats::predict(fit, baseline, type = "response"))
  expect_equal(sort(trial$data$ps), sort(p), tolerance = 1e-10)
  expect_true(all(p > 1e-8 & p < 1 - 1e-8))
})
