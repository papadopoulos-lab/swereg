# The knot warning of `splines::ns()` does not set `warn` on an IRR fit.
#
# `.tte_fit_irr()` takes `splines::ns(tstart, df = 3)` for 4 or more distinct
# values of `tstart`. `ns()` puts its interior knots at the tertiles. A short
# panel holds a third or more of its rows at `tstart = 0`, so the lower tertile
# ties with the boundary knot. `ns()` moves the knot just inside the boundary
# and warns "shoving 'interior' knots matching boundary knots to inside".
#
# The fit is sound. On this panel `tstart` takes 4 values, so the spline and
# `factor(tstart)` span the same columns: the treatment coefficient and its
# standard error are equal. Before 27.2.0 the warning still set `warn = TRUE`,
# and `$s3_analyze()` reported the ETT as a fit that raised a warning.

skip_if_not_installed("survey")

# 400 person-trials with 1 to 4 intervals of follow-up. About 57% of the rows
# sit at `tstart = 0`.
.nkw_panel <- function() {
  withr::local_seed(55)
  n <- 400L
  len <- sample(1:4, n, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1))
  exposed <- rep(c(TRUE, FALSE), length.out = n)
  d <- data.table::rbindlist(lapply(seq_len(n), function(i) {
    k <- len[i]
    return(data.table::data.table(
      id = sprintf("p%04d", i),
      tstart = 4L * (seq_len(k) - 1L),
      tstop = 4L * seq_len(k),
      exposed = exposed[i],
      person_weeks = 4L,
      w = 1
    ))
  }))
  d[, event := stats::rbinom(.N, 1L, ifelse(exposed, 0.06, 0.04))]
  return(d)
}

.nkw_design <- function() {
  return(TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    outcome_vars = "event",
    confounder_vars = character(0),
    follow_up_time = 16L
  ))
}

test_that("the short panel raises the knot warning inside the IRR fit", {
  d <- .nkw_panel()
  expect_identical(data.table::uniqueN(d$tstart), 4L)
  expect_gt(mean(d$tstart == 0L), 1 / 3)
  expect_warning(
    splines::ns(d$tstart, df = 3),
    "shoving 'interior' knots matching boundary knots to inside",
    fixed = TRUE
  )
  r <- .tte_fit_irr(d, "w", .nkw_design())
  expect_identical(
    deparse1(attr(r, "model_formula")),
    "event ~ exposed + splines::ns(tstart, df = 3) + offset(log(person_weeks))"
  )
})

test_that("the knot warning does not set warn, and the fit equals the factor(tstart) fit", {
  d <- .nkw_panel()
  r <- .tte_fit_irr(d, "w", .nkw_design())
  expect_false(r$warn)

  # The same model with `factor(tstart)`, which raises no warning.
  fr <- .tte_outcome_frame(d, .nkw_design(), "w")
  fit <- survey::svyglm(
    event ~ exposed + factor(tstart) + offset(log(person_weeks)),
    design = survey::svydesign(ids = ~id, weights = ~w, data = fr),
    family = stats::quasipoisson()
  )
  b <- stats::coef(summary(fit))["exposedTRUE", ]
  expect_equal(r$IRR, exp(b[["Estimate"]]), tolerance = 1e-8)
  expect_equal(
    r$IRR_upper,
    exp(b[["Estimate"]] + stats::qnorm(0.975) * b[["Std. Error"]]),
    tolerance = 1e-8
  )
})

test_that("any other warning inside the IRR fit still sets warn", {
  d <- .nkw_panel()
  real_svyglm <- survey::svyglm
  testthat::local_mocked_bindings(
    svyglm = function(...) {
      warning("glm.fit: algorithm did not converge", call. = FALSE)
      return(real_svyglm(...))
    },
    .package = "survey"
  )
  r <- .tte_fit_irr(d, "w", .nkw_design())
  expect_true(r$warn)
})
