# `.tte_ipcw_fit_one()` refits a censoring model with `discrete = FALSE` when
# the `discrete = TRUE` prediction fails.
#
# mgcv 1.9.4 `predict.bam()` on a `discrete = TRUE` fit stops with
# "object '<var>' not found" when `s()` sits beside a parametric term that
# transforms a variable. `.tte_time_term()` builds exactly that mix: `s()` for
# a time term with 10 or more distinct values, and `factor()` or
# `splines::ns()` for one with fewer. The fixture never names a variable `t`,
# which would find `base::t()`.

skip_if_not_installed("mgcv")

.ibr_data <- function() {
  withr::local_seed(6)
  n <- 3000L
  d <- data.table::data.table(
    x = stats::rnorm(n),
    g = sample(1:3, n, replace = TRUE),
    tt = sample(0:9, n, replace = TRUE),
    person_weeks = 4
  )
  d[, cens := stats::rbinom(.N, 1L, stats::plogis(-2 + 0.3 * x + 0.2 * g))]
  return(d)
}

# The `discrete = FALSE` fit of the same formula, as the reference.
.ibr_reference_q <- function(d, rhs) {
  f <- stats::as.formula(
    paste0("cens ~ ", rhs, " + offset(log(person_weeks))")
  )
  fit <- mgcv::bam(
    f,
    data = d,
    family = stats::binomial(link = "cloglog"),
    discrete = FALSE
  )
  return(1 - as.numeric(stats::predict(fit, newdata = d, type = "response")))
}

.ibr_fit <- function(d, terms) {
  return(.tte_ipcw_fit_one(
    d,
    "cens",
    terms,
    "denominator",
    "loss",
    "the pooled cohort",
    use_gam = TRUE
  ))
}

test_that("s(x) + factor(g) predicts through a refit without discrete", {
  d <- .ibr_data()
  res <- NULL
  expect_no_error(res <- .ibr_fit(d, c("s(x)", "factor(g)")))
  expect_identical(
    res$fit,
    "bam, discrete = FALSE, after the discrete prediction failed"
  )
  expect_equal(
    res$q,
    .ibr_reference_q(d, "s(x) + factor(g)"),
    tolerance = 1e-6
  )
})

test_that("s(x) + splines::ns(tt, df = 3) predicts through a refit without discrete", {
  d <- .ibr_data()
  res <- NULL
  expect_no_error(res <- .ibr_fit(d, c("s(x)", "splines::ns(tt, df = 3)")))
  expect_identical(
    res$fit,
    "bam, discrete = FALSE, after the discrete prediction failed"
  )
  expect_equal(
    res$q,
    .ibr_reference_q(d, "s(x) + splines::ns(tt, df = 3)"),
    tolerance = 1e-6
  )
})

test_that("a discrete fit that predicts keeps the discrete fit", {
  d <- .ibr_data()
  res <- .ibr_fit(d, c("s(x)", "s(tt, k = 5)"))
  expect_identical(res$fit, "bam, discrete = TRUE")
  res_glm <- .tte_ipcw_fit_one(
    d,
    "cens",
    c("x", "factor(g)"),
    "denominator",
    "loss",
    "the pooled cohort",
    use_gam = FALSE
  )
  expect_identical(res_glm$fit, "glm")
})
