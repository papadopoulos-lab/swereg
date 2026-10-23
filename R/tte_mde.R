# The minimum detectable effect that the PP results and ITT results sheets
# report beside each incidence rate ratio.

#' Minimum detectable incidence rate ratio from the pooled weighted rate
#'
#' This is the naive Poisson minimum detectable effect (MDE). Its inputs are
#' the weighted events and the weighted person-years of both arms.
#'
#' Under the null, both arms have the pooled rate
#' `rate_pooled = (e_int + e_cmp) / (py_int + py_cmp)`. Each arm then expects
#' `E_a = rate_pooled * py_a` events. The standard error of the log ratio is
#' `se = sqrt(1 / E_int + 1 / E_cmp)`. Let
#' `k = qnorm(1 - (1 - conf_level) / 2) + qnorm(power)`. The smallest ratios
#' that a two-sided test detects with that power are `exp(-k * se)` and
#' `exp(k * se)`.
#'
#' The minimum-events rule uses the same expected events, per follow-up year.
#' The pooled rate holds the observed events of both arms, so the MDE depends on
#' the outcome data.
#'
#' The calculation treats the weighted counts as Poisson counts. It ignores
#' the repeated contributions of one person to many sequential trials. So
#' this MDE can differ from the detectable effect of the weighted estimator
#' with person-clustered standard errors.
#'
#' The MDE is not observed power. Observed power is a function of the p-value
#' and tells the reader nothing new (Hoenig and Heisey 2001).
#'
#' @param e_int Numeric, weighted events in the intervention arm.
#' @param py_int Numeric, weighted person-years in the intervention arm.
#' @param e_cmp Numeric, weighted events in the comparator arm.
#' @param py_cmp Numeric, weighted person-years in the comparator arm.
#' @param power Numeric(1), at least 0.5 and below 1. A power below
#'   `(1 - conf_level) / 2` makes `k` negative and swaps the two ratios.
#' @param conf_level Numeric(1) strictly between 0 and 1, the two-sided
#'   interval level.
#' @return A list of four numeric vectors as long as `e_int`:
#'   `expected_events` (`E_int`), `expected_events_cmp` (`E_cmp`),
#'   `mde_protective` (below 1) and `mde_harmful` (above 1). The two MDE
#'   values are `NA` where `E_int` or `E_cmp` is 0 or not finite.
#' @noRd
.tte_mde <- function(
  e_int,
  py_int,
  e_cmp,
  py_cmp,
  power = 0.8,
  conf_level = 0.95
) {
  .tte_mde_check_level(power, "power")
  .tte_mde_check_level(conf_level, "conf_level")
  py_int <- as.numeric(py_int)
  py_cmp <- as.numeric(py_cmp)
  rate <- (as.numeric(e_int) + as.numeric(e_cmp)) / (py_int + py_cmp)
  e1 <- rate * py_int
  e0 <- rate * py_cmp
  se <- sqrt(1 / e1 + 1 / e0)
  k <- stats::qnorm(1 - (1 - conf_level) / 2) + stats::qnorm(power)
  ok <- is.finite(e0) & is.finite(e1) & e0 > 0 & e1 > 0
  protective <- ifelse(ok, exp(-k * se), NA_real_)
  harmful <- ifelse(ok, exp(k * se), NA_real_)
  both <- is.finite(protective) & is.finite(harmful)
  # With very large event counts, one or both ratios can round to exactly 1.
  # A pair with both ratios within 1e-12 of 1 MUST only hold
  # protective <= 1 <= harmful.
  near_one <- abs(protective - 1) <= 1e-12 & abs(harmful - 1) <= 1e-12
  stopifnot(
    "MDE orientation: protective < 1 < harmful" = all(
      (protective[both] < 1 & harmful[both] > 1) |
        (near_one[both] & protective[both] <= 1 & harmful[both] >= 1)
    )
  )
  return(list(
    expected_events = e1,
    expected_events_cmp = e0,
    mde_protective = protective,
    mde_harmful = harmful
  ))
}


#' Stop unless an MDE level is a single number in its accepted range.
#'
#' `conf_level` MUST be strictly between 0 and 1. `power` MUST be finite, at
#' least 0.5 and below 1. A power below `(1 - conf_level) / 2` makes `k` in
#' `.tte_mde()` negative.
#'
#' `.tte_mde()` and `.plan_export_tables()` both call it. The export calls it
#' first, because a plan without stored rates never reaches `.tte_mde()`.
#'
#' @param v The value to check.
#' @param nm Character(1), the argument name for the message. `"power"`
#'   selects the power range.
#' @return `v`, invisibly.
#' @noRd
.tte_mde_check_level <- function(v, nm) {
  if (identical(nm, "power")) {
    if (
      !is.numeric(v) ||
        length(v) != 1L ||
        !is.finite(v) ||
        v < 0.5 ||
        v >= 1
    ) {
      stop(
        "`power` must be a single finite number with 0.5 <= power < 1.",
        call. = FALSE
      )
    }
    return(invisible(v))
  }
  if (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0 || v >= 1) {
    stop(
      "`",
      nm,
      "` must be a single number strictly between 0 and 1.",
      call. = FALSE
    )
  }
  return(invisible(v))
}
