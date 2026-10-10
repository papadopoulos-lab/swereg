# The minimum detectable effect that the PP results and ITT results sheets
# report beside each incidence rate ratio.

#' Naive Poisson minimum detectable incidence rate ratio
#'
#' This is the naive Poisson minimum detectable effect (MDE). It uses the
#' unweighted comparator events `e0`, the comparator person-years `py0` and the
#' intervention person-years `py1`.
#'
#' Under the null, the intervention arm has the comparator rate, so it expects
#' `E1 = e0 / py0 * py1` events. The standard error of the log ratio is
#' `se = sqrt(1 / E1 + 1 / e0)`. Let
#' `k = qnorm(1 - (1 - conf_level) / 2) + qnorm(power)`. The smallest ratios
#' that a two-sided test detects with that power are `exp(-k * se)` and
#' `exp(k * se)`.
#'
#' The calculation ignores the weights and the repeated contributions of one
#' person to many sequential trials. So this MDE can differ from the detectable
#' effect of the weighted estimator with person-clustered standard errors.
#'
#' The MDE is a design property of the counts. It is not observed power, which
#' is a function of the p-value and tells the reader nothing new (Hoenig and
#' Heisey 2001).
#'
#' @param e0 Numeric, unweighted events in the comparator arm.
#' @param py0 Numeric, unweighted person-years in the comparator arm.
#' @param py1 Numeric, unweighted person-years in the intervention arm.
#' @param power Numeric(1), at least 0.5 and below 1. A power below
#'   `(1 - conf_level) / 2` makes `k` negative and swaps the two ratios.
#' @param conf_level Numeric(1) strictly between 0 and 1, the two-sided
#'   interval level.
#' @return A list of three numeric vectors as long as `e0`:
#'   `expected_events` (`E1`), `mde_protective` (below 1) and `mde_harmful`
#'   (above 1). The two MDE values are `NA` where `e0` or `E1` is 0 or not
#'   finite.
#' @noRd
.tte_mde <- function(e0, py0, py1, power = 0.8, conf_level = 0.95) {
  .tte_mde_check_level(power, "power")
  .tte_mde_check_level(conf_level, "conf_level")
  e0 <- as.numeric(e0)
  e1 <- e0 / as.numeric(py0) * as.numeric(py1)
  se <- sqrt(1 / e1 + 1 / e0)
  k <- stats::qnorm(1 - (1 - conf_level) / 2) + stats::qnorm(power)
  ok <- is.finite(e0) & is.finite(e1) & e0 > 0 & e1 > 0
  protective <- ifelse(ok, exp(-k * se), NA_real_)
  harmful <- ifelse(ok, exp(k * se), NA_real_)
  both <- is.finite(protective) & is.finite(harmful)
  stopifnot(
    "MDE orientation: protective < 1 < harmful" = all(
      protective[both] < 1 & harmful[both] > 1
    )
  )
  return(list(
    expected_events = e1,
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
#' first, because a plan without unweighted counts never reaches `.tte_mde()`.
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
