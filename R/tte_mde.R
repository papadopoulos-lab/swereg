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
#' person to many sequential trials. Both make the real variance larger, so
#' this MDE understates the true MDE.
#'
#' The MDE is a design property of the counts. It is not observed power, which
#' is a function of the p-value and tells the reader nothing new (Hoenig and
#' Heisey 2001).
#'
#' @param e0 Numeric, unweighted events in the comparator arm.
#' @param py0 Numeric, unweighted person-years in the comparator arm.
#' @param py1 Numeric, unweighted person-years in the intervention arm.
#' @param power Numeric(1) strictly between 0 and 1.
#' @param conf_level Numeric(1) strictly between 0 and 1, the two-sided
#'   interval level.
#' @return A list of three numeric vectors as long as `e0`:
#'   `expected_events` (`E1`), `mde_protective` (below 1) and `mde_harmful`
#'   (above 1). The two MDE values are `NA` where `e0` or `E1` is 0 or not
#'   finite.
#' @noRd
.tte_mde <- function(e0, py0, py1, power = 0.8, conf_level = 0.95) {
  for (nm in c("power", "conf_level")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0 || v >= 1) {
      stop(
        "`",
        nm,
        "` must be a single number strictly between 0 and 1.",
        call. = FALSE
      )
    }
  }
  e0 <- as.numeric(e0)
  e1 <- e0 / as.numeric(py0) * as.numeric(py1)
  se <- sqrt(1 / e1 + 1 / e0)
  k <- stats::qnorm(1 - (1 - conf_level) / 2) + stats::qnorm(power)
  ok <- is.finite(e0) & is.finite(e1) & e0 > 0 & e1 > 0
  return(list(
    expected_events = e1,
    mde_protective = ifelse(ok, exp(-k * se), NA_real_),
    mde_harmful = ifelse(ok, exp(k * se), NA_real_)
  ))
}
