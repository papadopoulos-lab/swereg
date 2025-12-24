#' Check for any TRUE values in a prior window
#'
#' Slides backward through a logical vector to check if any TRUE values
#' exist in the preceding window. Useful for determining if an event
#' occurred in a prior time period, excluding the current row.
#'
#' @param x Logical vector
#' @param window_excluding_wk0 Number of prior weeks to check, excluding current week (default 104, ~2 years)
#' @return Logical vector indicating if any TRUE in prior window
#'
#' @family survival_analysis
#' @seealso [steps_to_first()] for counting steps until first event
#'
#' @export
any_events_prior_to <- function(x, window_excluding_wk0 = 104L) {
  slider::slide_lgl(
    x,
    .f = ~ any(.x),
    .before = window_excluding_wk0,
    .after = -1L,
    .complete = FALSE
  )
}
