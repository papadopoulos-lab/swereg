#' Calculate steps until first TRUE in a forward window
#'
#' Slides forward through a logical vector to find the number of steps
#' until the first TRUE value. Useful for time-to-event calculations
#' in longitudinal registry data.
#'
#' @param x Logical vector
#' @param window_including_wk0 Total window size including current week (default 104, ~2 years)
#' @return Integer vector of steps until first TRUE, NA if none in window
#'
#' @family survival_analysis
#' @seealso [any_events_prior_to()] for checking if events occurred in prior window
#'
#' @export
steps_to_first <- function(x, window_including_wk0 = 104L) {
  slider::slide_int(
    x,
    .f = ~ {
      pos <- which(.x)[1]
      if (is.na(pos)) NA_integer_ else pos - 1L
    },
    .before = 0L,
    .after = window_including_wk0 - 1L,
    .complete = FALSE
  ) + 1L
}
