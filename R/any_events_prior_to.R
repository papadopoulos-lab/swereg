#' Check for any TRUE values in a prior window
#'
#' Checks if any TRUE values exist in the preceding window, excluding the
#' current row. Useful for determining if an event occurred in a prior time
#' period.
#'
#' @param x Logical vector
#' @param window_excluding_wk0 Number of prior weeks to check, excluding current
#'   week (default 104, ~2 years). Use a large number (e.g., 99999) for lifetime
#'   history.
#' @return Logical vector indicating if any TRUE in prior window
#'
#' @details
#' For infinite/large windows (>= 99999), uses O(n) cumsum approach.
#' For finite windows, uses O(n) data.table::frollsum.
#' Both exclude the current row (week 0).
#'
#' @family survival_analysis
#' @seealso [steps_to_first()] for counting steps until first event
#'
#' @export
any_events_prior_to <- function(x, window_excluding_wk0 = 104L) {
  n <- length(x)
  if (n == 0) return(logical(0))

  # Convert to integer for cumsum/frollsum
  x_int <- as.integer(x)

  # Both branches: build a running count of prior events, shift forward
  # by one row to exclude the current week, then test > 0. Using
  # data.table::shift() yields a single allocation; the previous
  # `c(FALSE, vec[-n] > 0L)` form allocated three copies of an n-vector
  # (slice, compare, prepend) per call. This function is called once per
  # person per exclusion criterion, so the saving multiplies.
  prior_counts <- if (window_excluding_wk0 >= 99999L) {
    cumsum(x_int)
  } else {
    data.table::frollsum(
      x_int, n = window_excluding_wk0, fill = 0L, align = "right"
    )
  }
  data.table::shift(prior_counts, n = 1L, fill = 0L) > 0L
}
