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

  if (window_excluding_wk0 >= 99999L) {
    # Infinite window: use cumsum, shift by 1 to exclude current row
    # cumsum gives running total, shift moves it forward so row i sees sum of rows 1:(i-1)
    cum <- cumsum(x_int)
    result <- c(FALSE, cum[-n] > 0L)
  } else {
    # Finite window: use frollsum with align="right", then shift
    # frollsum at position i gives sum of rows (i-window+1):i
    # We want sum of rows (i-window):(i-1), so shift by 1
    roll <- data.table::frollsum(x_int, n = window_excluding_wk0, fill = 0L, align = "right")
    result <- c(FALSE, roll[-n] > 0L)
  }

  result
}
