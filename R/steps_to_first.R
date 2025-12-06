#' Calculate steps until first TRUE in a forward window
#'
#' @param x Logical vector
#' @param window Forward window size (default 103)
#' @return Integer vector of steps until first TRUE, NA if none in window
#' @export
steps_to_first <- function(x, window = 103L) {
  slider::slide_int(
    x,
    .f = ~ {
      pos <- which(.x)[1]
      if (is.na(pos)) NA_integer_ else pos - 1L
    },
    .before = 0L,
    .after = window,
    .complete = FALSE
  ) + 1L
}
