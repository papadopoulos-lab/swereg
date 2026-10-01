#' Check for any TRUE values in a prior calendar window
#'
#' Returns, for each row, whether an earlier row is TRUE inside the N ISO weeks
#' before the first week of that row. The window counts calendar weeks from
#' `isoyearweek`, not rows.
#'
#' @param x Logical vector, one element per row.
#' @param window_excluding_wk0 Number of ISO weeks to look back, excluding the
#'   current row (default 104). A value of 99999 or more, `Inf` included, means
#'   every earlier row.
#' @param isoyearweek Character vector of the same length as `x`. Each element
#'   is an ISO week (`"2008-01"`) or an ISO year (`"2004-**"`). The rows MUST be
#'   in strictly increasing calendar order and MUST NOT overlap. An annual row
#'   and a weekly row of the same year overlap.
#' @return Logical vector of the same length as `x`.
#'
#' @details
#' A weekly row covers its own week. An annual row covers every ISO week of its
#' year, week 53 included where the year has one. The window of an annual row
#' ends at the week before the first week of its year. An earlier row is
#' inside the window when any of its weeks is inside the window. The current
#' row is never inside its own window.
#'
#' Missing values follow [any()]. The result is TRUE if a row in the window is
#' TRUE. Otherwise it is NA if a row in the window is NA, and FALSE if not.
#'
#' The function stops when:
#' * `isoyearweek` is missing.
#' * `x` and `isoyearweek` differ in length.
#' * a week is not in `cstime::dates_by_isoyearweek`.
#' * the rows are out of calendar order.
#'
#' @family survival_analysis
#' @seealso [steps_to_first()] for counting steps until first event
#'
#' @examples
#' any_events_prior_to(
#'   c(TRUE, FALSE, FALSE),
#'   window_excluding_wk0 = 1,
#'   isoyearweek = c("2008-01", "2008-02", "2008-03")
#' )
#'
#' @export
any_events_prior_to <- function(x, window_excluding_wk0 = 104L, isoyearweek) {
  if (missing(isoyearweek)) {
    stop(
      "isoyearweek is required: the window counts ISO weeks, not rows",
      call. = FALSE
    )
  }
  if (length(x) != length(isoyearweek)) {
    stop(
      "x and isoyearweek MUST have the same length (",
      length(x),
      " and ",
      length(isoyearweek),
      ")",
      call. = FALSE
    )
  }
  spans <- .isoyearweek_spans(isoyearweek)
  .check_span_order(spans$first, spans$last)
  return(.any_prior_in_spans(
    x,
    window_excluding_wk0,
    spans$first,
    spans$last
  ))
}


# The span lookup. Key "YYYY-WW" spans its own week; key "YYYY-**" spans the
# first to the last week of ISO year YYYY. A week index is the row number of
# cstime::dates_by_isoyearweek, which is keyed by isoyear and isoyearweek, so
# week 1 is 1900-01. Built on first use and kept for the session.
.isoyearweek_span_cache <- new.env(parent = emptyenv())

.isoyearweek_span_lookup <- function() {
  if (is.null(.isoyearweek_span_cache$key)) {
    w <- cstime::dates_by_isoyearweek
    idx <- seq_len(nrow(w))
    year_first <- idx[!duplicated(w$isoyear)]
    year_last <- idx[!duplicated(w$isoyear, fromLast = TRUE)]
    .isoyearweek_span_cache$isoyear <- w$isoyear
    .isoyearweek_span_cache$first <- c(idx, year_first)
    .isoyearweek_span_cache$last <- c(idx, year_last)
    .isoyearweek_span_cache$key <- c(
      w$isoyearweek,
      paste0(w$isoyear[year_first], "-**")
    )
  }
  return(.isoyearweek_span_cache)
}


#' The first and last week index of each isoyearweek
#'
#' @param isoyearweek Character vector of ISO weeks and ISO years.
#' @return A list with integer vectors `first` and `last`.
#' @noRd
.isoyearweek_spans <- function(isoyearweek) {
  lk <- .isoyearweek_span_lookup()
  m <- data.table::chmatch(as.character(isoyearweek), lk$key)
  if (anyNA(m)) {
    stop(
      "isoyearweek '",
      isoyearweek[is.na(m)][1L],
      "' is not an ISO week or an ISO year in cstime::dates_by_isoyearweek",
      call. = FALSE
    )
  }
  return(list(first = lk$first[m], last = lk$last[m]))
}


#' The isoyearweek a span was looked up from, for error messages
#'
#' @noRd
.isoyearweek_span_label <- function(first, last) {
  lk <- .isoyearweek_span_lookup()
  return(if (first == last) {
    lk$key[first]
  } else {
    paste0(lk$isoyear[first], "-**")
  })
}


#' Stop unless the rows are in strictly increasing calendar order
#'
#' Each row MUST start after the previous row ends, so no two rows share a
#' week. An annual row and a weekly row of the same ISO year share a week, so
#' they are refused too.
#'
#' @param first,last Integer week index of the first and the last week of each
#'   row of one person, from `.isoyearweek_spans()`.
#' @noRd
.check_span_order <- function(first, last) {
  n <- length(first)
  if (n < 2L) {
    return(invisible(NULL))
  }
  bad <- which(first[-1L] <= last[-n])
  if (length(bad) > 0L) {
    i <- bad[1L]
    stop(
      "isoyearweek is not in strictly increasing calendar order: row ",
      i + 1L,
      " ('",
      .isoyearweek_span_label(first[i + 1L], last[i + 1L]),
      "') does not start after row ",
      i,
      " ('",
      .isoyearweek_span_label(first[i], last[i]),
      "') ends",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}


#' Any TRUE among the earlier rows inside the window, given week spans
#'
#' The engine of [any_events_prior_to()]. A caller that evaluates one table by
#' person looks the spans up once for the whole table. It checks each person's
#' slice once with `.check_span_order()` and passes it here.
#'
#' Rows `1..k` end before the window starts, so rows `k + 1` to `i - 1` hold
#' the window of row `i`. Prefix sums of the events and of the NAs then count
#' both in O(1) per row.
#'
#' @param x Logical vector.
#' @param window_excluding_wk0 Window in ISO weeks. 99999 or more is lifetime.
#' @param first,last Integer week index of the first and the last week of each
#'   row, in an order `.check_span_order()` accepts.
#' @return Logical vector of the same length as `x`.
#' @noRd
.any_prior_in_spans <- function(x, window_excluding_wk0, first, last) {
  w <- window_excluding_wk0
  if (!is.numeric(w) || length(w) != 1L || is.na(w) || w < 0 ||
    (is.finite(w) && w != round(w))) {
    stop(
      "window_excluding_wk0 MUST be one whole number of weeks, 0 or more, ",
      "or Inf for a lifetime window",
      call. = FALSE
    )
  }
  n <- length(x)
  if (n == 0L) {
    return(logical(0))
  }
  # `as.integer()` of a lifetime window can overflow to NA, so a lifetime
  # window never reaches it.
  window <- if (window_excluding_wk0 >= 99999) {
    Inf
  } else {
    as.integer(window_excluding_wk0)
  }
  xi <- as.integer(x)
  na <- is.na(xi)
  any_na <- any(na)
  if (any_na) {
    xi[na] <- 0L
  }
  # cs[i] counts the events in rows 1..(i - 1).
  cs <- cumsum(c(0L, xi))
  lo <- findInterval(first - window - 1, last) + 1L
  idx <- seq_len(n)
  out <- cs[idx] > cs[lo]
  if (any_na) {
    cn <- cumsum(c(0L, na))
    out[!out & cn[idx] > cn[lo]] <- NA
  }
  return(out)
}
