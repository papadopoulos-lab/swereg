# =============================================================================
# The observation contract
# =============================================================================
# One definition of "observed", shared by the spec parser, TTEDesign and the
# s1 cache. An enrollment states how observation is encoded. It never lets the
# reader infer it.
#
#   observed_var: {column: rd_observed}      a real logical person-week column
#   observed_var: {sentinel: row_presence}   the skeleton is trimmed
#
# `row_presence` asserts that the caller already deleted every unobserved
# person-week, so a row exists if and only if the person was observed that
# week. The production skeleton is built this way. It deletes every
# person-week up to and including first immigration, every person-week on or
# after emigration, and every person-week after death. It keeps the death week
# itself. A real `observed` column there would hold TRUE on every retained row
# and could not represent an absent week. The sentinel makes that assumption
# explicit and testable. Row presence as a silent proxy stays forbidden.

# The sentinel values this version of swereg understands.
.TTE_OBSERVED_SENTINELS <- "row_presence"

#' Build a normalised observation encoding.
#'
#' @param column Character scalar or `NA_character_`, the logical column name.
#' @param sentinel Character scalar or `NA_character_`, the sentinel name.
#' @return A `tte_observed_var` list with `column` and `sentinel`.
#' @noRd
.tte_new_observed_var <- function(
  column = NA_character_,
  sentinel = NA_character_
) {
  return(structure(
    list(column = column, sentinel = sentinel),
    class = "tte_observed_var"
  ))
}

#' Normalise one `observed_var` declaration.
#'
#' The single entry point for the observation contract. The spec parser,
#' `TTEDesign$new()` and any later landmark code all go through it, so one
#' declaration cannot mean two things in two places.
#'
#' @param x The declaration. `NULL` when the caller declares nothing. A list
#'   with exactly one of `column` or `sentinel`. An already-normalised
#'   `tte_observed_var` passes through unchanged, so the function is
#'   idempotent.
#' @param context Character, the name to report in an error message.
#' @return `NULL` when `x` is `NULL`. Otherwise a `tte_observed_var` list.
#' @noRd
.tte_observed_var <- function(x, context = "observed_var") {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "tte_observed_var")) {
    return(x)
  }
  if (!is.list(x) || length(x) == 0L || is.null(names(x))) {
    stop(
      context,
      " must be a mapping with exactly one of `column` or `sentinel`. ",
      "Write `",
      context,
      ": {column: <name>}` for a real logical column, or `",
      context,
      ": {sentinel: row_presence}` for a trimmed skeleton.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(x), c("column", "sentinel"))
  if (length(unknown) > 0L) {
    stop(
      context,
      " has unknown key(s): ",
      paste(unknown, collapse = ", "),
      ". Use `column` or `sentinel`.",
      call. = FALSE
    )
  }
  # Test KEY PRESENCE, not value presence. `observed_var: {column: null,
  # sentinel: row_presence}` is valid YAML and parses to a two-key list whose
  # `column` value is NULL. A `!is.null()` test reads that as one key and
  # accepts it. A reader of the YAML sees two claims, so swereg MUST reject
  # it. `[[` is used throughout, because `$` does partial name matching.
  has_column <- "column" %in% names(x)
  has_sentinel <- "sentinel" %in% names(x)
  if (has_column && has_sentinel) {
    stop(
      context,
      " gives both `column` and `sentinel`. Give exactly one. ",
      "A named column and a trimmed skeleton are different claims.",
      call. = FALSE
    )
  }
  if (!has_column && !has_sentinel) {
    stop(
      context,
      " must give exactly one of `column` or `sentinel`.",
      call. = FALSE
    )
  }
  if (has_column) {
    value <- x[["column"]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      stop(
        context,
        "$column must be a single non-empty column name.",
        call. = FALSE
      )
    }
    return(.tte_new_observed_var(column = value))
  }
  value <- x[["sentinel"]]
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(context, "$sentinel must be a single sentinel name.", call. = FALSE)
  }
  if (!value %in% .TTE_OBSERVED_SENTINELS) {
    stop(
      context,
      "$sentinel is '",
      value,
      "', which swereg does not know. The known sentinel(s): ",
      paste(.TTE_OBSERVED_SENTINELS, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(.tte_new_observed_var(sentinel = value))
}

#' Read the column name out of an observation encoding.
#'
#' @param x A `tte_observed_var`, or `NULL`.
#' @return The column name, or `NULL` when the encoding names no column.
#' @noRd
.tte_observed_column <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  # `[[` is exact. `$` does partial name matching, which is unsafe on a field
  # read from a user's YAML file.
  col <- x[["column"]]
  if (is.null(col) || is.na(col)) {
    return(NULL)
  }
  return(col)
}

#' Check one arm tolerance.
#'
#' A tolerance MUST be a finite, representable, non-negative whole number of
#' weeks. The function NEVER returns `NA`.
#'
#' `is.finite()` carries three of the rejections at once: `NA`, `NaN`, `Inf`
#' and `-Inf` are all not finite. The upper bound carries the fourth.
#' `as.integer(3e9)` returns `NA` with only a warning, and `Inf` does the same,
#' so a value that passes the whole-number test can still land as `NA`. An `NA`
#' tolerance compares as neither tolerated nor discordant in every later
#' adherence rule, which is worse than a loud error here.
#'
#' @param x The declared value, or `NULL` for the default of zero weeks.
#' @param context Character, the name to report in an error message.
#' @return An integer scalar between 0 and `.Machine$integer.max`. Never `NA`.
#' @noRd
.tte_tolerance_weeks <- function(x, context) {
  if (is.null(x)) {
    return(0L)
  }
  ok <- is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x) &&
    x >= 0 &&
    x <= .Machine$integer.max &&
    x == trunc(x)
  if (!ok) {
    stop(
      context,
      " must be a single whole number of weeks, at least 0 and at most ",
      .Machine$integer.max,
      ". It MUST be finite. Got a ",
      class(x)[1],
      " of length ",
      length(x),
      ": ",
      paste(format(x), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(as.integer(x))
}
