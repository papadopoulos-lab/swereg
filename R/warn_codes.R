#' Warn when code patterns have no matches in the source data
#'
#' Per-literal source-data check. For each named entry in \code{code_list},
#' verifies that every individual literal prefix matches at least one code
#' present in \code{source_data}. Designed to be called \emph{before}
#' \code{\link{add_diagnoses}} / \code{\link{add_operations}} /
#' \code{\link{add_rx}} / \code{\link{add_cods}} so an unmatched literal is
#' loud at run-time even when its sibling literals would otherwise create a
#' non-empty output column. This complements
#' \code{\link{warn_empty_logical_cols}} (column-level check): the
#' column-level check fires only when a whole code-list entry produces zero
#' \code{TRUE}s, so it misses partial-failure cases (e.g., bracket ranges
#' that silently expand to nothing while sibling literal codes succeed).
#'
#' \code{"!"}-prefixed literals (row-level vetoes) are checked too, with
#' the leading \code{"!"} stripped before matching. A \code{"!"} literal
#' that matches nothing in the source means the veto is a no-op; this
#' catches typos in exclusion lists.
#'
#' Implementation: scans every character column in \code{source_data} once,
#' takes unique values, then for each literal tests
#' \code{any(startsWith(all_codes, lit))}. Cost is roughly
#' \code{O(unique_codes * n_literals)} and runs once per call. Because
#' codes are generally identical across processing batches, callers
#' running batched pipelines are encouraged to gate this call on the first
#' batch only (e.g. \code{if (file_number == 1) warn_unmatched_codes(...)})
#' to avoid redundant work.
#'
#' @param source_data A \code{data.frame}/\code{data.table} that
#'   \code{add_*} will scan. All character columns are pooled.
#' @param code_list The (already \code{\link{expand_code_list}}-expanded)
#'   named list passed to \code{add_*}.
#' @param call_label Short string identifying which \code{add_*} call this
#'   guard is for (used only in the warning message).
#' @return Invisibly returns \code{NULL}. Emits a single \code{warning()}
#'   if any literal has zero matches.
#' @examples
#' \dontrun{
#' diags_list <- swereg::expand_code_list(list(
#'   diag_acute_mi = c("I2[0-5]")
#' ))
#' swereg::warn_unmatched_codes(diagnoses, diags_list, "add_diagnoses")
#' swereg::add_diagnoses(skeleton, diagnoses, "lopnr", diags = diags_list)
#' }
#' @seealso \code{\link{expand_code_list}},
#'   \code{\link{warn_empty_logical_cols}}
#' @export
warn_unmatched_codes <- function(source_data, code_list, call_label = "") {
  if (is.null(code_list) || length(code_list) == 0L) return(invisible())

  char_cols <- names(source_data)[
    vapply(source_data, is.character, logical(1))
  ]
  if (length(char_cols) == 0L) return(invisible())

  all_codes <- unique(unlist(
    lapply(char_cols, function(cc) source_data[[cc]]),
    use.names = FALSE
  ))
  all_codes <- all_codes[!is.na(all_codes) & nzchar(all_codes)]
  if (length(all_codes) == 0L) return(invisible())

  unmatched <- list()
  session_active <- isTRUE(.swereg_check_session$active)
  for (group in names(code_list)) {
    literals <- code_list[[group]]
    if (length(literals) == 0L) next
    bare <- ifelse(
      startsWith(literals, "!"),
      substr(literals, 2L, nchar(literals)),
      literals
    )
    hits <- vapply(
      bare,
      function(lit) any(startsWith(all_codes, lit)),
      logical(1)
    )
    if (session_active) {
      .swereg_session_record_unmatched(call_label, group, bare, hits)
    } else {
      miss_idx <- !hits
      if (any(miss_idx)) unmatched[[group]] <- literals[miss_idx]
    }
  }

  if (session_active) return(invisible())

  if (length(unmatched) > 0L) {
    msgs <- vapply(names(unmatched), function(g) {
      paste0("  ", g, ": ", paste(unmatched[[g]], collapse = ", "))
    }, character(1))
    label <- if (nzchar(call_label)) paste0("[", call_label, "] ") else ""
    warning(
      label,
      "Code(s) with zero matches in source data ",
      "(pattern may not be understood, or code does not exist in registry):\n",
      paste(msgs, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible()
}

#' Warn when expected logical columns are missing or all-FALSE on a skeleton
#'
#' Column-level check, intended to be called \emph{after} an
#' \code{add_diagnoses() / add_operations() / add_rx() / add_cods()} call.
#' Every entry in the supplied \code{code_list} should produce a logical
#' column on \code{skeleton}; this helper warns if any are missing or
#' contain zero \code{TRUE}s across the entire dataset, which strongly
#' suggests the underlying pattern was not understood (e.g. matcher
#' regression, pattern doesn't match the source dictionary, or the column
#' was simply never created).
#'
#' Use together with \code{\link{warn_unmatched_codes}} (which fires
#' \emph{before} the \code{add_*} call and operates per individual literal
#' against the source data) for full coverage. The two checks are
#' complementary: \code{warn_unmatched_codes()} flags bad patterns even
#' when sibling literals would create a non-empty column,
#' \code{warn_empty_logical_cols()} flags whole-entry failures including
#' those caused by issues in the matcher itself.
#'
#' @param skeleton A skeleton \code{data.table} that has been processed by
#'   one or more \code{add_*} functions.
#' @param code_list The named list passed to the \code{add_*} call.
#' @param call_label Short string identifying which \code{add_*} call this
#'   guard is for (used only in the warning message).
#' @return Invisibly returns \code{NULL}. Emits up to two
#'   \code{warning()}s (missing columns, all-\code{FALSE} columns).
#' @examples
#' \dontrun{
#' diags_list <- swereg::expand_code_list(list(
#'   diag_acute_mi = c("I2[0-5]")
#' ))
#' swereg::add_diagnoses(skeleton, diagnoses, "lopnr", diags = diags_list)
#' swereg::warn_empty_logical_cols(skeleton, diags_list, "add_diagnoses")
#' }
#' @seealso \code{\link{warn_unmatched_codes}}
#' @export
warn_empty_logical_cols <- function(skeleton, code_list, call_label = "") {
  if (is.null(code_list) || length(code_list) == 0L) return(invisible())
  expected <- names(code_list)
  if (is.null(expected) || length(expected) == 0L) return(invisible())

  session_active <- isTRUE(.swereg_check_session$active)
  missing_cols <- setdiff(expected, names(skeleton))
  empty_cols <- character()
  for (col in expected) {
    present <- col %in% names(skeleton)
    ever_true <- FALSE
    if (present) {
      v <- skeleton[[col]]
      if (is.logical(v)) ever_true <- isTRUE(any(v, na.rm = TRUE))
    }
    if (session_active) {
      .swereg_session_record_empty(call_label, col, present, ever_true)
    } else if (present && !ever_true) {
      empty_cols <- c(empty_cols, col)
    }
  }

  if (session_active) return(invisible())

  label <- if (nzchar(call_label)) paste0("[", call_label, "] ") else ""

  if (length(missing_cols) > 0L) {
    warning(
      label,
      "Expected column(s) not created (",
      length(missing_cols), "): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(empty_cols) > 0L) {
    warning(
      label,
      "Column(s) with zero TRUE values (",
      length(empty_cols),
      "; pattern may not have been understood correctly): ",
      paste(empty_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible()
}
