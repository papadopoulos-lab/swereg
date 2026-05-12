# Per-literal pre-call source-data check (internal). For each named entry in
# `code_list`, verify that every literal prefix matches at least one code
# present in `source_data`. Called automatically by every `add_*()` via
# `.swereg_codes_pre()` so unmatched literals are loud at run-time even when
# sibling literals would otherwise create a non-empty output column.
#
# `"!"`-prefixed literals (row-level vetoes) are checked too with the
# leading `"!"` stripped, so typos in exclusion lists also surface.
#
# Inside an active code-check session, hits accumulate into the session
# instead of warning immediately; the parent process emits one consolidated
# warning at the end of `RegistryStudy$process_skeletons()`.
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

# Post-call column-level check (internal). Every entry in `code_list` should
# produce a logical column on `skeleton`; warn if any are missing or contain
# zero TRUEs across the dataset (strongly suggests the underlying pattern
# was not understood). Called automatically by every `add_*()` via
# `.swereg_codes_post()`. Inside an active session, hits accumulate; the
# parent emits a consolidated warning at the end of `process_skeletons()`.
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
