# Internal helpers used by every add_* function to:
#   1. Expand bracket / range syntax in pattern values via expand_code_list().
#   2. Optionally run pre-call (warn_unmatched_codes) and post-call
#      (warn_empty_logical_cols) sanity checks.
#
# The checks are gated on the package option `swereg.check_codes`, which
# defaults to TRUE. Callers running batched pipelines (e.g. one call per
# raw-data file) typically want the checks once per delivery rather than
# per batch and can do:
#
#   options(swereg.check_codes = isTRUE(file_number == 1))
#
# Returns the expanded code list. Skipping the call entirely (when codes
# is NULL or empty) is harmless: expand_code_list() handles empty input
# and warn_*() functions short-circuit on empty input.

.swereg_check_codes_enabled <- function() {
  isTRUE(getOption("swereg.check_codes", TRUE))
}

.swereg_codes_pre <- function(codes, source_data, call_label,
                              syntax_check = TRUE) {
  expanded <- expand_code_list(codes)
  if (.swereg_check_codes_enabled()) {
    if (isTRUE(syntax_check)) {
      .check_pattern_syntax(
        unlist(expanded, use.names = FALSE),
        call_label = call_label
      )
    }
    warn_unmatched_codes(source_data, expanded, call_label)
  }
  expanded
}

.swereg_codes_post <- function(skeleton, expanded_codes, call_label) {
  if (.swereg_check_codes_enabled()) {
    warn_empty_logical_cols(skeleton, expanded_codes, call_label)
  }
  invisible()
}

# Cheap, data-free syntax check on already-expanded literals. Runs in
# milliseconds at the first add_*() call and catches the most common
# class of bug (regex metacharacters in startsWith-based patterns) at
# minute 1 of a multi-hour pipeline rather than at hour 6.
#
# The check is intentionally syntactic, not registry-specific: a
# per-registry format check (e.g. "ICD-10 codes look like F648") is too
# false-positive-prone across the variety of code systems the add_*
# family covers (ICD-10, ICD-9, ICD-7, ATC, NOMESCO ops, ICD-O-3,
# SNOMED, ekod, ...). Bracket characters are not flagged here -- by the
# time we run, expand_codes() has already consumed them; a surviving
# "[" or "]" indicates a malformed pattern (unbalanced brackets) and
# IS flagged via the meta-character branch.
#
# Caller: invoked from .swereg_codes_pre() with the call_label, so
# the warning attributes the bug to the right add_*() function. The
# check should NOT run for add_rx(source = "produkt") because product
# names are exact-matched via %chin% and may legitimately contain
# parentheses, plus signs, etc.
.check_pattern_syntax <- function(literals, call_label = "") {
  if (length(literals) == 0L) return(invisible())
  bare <- ifelse(startsWith(literals, "!"),
                 substr(literals, 2L, nchar(literals)),
                 literals)

  empty <- !nzchar(bare)
  # Regex metacharacters that won't match under startsWith():
  #   ^ $ * + ? . ( ) | \ [ ]
  # "." is included: Swedish registries strip the dot from ICD codes
  # before delivery, so a "." in a pattern is almost always a regex
  # bug, not data.
  meta_re <- "[\\^\\$\\*\\+\\?\\.\\(\\)\\|\\\\\\[\\]]"
  has_meta <- grepl(meta_re, bare, perl = TRUE)

  bad_empty <- literals[empty]
  bad_meta  <- literals[has_meta & !empty]

  label <- if (nzchar(call_label)) paste0("[", call_label, "] ") else ""

  if (length(bad_empty) > 0L) {
    warning(
      label,
      "Empty pattern(s) detected (",
      length(bad_empty), "). An empty pattern matches everything under ",
      "startsWith() and is almost certainly a bug.",
      call. = FALSE
    )
  }
  if (length(bad_meta) > 0L) {
    warning(
      label,
      "Pattern(s) contain regex metacharacters that will NOT match ",
      "under startsWith() (",
      length(bad_meta), "): ",
      paste(utils::head(unique(bad_meta), 10L), collapse = ", "),
      if (length(unique(bad_meta)) > 10L) ", ..." else "",
      ". Remove ^ $ * + ? . ( ) | \\ [ ] from the pattern -- add_*() ",
      "matches via startsWith(), not regex.",
      call. = FALSE
    )
  }
  invisible()
}
