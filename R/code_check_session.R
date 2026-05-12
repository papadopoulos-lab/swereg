# Session-aggregated code checks ---------------------------------------------
#
# When running batched pipelines (e.g. ~2000 raw-data batches per delivery),
# per-batch invocations of warn_unmatched_codes() / warn_empty_logical_cols()
# produce false positives: rare diagnosis/medication/operation codes
# legitimately appear in only a handful of batches, so any single batch will
# warn about them even when everything is correct.
#
# The fix is to aggregate across batches: a literal is only suspicious if it
# never matched in ANY batch; a column is only suspicious if it was always
# all-FALSE (or always missing). This module provides a session in which the
# usual warn_* helpers accumulate state instead of warning, plus a finalizer
# that emits a single consolidated warning on session close.

.swereg_check_session <- new.env(parent = emptyenv())
.swereg_check_session$active <- FALSE
# Reference count: nested start/end calls are no-ops at every level except
# the outermost. This lets RegistryStudy$process_skeletons() open an
# auto-session safely even when the user has manually opened one around
# the whole pipeline (and vice versa). The outermost end is the one that
# emits the consolidated warnings.
.swereg_check_session$depth <- 0L
# unmatched[[call_label]][[group]] is a logical named vector where names are
# literals (with leading "!" stripped) and value TRUE means "still unmatched
# anywhere". Once a literal is seen as a match in any batch the entry flips
# to FALSE and stays FALSE.
.swereg_check_session$unmatched <- list()
# empty[[call_label]][[col]] is list(ever_true = logical(1),
# ever_present = logical(1)).
.swereg_check_session$empty <- list()

#' Aggregate code-list sanity checks across a batched pipeline
#'
#' When running an \code{add_*} pipeline over many batches of raw data
#' (e.g. ~2000 batches per delivery), the per-call checks performed by
#' \code{\link{warn_unmatched_codes}} / \code{\link{warn_empty_logical_cols}}
#' are noisy: any rare code that legitimately appears in only some batches
#' will trigger an "unmatched" warning on every other batch. Wrapping the
#' loop in \code{start_code_check_session()} / \code{end_code_check_session()}
#' converts those per-batch warnings into a single end-of-pipeline report
#' that flags only literals that never matched in ANY batch and columns
#' that were never \code{TRUE} in ANY batch.
#'
#' While a session is active, every \code{warn_unmatched_codes()} call
#' (including the ones invoked internally by \code{\link{add_diagnoses}} and
#' siblings) records per-(call_label, group, literal) whether it has yet
#' been matched in some batch, instead of warning immediately. Likewise,
#' \code{warn_empty_logical_cols()} records per-(call_label, column)
#' whether the column has yet been observed and whether it has yet contained
#' any \code{TRUE} value. \code{end_code_check_session()} consults those
#' accumulators and emits one grouped \code{warning()} per call label for
#' anything still unmatched / always empty / always missing, then clears
#' the state.
#'
#' Sessions nest via reference counting: a nested
#' \code{start_code_check_session()} bumps an internal depth counter and
#' preserves the existing accumulators, and only the outermost
#' \code{end_code_check_session()} (the one that brings the depth back to
#' zero) emits the consolidated warnings. This means
#' \code{RegistryStudy$process_skeletons()} can safely open its own
#' auto-session without clobbering a session the caller has already
#' opened around the whole pipeline. \code{end_code_check_session()} is
#' safe to call when no session is active (no-op).
#'
#' @return Both functions return \code{invisible(NULL)}.
#'   \code{end_code_check_session()} additionally emits aggregated
#'   warnings if any literals or columns failed to match across the
#'   entire session.
#' @examples
#' \dontrun{
#' swereg::start_code_check_session()
#' for (file_number in seq_along(batches)) {
#'   skeleton <- create_skeleton(...)
#'   swereg::add_diagnoses(skeleton, batches[[file_number]], "lopnr",
#'                         codes = my_codes)
#'   # ...
#' }
#' swereg::end_code_check_session()
#' }
#' @seealso \code{\link{warn_unmatched_codes}},
#'   \code{\link{warn_empty_logical_cols}}
#' @export
start_code_check_session <- function() {
  if (isTRUE(.swereg_check_session$active)) {
    # Already inside a session -- bump depth, keep accumulators.
    .swereg_check_session$depth <- .swereg_check_session$depth + 1L
    return(invisible())
  }
  .swereg_check_session$active <- TRUE
  .swereg_check_session$depth <- 1L
  .swereg_check_session$unmatched <- list()
  .swereg_check_session$empty <- list()
  invisible()
}

#' @rdname start_code_check_session
#' @export
end_code_check_session <- function() {
  if (!isTRUE(.swereg_check_session$active)) return(invisible())
  .swereg_check_session$depth <- .swereg_check_session$depth - 1L
  if (.swereg_check_session$depth > 0L) return(invisible())

  unmatched <- .swereg_check_session$unmatched
  empty <- .swereg_check_session$empty
  .swereg_check_session$active <- FALSE
  .swereg_check_session$depth <- 0L
  .swereg_check_session$unmatched <- list()
  .swereg_check_session$empty <- list()

  for (call_label in names(unmatched)) {
    groups <- unmatched[[call_label]]
    still_missing <- list()
    for (g in names(groups)) {
      lits <- names(groups[[g]])[groups[[g]]]
      if (length(lits) > 0L) still_missing[[g]] <- lits
    }
    if (length(still_missing) > 0L) {
      msgs <- vapply(names(still_missing), function(g) {
        paste0("  ", g, ": ", paste(still_missing[[g]], collapse = ", "))
      }, character(1))
      label <- if (nzchar(call_label)) paste0("[", call_label, "] ") else ""
      warning(
        label,
        "Code(s) with zero matches across all batches ",
        "(pattern may not be understood, or code does not exist in registry):\n",
        paste(msgs, collapse = "\n"),
        call. = FALSE
      )
    }
  }

  for (call_label in names(empty)) {
    cols <- empty[[call_label]]
    missing_cols <- character()
    empty_cols <- character()
    for (col in names(cols)) {
      st <- cols[[col]]
      if (!isTRUE(st$ever_present)) {
        missing_cols <- c(missing_cols, col)
      } else if (!isTRUE(st$ever_true)) {
        empty_cols <- c(empty_cols, col)
      }
    }
    label <- if (nzchar(call_label)) paste0("[", call_label, "] ") else ""
    if (length(missing_cols) > 0L) {
      warning(
        label,
        "Expected column(s) never created in any batch (",
        length(missing_cols), "): ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    if (length(empty_cols) > 0L) {
      warning(
        label,
        "Column(s) all-FALSE across all batches (",
        length(empty_cols),
        "; pattern may not have been understood correctly): ",
        paste(empty_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible()
}

# Internal: update unmatched accumulator from a single warn_unmatched_codes
# invocation. `bare_literals` is the leading-"!" stripped literals for one
# group; `hits` is a logical vector of the same length, TRUE where the
# literal matched the current batch.
.swereg_session_record_unmatched <- function(call_label, group, bare_literals, hits) {
  bucket <- .swereg_check_session$unmatched[[call_label]]
  if (is.null(bucket)) bucket <- list()
  prev <- bucket[[group]]
  if (is.null(prev)) {
    # First time we see this group: every literal starts as still-unmatched,
    # then we flip to FALSE for the ones that hit in this batch.
    prev <- stats::setNames(rep(TRUE, length(bare_literals)), bare_literals)
  } else {
    # Union of literal sets (in case different batches pass different lists).
    new_lits <- setdiff(bare_literals, names(prev))
    if (length(new_lits) > 0L) {
      prev <- c(prev, stats::setNames(rep(TRUE, length(new_lits)), new_lits))
    }
  }
  matched_now <- bare_literals[hits]
  if (length(matched_now) > 0L) {
    prev[matched_now] <- FALSE
  }
  bucket[[group]] <- prev
  .swereg_check_session$unmatched[[call_label]] <- bucket
  invisible()
}

# Internal: update empty-cols accumulator from a single
# warn_empty_logical_cols invocation.
.swereg_session_record_empty <- function(call_label, col, present, ever_true) {
  bucket <- .swereg_check_session$empty[[call_label]]
  if (is.null(bucket)) bucket <- list()
  prev <- bucket[[col]]
  if (is.null(prev)) prev <- list(ever_true = FALSE, ever_present = FALSE)
  if (isTRUE(present)) prev$ever_present <- TRUE
  if (isTRUE(ever_true)) prev$ever_true <- TRUE
  bucket[[col]] <- prev
  .swereg_check_session$empty[[call_label]] <- bucket
  invisible()
}
