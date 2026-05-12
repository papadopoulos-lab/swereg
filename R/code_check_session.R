# Per-batch code-check session ------------------------------------------------
#
# Internal infrastructure that lets `add_*()` functions buffer their per-call
# sanity-check warnings into an in-memory accumulator instead of firing them
# immediately. The accumulator is snapshotted at the end of every batch by
# `.process_one_batch()` and persisted into the batch's `meta_*.qs2` sidecar.
# `RegistryStudy$process_skeletons()` reads every batch's snapshot at the end
# of a run, merges them, and emits one consolidated warning covering the
# whole run (sequential or parallel).
#
# Nothing here is exported. Users do not interact with the session directly:
# they get cross-batch aggregation automatically when running through
# `RegistryStudy$process_skeletons()`, and per-call warnings otherwise.

.swereg_check_session <- new.env(parent = emptyenv())
.swereg_check_session$active <- FALSE
# Reference-counted depth: a nested .start_code_check_session() bumps the
# counter and preserves accumulators; only the outermost .end clears state.
# No current caller nests, but the counter is cheap and keeps a safety net
# in place if a future internal caller wraps a session inside another.
.swereg_check_session$depth <- 0L
# unmatched[[call_label]][[group]] is a logical named vector keyed by literal
# (with leading "!" stripped). TRUE means "still unmatched anywhere" -- once
# any batch matches the literal it flips to FALSE permanently.
.swereg_check_session$unmatched <- list()
# empty[[call_label]][[col]] is list(ever_true = logical(1),
# ever_present = logical(1)). Both are OR-accumulated across batches.
.swereg_check_session$empty <- list()

.start_code_check_session <- function() {
  if (isTRUE(.swereg_check_session$active)) {
    .swereg_check_session$depth <- .swereg_check_session$depth + 1L
    return(invisible())
  }
  .swereg_check_session$active <- TRUE
  .swereg_check_session$depth <- 1L
  .swereg_check_session$unmatched <- list()
  .swereg_check_session$empty <- list()
  invisible()
}

# Tear down the session and clear state. Does NOT emit warnings: the
# accumulator should be snapshotted via .code_check_snapshot() *before*
# this is called, embedded in the batch's meta file, and emitted later by
# `.code_check_emit()` after merging across all batches in the run.
.end_code_check_session <- function() {
  if (!isTRUE(.swereg_check_session$active)) return(invisible())
  .swereg_check_session$depth <- .swereg_check_session$depth - 1L
  if (.swereg_check_session$depth > 0L) return(invisible())
  .swereg_check_session$active <- FALSE
  .swereg_check_session$depth <- 0L
  .swereg_check_session$unmatched <- list()
  .swereg_check_session$empty <- list()
  invisible()
}

# Snapshot the current session state as a plain serialisable list. Safe to
# call when no session is active (returns an empty snapshot).
.code_check_snapshot <- function() {
  list(
    unmatched = .swereg_check_session$unmatched,
    empty     = .swereg_check_session$empty
  )
}

# Merge a list of per-batch snapshots into one combined snapshot with the
# same shape, preserving the single-process semantics:
#   * unmatched: a literal is "still unmatched" iff it is "still unmatched"
#     in EVERY snapshot that observed it. A snapshot that never observed
#     the literal does not contribute (absence == "no information").
#   * empty: ever_true / ever_present are OR-accumulated across snapshots.
.code_check_merge <- function(snapshots) {
  merged_unmatched <- list()
  merged_empty <- list()

  for (snap in snapshots) {
    if (is.null(snap)) next

    # ---- unmatched ----
    for (call_label in names(snap$unmatched)) {
      groups <- snap$unmatched[[call_label]]
      bucket <- merged_unmatched[[call_label]] %||% list()
      for (g in names(groups)) {
        new <- groups[[g]]
        prev <- bucket[[g]]
        if (is.null(prev)) {
          bucket[[g]] <- new
        } else {
          # Union the literal sets. AND the still-unmatched flags: a literal
          # is still unmatched iff every snapshot that saw it left it TRUE.
          all_lits <- union(names(prev), names(new))
          combined <- stats::setNames(logical(length(all_lits)), all_lits)
          for (lit in all_lits) {
            in_prev <- lit %in% names(prev)
            in_new  <- lit %in% names(new)
            combined[lit] <- (if (in_prev) prev[[lit]] else TRUE) &&
                             (if (in_new)  new[[lit]]  else TRUE)
          }
          bucket[[g]] <- combined
        }
      }
      merged_unmatched[[call_label]] <- bucket
    }

    # ---- empty ----
    for (call_label in names(snap$empty)) {
      cols <- snap$empty[[call_label]]
      bucket <- merged_empty[[call_label]] %||% list()
      for (col in names(cols)) {
        st <- cols[[col]]
        prev <- bucket[[col]]
        if (is.null(prev)) {
          bucket[[col]] <- st
        } else {
          bucket[[col]] <- list(
            ever_true    = isTRUE(prev$ever_true)    || isTRUE(st$ever_true),
            ever_present = isTRUE(prev$ever_present) || isTRUE(st$ever_present)
          )
        }
      }
      merged_empty[[call_label]] <- bucket
    }
  }

  list(unmatched = merged_unmatched, empty = merged_empty)
}

# Emit the consolidated warnings for a merged snapshot. No-op on an empty
# snapshot. Same warning format as the per-batch warn_*() helpers used to
# emit, just labelled "across all batches".
.code_check_emit <- function(merged) {
  if (is.null(merged)) return(invisible())

  for (call_label in names(merged$unmatched)) {
    groups <- merged$unmatched[[call_label]]
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

  for (call_label in names(merged$empty)) {
    cols <- merged$empty[[call_label]]
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

# Update unmatched accumulator from one warn_unmatched_codes() invocation.
# `bare_literals` is the leading-"!" stripped literals for one group;
# `hits` is a logical vector of the same length, TRUE where the literal
# matched in the current batch.
.swereg_session_record_unmatched <- function(call_label, group, bare_literals, hits) {
  bucket <- .swereg_check_session$unmatched[[call_label]]
  if (is.null(bucket)) bucket <- list()
  prev <- bucket[[group]]
  if (is.null(prev)) {
    prev <- stats::setNames(rep(TRUE, length(bare_literals)), bare_literals)
  } else {
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

# Update empty-cols accumulator from one warn_empty_logical_cols() invocation.
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
