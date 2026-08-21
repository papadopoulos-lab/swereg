# The per-batch pipeline, and the two batch targets that run it in a worker.

# Execute the full per-batch pipeline for ONE batch. Extracted to a
# file-level helper so both the serial branch of process_skeletons() and
# its worker subprocesses (via .process_one_batch_snapshot) call the
# exact same code.
#
# The four phases run in their numbered order: 1, 1b, 2, 3.
#
# Phase 1 (framework): load the existing skeleton. Rebuild the base from
# scratch when the file is missing. Rebuild it too when framework_fn_hash,
# trim_fn_hash or phase_order does not match the study's current identity.
# A rebuild resets phase-2 and phase-3 state.
#
# Phase 1b (trim): on a rebuild only, run the registered trim on the
# fresh base and rebind the skeleton data to what it returns. This is the
# only phase that may delete rows, and it runs before phases 2 and 3.
#
# Phase 2 (codes): hand the per-entry diff to Skeleton$sync_with_registry().
#
# Phase 3 (randvars): hand the divergence-point rewind-and-replay logic
# to Skeleton$sync_randvars(). It runs LAST, so a randvars step may read a
# code column.
#
# batch_data is loaded lazily inside `load_bd()` so the rawbatch read is
# shared across phases (or skipped entirely when nothing needs to run).
#
# --- rawbatch write target (shape B) ----------------------------------------

#' Write one rawbatch slice atomically -- the `save_rawbatch()` batch target
#'
#' The `n_workers > 1` path's batch target: dispatched via `.batch_stream()`
#' with `style = "staged_writer"`, so the daemon writes its slice by calling
#' `batchit::where_to_write_output("rawbatch")` for the final destination
#' path rather than receiving it as an argument -- `where_to_write_output()`
#' only resolves inside an active staged_writer run, so this worker cannot be
#' called directly in-process. The serial default (`n_workers = 1`) therefore
#' does NOT call this worker; it calls [qs2_write_atomic()] directly (no
#' process boundary, no staged run, so no dispatcher and no mirai requirement).
#' Using [qs2_write_atomic()] here -- rather than the hand-inlined temp+rename
#' the old daemon expression carried "because swereg may not be loaded in the
#' daemon" -- is the point: the generic worker loads swereg before executing
#' any target, so the hardened writer (collision-resistant temp name, cleanup
#' on any R-level failure) is available and there is no second,
#' divergence-prone copy of its logic to keep in sync.
#'
#' @param slice The rawbatch payload for one batch (data.table, or named list
#'   of them).
#' @param n_threads qs2 serialization threads (1 in a daemon -- parallelism
#'   there comes from the daemons; the machine count when called serially).
#' @return `TRUE`, invisibly.
#' @noRd
.rawbatch_write_worker <- function(slice, n_threads) {
  qs2_write_atomic(
    slice,
    .batch_where_to_write_output("rawbatch"),
    nthreads = n_threads
  )
  invisible(TRUE)
}

# --- skeleton batch target (shape A, snapshot payload) ----------------------

#' Process one skeleton batch from a study snapshot -- the
#' `process_skeletons()` batch target
#'
#' Thin subprocess wrapper around [.process_one_batch()]: reads the study from
#' a snapshot file and delegates. The snapshot indirection is load-bearing, not
#' convenience: the study object is ~5.7 MB in production, and the shape-A
#' runner materialises EVERY item envelope up front -- so putting the study in
#' each of 2,194 items would serialize ~12.5 GB before the first worker
#' launched (the old callr engine serialized it only per launched batch,
#' ~n_workers in flight). The parent writes the snapshot ONCE and each item
#' carries only its path plus small scalars; test-batch_skeletons_production.R
#' pins both the single write and the item-size bound.
#'
#' @param snapshot_path Path to the study snapshot (`qs2_write_atomic()` of the
#'   `RegistryStudy`), written once per `process_skeletons()` call.
#' @param batch_idx Integer batch number to process.
#' @param framework_hash,trim_hash,phase_order,randvars_hashes,current_fps Pipeline
#'   identity, passed through to [.process_one_batch()] (computed once in the
#'   parent -- stable across the whole run).
#' @param n_threads data.table threads for this worker (per-worker share,
#'   decided by the parent).
#' @return `NULL`, invisibly -- skeleton + meta land on disk.
#' @noRd
.process_one_batch_snapshot <- function(
  snapshot_path,
  batch_idx,
  framework_hash,
  trim_hash,
  phase_order,
  randvars_hashes,
  current_fps,
  n_threads
) {
  data.table::setDTthreads(n_threads)
  study <- qs2_read(snapshot_path)
  .process_one_batch(
    study = study,
    i = batch_idx,
    framework_hash = framework_hash,
    trim_hash = trim_hash,
    phase_order = phase_order,
    randvars_hashes = randvars_hashes,
    current_fps = current_fps
  )
  invisible(NULL)
}

# The `framework_hash`, `trim_hash`, `phase_order`, `randvars_hashes`, and
# `current_fps` arguments are passed in rather than recomputed per batch. They
# are stable across the whole process_skeletons() run, so computing them once
# up-front is cheaper.
.process_one_batch <- function(
  study,
  i,
  framework_hash,
  trim_hash,
  phase_order,
  randvars_hashes,
  current_fps
) {
  # Meta-only fast path: read the few-KB sidecar before touching the
  # full skeleton. If every stored hash matches the current pipeline
  # AND every currently-registered population by-spec is already
  # cached in the meta, this batch is fully up to date and we return
  # without paying the full deserialise cost.
  meta <- study$load_skeleton_meta(i)
  pipeline_ok <- .meta_matches_pipeline(
    meta,
    framework_hash,
    trim_hash,
    phase_order,
    randvars_hashes,
    current_fps
  )
  specs_ok <- .meta_has_all_specs(meta, study$population_by_specs)
  if (pipeline_ok && specs_ok) {
    return(invisible(NULL))
  }

  # Meta-only refresh: skeleton work already on disk is still valid;
  # only the meta is stale (one or more registered population specs
  # missing from its population_aggregations). Reload the skeleton
  # from disk, rewrite the meta with fresh aggregations -- skip
  # framework / randvars / codes entirely.
  if (pipeline_ok && !specs_ok) {
    sk <- study$load_skeleton(i)
    if (is.null(sk)) {
      stop(
        "Meta-only refresh requested for batch ",
        i,
        " but skeleton file is missing on disk.",
        call. = FALSE
      )
    }
    study$write_skeleton_meta(sk)
    return(invisible(sk))
  }

  sk <- study$load_skeleton(i)
  batch_data <- NULL
  load_bd <- function() {
    if (is.null(batch_data)) {
      batch_data <<- study$load_rawbatch(i)
    }
    batch_data
  }

  # Phase 1: framework — full rebuild on hash change (or when no
  # skeleton exists yet for this batch). A trim change rebuilds too:
  # the trim deletes rows, and rewinding a deletion is impossible, so
  # the only correct answer is a fresh base.
  #
  # A phase-order change rebuilds for the same reason. A skeleton written
  # under the old order reads NULL, and its randvars columns hold whatever
  # the steps computed with no code column in sight. Only a fresh base
  # replays them against the code columns.
  if (
    is.null(sk) ||
      !identical(sk$framework_fn_hash, framework_hash) ||
      !identical(sk$trim_fn_hash, trim_hash) ||
      !identical(sk$phase_order, phase_order)
  ) {
    bd <- load_bd()
    base_dt <- study$framework_fn(bd, study)
    if (!data.table::is.data.table(base_dt)) {
      stop(
        "framework_fn must return a data.table; got ",
        paste(class(base_dt), collapse = "/"),
        " for batch ",
        i,
        call. = FALSE
      )
    }
    sk <- Skeleton$new(data = base_dt, batch_number = i)
    sk$framework_fn_hash <- framework_hash
    sk$trim_fn_hash <- trim_hash
    sk$phase_order <- phase_order
    # New base -> phase-2 and phase-3 state must re-apply
    sk$applied_registry <- list()
    sk$randvars_state <- list()

    # Phase 1b: trim. This is the one declared place that may delete
    # skeleton rows. It runs on the fresh base, after the framework and
    # before the code registry. Every code entry then sees the row set
    # the trim leaves behind.
    #
    # It sits INSIDE the rebuild block on purpose. The gate above
    # rebuilds whenever the trim's identity changes, so a fresh base is
    # the only input the trim ever sees. Move it outside this block and
    # an unrelated code edit re-runs it against data it already trimmed.
    # Any trim that is not a pure predicate filter then deletes more
    # rows on every such edit.
    if (!is.null(study$trim_fn)) {
      trimmed <- study$trim_fn(sk$data, bd, study)
      if (!data.table::is.data.table(trimmed)) {
        stop(
          "trim_fn must return a data.table; got ",
          paste(class(trimmed), collapse = "/"),
          " for batch ",
          i,
          call. = FALSE
        )
      }
      sk$data <- trimmed
    }
  }

  # Phase 2: codes. Incremental per-entry sync. It runs BEFORE randvars,
  # so a randvars step may read the columns it writes.
  sk$sync_with_registry(
    current_fps = current_fps,
    registry = study$code_registry,
    batch_data_loader = load_bd,
    id_col = study$id_col
  )

  # Phase 3: randvars. Divergence-point rewind and replay.
  #
  # $randvars_hashes() folds the code registry fingerprints into every
  # step's hash. So a registry edit moves every hash, the rewind starts at
  # the first step, and the replay reads the freshly-applied code columns.
  sk$sync_randvars(
    randvars_fns = study$randvars_fns,
    randvars_hashes = randvars_hashes,
    batch_data_loader = load_bd,
    config = study
  )

  study$save_skeleton(sk)
  invisible(sk)
}
