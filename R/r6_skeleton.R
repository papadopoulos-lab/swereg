.SKELETON_SCHEMA_VERSION <- 1L

#' Skeleton: per-batch time grid + derived columns with provenance
#'
#' @description
#' A `Skeleton` is a single batch's person-week data.table plus its full
#' provenance. The provenance is five things:
#' \itemize{
#'   \item the hash of the framework function that built the base time grid
#'   \item the identity of the trim function that deleted rows from it
#'   \item the phase order that produced it
#'   \item an ordered record of every randvars function applied to it
#'   \item a fingerprint map of every code_registry entry whose columns
#'     live in the data
#' }
#'
#' This is the on-disk unit produced by [RegistryStudy]`$process_skeletons()`.
#' One file per batch.
#'
#' `Skeleton` objects are rarely constructed directly. Use
#' [RegistryStudy]`$load_skeleton(batch_number)` to read one from disk and
#' [RegistryStudy]`$save_skeleton(sk)` to write one back.
#'
#' @section Phase provenance fields:
#' \describe{
#'   \item{`framework_fn_hash`}{xxhash64 of `list(body(fn), formals(fn))`
#'     for the framework function that built `self$data`. Used by
#'     `$process_skeletons()` to decide whether to rebuild this batch from
#'     scratch (phase 1) when the framework code has changed.}
#'   \item{`trim_fn_hash`}{Identity of the trim function (phase 1b) that
#'     ran on `self$data`. Three values, and each means something
#'     different:
#'     \itemize{
#'       \item An xxhash64 digest: that trim function ran.
#'       \item `"__swereg_no_trim__"`: the study registered no trim, and
#'         this skeleton was built by a swereg that knows about trims.
#'       \item `NULL`: this skeleton was written before the trim phase
#'         existed. `$process_skeletons()` rebuilds it once.
#'     }
#'     The last two MUST stay distinct. If both were `NULL`, adding a trim
#'     to an existing study would rebuild nothing.}
#'   \item{`phase_order`}{Character vector naming the order the phases ran
#'     in. This swereg writes `c("framework", "codes", "randvars")`. A
#'     skeleton written by a swereg that ran the code registry after
#'     randvars carries `NULL`, and `$process_skeletons()` rebuilds it
#'     once. The rebuild is the only correct answer. A randvars step may
#'     read a code column, and no rewind can add a value the old order
#'     never wrote.}
#'   \item{`applied_registry`}{Named list keyed by code_registry entry
#'     fingerprint. Each value is a minimal descriptor sufficient to
#'     recompute the entry's column names via `.entry_columns()` at drop
#'     time, without re-running `fn`:
#'     \itemize{
#'       \item Primary entries (from `$register_codes()`) store
#'         `list(codes, groups, combine_as, label, fn_args)`.
#'       \item Derived entries (from `$register_derived_codes()`) store
#'         `list(kind = "derived", codes, from, as, label)`. `.entry_columns()`
#'         branches on the entry's `kind` field (defaulting to `"primary"`
#'         when absent) so both shapes produce the right column
#'         predictions at drop time.
#'     }
#'     The entry's `fn` is NOT stored -- serializing R function objects
#'     carries enclosing-environment bloat and we never call `fn` at
#'     drop time anyway.}
#'   \item{`randvars_state`}{Named ordered list, one entry per phase-3 step
#'     that's been applied. Each value is `list(fn_hash = ..., added_columns
#'     = ...)`. `fn_hash` is the hash of the function that ran; `added_columns`
#'     is the character vector of column names it wrote, recorded via a
#'     before/after diff at apply time (since randvars functions are
#'     arbitrary user code whose outputs can't be predicted from metadata).}
#' }
#'
#' @examples
#' \dontrun{
#' # Load a persisted skeleton from disk and inspect its provenance.
#' sk <- study$load_skeleton(batch_number = 1L)
#' sk                              # print summary
#' sk$data                         # the underlying data.table
#' sk$framework_fn_hash            # hash of the phase-1 fn that built it
#' sk$trim_fn_hash                 # identity of the phase-1b trim
#' sk$phase_order                  # the order the phases ran in
#' names(sk$randvars_state)        # applied phase-3 steps in order
#' length(sk$applied_registry)     # applied code registry entries
#' sk$pipeline_hash()              # rolled-up provenance scalar
#'
#' # Check consistency with the study's current pipeline.
#' identical(sk$pipeline_hash(), study$pipeline_hash())
#'
#' # Write back after manual editing (rare; process_skeletons handles
#' # this automatically).
#' study$save_skeleton(sk)
#' }
#'
#' @seealso [RegistryStudy] for the pipeline that produces and consumes
#'   `Skeleton` objects; [CandidatePath] for the directory resolution
#'   mechanism behind `study$load_skeleton()` / `$save_skeleton()`.
#' @family skeleton_pipeline
#' @export
Skeleton <- R6::R6Class(
  "Skeleton",
  public = list(
    #' @field data The underlying `data.table` (time grid + derived columns).
    data = NULL,

    #' @field batch_number Integer batch index.
    batch_number = NULL,

    #' @field framework_fn_hash xxhash64 of the framework function that
    #'   built `self$data`.
    framework_fn_hash = NULL,

    #' @field trim_fn_hash Identity of the trim function (phase 1b) that
    #'   ran on `self$data`. An xxhash64 digest, or the sentinel
    #'   `"__swereg_no_trim__"` when the study registers no trim, or
    #'   `NULL` when this skeleton predates the trim phase.
    trim_fn_hash = NULL,

    #' @field phase_order Character vector naming the order the phases ran
    #'   in. `$process_skeletons()` stamps it on every rebuild, exactly as
    #'   it stamps `framework_fn_hash`. `NULL` on a fresh object, and on a
    #'   skeleton that predates the move of the code registry ahead of
    #'   randvars.
    phase_order = NULL,

    #' @field applied_registry Named list (keyed by code_registry entry
    #'   fingerprint). Each value is a minimal descriptor: for primary
    #'   entries it's `list(codes, groups, combine_as, label, fn_args)`;
    #'   for derived entries (from `$register_derived_codes()`) it's
    #'   `list(kind = "derived", codes, from, as, label)`. See the
    #'   class-level "Phase provenance fields" section for why both
    #'   shapes omit `fn`.
    applied_registry = NULL,

    #' @field randvars_state Named ordered list, one entry per phase-3 step
    #'   that's been applied. Each value is
    #'   `list(fn_hash = ..., added_columns = ...)`.
    randvars_state = NULL,

    #' @field created_at POSIXct timestamp for when this `Skeleton` object
    #'   was constructed.
    created_at = NULL,

    #' @description Construct a new `Skeleton` wrapping an existing
    #'   `data.table`. Typically called by [RegistryStudy]`$process_skeletons()`
    #'   after the framework function produces the base time grid.
    #' @param data The base `data.table` to wrap.
    #' @param batch_number Integer batch index.
    initialize = function(data, batch_number) {
      if (!data.table::is.data.table(data)) {
        stop("data must be a data.table", call. = FALSE)
      }
      self$data               <- data
      self$batch_number       <- as.integer(batch_number)
      self$framework_fn_hash  <- NULL
      self$trim_fn_hash       <- NULL
      self$phase_order        <- NULL
      self$applied_registry   <- list()
      self$randvars_state     <- list()
      self$created_at         <- Sys.time()
      private$.schema_version <- .SKELETON_SCHEMA_VERSION
      invisible(self)
    },

    #' @description Check this object's schema version against the current
    #'   `Skeleton` schema version. Errors with an actionable migration
    #'   message on mismatch.
    check_version = function() {
      current <- .SKELETON_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        stop(
          class(self)[1], " on disk has schema version ", saved,
          " but this swereg requires version ", current, ".\n",
          "Run study$delete_skeletons() and re-run $process_skeletons() to regenerate.",
          call. = FALSE
        )
      }
      invisible(TRUE)
    },

    #' @description Compute this skeleton's total pipeline hash from its
    #'   own stored provenance.
    #'
    #'   `sk$pipeline_hash() == study$pipeline_hash()` is necessary for a
    #'   synced skeleton. It is not sufficient. Unequal hashes mean the
    #'   skeleton is definitely stale. Equal hashes mean only that
    #'   nothing changed among the inputs both hashes cover. Those
    #'   inputs are the framework function, the trim identity, the phase
    #'   order, the randvars sequence and the code registry
    #'   fingerprints.
    #'
    #'   Two inputs sit outside both hashes: the rawbatch data, and
    #'   whatever a registered function calls or reads from its
    #'   environment. A change to either one leaves the hashes equal
    #'   over a stale skeleton. See [RegistryStudy]`$randvars_hashes()`
    #'   for why.
    #'
    #'   A skeleton written before `phase_order` existed carries `NULL`
    #'   there, so its hash differs and
    #'   `$assert_skeletons_consistent()` names it.
    #' @return A single character string (xxhash64 digest).
    pipeline_hash = function() {
      digest::digest(
        list(
          framework = self$framework_fn_hash,
          trim = self$trim_fn_hash,
          phase_order = self$phase_order,
          randvars = vapply(
            self$randvars_state,
            function(x) x$fn_hash %||% NA_character_,
            character(1)
          ),
          codes = names(self$applied_registry) %||% character(0)
        ),
        algo = "xxhash64"
      )
    },

    #' @description Apply one code_registry entry to `self$data`, mutating
    #'   it in place, and record a minimal descriptor of the entry under
    #'   its fingerprint so a future `$drop_code_entry(fingerprint)` call
    #'   knows which columns to remove. The stored descriptor shape
    #'   depends on `entry$kind`: primary entries store the
    #'   `codes/groups/combine_as/label/fn_args` quintuple, derived
    #'   entries store `list(kind = "derived", codes, from, as, label)`.
    #'   For derived entries, `batch_data` is unused -- the apply just
    #'   ORs already-existing skeleton columns under new names.
    #' @param entry A code_registry entry (as constructed by
    #'   [RegistryStudy]`$register_codes()` or
    #'   [RegistryStudy]`$register_derived_codes()`).
    #' @param batch_data Named list of data.tables from
    #'   [RegistryStudy]`$load_rawbatch()`. Ignored for derived entries.
    #' @param id_col Character. Person-ID column name.
    #' @param fingerprint Character. The xxhash64 fingerprint for `entry`
    #'   (computed by [RegistryStudy]`$code_registry_fingerprints()`).
    apply_code_entry = function(entry, batch_data, id_col, fingerprint) {
      .apply_code_entry_impl(self$data, batch_data, entry, id_col)

      # No per-column counts here. A phase-3 randvar can delete rows
      # after this point, which makes an apply-time count describe rows
      # the written skeleton no longer holds.
      # `$refresh_code_entry_counts()` fills `$counts` in once, from the
      # final data, and `RegistryStudy$save_skeleton()` calls it before
      # either file is written.
      base <- if (identical(entry$kind %||% "primary", "derived")) {
        list(
          kind  = "derived",
          codes = entry$codes,
          from  = entry$from,
          as    = entry$as,
          label = entry$label
        )
      } else {
        list(
          codes      = entry$codes,
          groups     = entry$groups,
          combine_as = entry$combine_as,
          label      = entry$label,
          fn_args    = entry$fn_args
        )
      }
      self$applied_registry[[fingerprint]] <- base
      invisible(self)
    },

    #' @description Recompute the per-column counts of every applied code
    #'   entry from this skeleton's current data. Call it after the last
    #'   phase runs, so the counts describe the skeleton that gets
    #'   written.
    #'
    #'   `$apply_code_entry()` records no counts.
    #'   [RegistryStudy]`$save_skeleton()` is the one site that computes
    #'   them. It calls this method before it writes the skeleton file
    #'   and the meta sidecar, so both files report the same data.
    #'
    #'   Column names come from `.entry_columns()` on each stored
    #'   descriptor, which is the prediction `$drop_code_entry()` also
    #'   uses. The method skips a predicted column that the data does not
    #'   hold.
    #' @return This `Skeleton`, invisibly.
    refresh_code_entry_counts = function() {
      for (fp in names(self$applied_registry)) {
        stored <- self$applied_registry[[fp]]
        cols <- intersect(.entry_columns(stored), names(self$data))
        self$applied_registry[[fp]]$counts <- .compute_entry_column_counts(
          self$data,
          cols
        )
      }
      invisible(self)
    },

    #' @description Drop every column that the registry entry with the
    #'   given fingerprint contributed to `self$data`, and clear its
    #'   descriptor from `self$applied_registry`. Columns are computed
    #'   from the stored descriptor via `.entry_columns()` -- no lookup
    #'   map, no before/after diff.
    #'
    #'   Tolerates missing columns (e.g. after a partial-state crash): the
    #'   column set is intersected with `names(self$data)` before dropping,
    #'   so the method is a safe idempotent operation.
    #' @param fingerprint Character. Fingerprint of the entry to drop.
    drop_code_entry = function(fingerprint) {
      stored <- self$applied_registry[[fingerprint]]
      if (is.null(stored)) return(invisible(self))
      cols_present <- intersect(.entry_columns(stored), names(self$data))
      if (length(cols_present)) {
        self$data[, (cols_present) := NULL]
      }
      self$applied_registry[[fingerprint]] <- NULL
      invisible(self)
    },

    #' @description Bring this skeleton into sync with the given code
    #'   registry (phase 2 of `$process_skeletons()`). Entries in
    #'   `stored - current` are dropped (their columns removed via
    #'   `.entry_columns()` on the stored descriptor). Entries in
    #'   `current - stored` are applied via `$apply_code_entry()`.
    #'
    #'   "Changed" entries -- same `label` but different `codes` / `groups`
    #'   / etc. -- are handled automatically without special casing: their
    #'   old fingerprint lives in `stored` (so the old descriptor's columns
    #'   get dropped) and their new fingerprint lives in `current` (so the
    #'   new entry gets freshly applied).
    #'
    #'   Rawbatches are loaded lazily via `batch_data_loader`: if no new
    #'   entries need to be applied, the loader is never called.
    #' @param current_fps Character vector of fingerprints for the current
    #'   registry, in registry order.
    #' @param registry The current `RegistryStudy$code_registry` list.
    #' @param batch_data_loader Zero-argument closure returning the
    #'   rawbatch data for this batch.
    #' @param id_col Character. Person-ID column name.
    sync_with_registry = function(current_fps, registry, batch_data_loader, id_col) {
      stored_fps <- names(self$applied_registry)

      for (fp in setdiff(stored_fps, current_fps)) {
        self$drop_code_entry(fp)
      }

      to_add <- setdiff(current_fps, stored_fps)
      if (length(to_add) == 0L) return(invisible(self))

      batch_data <- batch_data_loader()
      for (i in seq_along(registry)) {
        fp <- current_fps[[i]]
        if (fp %in% to_add) {
          self$apply_code_entry(registry[[i]], batch_data, id_col, fp)
        }
      }
      invisible(self)
    },

    #' @description Bring this skeleton into sync with the currently-
    #'   registered phase-3 step sequence (phase 3 of
    #'   `$process_skeletons()`).
    #'
    #'   Uses "divergence-point + rewind and replay" semantics:
    #'   1. Scan the stored step sequence (`names(self$randvars_state)` +
    #'      stored `fn_hash`s) against the current sequence
    #'      (`names(randvars_fns)` + `randvars_hashes`). Find the first
    #'      position where the name or hash differs, or where one sequence
    #'      ends.
    #'   2. Rewind: drop the stored `added_columns` of every step from the
    #'      divergence point forward, in stored order.
    #'   3. Replay: run the current steps from the divergence point
    #'      forward, in current order, recording each step's hash + new
    #'      `added_columns`.
    #'
    #'   This handles add, remove, edit, and reorder uniformly because any
    #'   of those operations changes either the name sequence or the hash
    #'   sequence, and the first mismatch point is the divergence point.
    #'   When no divergence exists, the method is a no-op and
    #'   `batch_data_loader` is never called.
    #'
    #'   A step MUST NOT change the row count. The method compares `nrow`
    #'   before and after each replayed function. It stops when the count
    #'   moves, and names the step. Row deletion belongs to the trim
    #'   registered with [RegistryStudy]`$register_trim()`, which runs on a
    #'   fresh base before the code registry.
    #' @param randvars_fns Named ordered list of phase-3 functions (from
    #'   `RegistryStudy$randvars_fns`).
    #' @param randvars_hashes Character vector parallel to `randvars_fns`
    #'   with the xxhash64 of each function's body + formals.
    #' @param batch_data_loader Zero-argument closure returning the
    #'   rawbatch data for this batch.
    #' @param config The owning `RegistryStudy` (passed as the third
    #'   argument to each randvars function).
    sync_randvars = function(randvars_fns, randvars_hashes, batch_data_loader, config) {
      stored <- self$randvars_state
      new_names <- names(randvars_fns)
      old_names <- names(stored)
      n <- max(length(new_names), length(old_names))

      # Find the divergence point
      diverge_at <- NA_integer_
      if (n > 0L) {
        for (i in seq_len(n)) {
          if (i > length(new_names) || i > length(old_names)) {
            diverge_at <- i; break
          }
          if (!identical(new_names[[i]], old_names[[i]])) {
            diverge_at <- i; break
          }
          if (!identical(
            stored[[old_names[[i]]]]$fn_hash,
            unname(randvars_hashes[[new_names[[i]]]])
          )) {
            diverge_at <- i; break
          }
        }
      }
      if (is.na(diverge_at)) return(invisible(self))

      # Rewind: drop stored columns of every step from diverge_at forward
      if (diverge_at <= length(old_names)) {
        for (j in diverge_at:length(old_names)) {
          old_nm <- old_names[[j]]
          cols <- stored[[old_nm]]$added_columns
          cols_present <- intersect(cols, names(self$data))
          if (length(cols_present)) self$data[, (cols_present) := NULL]
          self$randvars_state[[old_nm]] <- NULL
        }
      }

      # Replay: run current steps from diverge_at forward.
      #
      # The user fn is expected to mutate `self$data` in place via
      # data.table `:=` semantics, but real-world fns sometimes FILTER
      # rows with `skeleton <- skeleton[cond]`, which rebinds the local
      # variable to a NEW data.table that the caller never sees unless
      # we capture the return value. We accept either form: if the fn
      # returns a data.table, we rebind `self$data` to it; otherwise we
      # assume the mutation happened in place.
      if (diverge_at <= length(new_names)) {
        batch_data <- batch_data_loader()
        for (j in diverge_at:length(new_names)) {
          new_nm <- new_names[[j]]
          fn <- randvars_fns[[new_nm]]
          before <- copy(names(self$data))
          nrow_before <- nrow(self$data)
          result <- fn(self$data, batch_data, config)
          if (data.table::is.data.table(result)) {
            self$data <- result
          }
          # Row count check, per step, after the rebind and before the
          # provenance record. `new_nm` is in hand here, so the error can
          # name the step the user has to edit. A stop here also leaves
          # `self$randvars_state` without an entry for a step that never
          # completed.
          if (nrow(self$data) != nrow_before) {
            stop(
              "$register_randvars(\"", new_nm, "\") changed skeleton row ",
              "count: before = ", nrow_before, ", after = ", nrow(self$data),
              ". A randvars step MUST NOT change the row count. ",
              .row_deletion_guidance(),
              call. = FALSE
            )
          }
          self$randvars_state[[new_nm]] <- list(
            fn_hash       = unname(randvars_hashes[[new_nm]]),
            added_columns = setdiff(names(self$data), before)
          )
        }
      }

      invisible(self)
    },

    #' @description Save this `Skeleton` to disk as
    #'   `skeleton_NNN.qs2` inside `dir`. Prefer
    #'   [RegistryStudy]`$save_skeleton(sk)` which supplies
    #'   `self$data_skeleton_dir` automatically.
    #' @param dir Character. Destination directory.
    #' @return The full path the file was written to, invisibly.
    save = function(dir) {
      path <- file.path(dir, sprintf("skeleton_%05d.qs2", self$batch_number))
      qs2_write_atomic(self, path, nthreads = .safe_n_cores())
      invisible(path)
    },

    #' @description Print a compact summary of this skeleton.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Skeleton batch ", self$batch_number, ">\n", sep = "")
      cat("  rows:             ", format(nrow(self$data), big.mark = ","), "\n", sep = "")
      cat("  cols:             ", ncol(self$data), "\n", sep = "")
      cat("  framework_hash:   ", substr(self$framework_fn_hash %||% "(none)", 1, 12), "\n", sep = "")
      cat("  trim_hash:        ", substr(self$trim_fn_hash %||% "(pre-trim)", 1, 12), "\n", sep = "")
      cat("  phase_order:      ", .format_phase_order(self$phase_order, "(none)"), "\n", sep = "")
      cat("  randvars steps:   ", length(self$randvars_state), "\n", sep = "")
      cat("  applied codes:    ", length(self$applied_registry), "\n", sep = "")
      pipeline_hash <- tryCatch(self$pipeline_hash(), error = function(e) "(error)")
      cat("  pipeline_hash:    ", substr(pipeline_hash, 1, 12), "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    .schema_version = NULL
  )
)
