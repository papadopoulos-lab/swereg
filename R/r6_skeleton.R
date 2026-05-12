.SKELETON_SCHEMA_VERSION <- 1L

#' Skeleton: per-batch time grid + derived columns with provenance
#'
#' @description
#' A `Skeleton` is a single batch's person-week data.table plus its full
#' provenance: the hash of the framework function that built the base time
#' grid, an ordered record of every randvars function that has been applied
#' to it, and a fingerprint map of every code_registry entry whose columns
#' currently live in the data.
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
    #'   own stored provenance. Invariant:
    #'   `sk$pipeline_hash() == study$pipeline_hash()` iff the skeleton is
    #'   fully synced with the study's currently-registered framework +
    #'   randvars + codes.
    #' @return A single character string (xxhash64 digest).
    pipeline_hash = function() {
      digest::digest(
        list(
          framework = self$framework_fn_hash,
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
      self$applied_registry[[fingerprint]] <- if (
        identical(entry$kind %||% "primary", "derived")
      ) {
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
          result <- fn(self$data, batch_data, config)
          if (data.table::is.data.table(result)) {
            self$data <- result
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
      qs2::qs_save(self, path, nthreads = parallel::detectCores())
      invisible(path)
    },

    #' @description Print a compact summary of this skeleton.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Skeleton batch ", self$batch_number, ">\n", sep = "")
      cat("  rows:             ", format(nrow(self$data), big.mark = ","), "\n", sep = "")
      cat("  cols:             ", ncol(self$data), "\n", sep = "")
      cat("  framework_hash:   ", substr(self$framework_fn_hash %||% "(none)", 1, 12), "\n", sep = "")
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
