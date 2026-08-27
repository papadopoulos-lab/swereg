# Path resolution (first_existing_path / invalidate_candidate_paths) lives in
# R/path_resolution.R. Directory candidate state is held inside CandidatePath
# instances -- see R/r6_candidate_path.R.

.REGISTRY_STUDY_SCHEMA_VERSION <- 6L

# =============================================================================
# RegistryStudy R6 Class
# =============================================================================
# Unified class managing the full skeleton pipeline lifecycle:
#   - Batch configuration
#   - Runtime state
#   - Declarative code registry (register_codes)
#   - Batch processing with parallel support
#
# Directory layout: the constructor takes data_rawbatch_dir and data_skeleton_dir
# as candidate paths. Each is the exact directory for rawbatch or skeleton files.
# The first existing candidate is used; if only one candidate is given and it
# does not exist, it is created.
#
# Directory resolution is portable across machines: the constructor stores
# candidate paths, and active bindings lazily resolve to the first existing
# directory. Resolved paths are cached but auto-invalidated when the cached
# path no longer exists.
#
# Code registry: each register_codes() call declares codes, the function to
# apply them, which data groups to use, and optional prefixing/combining.
# =============================================================================

#' RegistryStudy: Unified R6 class for skeleton pipeline
#'
#' Manages the full skeleton pipeline lifecycle: portable batch
#' directories, batch splitting, raw registry loading, the declarative
#' code registry, and the orchestrated per-batch processing
#' (framework -> trim -> codes -> randvars) that produces one [Skeleton]
#' file per batch with incremental invalidation.
#'
#' @section Portable Directory Resolution:
#' Directories are stored as candidate path vectors and resolved lazily via
#' [CandidatePath] active bindings. The first existing directory wins and is
#' cached. If the cached path becomes invalid (e.g. after moving to a
#' different machine), the binding automatically re-resolves from the
#' candidate list.
#'
#' @section Per-batch pipeline:
#' `$process_skeletons()` runs four phases per batch, with per-phase
#' incremental invalidation so editing one step only re-runs what it
#' affects:
#' \describe{
#'   \item{Phase 1 -- framework}{A single user function registered via
#'     `$register_framework(fn)`, signature `(batch_data, config)`, returns
#'     a fresh base `data.table` (time grid + structural censoring).
#'     Full rebuild on `body(fn)` / `formals(fn)` hash change.}
#'   \item{Phase 1b -- trim}{At most one user function registered via
#'     `$register_trim(fn)`, signature `(skeleton, batch_data, config)`,
#'     returning a `data.table`. It is the one declared place in the
#'     pipeline that MAY delete skeleton rows. It runs immediately after
#'     the framework and before the code registry, so every code entry
#'     sees the final row set. A change to the registered trim rebuilds
#'     the base, because a deletion cannot be rewound. It runs only on a
#'     rebuild, so it always reads a fresh base.}
#'   \item{Phase 2 -- codes}{The declarative code registry, built via
#'     `$register_codes()` (primary) and `$register_derived_codes()`
#'     (derived). Per-entry fingerprint diff: entries no longer present
#'     are dropped, new or modified entries are freshly applied. Derived
#'     entry fingerprints fold in their upstream primary fingerprints so
#'     upstream behavior edits cascade correctly.}
#'   \item{Phase 3 -- randvars}{An ordered named list of user functions
#'     registered via `$register_randvars(name, fn)`, each signature
#'     `(skeleton, batch_data, config)`. Divergence-point rewind-and-replay
#'     invalidation: the first step whose name or hash differs from the
#'     stored sequence triggers a drop of its columns and replay of it plus
#'     everything downstream of it. Add/remove/edit/reorder all handled
#'     uniformly.}
#' }
#' Phase 2 runs BEFORE phase 3, so a phase-3 step MAY read a phase-2
#' column. `$randvars_hashes()` folds the code registry fingerprints into
#' every step's hash, so a registry edit replays every step against the
#' new columns. See the [Skeleton] class for the on-disk provenance
#' format.
#'
#' The order is recorded on each [Skeleton] as `phase_order`. A skeleton
#' written under the old order carries `NULL` there, and
#' `$process_skeletons()` rebuilds it once.
#'
#' Only phase 1b may delete rows. A phase-3 step that filters rows breaks
#' the rewind-and-replay contract, because rewind drops columns and cannot
#' restore rows. Move any such filter into `$register_trim()`.
#'
#' @section Code Registry:
#' Primary entries are registered via `$register_codes()`, which declares
#' codes, the function to apply them (e.g. `add_diagnoses`, `add_cods`),
#' which rawbatch groups to use, and optional prefixing/combining.
#' Derived entries are registered via `$register_derived_codes()` and
#' OR together already-existing skeleton columns from upstream primary
#' entries -- useful when the combined column needs to draw from
#' registrations that use DIFFERENT `fn`s (something `combine_as` can't
#' express because it re-runs the same `fn` on rbound data).
#'
#' @examples
#' \dontrun{
#' study <- RegistryStudy$new(
#'   data_rawbatch_dir = c("/linux/.../rawbatch/", "C:/win/.../rawbatch/"),
#'   data_skeleton_dir = c("/linux/.../skeleton/", "C:/win/.../skeleton/"),
#'   data_raw_dir      = c("/linux/.../raw/",      "C:/win/.../raw/"),
#'   group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors")
#' )
#'
#' # Phase 1: framework (structural time grid + censoring)
#' study$register_framework(my_framework_fn)
#'
#' # Phase 1b: trim (the only place rows may be deleted)
#' study$register_trim(my_trim_fn)
#'
#' # Phase 2: codes. Primary entries first, derived entries after.
#' study$register_codes(
#'   codes      = list(e11 = c("E11"), vte = c("I26", "I80")),
#'   fn         = swereg::add_diagnoses,
#'   groups     = list(ov = "outpatient", sv = "inpatient"),
#'   combine_as = "os"
#' )
#' study$register_codes(
#'   codes   = list(e11 = c("E11"), vte = c("I26", "I80")),
#'   fn      = swereg::add_cods,
#'   fn_args = list(cod_type = "underlying"),
#'   groups  = list(dorsu = "dors")
#' )
#' study$register_codes(
#'   codes   = list(e11 = c("E11"), vte = c("I26", "I80")),
#'   fn      = swereg::add_cods,
#'   fn_args = list(cod_type = "multiple"),
#'   groups  = list(dorsm = "dors")
#' )
#' # Build osd_e11 = os_e11 | dorsu_e11 | dorsm_e11 (same codes list
#' # shared by reference so an edit in one place cascades to all four)
#' study$register_derived_codes(
#'   codes = list(e11 = c("E11"), vte = c("I26", "I80")),
#'   from  = c("os", "dorsu", "dorsm"),
#'   as    = "osd"
#' )
#'
#' # Phase 3: randvars (ordered user steps; order = execution order).
#' # They run after the code registry, so they may read its columns.
#' study$register_randvars("demographics", my_demographics_fn)
#' study$register_randvars("exposure",     my_exposure_fn)
#'
#' study$set_ids(ids)
#' study$save_rawbatch("lmed", lmed_data)
#' study$describe_codes()
#' study$process_skeletons(n_workers = 4L)
#'
#' # Per-batch provenance and cross-batch consistency check
#' sk <- study$load_skeleton(1L)
#' sk$pipeline_hash() == study$pipeline_hash()  # FALSE => definitely stale
#' study$assert_skeletons_consistent()          # errors on mixed state
#' }
#'
#' @seealso [Skeleton] for the per-batch on-disk format and provenance
#'   fields; [CandidatePath] for the multi-host directory resolution
#'   mechanism; [add_diagnoses], [add_cods], [add_rx] for common `fn`
#'   choices in `$register_codes()`.
#' @family skeleton_pipeline
#' @export
RegistryStudy <- R6::R6Class(
  "RegistryStudy",
  public = list(
    # --- Config fields (set at construction, rarely changed) ---

    #' @field group_names Character vector. Names of rawbatch groups.
    group_names = NULL,

    #' @field batch_size Integer. Number of IDs per batch.
    batch_size = NULL,

    #' @field seed Integer. Shuffle seed for reproducibility.
    seed = NULL,

    #' @field id_col Character. Person ID column name.
    id_col = NULL,

    # --- Runtime state ---

    #' @field n_ids Integer. Total number of IDs across all batches.
    n_ids = NULL,

    #' @field n_batches Integer. Number of batches.
    n_batches = NULL,

    #' @field batch_id_list List of ID vectors, one per batch.
    batch_id_list = NULL,

    #' @field groups_saved Character vector of rawbatch groups saved to disk.
    groups_saved = NULL,

    # --- Code registry ---

    #' @field code_registry List of code registration entries, appended
    #'   to by `$register_codes()` and `$register_derived_codes()`.
    #'   Primary entries (from `$register_codes()`) are plain lists
    #'   with: `codes, fn, fn_args, groups, combine_as, label`. Derived
    #'   entries (from `$register_derived_codes()`) are tagged with
    #'   `kind = "derived"` and hold `codes, from, as, label` instead
    #'   -- no `fn`, no `groups`, no raw data access. The dispatcher
    #'   `.apply_code_entry_impl()` branches on the entry's `kind`
    #'   field, defaulting to `"primary"` when absent.
    code_registry = NULL,

    #' @field created_at POSIXct. Timestamp when this study was created.
    created_at = NULL,

    # --- Directory candidates (CandidatePath instances) ---

    #' @field data_rawbatch_cp [CandidatePath] for the rawbatch directory.
    data_rawbatch_cp = NULL,

    #' @field data_skeleton_cp [CandidatePath] for the skeleton directory.
    data_skeleton_cp = NULL,

    #' @field data_meta_cp [CandidatePath] for the metadata directory
    #'   (holds `registrystudy.qs2`). Defaults to the rawbatch directory
    #'   for backward compatibility.
    data_meta_cp = NULL,

    #' @field data_raw_cp [CandidatePath] for the raw-registry directory,
    #'   or NULL if not configured.
    data_raw_cp = NULL,

    #' @field data_summaries_cp [CandidatePath] for the audit-track
    #'   summaries directory (git-tracked TSV per full run), or NULL if
    #'   the feature is not configured. When NULL, `$compute_summary()`
    #'   still writes the local `summary.qs2` and `status.txt` but skips
    #'   the TSV.
    data_summaries_cp = NULL,

    # --- Phase-1 and phase-3 registration ---

    #' @field framework_fn Function of signature `(batch_data, config)`
    #'   returning a fresh base skeleton `data.table` (phase 1). Set via
    #'   `$register_framework()`. `$process_skeletons()` re-runs this
    #'   function per batch when its body/formals hash changes.
    framework_fn = NULL,

    #' @field trim_fn Function of signature `(skeleton, batch_data, config)`
    #'   returning a `data.table` (phase 1b), or NULL. Set via
    #'   `$register_trim()`. It is the one place in the pipeline that may
    #'   delete skeleton rows.
    trim_fn = NULL,

    #' @field randvars_fns Named ordered list of phase-3 functions, each
    #'   with signature `(skeleton, batch_data, config)`. Populated via
    #'   `$register_randvars(name, fn)`. Registration order is execution
    #'   order. `$process_skeletons()` uses
    #'   `Skeleton$sync_randvars()`'s divergence-point rewind-and-replay
    #'   to apply changes incrementally.
    randvars_fns = NULL,

    #' @field skeleton_manifest List, or NULL. The commit record for the
    #'   skeleton dataset: proof that `$process_skeletons()` ran to completion
    #'   and produced a complete, uniformly-built set of batches, plus an
    #'   identity for exactly which batches those were and what built them.
    #'   Written to `registrystudy.qs2` by `$process_skeletons()`, atomically,
    #'   via `$save_meta()`.
    #'
    #'   NULL means "no trustworthy skeleton dataset". `$process_skeletons()`
    #'   clears it *before* doing any work and only re-commits it at the end if
    #'   the result validates, so a killed or partially-failed run leaves NULL
    #'   rather than a stale record vouching for skeletons it no longer
    #'   describes. Downstream stages that need to know what they are reading
    #'   (s1) must load this from disk rather than from an embedded study copy,
    #'   which is frozen at the time the plan was saved.
    #'
    #'   Fields: `committed_at`, `swereg_version`, `n_batches`, `batches` (the
    #'   exact sorted batch IDs -- a count alone cannot tell batches 1..N from
    #'   2..N+1), `pipeline_hash` (shared by every batch) and `identity` (a
    #'   digest over the ordered per-batch (batch, pipeline_hash, saved_at)
    #'   triples, so it moves when any batch is rebuilt even if the code did
    #'   not change).
    skeleton_manifest = NULL,

    #' @field population_by_specs List of character vectors. Each element
    #'   declares one `by` aggregation that will be pre-computed during
    #'   `$process_skeletons()` and stored in each batch's meta sidecar,
    #'   so that `$population(by = ...)` is a fast meta-only walk. Read
    #'   back with `$population(by = <one of the declared specs>)`.
    population_by_specs = list(),

    # --- Constructor ---

    #' @description Create a new RegistryStudy object.
    #' @param data_rawbatch_dir Character vector of candidate paths for
    #'   rawbatch files. The first existing path is used; a single non-existing
    #'   path is created automatically.
    #' @param group_names Character vector of rawbatch group names.
    #' @param data_skeleton_dir Character vector of candidate paths for
    #'   skeleton output. Defaults to same candidates as `data_rawbatch_dir`.
    #' @param data_meta_dir Character vector of candidate paths for the
    #'   metadata directory holding `registrystudy.qs2`. Defaults to same
    #'   candidates as `data_rawbatch_dir` (backward compatible). Pass an
    #'   explicit value -- e.g. the parent of rawbatch -- to keep the
    #'   singleton control file out of the per-batch data directory.
    #' @param data_raw_dir Character vector of candidate paths for raw registry
    #'   files (optional). NULL if raw data paths are managed externally.
    #' @param data_summaries_dir Optional character vector of candidate
    #'   paths for the audit-track summaries directory (typically inside
    #'   the project git repo, e.g. `dev/summaries/`). When NULL
    #'   (default), `$compute_summary()` still writes `summary.qs2` and
    #'   `status.txt` to the skeleton directory but skips the
    #'   git-tracked TSV.
    #' @param batch_size Integer. Number of IDs per batch. Default: 1000L.
    #' @param seed Integer. Shuffle seed.
    #' @param id_col Character. Person ID column name.
    #' @param population_by_specs Optional list of character vectors. Each
    #'   element declares one `by` aggregation pre-computed during
    #'   `$process_skeletons()` and stored in each batch's meta sidecar
    #'   for fast `$population(by)` access. Example:
    #'   `list(c("rd_age_continuous"), c("rd_age_continuous", "ri_is_amab"))`.
    #'   Default: empty list.
    initialize = function(
      data_rawbatch_dir,
      group_names = c(
        "lmed",
        "inpatient",
        "outpatient",
        "cancer",
        "dors",
        "other"
      ),
      data_skeleton_dir = data_rawbatch_dir,
      data_meta_dir = data_rawbatch_dir,
      data_raw_dir = NULL,
      data_summaries_dir = NULL,
      batch_size = 1000L,
      seed = 4L,
      id_col = "lopnr",
      population_by_specs = list()
    ) {
      self$data_rawbatch_cp <- CandidatePath$new(
        data_rawbatch_dir,
        "data_rawbatch_dir"
      )
      self$data_skeleton_cp <- CandidatePath$new(
        data_skeleton_dir,
        "data_skeleton_dir"
      )
      self$data_meta_cp <- CandidatePath$new(data_meta_dir, "data_meta_dir")
      if (!is.null(data_raw_dir)) {
        self$data_raw_cp <- CandidatePath$new(data_raw_dir, "data_raw_dir")
      }
      if (!is.null(data_summaries_dir)) {
        self$data_summaries_cp <- CandidatePath$new(
          data_summaries_dir,
          "data_summaries_dir"
        )
      }
      self$group_names <- group_names
      self$batch_size <- as.integer(batch_size)
      self$seed <- as.integer(seed)
      self$id_col <- id_col
      self$population_by_specs <- .validate_population_by_specs(
        population_by_specs
      )

      # Eagerly resolve (and auto-create if needed) rawbatch, skeleton, meta dirs
      self$data_rawbatch_cp$resolve()
      self$data_skeleton_cp$resolve()
      self$data_meta_cp$resolve()

      # Initialize empty state
      self$n_ids <- 0L
      self$n_batches <- 0L
      self$batch_id_list <- list()
      self$groups_saved <- character(0)

      # Initialize empty code registry and empty phase-3 registration list
      self$code_registry <- list()
      self$randvars_fns <- list()

      self$created_at <- Sys.time()

      private$.schema_version <- .REGISTRY_STUDY_SCHEMA_VERSION

      return(invisible(self))
    },

    #' @description Check if this object's schema version matches the current
    #' class version. Errors if the object was saved with an older schema.
    #' @return `invisible(TRUE)` if versions match. Errors otherwise with an
    #'   actionable migration message.
    check_version = function() {
      current <- .REGISTRY_STUDY_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        stop(
          class(self)[1],
          " on disk has schema version ",
          saved,
          " but this swereg requires version ",
          current,
          ".\n",
          "Regenerate by re-running the upstream registrystudy generator ",
          "(e.g. run_generic_create_datasets_v2.R). Note: schema v4 also ",
          "changed the Skeleton file format; running $process_skeletons() ",
          "auto-migrates existing bare-data.table skeleton files on first ",
          "load but re-runs the full pipeline once to populate the new ",
          "provenance fields.",
          call. = FALSE
        )
      }
      return(invisible(TRUE))
    },

    # --- Phase registration (framework + trim + randvars) ---

    #' @description Register the framework function (phase 1). Called once
    #'   per batch at the start of `$process_skeletons()`, with signature
    #'   `function(batch_data, config)`, returns a fresh `data.table`
    #'   containing the base time grid + censoring. Everything downstream
    #'   builds on this output. A change to the function body or formals
    #'   triggers a full rebuild of every batch on the next
    #'   `$process_skeletons()` run.
    #' @param fn A function of signature `(batch_data, config)` returning
    #'   a `data.table`.
    #' @return `invisible(self)`.
    register_framework = function(fn) {
      stopifnot(is.function(fn))
      self$framework_fn <- fn
      return(invisible(self))
    },

    #' @description Register the trim function (phase 1b). A study may
    #'   register at most one. It runs immediately after the framework
    #'   and before the code registry, so every code entry sees the row
    #'   set the trim leaves behind.
    #'
    #'   `$register_trim()` stops on a second call, naming the trim
    #'   already registered. `$register_framework()` overwrites in the
    #'   same situation. The two differ deliberately. A study has one
    #'   framework by construction. A second `$register_trim()` call is a
    #'   script that means to delete rows in two places.
    #'
    #'   This is the one declared place in the pipeline that MAY delete
    #'   skeleton rows. Phase 3 MUST NOT: its rewind drops columns and
    #'   cannot restore rows.
    #'
    #'   `fn` runs exactly once per rebuild, on a fresh base. It never
    #'   sees data it already trimmed, so it MAY delete a fixed count or
    #'   a fraction. An edit to a randvars step or to a code entry
    #'   rebuilds nothing and therefore re-runs no trim.
    #'
    #'   A change to `body(fn)` or `formals(fn)` rebuilds the base
    #'   skeleton of every batch. So does adding a trim to a study that
    #'   had none, and so does removing one.
    #' @param fn A function of signature `(skeleton, batch_data, config)`
    #'   returning a `data.table`. `$process_skeletons()` rebinds the
    #'   skeleton data to what it returns, and stops when it returns
    #'   anything else.
    #' @return `invisible(self)`.
    register_trim = function(fn) {
      stopifnot(is.function(fn))
      if (!is.null(self$trim_fn)) {
        stop(
          "A trim function is already registered (hash ",
          .hash_function(self$trim_fn),
          "). A study may register at most one trim. ",
          "Edit the registered function instead of calling ",
          "$register_trim() again.",
          call. = FALSE
        )
      }
      self$trim_fn <- fn
      return(invisible(self))
    },

    #' @description Register one phase-3 "random variables" step. Phase 3
    #'   is an ordered sequence of user-supplied functions; each call to
    #'   `$register_randvars()` appends one step to the end of the
    #'   sequence. Registration order is execution order at
    #'   `$process_skeletons()` time.
    #'
    #'   Signature of `fn`: `function(skeleton, batch_data, config)`. It
    #'   mutates `skeleton` in place and must ONLY ADD columns (never
    #'   modifying or deleting existing ones -- the drop-and-replay
    #'   tracking depends on this invariant).
    #'
    #'   Phase 3 runs after phase 2, so `fn` MAY read a code registry
    #'   column.
    #'
    #'   An edit to `fn`'s body, under the same `name`, changes the step's
    #'   hash. It replays this step and every step after it. A change to
    #'   the framework function, or to any code registry entry, replays
    #'   the whole sequence: `$randvars_hashes()` folds both into every
    #'   step's hash.
    #'
    #'   `fn` MUST NOT change the row count.
    #'   `Skeleton$sync_randvars()` compares the row count before and
    #'   after every replayed step. It stops the run when the count
    #'   moves, and it names the step. Register a row filter with
    #'   `$register_trim()` instead. The trim runs on a fresh base,
    #'   before the code registry, so every later phase sees the rows it
    #'   leaves.
    #'
    #'   Nothing checks the add-only contract. Rewind drops the columns a
    #'   step recorded. It cannot restore a column the step overwrote.
    #' @param name Character scalar. The user-facing step name. Used as
    #'   the key in `Skeleton$randvars_state` and in the divergence-point
    #'   comparison.
    #' @param fn A function of signature `(skeleton, batch_data, config)`.
    #' @return `invisible(self)`.
    register_randvars = function(name, fn) {
      stopifnot(
        is.character(name),
        length(name) == 1L,
        nzchar(name),
        is.function(fn)
      )
      if (is.null(self$randvars_fns)) {
        self$randvars_fns <- list()
      }
      if (name %in% names(self$randvars_fns)) {
        stop(
          "A phase-3 step named '",
          name,
          "' is already registered. ",
          "Phase-3 step names must be unique.",
          call. = FALSE
        )
      }
      self$randvars_fns[[name]] <- fn
      return(invisible(self))
    },

    # --- Code registry fingerprints + adopt runtime state ---

    #' @description Return the xxhash64 fingerprint of every entry in
    #'   `self$code_registry`, in registry order.
    #'
    #'   Primary entries: fingerprint depends on
    #'   `(codes, label, groups, fn_args, combine_as)` and on the hash of
    #'   the entry's `fn`. Two primary entries produce the same
    #'   fingerprint, and are therefore treated as "the same entry"
    #'   across runs, only when all six agree. An edit to a registered
    #'   code function's body re-applies that entry.
    #'
    #'   Derived entries: fingerprint depends on `(codes, from, as)` PLUS
    #'   the fingerprints of every upstream primary entry whose output
    #'   prefix is referenced in `from`. This cascades invalidation when
    #'   an upstream primary's `fn_args` / `groups` / `codes` change,
    #'   without requiring the user to touch the derived entry. Computed
    #'   in a two-pass walk: primary fingerprints first, then derived
    #'   fingerprints using the already-computed upstream fingerprints.
    #'
    #'   Used by `Skeleton$sync_with_registry()` for incremental
    #'   per-entry add/drop.
    #' @return Character vector of fingerprints.
    code_registry_fingerprints = function() {
      return(.code_registry_fingerprints(self$code_registry))
    },

    #' @description Return one hash per registered phase-3 step, named by
    #'   step name and in registration order.
    #'
    #'   Each step's hash folds in four inputs. They are the step
    #'   function's own body and formals, the framework function's hash,
    #'   the trim function's identity, and the code registry fingerprint
    #'   set. Every step takes the same framework, trim and code registry
    #'   components. So a change to any of the three diverges at step 1,
    #'   and `Skeleton$sync_randvars()` replays the whole sequence.
    #'
    #'   The framework component is `NA_character_` when no framework
    #'   function is registered. The trim component is
    #'   `"__swereg_no_trim__"` when no trim function is registered.
    #'
    #'   Two inputs are NOT covered. A change to either one replays
    #'   nothing:
    #'
    #'   - Whatever a registered function calls or reads. Each hash
    #'     covers that function's own body and formals. It does not
    #'     follow a call into a helper, and it does not read a variable
    #'     the function captured from its environment.
    #'   - The rawbatch data. Nothing hashes raw content, so new raw data
    #'     alone replays nothing.
    #'
    #'   `$pipeline_hash()` and `$process_skeletons()` both call this.
    #'   `$process_skeletons()` passes the result to
    #'   `Skeleton$sync_randvars()`, which stores each step's hash in
    #'   `Skeleton$randvars_state` and compares it on the next run.
    #' @return Named character vector of xxhash64 digests, parallel to
    #'   `self$randvars_fns`. `character(0)` when no step is registered.
    randvars_hashes = function() {
      return(.randvars_hashes(
        self$randvars_fns,
        self$framework_fn,
        self$trim_fn,
        self$code_registry_fingerprints()
      ))
    },

    #' @description Compute this study's current total pipeline hash from
    #'   the registered framework, the trim, the phase order, the
    #'   randvars sequence and the code registry. Answer to "what would a
    #'   freshly-built skeleton look like?"
    #'
    #'   `sk$pipeline_hash() == study$pipeline_hash()` is necessary for a
    #'   synced skeleton. It is not sufficient. Unequal hashes mean the
    #'   skeleton is definitely stale. Equal hashes mean only that
    #'   nothing changed among those five inputs.
    #'
    #'   Two inputs sit outside both hashes: the rawbatch data, and
    #'   whatever a registered function calls or reads from its
    #'   environment. A change to either one leaves the hashes equal
    #'   over a stale skeleton. `$randvars_hashes()` says why.
    #'
    #'   `.PHASE_ORDER` is a package constant, so it never discriminates
    #'   between two studies. It discriminates on the SKELETON side, where
    #'   an old skeleton reads `NULL`. Both hashes fold it in, so the
    #'   comparison stays meaningful.
    #' @return A single character string (xxhash64 digest).
    pipeline_hash = function() {
      return(.pipeline_hash(
        self$framework_fn,
        self$trim_fn,
        self$randvars_hashes(),
        self$code_registry_fingerprints()
      ))
    },

    #' @description Copy runtime state (IDs, batch list, saved groups)
    #'   from another `RegistryStudy` into this one, WITHOUT touching
    #'   config fields (group_names, code_registry, directory candidates,
    #'   framework/randvars registration, schema version, etc.).
    #'
    #'   Use case: in `run_generic_create_datasets_v2.R`, the generator
    #'   script constructs a fresh study every run with the current
    #'   in-memory config, then on re-runs calls
    #'   `$adopt_runtime_state_from(qs2_read(self$meta_file))` to pick up
    #'   batch ids and saved-group state without silently adopting a
    #'   stale code registry or group name list.
    #' @param other Another `RegistryStudy` to copy runtime state from.
    #' @return `invisible(self)`.
    adopt_runtime_state_from = function(other) {
      stopifnot(inherits(other, "RegistryStudy"))
      self$n_ids <- other$n_ids
      self$n_batches <- other$n_batches
      self$batch_id_list <- other$batch_id_list
      self$groups_saved <- other$groups_saved
      return(invisible(self))
    },

    # --- Code registry methods ---

    #' @description Register code definitions for the code registry.
    #'
    #' Each call declares codes, the function to apply them, which batch data
    #' groups to use, and optional prefixing/combining. Appends to
    #' `self$code_registry`.
    #'
    #' @param codes Named list of code vectors (e.g. ICD-10, ATC, operation codes).
    #' @param fn Function to call (e.g. `add_diagnoses`, `add_rx`).
    #' @param groups Named list mapping prefixes to group names. Unnamed elements
    #'   get no prefix. Each element is a character vector of group names to
    #'   rbindlist before calling `fn`.
    #' @param fn_args Named list of extra arguments to pass to `fn`
    #'   (e.g. `list(source = "atc")`).
    #' @param combine_as Character or NULL. If non-NULL, also run `fn` on all
    #'   groups combined, using this as the prefix.
    #' @param label Character. Human-readable label for describe_codes() output.
    #'   Defaults to deparse(substitute(fn)).
    register_codes = function(
      codes,
      fn,
      groups,
      fn_args = list(),
      combine_as = NULL,
      label = NULL
    ) {
      if (is.null(label)) {
        label <- deparse(substitute(fn))
      }

      # Normalize groups: ensure it's a list
      if (!is.list(groups)) {
        groups <- as.list(groups)
      }

      entry <- list(
        codes = codes,
        fn = fn,
        fn_args = fn_args,
        groups = groups,
        combine_as = combine_as,
        label = label
      )
      self$code_registry[[length(self$code_registry) + 1L]] <- entry
      return(invisible(self))
    },

    #' @description Register a derived code entry: one that doesn't read
    #'   rawbatch data, but instead ORs together already-existing
    #'   skeleton columns from earlier primary entries.
    #'
    #'   For each name `<nm>` in `codes`, a new column `<as>_<nm>` is
    #'   written as `Reduce("|", list(get("<from[1]>_<nm>"), ...))`. The
    #'   `codes` list pattern values are ignored at apply time but DO
    #'   participate in the fingerprint, so editing the code list
    #'   triggers replay. The fingerprint also folds in the fingerprints
    #'   of every upstream primary entry whose output prefix appears in
    #'   `from`, so upstream behavior edits (e.g. `cod_type` on an
    #'   `add_cods` primary) cascade into derived replay automatically.
    #'
    #'   The derived entry runs in registration order during phase-2
    #'   sync, so any primary registrations whose output columns it
    #'   references MUST be registered BEFORE this call.
    #' @param codes Named list. Keys name the output columns' suffixes;
    #'   the pattern values are ignored at apply time.
    #' @param from Character vector of source prefixes (e.g.
    #'   `c("os", "dorsu", "dorsm")`).
    #' @param as Character scalar: the output column prefix.
    register_derived_codes = function(codes, from, as) {
      stopifnot(
        is.list(codes),
        length(codes) > 0L,
        !is.null(names(codes)),
        all(nzchar(names(codes))),
        is.character(from),
        length(from) >= 1L,
        all(nzchar(from)),
        is.character(as),
        length(as) == 1L,
        nzchar(as)
      )
      entry <- list(
        kind = "derived",
        codes = codes,
        from = from,
        as = as,
        label = sprintf(
          "derived: %s_* = %s",
          as,
          paste(paste0(from, "_*"), collapse = " | ")
        )
      )
      self$code_registry[[length(self$code_registry) + 1L]] <- entry
      return(invisible(self))
    },

    # --- Apply codes to skeleton ---

    #' @description Apply all registered codes to a skeleton data.table.
    #'   Thin loop over `self$code_registry` that delegates per-entry work
    #'   to the file-level `.apply_code_entry_impl()` helper. Kept for
    #'   backwards-compatible "apply everything at once" callers; the
    #'   incremental code-registry sync inside the Skeleton R6 class
    #'   calls `.apply_code_entry_impl()` directly on one entry at a time.
    #' @param skeleton data.table. The person-week skeleton to modify in place.
    #' @param batch_data Named list of data.tables from load_rawbatch().
    apply_codes_to_skeleton = function(skeleton, batch_data) {
      for (reg in self$code_registry) {
        .apply_code_entry_impl(skeleton, batch_data, reg, self$id_col)
      }
      return(invisible(skeleton))
    },

    # --- Batch pipeline methods ---

    #' @description Set IDs and split into batches.
    #' @param ids Vector of person IDs.
    set_ids = function(ids) {
      ids <- unique(ids)
      set.seed(self$seed)
      ids <- sample(ids)

      n_chunks <- ceiling(length(ids) / self$batch_size)
      batch_id_list <- split(ids, ceiling(seq_along(ids) / self$batch_size))

      self$n_ids <- as.integer(length(ids))
      self$n_batches <- as.integer(length(batch_id_list))
      self$batch_id_list <- batch_id_list

      # Scan disk for existing rawbatch groups
      self$groups_saved <- .detect_rawbatch_groups(
        self$data_rawbatch_dir,
        self$group_names,
        self$n_batches
      )

      return(invisible(self))
    },

    #' @description Write only the `meta_%05d.qs2` sidecar for one
    #'   batch (no skeleton file write). Used by the meta-only refresh
    #'   path in `.process_one_batch()` when the skeleton on disk is
    #'   still valid but its meta is missing a newly-registered
    #'   `population_by_specs` entry.
    #'
    #'   This method does not recompute the code-entry counts. Both of
    #'   its callers pass a skeleton whose counts already describe its
    #'   own data. `$save_skeleton()` refreshes them first. The
    #'   meta-only refresh path reads a skeleton back from disk without
    #'   changing it.
    #' @param sk A [Skeleton] to derive the meta from.
    #' @return Invisible NULL.
    #' @keywords internal
    write_skeleton_meta = function(sk) {
      stopifnot(inherits(sk, "Skeleton"))
      meta <- .build_skeleton_meta(
        sk,
        population_by_specs = self$population_by_specs %||% list()
      )
      qs2_write_atomic(meta, self$skeleton_meta_path(sk$batch_number))
      return(invisible(NULL))
    },

    #' @description Read the `meta_%05d.qs2` sidecar for one batch.
    #'   Returns `NULL` if missing or unreadable (treated as cache miss
    #'   by the fast path in `.process_one_batch()`).
    #' @param batch_number Integer batch index.
    #' @return A list (the meta payload) or `NULL`.
    #' @keywords internal
    load_skeleton_meta = function(batch_number) {
      path <- self$skeleton_meta_path(batch_number)
      if (!file.exists(path)) {
        return(NULL)
      }
      return(tryCatch(qs2::qs_read(path), error = function(e) NULL))
    },

    #' @description Filesystem path of a meta sidecar.
    #' @param batch_number Integer batch index.
    #' @return Character. The full path.
    #' @keywords internal
    skeleton_meta_path = function(batch_number) {
      return(file.path(
        self$data_skeleton_dir,
        sprintf("meta_%05d.qs2", as.integer(batch_number))
      ))
    },

    #' @description Summary of per-batch pipeline hashes across all
    #'   currently-persisted skeleton files in `self$data_skeleton_dir`.
    #'   Use this to spot batches out of sync with each other or with
    #'   `self$pipeline_hash()`.
    #'
    #'   Files that are not valid `Skeleton` R6 objects (e.g. unreadable
    #'   or corrupted) surface as rows with `NA` `pipeline_hash`,
    #'   `NA` `framework_fn_hash`, `NA` `trim_fn_hash` and `NA`
    #'   `phase_order`.
    #' @return A `data.table` with columns: batch, pipeline_hash,
    #'   framework_fn_hash, trim_fn_hash, phase_order, n_randvars,
    #'   n_code_entries, saved_at. `phase_order` is the stored character
    #'   vector collapsed with `" -> "`, so one batch is one row.
    skeleton_pipeline_hashes = function() {
      dir <- self$data_skeleton_dir
      files <- list.files(
        dir,
        pattern = "^skeleton_\\d+\\.qs2$",
        full.names = TRUE
      )
      if (length(files) == 0L) {
        return(data.table::data.table(
          batch = integer(),
          pipeline_hash = character(),
          framework_fn_hash = character(),
          trim_fn_hash = character(),
          phase_order = character(),
          n_randvars = integer(),
          n_code_entries = integer(),
          saved_at = as.POSIXct(character())
        ))
      }

      # Meta-first: every batch normally has a meta_*.qs2 sidecar that
      # carries the pipeline-hash inputs in a few KB. Read those instead
      # of deserialising every full skeleton. Fall back to loading the
      # skeleton when meta is missing or unreadable -- typically only
      # happens for skeleton files written by an older swereg before
      # meta sidecars existed (re-run $process_skeletons() to backfill).
      rows <- progressr::with_progress({
        p <- progressr::progressor(steps = length(files))
        lapply(files, function(f) {
          batch <- as.integer(
            regmatches(
              basename(f),
              regexec("skeleton_(\\d+)\\.qs2$", basename(f))
            )[[1]][2]
          )
          p(message = sprintf("batch %d", batch))

          meta <- self$load_skeleton_meta(batch)
          if (!is.null(meta)) {
            randvars_hashes <- vapply(
              meta$randvars_state,
              function(x) x$fn_hash %||% NA_character_,
              character(1)
            )
            # Same input list, in the same order, as
            # `Skeleton$pipeline_hash()`. The two MUST stay aligned: this
            # branch and the skeleton fallback below both feed
            # `.commit_skeleton_manifest()`, which compares them against
            # `$pipeline_hash()`.
            pipeline_hash <- digest::digest(
              list(
                framework = meta$framework_fn_hash,
                trim = meta$trim_fn_hash,
                phase_order = meta$phase_order,
                randvars = randvars_hashes,
                codes = names(meta$applied_registry) %||% character(0)
              ),
              algo = "xxhash64"
            )
            return(data.table::data.table(
              batch = batch,
              pipeline_hash = pipeline_hash,
              framework_fn_hash = meta$framework_fn_hash %||% NA_character_,
              trim_fn_hash = meta$trim_fn_hash %||% NA_character_,
              phase_order = .format_phase_order(meta$phase_order),
              n_randvars = length(meta$randvars_state),
              n_code_entries = length(meta$applied_registry),
              saved_at = meta$built_at %||% as.POSIXct(NA)
            ))
          }

          # Fallback: meta missing -> load full skeleton.
          obj <- tryCatch(qs2::qs_read(f), error = function(e) NULL)
          if (inherits(obj, "Skeleton")) {
            return(data.table::data.table(
              batch = batch,
              pipeline_hash = obj$pipeline_hash(),
              framework_fn_hash = obj$framework_fn_hash %||% NA_character_,
              trim_fn_hash = obj$trim_fn_hash %||% NA_character_,
              phase_order = .format_phase_order(obj$phase_order),
              n_randvars = length(obj$randvars_state),
              n_code_entries = length(obj$applied_registry),
              saved_at = obj$created_at %||% as.POSIXct(NA)
            ))
          }
          # Unreadable or not a Skeleton R6: surface with NA
          return(data.table::data.table(
            batch = batch,
            pipeline_hash = NA_character_,
            framework_fn_hash = NA_character_,
            trim_fn_hash = NA_character_,
            phase_order = NA_character_,
            n_randvars = NA_integer_,
            n_code_entries = NA_integer_,
            saved_at = as.POSIXct(NA)
          ))
        })
      })
      out <- data.table::rbindlist(rows)
      data.table::setorder(out, batch)
      return(out[])
    },

    #' @description Assert that every persisted skeleton file has the
    #'   same pipeline hash AND that it matches this study's current
    #'   pipeline hash. Errors loudly with an actionable message if not.
    #'
    #'   Intended as a pre-flight check at the top of downstream
    #'   consumers like `tteplan_from_spec_and_registrystudy()`, so
    #'   partial-rebuild stragglers or config drift never silently flow
    #'   into a TTE plan.
    #' @return The single pipeline hash on success, invisibly.
    assert_skeletons_consistent = function() {
      hashes <- self$skeleton_pipeline_hashes()
      if (nrow(hashes) == 0L) {
        stop(
          "No skeleton files found in ",
          self$data_skeleton_dir,
          ". Run $process_skeletons() first.",
          call. = FALSE
        )
      }

      if (any(is.na(hashes$pipeline_hash))) {
        bad <- hashes[is.na(pipeline_hash), batch]
        stop(
          "Skeleton files have no pipeline hash (unreadable or not a ",
          "Skeleton R6 object): batches ",
          .format_batch_range(bad),
          ". Delete the affected files and re-run $process_skeletons().",
          call. = FALSE
        )
      }

      unique_hashes <- unique(hashes$pipeline_hash)
      if (length(unique_hashes) > 1L) {
        counts <- hashes[, .N, by = pipeline_hash]
        stop(
          "Inconsistent skeleton pipeline hashes across batches. Found ",
          length(unique_hashes),
          " distinct hashes:\n",
          paste0(
            "  ",
            counts$pipeline_hash,
            " (",
            counts$N,
            " batches)",
            collapse = "\n"
          ),
          "\nRun $process_skeletons() to bring all batches up to date. ",
          "See $skeleton_pipeline_hashes() for the per-batch breakdown.",
          call. = FALSE
        )
      }

      current <- self$pipeline_hash()
      if (!identical(unique_hashes, current)) {
        stop(
          "Skeleton pipeline hash on disk (",
          unique_hashes,
          ") does not match this study's current pipeline hash (",
          current,
          "). Run $process_skeletons() to regenerate.",
          call. = FALSE
        )
      }

      return(invisible(current))
    },

    #' @description Orchestrate the skeleton pipeline per batch.
    #'
    #'   Reads `self$framework_fn` (phase 1), `self$trim_fn` (phase 1b),
    #'   `self$code_registry` (phase 2), and `self$randvars_fns`
    #'   (phase 3) from the study. Applies them via the incremental logic
    #'   on [Skeleton]. Exact per-batch work:
    #'
    #'   1. Load existing skeleton via `self$load_skeleton(i)`. Rebuild
    #'      the base from scratch when the file is missing. Rebuild it
    #'      too when `framework_fn_hash`, `trim_fn_hash` or `phase_order`
    #'      does not match the study's current identity. A rebuild calls
    #'      `self$framework_fn(batch_data, self)`, wraps the result in a
    #'      fresh [Skeleton], and resets phases 2 and 3. (Phase 1.)
    #'   1b. On a rebuild only, call
    #'      `self$trim_fn(sk$data, batch_data, self)` and rebind
    #'      `sk$data` to what it returns. Skipped when no trim is
    #'      registered. (Phase 1b.)
    #'   2. Call `sk$sync_with_registry()` with
    #'      `self$code_registry_fingerprints()`. Entries present on disk
    #'      but not in the current registry are dropped (via
    #'      `.entry_columns()` on the stored descriptor); entries present
    #'      in the current registry but not on disk are applied fresh.
    #'      (Phase 2.)
    #'   3. Call `sk$sync_randvars()` with the current ordered
    #'      `self$randvars_fns` and their body/formals hashes. Divergence-
    #'      point rewind-and-replay semantics drop and re-run the
    #'      affected phase-3 steps only. A step MAY read a phase-2
    #'      column, because phase 2 already ran. (Phase 3.)
    #'   4. Save via `self$save_skeleton(sk)`.
    #'
    #'   `batch_data` is loaded lazily -- exactly once per batch, by
    #'   whichever phase needs it first. If no phase needs it (everything
    #'   already in sync), the rawbatch read is skipped entirely and the
    #'   per-batch work is just load → save.
    #'
    #' @param batches Integer vector of batch indices to process, or
    #'   `NULL` (default) for all batches in `self$batch_id_list`.
    #' @param n_workers Integer. Number of parallel workers (1 = sequential).
    #'   When `> 1`, each batch runs in a fresh worker subprocess via the
    #'   generic batch runner.
    #' @param ... Additional arguments (unused; reserved for future use).
    #' @return `invisible(self)`.
    process_skeletons = function(
      batches = NULL,
      n_workers = default_n_workers("skeleton"),
      ...
    ) {
      # Validate FIRST -- before any self$ mutation and before the destructive
      # manifest invalidation below. It used to run after `randvars_fns` was
      # changed NULL -> list() and after the manifest was cleared, so `NA` or a
      # bad type mutated state and destroyed the committed manifest and only
      # then errored; `0`/`-1` silently selected serial; and `1.5` launched TWO
      # workers, since the dispatch guard is `length(active) < n_workers`.
      n_workers <- .validate_n_workers(n_workers, "process_skeletons()")

      if (is.null(self$framework_fn)) {
        stop(
          "RegistryStudy has no framework_fn registered. Call ",
          "$register_framework(fn) before $process_skeletons().",
          call. = FALSE
        )
      }
      if (is.null(self$randvars_fns)) {
        self$randvars_fns <- list()
      }

      # Commit protocol, opening half. Everything from here until the matching
      # .commit_skeleton_manifest() below is "the dataset is being rebuilt and
      # cannot be trusted", and that has to survive a kill -- so it is recorded
      # on disk now rather than assumed.
      #
      # SINGLE-WRITER INVARIANT: exactly one $process_skeletons() may run against
      # a skeleton directory at a time, and no s1 may read it while one does.
      # Nothing here enforces that. Two concurrent writers interleave as
      # clear(A), clear(B), commit(A), B-replaces-skeletons-and-dies -- leaving
      # A's manifest vouching for B's skeletons. That is a logical race; atomic
      # writes cannot fix it. Serialise the stages, as the drivers do.

      private$.invalidate_skeleton_manifest()

      # Capture this BEFORE the line below overwrites the parameter. Reading
      # is.null(batches) at the end of the function would always be FALSE, so a
      # full run that failed to validate would decline to commit a manifest and
      # still exit 0 -- and the caller's file-count gate would report success.
      full_run <- is.null(batches)

      if (is.null(batches)) {
        batches <- seq_len(self$n_batches)
      }

      framework_hash <- .hash_function(self$framework_fn)
      trim_hash <- .trim_hash(self$trim_fn)
      phase_order <- .PHASE_ORDER
      randvars_hashes <- self$randvars_hashes()
      current_fps <- self$code_registry_fingerprints()

      # Code-check warnings are aggregated via the meta sidecar files,
      # not via in-memory session state. Each .process_one_batch() opens
      # a per-batch session, snapshots the accumulator into the batch's
      # meta_*.qs2, and closes the session. After the batch loop below,
      # we read every batch-in-scope's meta, merge the snapshots, and
      # emit one consolidated warning. Works identically for sequential
      # and parallel runs because nothing has to cross the worker
      # process boundary in memory -- everything goes via disk.

      # Fail fast on any batch failure. These pipelines run unattended
      # for days; if a batch fails 10 minutes in (e.g. a systematic
      # bug, a missing column, an unreadable rawbatch file), pushing
      # through the remaining 149 batches over 4 more days before
      # surfacing the error is exactly the wrong tradeoff. Halt
      # immediately with the underlying message preserved so the user
      # can SSH in, fix the root cause, and rerun. Successful batches
      # are already persisted to disk by .process_one_batch() and will
      # be skipped on rerun via framework-hash matching, so no work is
      # lost.

      if (n_workers <= 1L) {
        progressr::with_progress({
          p <- progressr::progressor(steps = length(batches))
          for (i in batches) {
            tryCatch(
              .process_one_batch(
                study = self,
                i = i,
                framework_hash = framework_hash,
                trim_hash = trim_hash,
                phase_order = phase_order,
                randvars_hashes = randvars_hashes,
                current_fps = current_fps
              ),
              error = function(e) {
                stop(
                  sprintf(
                    "process_skeletons() halted on batch %d: %s\n\nSuccessful batches up to this point are persisted on disk; rerun with `batches = ...` to retry from this one.",
                    i,
                    conditionMessage(e)
                  ),
                  call. = FALSE
                )
              }
            )
            gc()
            p(
              message = sprintf(
                "%s batch %d",
                format(Sys.time(), "%H:%M:%S"),
                i
              )
            )
          }
        })
      } else {
        threads_per_worker <- max(
          1L,
          floor(.safe_n_cores() / n_workers)
        )
        cat(sprintf(
          "Running %d batches: %d workers x %d threads each\n",
          length(batches),
          n_workers,
          threads_per_worker
        ))

        # The study snapshot is written ONCE; every item carries only its path
        # plus small scalars. The old callr engine serialized the ~5.7 MB study
        # per LAUNCHED batch (~n_workers in flight); the shape-A runner
        # materialises every item envelope up front, so a naive translation
        # putting the study in each of 2,194 items would have serialized
        # ~12.5 GB before the first worker launched. One snapshot + tiny items
        # is the contract-fitting form (pinned by
        # test-batch_skeletons_production.R).
        snapshot_path <- tempfile(
          pattern = "process_skeletons_study_",
          fileext = ".qs2"
        )
        on.exit(unlink(snapshot_path, force = TRUE), add = TRUE)
        qs2_write_atomic(self, snapshot_path, nthreads = .safe_n_cores())

        items <- lapply(batches, function(i) {
          return(list(
            snapshot_path = snapshot_path,
            batch_idx = i,
            framework_hash = framework_hash,
            trim_hash = trim_hash,
            phase_order = phase_order,
            randvars_hashes = randvars_hashes,
            current_fps = current_fps,
            n_threads = threads_per_worker
          ))
        })
        names(items) <- sprintf("batch_%05d", batches)

        # No worker-level session: .process_one_batch() opens its own
        # per-batch session, snapshots into the meta sidecar, and closes it.
        # The parent reads every batch's meta after the loop finishes and
        # emits one consolidated warning covering all workers.
        tryCatch(
          progressr::with_progress({
            p <- progressr::progressor(steps = length(items))
            .batch_run(
              target = .batch_target("swereg", ".process_one_batch_snapshot"),
              items = items,
              n_workers = n_workers,
              dev_path = .swereg_dev_path(),
              p = p,
              collect = FALSE
            )
          }),
          error = function(e) {
            stop(
              sprintf(
                "process_skeletons() halted on batch failure: %s\n\nIn-flight workers were killed. Successful batches are persisted on disk; rerun with `batches = ...` to retry from the failed one.",
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      }

      # Derived outputs: per-spec population tables and the study-wide
      # summary. Both are cheap meta walks (KB per batch) -- always
      # run them so `study$population(by)` and `study$summary` are
      # ready to read on disk.
      for (spec in self$population_by_specs %||% list()) {
        tryCatch(
          private$.compute_population_for_spec(spec),
          error = function(e) {
            return(warning(
              "Skipping population for spec ",
              .population_spec_key(spec),
              ": ",
              conditionMessage(e),
              call. = FALSE
            ))
          }
        )
      }
      private$.compute_summary()

      # Commit protocol, closing half. Only now, with every batch written and
      # the derived artefacts built, can the dataset earn a manifest -- and only
      # if it actually validates. Note validation always covers the WHOLE
      # directory, never just `batches`, because the whole directory is what s1
      # reads: a subset run therefore still commits when the resulting dataset
      # validates. full_run only decides whether FAILING to validate raises.
      private$.commit_skeleton_manifest(full_run = full_run)

      return(invisible(self))
    }
  ),

  active = list(
    #' @field data_rawbatch_dir Character (read-only). Resolved rawbatch
    #'   directory for the current host. Lazily resolved from
    #'   `self$data_rawbatch_cp`.
    data_rawbatch_dir = function(value) {
      if (!missing(value)) {
        stop(
          "data_rawbatch_dir is read-only; set via constructor",
          call. = FALSE
        )
      }
      return(self$data_rawbatch_cp$resolve())
    },

    #' @field data_skeleton_dir Character (read-only). Resolved skeleton
    #'   directory for the current host.
    data_skeleton_dir = function(value) {
      if (!missing(value)) {
        stop(
          "data_skeleton_dir is read-only; set via constructor",
          call. = FALSE
        )
      }
      return(self$data_skeleton_cp$resolve())
    },

    #' @field data_meta_dir Character (read-only). Resolved metadata
    #'   directory for the current host (where `registrystudy.qs2` lives).
    data_meta_dir = function(value) {
      if (!missing(value)) {
        stop("data_meta_dir is read-only; set via constructor", call. = FALSE)
      }
      return(self$data_meta_cp$resolve())
    },

    #' @field data_raw_dir Character or NULL (read-only). Resolved raw-registry
    #'   directory, or NULL if not configured.
    data_raw_dir = function(value) {
      if (!missing(value)) {
        stop("data_raw_dir is read-only; set via constructor", call. = FALSE)
      }
      if (is.null(self$data_raw_cp)) {
        return(NULL)
      }
      return(self$data_raw_cp$resolve())
    },

    #' @field data_summaries_dir Character or NULL (read-only). Resolved
    #'   audit-track summaries directory for the current host, or NULL if
    #'   not configured.
    data_summaries_dir = function(value) {
      if (!missing(value)) {
        stop(
          "data_summaries_dir is read-only; set via constructor",
          call. = FALSE
        )
      }
      if (is.null(self$data_summaries_cp)) {
        return(NULL)
      }
      return(self$data_summaries_cp$resolve())
    },

    #' @field skeleton_files Character vector (read-only). Skeleton output file
    #'   paths detected on disk. Scans `skeleton_dir` on each access.
    skeleton_files = function(value) {
      if (!missing(value)) {
        stop("skeleton_files is read-only; populated from disk", call. = FALSE)
      }
      return(.detect_skeleton_files(self$data_skeleton_dir))
    },

    #' @field expected_skeleton_file_count Integer (read-only). Expected number
    #'   of skeleton files (one per batch).
    expected_skeleton_file_count = function() {
      return(as.integer(self$n_batches))
    },

    #' @field meta_file Character. Path to the on-disk metadata file
    #'   (`registrystudy.qs2`) inside `data_meta_dir`.
    meta_file = function() {
      return(file.path(self$data_meta_dir, "registrystudy.qs2"))
    },

    #' @field summary List or NULL (read-only). The `summary.qs2`
    #'   payload written by `$process_skeletons()` (per-column counts,
    #'   registry-wide totals, build metadata). NULL with a one-line
    #'   message if the file is missing.
    summary = function(value) {
      if (!missing(value)) {
        stop(
          "summary is read-only; populated by $process_skeletons()",
          call. = FALSE
        )
      }
      path <- file.path(self$data_skeleton_dir, "summary.qs2")
      if (!file.exists(path)) {
        message(
          "summary.qs2 not found; run $process_skeletons() to produce it."
        )
        return(NULL)
      }
      return(qs2::qs_read(path))
    }
  ),

  private = list(
    .schema_version = NULL
  )
)
