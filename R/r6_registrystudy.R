# Path resolution (first_existing_path / invalidate_candidate_paths) lives in
# R/path_resolution.R. Directory candidate state is held inside CandidatePath
# instances -- see R/r6_candidate_path.R.

# Detect if swereg was loaded via devtools::load_all()
.swereg_dev_path <- function() {
  pkg_path <- system.file(package = "swereg")
  if (!nzchar(pkg_path)) {
    return(NULL)
  }
  in_library <- any(vapply(
    .libPaths(),
    function(lp) startsWith(pkg_path, lp),
    logical(1)
  ))
  if (in_library) return(NULL)
  # When dev-loaded, system.file() returns the inst/ subdir of the source
  # tree; devtools::load_all() expects the package root, so strip /inst.
  if (basename(pkg_path) == "inst") pkg_path <- dirname(pkg_path)
  pkg_path
}

# Detect rawbatch groups on disk
.detect_rawbatch_groups <- function(rawbatch_dir, group_names, n_batches) {
  saved <- character(0)
  for (g in group_names) {
    all_exist <- all(vapply(
      seq_len(n_batches),
      function(b) {
        file.exists(file.path(
          rawbatch_dir,
          sprintf("%05d_rawbatch_%s.qs2", b, g)
        ))
      },
      logical(1)
    ))
    if (all_exist) saved <- c(saved, g)
  }
  saved
}

# Detect skeleton files on disk
.detect_skeleton_files <- function(skeleton_dir) {
  if (!dir.exists(skeleton_dir)) {
    return(character(0))
  }
  files <- list.files(
    skeleton_dir,
    pattern = "skeleton_\\d+\\.qs2$",
    full.names = TRUE
  )
  sort(files)
}

# Format byte counts for display
.format_bytes <- function(bytes) {
  if (bytes >= 1e9) {
    sprintf("%.1f GB", bytes / 1e9)
  } else if (bytes >= 1e6) {
    sprintf("%.1f MB", bytes / 1e6)
  } else if (bytes >= 1e3) {
    sprintf("%.1f KB", bytes / 1e3)
  } else {
    paste(bytes, "B")
  }
}

# Predict the character vector of column names that a single code within a
# registry entry will contribute to a skeleton. Mirrors the prefixing logic
# inside .apply_code_entry_impl() below.
#
# Primary entries: for each (group_prefix, code_name) pair in
# (groups x code_name), the column is `prefix_code_name` (or just
# `code_name` when the prefix is empty). When `combine_as` is set, one
# additional column is produced with `combine_as_code_name`.
#
# Derived entries (kind = "derived"): one column per code_name of the form
# `<as>_<code_name>`, built by OR-ing `<from[i]>_<code_name>` across the
# upstream source prefixes. No group machinery.
#
# This is the single source of truth for column-name prediction. It is
# used by:
#   - RegistryStudy$summary_table() (via .entry_columns())
#   - Skeleton$drop_code_entry() via .entry_columns() on stored descriptors,
#     to know which columns to remove when a registry entry is dropped
.generated_columns_for_entry <- function(reg, code_name) {
  kind <- reg$kind %||% "primary"
  if (identical(kind, "derived")) {
    return(paste0(reg$as, "_", code_name))
  }
  cols <- character()
  for (i in seq_along(reg$groups)) {
    prefix <- names(reg$groups)[i]
    if (!is.null(prefix) && nzchar(prefix)) {
      cols <- c(cols, paste0(prefix, "_", code_name))
    } else {
      cols <- c(cols, code_name)
    }
  }
  if (!is.null(reg$combine_as)) {
    cols <- c(cols, paste0(reg$combine_as, "_", code_name))
  }
  cols
}

# Vectorized wrapper: predict the full character vector of column names a
# registry entry contributes, across ALL its code names. Used by the
# Skeleton R6 class at drop time. The prediction MUST stay in sync with
# the behavior of every built-in `fn` (add_diagnoses, add_rx, add_operations,
# add_icdo3s, add_quality_registry); the parity tests in
# tests/testthat/test-entry_columns_parity.R enforce this invariant.
.entry_columns <- function(reg) {
  unlist(
    lapply(
      names(reg$codes),
      function(code_name) .generated_columns_for_entry(reg, code_name)
    ),
    use.names = FALSE
  ) %||% character()
}

# Apply ONE registry entry to a skeleton, mutating it in place.
#
# This is the per-entry body extracted from apply_codes_to_skeleton() so the
# Skeleton R6 class can call it one entry at a time during incremental code
# registry sync. apply_codes_to_skeleton() itself becomes a thin loop around
# this helper for the "apply everything at once" path; behavior is unchanged.
#
# Derived entries (kind = "derived") bypass batch_data entirely -- they
# OR together already-existing skeleton columns under new names. All
# source columns referenced by `reg$from` must already exist on the
# skeleton; they're produced by PRIMARY entries registered earlier in
# registration order. sync_with_registry() walks the registry in order,
# which keeps this invariant during incremental apply.
#
# The column-naming logic here MUST match .entry_columns() above or the
# Skeleton's drop_code_entry() will leak orphan columns. The parity tests
# guard this invariant.
.apply_code_entry_impl <- function(skeleton, batch_data, reg, id_col) {
  kind <- reg$kind %||% "primary"
  if (identical(kind, "derived")) {
    for (nm in names(reg$codes)) {
      src_cols <- paste0(reg$from, "_", nm)
      missing_cols <- setdiff(src_cols, names(skeleton))
      if (length(missing_cols)) {
        stop(
          "register_derived_codes: source columns missing from skeleton: ",
          paste(missing_cols, collapse = ", "),
          ". Primary entries producing these columns must be registered ",
          "BEFORE the derived entry.",
          call. = FALSE
        )
      }
      out_col <- paste0(reg$as, "_", nm)
      skeleton[
        ,
        (out_col) := Reduce("|", lapply(src_cols, function(c) get(c)))
      ]
    }
    return(invisible(skeleton))
  }

  # Per-group calls
  for (i in seq_along(reg$groups)) {
    group_names <- reg$groups[[i]]
    prefix <- names(reg$groups)[i]

    # Get data: rbindlist if multiple groups
    data_list <- Filter(
      function(x) !is.null(x) && nrow(x) > 0,
      lapply(group_names, function(g) batch_data[[g]])
    )
    if (length(data_list) == 0) next
    data <- data.table::rbindlist(
      data_list,
      use.names = TRUE,
      fill = TRUE
    )

    # Prefix code names
    if (!is.null(prefix) && nzchar(prefix)) {
      prefixed_codes <- stats::setNames(
        reg$codes,
        paste0(prefix, "_", names(reg$codes))
      )
    } else {
      prefixed_codes <- reg$codes
    }

    # Call fn, wrapped with the add_* contract validator so misbehaving
    # user-registered fns fail loudly instead of silently corrupting the
    # skeleton. Input-data mutation is not checked here (harmless in the
    # batched pipeline; several built-ins deliberately mutate the input
    # as scratch space).
    snap <- skeleton_snapshot(skeleton)
    do.call(
      reg$fn,
      c(
        list(skeleton, data, id_name = id_col, codes = prefixed_codes),
        reg$fn_args
      )
    )
    validate_skeleton_after_add(
      skeleton,
      snap,
      expected_new_cols = names(prefixed_codes),
      context = sprintf("$register_codes(%s)", reg$label %||% "<anon>")
    )
  }

  # Combined (combine_as)
  if (!is.null(reg$combine_as)) {
    all_groups <- unique(unlist(reg$groups))
    data_list <- Filter(
      function(x) !is.null(x) && nrow(x) > 0,
      lapply(all_groups, function(g) batch_data[[g]])
    )
    if (length(data_list) > 0) {
      combined_data <- data.table::rbindlist(
        data_list,
        use.names = TRUE,
        fill = TRUE
      )
      combined_codes <- stats::setNames(
        reg$codes,
        paste0(reg$combine_as, "_", names(reg$codes))
      )
      snap <- skeleton_snapshot(skeleton)
      do.call(
        reg$fn,
        c(
          list(
            skeleton,
            combined_data,
            id_name = id_col,
            codes = combined_codes
          ),
          reg$fn_args
        )
      )
      validate_skeleton_after_add(
        skeleton,
        snap,
        expected_new_cols = names(combined_codes),
        context = sprintf(
          "$register_codes(%s, combine_as = %s)",
          reg$label %||% "<anon>", reg$combine_as
        )
      )
    }
  }

  invisible(skeleton)
}


# Per-column counts emitted by Skeleton$apply_code_entry() for every column
# a registry entry contributes to the skeleton. Stored on the entry's
# applied_registry record (under $counts) so the meta sidecar serialises
# them automatically and $compute_summary() can sum across batches without
# touching the heavy skeleton data files.
#
# Returns a named list keyed by column name; each value is a small list:
#   $n_persons_with       distinct ids where the column was TRUE in this batch
#   $n_person_weeks_with  rows where the column was TRUE in this batch
#
# Only logical columns get counts; non-logical columns (factor / numeric /
# character demographics added by randvars or framework) are silently
# skipped because $compute_summary() reports cohort presence, not value
# distributions -- those belong in $compute_population() or downstream
# Table 1 logic.
.compute_entry_column_counts <- function(dt, cols, id_col) {
  if (length(cols) == 0L) return(list())
  ids_all <- dt[[id_col]]
  out <- vector("list", length(cols))
  names(out) <- cols
  for (col in cols) {
    v <- dt[[col]]
    if (!is.logical(v)) next
    n_weeks <- sum(v, na.rm = TRUE)
    n_persons <- if (n_weeks == 0L) 0L else data.table::uniqueN(ids_all[which(v)])
    out[[col]] <- list(
      n_persons_with      = as.integer(n_persons),
      n_person_weeks_with = as.integer(n_weeks)
    )
  }
  # Strip skipped (NULL) entries so callers can `length(counts)` cleanly.
  out[!vapply(out, is.null, logical(1))]
}


# Compute a stable-across-sessions xxhash64 digest of a function's body and
# formal arguments. Used by RegistryStudy$process_skeletons() to detect
# edits to the framework_fn / randvars_fns closures so phase 1 and phase 3
# can re-run on exactly the batches that need it.
#
# We deliberately hash only list(body(fn), formals(fn)) and not fn itself,
# because the full function object includes its enclosing environment,
# which varies across R sessions and would make hashes non-deterministic.
.hash_function <- function(fn) {
  stopifnot(is.function(fn))
  digest::digest(
    list(body = body(fn), formals = formals(fn)),
    algo = "xxhash64"
  )
}

# Compute a stable fingerprint for one PRIMARY code_registry entry. Two
# primary entries with identical (codes, label, groups, fn_args,
# combine_as) produce the same fingerprint and are therefore "the same
# entry" across runs. Deliberately excludes `fn` for the same reason as
# .hash_function().
#
# Derived entries are NOT fingerprinted via this helper: their fingerprint
# depends on the fingerprints of upstream primary entries (so that edits
# to an upstream primary's `fn_args` or `groups` cascade into a derived
# re-apply), which can only be computed in the two-pass walk inside
# RegistryStudy$code_registry_fingerprints(). Passing a derived entry
# here is a programming error and triggers a loud stop().
#
# Preserves the EXACT primary payload shape from the pre-derived release
# so existing skeletons don't cascade a full rebuild just because the
# derived feature landed.
.fingerprint_entry <- function(reg) {
  kind <- reg$kind %||% "primary"
  if (identical(kind, "derived")) {
    stop(
      "Derived entries must be fingerprinted via ",
      "RegistryStudy$code_registry_fingerprints() so upstream primary ",
      "fingerprints are folded in.",
      call. = FALSE
    )
  }
  digest::digest(
    list(
      codes      = reg$codes,
      label      = reg$label,
      groups     = reg$groups,
      fn_args    = reg$fn_args,
      combine_as = reg$combine_as
    ),
    algo = "xxhash64"
  )
}

# Collapse sequential integer runs into a compact range string.
# c(1, 2, 3, 5, 6, 7, 10) -> "1-3, 5-7, 10"
# Empty input returns "(none)".
.format_batch_range <- function(batches) {
  if (length(batches) == 0L) return("(none)")
  x <- sort(unique(as.integer(batches)))
  diffs <- c(Inf, diff(x))
  starts <- x[diffs != 1L]
  ends <- c(x[which(diffs != 1L)[-1] - 1L], x[length(x)])
  parts <- ifelse(starts == ends,
                  as.character(starts),
                  paste0(starts, "-", ends))
  paste(parts, collapse = ", ")
}


# Execute the full three-phase pipeline for ONE batch. Extracted to a
# file-level helper so both the serial branch of process_skeletons() and
# its callr subprocess workers can call the exact same code.
#
# Phase 1 (framework): load the existing skeleton; if missing OR its
# framework_fn_hash doesn't match the current framework's hash, rebuild
# the base skeleton from scratch and reset phase-2 and phase-3 state.
#
# Phase 3 (randvars): hand the divergence-point rewind-and-replay logic
# to Skeleton$sync_randvars().
#
# Phase 2 (codes): hand the per-entry diff to Skeleton$sync_with_registry().
#
# batch_data is loaded lazily inside `load_bd()` so the rawbatch read is
# shared across phases (or skipped entirely when nothing needs to run).
#
# The `framework_hash`, `randvars_hashes`, and `current_fps` arguments
# are passed in rather than recomputed per batch because the hashes are
# stable across the whole process_skeletons() run -- cheaper to compute
# once up-front.
.process_one_batch <- function(study, i,
                               framework_hash,
                               randvars_hashes,
                               current_fps) {

  # Meta-only fast path: read the few-KB sidecar before touching the
  # full skeleton. If every stored hash matches the current pipeline,
  # this batch is already up to date and we return without paying the
  # full deserialise cost.
  meta <- study$load_skeleton_meta(i)
  if (.meta_matches_pipeline(meta, framework_hash, randvars_hashes, current_fps)) {
    return(invisible(NULL))
  }

  sk <- study$load_skeleton(i)
  batch_data <- NULL
  load_bd <- function() {
    if (is.null(batch_data)) batch_data <<- study$load_rawbatch(i)
    batch_data
  }

  # Phase 1: framework — full rebuild on hash change (or when no
  # skeleton exists yet for this batch)
  if (is.null(sk) || !identical(sk$framework_fn_hash, framework_hash)) {
    bd <- load_bd()
    base_dt <- study$framework_fn(bd, study)
    if (!data.table::is.data.table(base_dt)) {
      stop(
        "framework_fn must return a data.table; got ",
        paste(class(base_dt), collapse = "/"),
        " for batch ", i,
        call. = FALSE
      )
    }
    sk <- Skeleton$new(data = base_dt, batch_number = i)
    sk$framework_fn_hash <- framework_hash
    # New base -> phase-2 and phase-3 state must re-apply
    sk$applied_registry <- list()
    sk$randvars_state <- list()
  }

  # Phase 3: randvars — divergence-point rewind and replay
  sk$sync_randvars(
    randvars_fns      = study$randvars_fns,
    randvars_hashes   = randvars_hashes,
    batch_data_loader = load_bd,
    config            = study
  )

  # Phase 2: codes — incremental per-entry sync
  sk$sync_with_registry(
    current_fps       = current_fps,
    registry          = study$code_registry,
    batch_data_loader = load_bd,
    id_col            = study$id_col
  )

  study$save_skeleton(sk)
  invisible(sk)
}


.REGISTRY_STUDY_SCHEMA_VERSION <- 5L

# Build the meta sidecar payload from a fully-built skeleton + the per-batch
# code-check accumulator snapshot. Stored next to the skeleton file as
# meta_%05d.qs2 by RegistryStudy$save_skeleton(). The meta-only fast path
# in .process_one_batch() reads this and skips loading the heavy skeleton
# entirely if every hash matches.
.build_skeleton_meta <- function(sk, id_col = "id") {
  ids <- sk$data[[id_col]]
  list(
    schema_version    = .REGISTRY_STUDY_SCHEMA_VERSION,
    swereg_version    = as.character(utils::packageVersion("swereg")),
    framework_fn_hash = sk$framework_fn_hash,
    randvars_state    = sk$randvars_state,
    applied_registry  = sk$applied_registry,
    n_rows            = nrow(sk$data),
    n_persons         = if (is.null(ids)) NA_integer_ else data.table::uniqueN(ids),
    built_at          = Sys.time()
  )
}

# True iff the meta entry is structurally valid AND the schema version
# matches AND every persisted hash matches the corresponding "current"
# hash from the run-wide pipeline state. The fast-path skip is all-or-
# nothing: any field that disagrees forces a load_skeleton() + per-phase
# replay through the existing logic.
.meta_matches_pipeline <- function(meta, framework_hash, randvars_hashes, current_fps) {
  if (is.null(meta)) return(FALSE)
  if (!identical(meta$schema_version, .REGISTRY_STUDY_SCHEMA_VERSION)) return(FALSE)
  if (!identical(meta$framework_fn_hash, framework_hash)) return(FALSE)

  # Randvars: compare values + names. Empty cases need to compare as
  # empty regardless of representation (NULL list vs named character(0)).
  stored_randvars_hashes <- vapply(
    meta$randvars_state %||% list(),
    function(x) x$fn_hash %||% NA_character_,
    character(1)
  )
  if (!identical(unname(stored_randvars_hashes), unname(as.character(randvars_hashes)))) return(FALSE)
  if (!identical(names(meta$randvars_state) %||% character(0),
                 names(randvars_hashes)     %||% character(0))) return(FALSE)

  # Code registry fingerprints: compare as character vectors. Empty list's
  # names() is NULL; empty fingerprint set is character(0); coerce both
  # to character(0) before comparing.
  stored_fp <- names(meta$applied_registry) %||% character(0)
  if (!identical(stored_fp, unname(as.character(current_fps)))) return(FALSE)

  # Per-column counts must be present on every entry. Older meta sidecars
  # (built before $compute_summary() existed) lack this field; treat them
  # as stale so the next $process_skeletons() rebuilds the batch and
  # populates the counts for the summary aggregator.
  if (length(meta$applied_registry) > 0L) {
    if (any(vapply(meta$applied_registry, function(e) is.null(e$counts),
                   logical(1)))) return(FALSE)
  }

  TRUE
}

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
#' code registry, and the three-phase orchestrated per-batch processing
#' (framework -> randvars -> codes) that produces one [Skeleton] file
#' per batch with incremental invalidation.
#'
#' @section Portable Directory Resolution:
#' Directories are stored as candidate path vectors and resolved lazily via
#' [CandidatePath] active bindings. The first existing directory wins and is
#' cached. If the cached path becomes invalid (e.g. after moving to a
#' different machine), the binding automatically re-resolves from the
#' candidate list.
#'
#' @section Three-phase pipeline:
#' `$process_skeletons()` runs three phases per batch, with per-phase
#' incremental invalidation so editing one step only re-runs what it
#' affects:
#' \describe{
#'   \item{Phase 1 -- framework}{A single user function registered via
#'     `$register_framework(fn)`, signature `(batch_data, config)`, returns
#'     a fresh base `data.table` (time grid + structural censoring).
#'     Full rebuild on `body(fn)` / `formals(fn)` hash change.}
#'   \item{Phase 3 -- randvars}{An ordered named list of user functions
#'     registered via `$register_randvars(name, fn)`, each signature
#'     `(skeleton, batch_data, config)`. Divergence-point rewind-and-replay
#'     invalidation: the first step whose name or hash differs from the
#'     stored sequence triggers a drop of its columns and replay of it plus
#'     everything downstream of it. Add/remove/edit/reorder all handled
#'     uniformly.}
#'   \item{Phase 2 -- codes}{The declarative code registry, built via
#'     `$register_codes()` (primary) and `$register_derived_codes()`
#'     (derived). Per-entry fingerprint diff: entries no longer present
#'     are dropped, new or modified entries are freshly applied. Derived
#'     entry fingerprints fold in their upstream primary fingerprints so
#'     upstream behavior edits cascade correctly.}
#' }
#' Phase 2 runs AFTER phase 3, so phase-3 steps cannot read phase-2
#' columns. See the [Skeleton] class for the on-disk provenance format.
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
#' # Phase 3: randvars (ordered user steps; order = execution order)
#' study$register_randvars("demographics", my_demographics_fn)
#' study$register_randvars("exposure",     my_exposure_fn)
#'
#' # Phase 2: codes. Primary entries first, derived entries after.
#' study$register_codes(
#'   codes      = list(f20 = c("F20"), vte = c("I26", "I80")),
#'   fn         = swereg::add_diagnoses,
#'   groups     = list(ov = "outpatient", sv = "inpatient"),
#'   combine_as = "os"
#' )
#' study$register_codes(
#'   codes   = list(f20 = c("F20"), vte = c("I26", "I80")),
#'   fn      = swereg::add_cods,
#'   fn_args = list(cod_type = "underlying"),
#'   groups  = list(dorsu = "dors")
#' )
#' study$register_codes(
#'   codes   = list(f20 = c("F20"), vte = c("I26", "I80")),
#'   fn      = swereg::add_cods,
#'   fn_args = list(cod_type = "multiple"),
#'   groups  = list(dorsm = "dors")
#' )
#' # Build osd_f20 = os_f20 | dorsu_f20 | dorsm_f20 (same codes list
#' # shared by reference so an edit in one place cascades to all four)
#' study$register_derived_codes(
#'   codes = list(f20 = c("F20"), vte = c("I26", "I80")),
#'   from  = c("os", "dorsu", "dorsm"),
#'   as    = "osd"
#' )
#'
#' study$set_ids(ids)
#' study$save_rawbatch("lmed", lmed_data)
#' study$describe_codes()
#' study$process_skeletons(n_workers = 4L)
#'
#' # Per-batch provenance and cross-batch consistency check
#' sk <- study$load_skeleton(1L)
#' sk$pipeline_hash() == study$pipeline_hash()  # TRUE iff in sync
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

    #' @field data_pipeline_snapshot_cp [CandidatePath] for the
    #'   pipeline-snapshot directory (one TSV file per host, git-tracked),
    #'   or NULL if the feature is not configured. When NULL,
    #'   `$write_pipeline_snapshot()` is a silent no-op.
    data_pipeline_snapshot_cp = NULL,

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

    #' @field randvars_fns Named ordered list of phase-3 functions, each
    #'   with signature `(skeleton, batch_data, config)`. Populated via
    #'   `$register_randvars(name, fn)`. Registration order is execution
    #'   order. `$process_skeletons()` uses
    #'   `Skeleton$sync_randvars()`'s divergence-point rewind-and-replay
    #'   to apply changes incrementally.
    randvars_fns = NULL,

    #' @field host_label Optional character scalar. Overrides
    #'   `Sys.info()[["nodename"]]` when naming the per-host pipeline
    #'   snapshot file. Useful when hostnames are ambiguous or overly
    #'   dynamic.
    host_label = NULL,

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
    #' @param data_pipeline_snapshot_dir Optional character vector of
    #'   candidate paths for a git-tracked pipeline-snapshot directory
    #'   (one TSV per host). When NULL (default), the snapshot feature is
    #'   disabled and `$write_pipeline_snapshot()` is a no-op.
    #' @param data_summaries_dir Optional character vector of candidate
    #'   paths for the audit-track summaries directory (typically inside
    #'   the project git repo, e.g. `dev/summaries/`). When NULL
    #'   (default), `$compute_summary()` still writes `summary.qs2` and
    #'   `status.txt` to the skeleton directory but skips the
    #'   git-tracked TSV.
    #' @param batch_size Integer. Number of IDs per batch. Default: 1000L.
    #' @param seed Integer. Shuffle seed.
    #' @param id_col Character. Person ID column name.
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
      data_pipeline_snapshot_dir = NULL,
      data_summaries_dir = NULL,
      batch_size = 1000L,
      seed = 4L,
      id_col = "lopnr"
    ) {
      self$data_rawbatch_cp <- CandidatePath$new(data_rawbatch_dir, "data_rawbatch_dir")
      self$data_skeleton_cp <- CandidatePath$new(data_skeleton_dir, "data_skeleton_dir")
      self$data_meta_cp     <- CandidatePath$new(data_meta_dir,     "data_meta_dir")
      if (!is.null(data_raw_dir)) {
        self$data_raw_cp <- CandidatePath$new(data_raw_dir, "data_raw_dir")
      }
      if (!is.null(data_pipeline_snapshot_dir)) {
        self$data_pipeline_snapshot_cp <- CandidatePath$new(
          data_pipeline_snapshot_dir, "data_pipeline_snapshot_dir"
        )
      }
      if (!is.null(data_summaries_dir)) {
        self$data_summaries_cp <- CandidatePath$new(
          data_summaries_dir, "data_summaries_dir"
        )
      }
      self$group_names <- group_names
      self$batch_size <- as.integer(batch_size)
      self$seed <- as.integer(seed)
      self$id_col <- id_col

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

      invisible(self)
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
          class(self)[1], " on disk has schema version ", saved,
          " but this swereg requires version ", current, ".\n",
          "Regenerate by re-running the upstream registrystudy generator ",
          "(e.g. run_generic_create_datasets_v2.R). Note: schema v4 also ",
          "changed the Skeleton file format; running $process_skeletons() ",
          "auto-migrates existing bare-data.table skeleton files on first ",
          "load but re-runs the full pipeline once to populate the new ",
          "provenance fields.",
          call. = FALSE
        )
      }
      invisible(TRUE)
    },

    # --- Phase registration (framework + randvars) ---

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
      invisible(self)
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
    #'   Editing `fn`'s body (keeping the same `name`) changes the hash
    #'   and triggers a re-run of this step and everything downstream of
    #'   it in the sequence.
    #' @param name Character scalar. The user-facing step name. Used as
    #'   the key in `Skeleton$randvars_state` and in the divergence-point
    #'   comparison.
    #' @param fn A function of signature `(skeleton, batch_data, config)`.
    #' @return `invisible(self)`.
    register_randvars = function(name, fn) {
      stopifnot(
        is.character(name), length(name) == 1L, nzchar(name),
        is.function(fn)
      )
      if (is.null(self$randvars_fns)) self$randvars_fns <- list()
      if (name %in% names(self$randvars_fns)) {
        stop(
          "A phase-3 step named '", name, "' is already registered. ",
          "Phase-3 step names must be unique.",
          call. = FALSE
        )
      }
      self$randvars_fns[[name]] <- fn
      invisible(self)
    },

    # --- Code registry fingerprints + adopt runtime state ---

    #' @description Return the xxhash64 fingerprint of every entry in
    #'   `self$code_registry`, in registry order.
    #'
    #'   Primary entries: fingerprint depends on
    #'   `(codes, label, groups, fn_args, combine_as)` -- two primary
    #'   entries with identical config produce the same fingerprint and
    #'   are therefore treated as "the same entry" across runs.
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
      n <- length(self$code_registry)
      if (n == 0L) return(character(0))

      fps <- character(n)
      # Pass 1: primary fingerprints.
      for (i in seq_len(n)) {
        reg <- self$code_registry[[i]]
        if (!identical(reg$kind %||% "primary", "derived")) {
          fps[i] <- .fingerprint_entry(reg)
        }
      }
      # Pass 2: derived fingerprints, folding in upstream primary fps.
      for (i in seq_len(n)) {
        reg <- self$code_registry[[i]]
        if (!identical(reg$kind %||% "primary", "derived")) next
        upstream <- character()
        for (j in seq_len(i - 1L)) {
          pri <- self$code_registry[[j]]
          if (identical(pri$kind %||% "primary", "derived")) next
          prefixes <- c(names(pri$groups), pri$combine_as)
          prefixes <- prefixes[!is.null(prefixes) & nzchar(prefixes)]
          if (any(prefixes %in% reg$from)) {
            upstream <- c(upstream, fps[j])
          }
        }
        fps[i] <- digest::digest(
          list(
            kind     = "derived",
            codes    = reg$codes,
            from     = reg$from,
            as       = reg$as,
            upstream = upstream
          ),
          algo = "xxhash64"
        )
      }
      fps
    },

    #' @description Compute this study's current total pipeline hash from
    #'   the registered framework, randvars sequence, and code registry.
    #'   Answer to "what would a freshly-built skeleton look like?"
    #'
    #'   Invariant: `sk$pipeline_hash() == study$pipeline_hash()` iff the
    #'   skeleton is fully synced with the study's current registered
    #'   framework + randvars + codes.
    #' @return A single character string (xxhash64 digest).
    pipeline_hash = function() {
      randvars_hashes <- if (length(self$randvars_fns) == 0L) {
        character(0)
      } else {
        vapply(self$randvars_fns, .hash_function, character(1))
      }
      framework_hash <- if (is.null(self$framework_fn)) {
        NA_character_
      } else {
        .hash_function(self$framework_fn)
      }
      digest::digest(
        list(
          framework = framework_hash,
          randvars  = randvars_hashes,
          codes     = self$code_registry_fingerprints()
        ),
        algo = "xxhash64"
      )
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
      self$n_ids         <- other$n_ids
      self$n_batches     <- other$n_batches
      self$batch_id_list <- other$batch_id_list
      self$groups_saved  <- other$groups_saved
      invisible(self)
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
      invisible(self)
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
        kind  = "derived",
        codes = codes,
        from  = from,
        as    = as,
        label = sprintf(
          "derived: %s_* = %s",
          as,
          paste(paste0(from, "_*"), collapse = " | ")
        )
      )
      self$code_registry[[length(self$code_registry) + 1L]] <- entry
      invisible(self)
    },

    #' @description Print human-readable description of all registered codes.
    describe_codes = function() {
      if (length(self$code_registry) == 0) {
        cat("No codes registered.\n")
        return(invisible(self))
      }

      for (reg in self$code_registry) {
        codes <- reg$codes
        cat(sprintf(
          "\n=== %s (%d entries) ===\n",
          reg$label,
          length(codes)
        ))

        if (identical(reg$kind %||% "primary", "derived")) {
          cat(sprintf(
            "  Kind: derived (%s_* = %s)\n",
            reg$as,
            paste(paste0(reg$from, "_*"), collapse = " | ")
          ))
        } else {
          # Describe groups
          group_descs <- vapply(seq_along(reg$groups), function(i) {
            prefix <- names(reg$groups)[i]
            grps <- reg$groups[[i]]
            if (is.null(prefix) || !nzchar(prefix)) {
              paste(grps, collapse = " + ")
            } else {
              paste0(prefix, " (", paste(grps, collapse = " + "), ")")
            }
          }, character(1))
          cat(sprintf("  Groups: %s\n", paste(group_descs, collapse = ", ")))

          if (!is.null(reg$combine_as)) {
            cat(sprintf("  Combined as: %s_*\n", reg$combine_as))
          }

          # Extra fn_args
          if (length(reg$fn_args) > 0) {
            args_str <- paste(
              names(reg$fn_args),
              vapply(reg$fn_args, deparse, character(1)),
              sep = " = ",
              collapse = ", "
            )
            cat(sprintf("  Extra args: %s\n", args_str))
          }
        }

        cat("\n")
        for (nm in names(codes)) {
          code_val <- codes[[nm]]
          code_str <- if (isTRUE(code_val)) {
            "event flag"
          } else if (is.call(code_val) || is.name(code_val)) {
            deparse(code_val)
          } else {
            paste(code_val, collapse = ", ")
          }
          cat(sprintf(
            "  %s: %s\n",
            nm,
            code_str
          ))
          gen_cols <- .generated_columns_for_entry(reg, nm)
          cat(sprintf(
            "    -> columns: %s\n",
            paste(gen_cols, collapse = ", ")
          ))
        }
      }
      invisible(self)
    },

    #' @description Return a data.table summarizing all registered codes.
    #' @return data.table with columns: name, codes, label, generated_columns.
    summary_table = function() {
      rows <- list()

      for (reg in self$code_registry) {
        for (nm in names(reg$codes)) {
          gen_cols <- .generated_columns_for_entry(reg, nm)
          code_val <- reg$codes[[nm]]
          code_str <- if (isTRUE(code_val)) {
            "event flag"
          } else if (is.call(code_val) || is.name(code_val)) {
            deparse(code_val)
          } else {
            paste(code_val, collapse = ", ")
          }
          rows[[length(rows) + 1L]] <- list(
            name = nm,
            codes = code_str,
            label = reg$label,
            generated_columns = paste(gen_cols, collapse = ", ")
          )
        }
      }

      if (length(rows) == 0) {
        return(data.table::data.table(
          name = character(0),
          codes = character(0),
          label = character(0),
          generated_columns = character(0)
        ))
      }
      data.table::rbindlist(rows)
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
      invisible(skeleton)
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

      invisible(self)
    },

    #' @description Save rawbatch files for one group.
    #' @param group Character. Group name (must be in group_names).
    #' @param data data.table or named list of data.tables.
    save_rawbatch = function(group, data) {
      if (!group %in% self$group_names) {
        stop(
          "group '",
          group,
          "' not in group_names: ",
          paste(self$group_names, collapse = ", ")
        )
      }

      if (group %in% self$groups_saved) {
        cat(
          "Skipping '",
          group,
          "' -- all rawbatch files already exist\n",
          sep = ""
        )
        return(invisible(self))
      }

      id_col <- self$id_col
      n_threads <- parallel::detectCores()

      for (b in seq_along(self$batch_id_list)) {
        batch_ids <- self$batch_id_list[[b]]
        if (data.table::is.data.table(data)) {
          batch_data <- data[get(id_col) %in% batch_ids]
        } else {
          batch_data <- lapply(data, function(dt) {
            if (data.table::is.data.table(dt)) {
              dt[get(id_col) %in% batch_ids]
            } else {
              dt
            }
          })
        }
        outfile <- file.path(
          self$data_rawbatch_dir,
          sprintf("%05d_rawbatch_%s.qs2", b, group)
        )
        qs2::qs_save(batch_data, outfile, nthreads = n_threads)
        cat(
          "  batch",
          b,
          "/",
          self$n_batches,
          "(",
          length(batch_ids),
          "IDs) ->",
          group,
          "\n"
        )
      }

      self$groups_saved <- sort(unique(c(self$groups_saved, group)))
      invisible(self)
    },

    #' @description Load rawbatch files for a single batch.
    #' @param batch_number Integer. 1-indexed batch number.
    #' @return Named list of data.tables.
    load_rawbatch = function(batch_number) {
      if (batch_number < 1 || batch_number > self$n_batches) {
        stop(
          "batch_number must be between 1 and ",
          self$n_batches,
          " (got ",
          batch_number,
          ")"
        )
      }

      n_threads <- data.table::getDTthreads()
      result <- list()

      for (g in self$group_names) {
        fpath <- file.path(
          self$data_rawbatch_dir,
          sprintf("%05d_rawbatch_%s.qs2", batch_number, g)
        )
        if (!file.exists(fpath)) {
          stop("Rawbatch file missing: ", fpath)
        }
        obj <- qs2_read(fpath, nthreads = n_threads)

        if (is.list(obj) && !data.table::is.data.table(obj)) {
          for (nm in names(obj)) {
            result[[nm]] <- obj[[nm]]
          }
        } else {
          result[[g]] <- obj
        }
      }

      result
    },

    #' @description Load a skeleton file for `batch_number` as a
    #'   [Skeleton] R6 object. Returns `NULL` if the file is missing
    #'   (caller rebuilds from scratch). Errors if the file on disk is
    #'   not a `Skeleton` R6 object (e.g. corrupted or from an
    #'   incompatible version of swereg).
    #' @param batch_number Integer batch index.
    #' @return A [Skeleton], or `NULL` if the file is missing.
    load_skeleton = function(batch_number) {
      path <- file.path(
        self$data_skeleton_dir,
        sprintf("skeleton_%05d.qs2", as.integer(batch_number))
      )
      if (!file.exists(path)) return(NULL)

      obj <- qs2::qs_read(path)
      if (!inherits(obj, "Skeleton")) {
        stop(
          "Skeleton file is not a Skeleton R6 object: ", path, "\n",
          "Delete the file and re-run $process_skeletons() to rebuild.",
          call. = FALSE
        )
      }
      obj$check_version()
      # qs2 round-tripping drops data.table over-allocation
      # (`truelength` becomes 0). Without this refresh, the first
      # `:=` that adds a column would silently reallocate the
      # data.table at a new memory address, leaving `obj$data`
      # pointing at the old version (because data.table rebinds the
      # caller's variable on realloc, and the caller here is the
      # helper function that received `self$data` by value).
      #
      # `setalloccol()` allocates a new data.table HEADER with
      # N free column slots (N = `getOption("datatable.alloccol",
      # 4096L)`). The actual column DATA stays shared by reference,
      # so memory overhead is ~8-16 bytes per over-allocation slot
      # -- ~32-64 KB per skeleton regardless of row count, not a
      # full copy. The assignment rebinds `obj$data` (a public R6
      # field) to the new header so it survives subsequent `:=`
      # in-place mutations without reallocation.
      #
      # 4096 headroom slots comfortably supports code registries
      # with several hundred entries. Studies that grow beyond that
      # can bump via `options(datatable.alloccol = 8192L)` at the
      # top of their generator script.
      obj$data <- data.table::setalloccol(
        obj$data,
        n = getOption("datatable.alloccol", 4096L)
      )
      obj
    },

    #' @description Save a [Skeleton] to this study's skeleton directory,
    #'   plus a small `meta_%05d.qs2` sidecar capturing provenance hashes
    #'   and the per-batch code-check accumulator snapshot. Subsequent
    #'   `$process_skeletons()` runs read the meta first and skip loading
    #'   the heavy skeleton entirely when every hash still matches.
    #'
    #'   Skeleton is written first, then meta. A crash between the two
    #'   leaves a stale meta on disk; the next run reads it, finds the
    #'   hashes don't match the current pipeline, falls through to the
    #'   slow path, and rewrites both.
    #' @param sk A [Skeleton] to persist.
    #' @return The full path the skeleton file was written to, invisibly.
    save_skeleton = function(sk) {
      stopifnot(inherits(sk, "Skeleton"))
      sk_path <- sk$save(self$data_skeleton_dir)
      meta <- .build_skeleton_meta(sk, id_col = self$id_col)
      qs2::qs_save(meta, self$skeleton_meta_path(sk$batch_number))
      invisible(sk_path)
    },

    #' @description Read the `meta_%05d.qs2` sidecar for one batch.
    #'   Returns `NULL` if missing or unreadable (treated as cache miss
    #'   by the fast path in `.process_one_batch()`).
    #' @param batch_number Integer batch index.
    #' @return A list (the meta payload) or `NULL`.
    #' @keywords internal
    load_skeleton_meta = function(batch_number) {
      path <- self$skeleton_meta_path(batch_number)
      if (!file.exists(path)) return(NULL)
      tryCatch(qs2::qs_read(path), error = function(e) NULL)
    },

    #' @description Filesystem path of a meta sidecar.
    #' @param batch_number Integer batch index.
    #' @return Character. The full path.
    #' @keywords internal
    skeleton_meta_path = function(batch_number) {
      file.path(
        self$data_skeleton_dir,
        sprintf("meta_%05d.qs2", as.integer(batch_number))
      )
    },

    #' @description Summary of per-batch pipeline hashes across all
    #'   currently-persisted skeleton files in `self$data_skeleton_dir`.
    #'   Use this to spot batches out of sync with each other or with
    #'   `self$pipeline_hash()`.
    #'
    #'   Files that are not valid `Skeleton` R6 objects (e.g. unreadable
    #'   or corrupted) surface as rows with `NA` `pipeline_hash` and
    #'   `NA` `framework_fn_hash`.
    #' @return A `data.table` with columns: batch, pipeline_hash,
    #'   framework_fn_hash, n_randvars, n_code_entries, saved_at.
    skeleton_pipeline_hashes = function() {
      dir <- self$data_skeleton_dir
      files <- list.files(
        dir,
        pattern = "^skeleton_\\d+\\.qs2$",
        full.names = TRUE
      )
      if (length(files) == 0L) {
        return(data.table::data.table(
          batch             = integer(),
          pipeline_hash     = character(),
          framework_fn_hash = character(),
          n_randvars        = integer(),
          n_code_entries    = integer(),
          saved_at          = as.POSIXct(character())
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
            regmatches(basename(f),
                       regexec("skeleton_(\\d+)\\.qs2$", basename(f)))[[1]][2]
          )
          p(message = sprintf("batch %d", batch))

          meta <- self$load_skeleton_meta(batch)
          if (!is.null(meta)) {
            randvars_hashes <- vapply(
              meta$randvars_state,
              function(x) x$fn_hash %||% NA_character_,
              character(1)
            )
            pipeline_hash <- digest::digest(
              list(
                framework = meta$framework_fn_hash,
                randvars  = randvars_hashes,
                codes     = names(meta$applied_registry) %||% character(0)
              ),
              algo = "xxhash64"
            )
            return(data.table::data.table(
              batch             = batch,
              pipeline_hash     = pipeline_hash,
              framework_fn_hash = meta$framework_fn_hash %||% NA_character_,
              n_randvars        = length(meta$randvars_state),
              n_code_entries    = length(meta$applied_registry),
              saved_at          = meta$built_at %||% as.POSIXct(NA)
            ))
          }

          # Fallback: meta missing -> load full skeleton.
          obj <- tryCatch(qs2::qs_read(f), error = function(e) NULL)
          if (inherits(obj, "Skeleton")) {
            return(data.table::data.table(
              batch             = batch,
              pipeline_hash     = obj$pipeline_hash(),
              framework_fn_hash = obj$framework_fn_hash %||% NA_character_,
              n_randvars        = length(obj$randvars_state),
              n_code_entries    = length(obj$applied_registry),
              saved_at          = obj$created_at %||% as.POSIXct(NA)
            ))
          }
          # Unreadable or not a Skeleton R6: surface with NA
          data.table::data.table(
            batch             = batch,
            pipeline_hash     = NA_character_,
            framework_fn_hash = NA_character_,
            n_randvars        = NA_integer_,
            n_code_entries    = NA_integer_,
            saved_at          = as.POSIXct(NA)
          )
        })
      })
      out <- data.table::rbindlist(rows)
      data.table::setorder(out, batch)
      out[]
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
          "No skeleton files found in ", self$data_skeleton_dir,
          ". Run $process_skeletons() first.",
          call. = FALSE
        )
      }

      if (any(is.na(hashes$pipeline_hash))) {
        bad <- hashes[is.na(pipeline_hash), batch]
        stop(
          "Skeleton files have no pipeline hash (unreadable or not a ",
          "Skeleton R6 object): batches ", .format_batch_range(bad),
          ". Delete the affected files and re-run $process_skeletons().",
          call. = FALSE
        )
      }

      unique_hashes <- unique(hashes$pipeline_hash)
      if (length(unique_hashes) > 1L) {
        counts <- hashes[, .N, by = pipeline_hash]
        stop(
          "Inconsistent skeleton pipeline hashes across batches. Found ",
          length(unique_hashes), " distinct hashes:\n",
          paste0("  ", counts$pipeline_hash, " (", counts$N, " batches)",
                 collapse = "\n"),
          "\nRun $process_skeletons() to bring all batches up to date. ",
          "See $skeleton_pipeline_hashes() for the per-batch breakdown.",
          call. = FALSE
        )
      }

      current <- self$pipeline_hash()
      if (!identical(unique_hashes, current)) {
        stop(
          "Skeleton pipeline hash on disk (", unique_hashes,
          ") does not match this study's current pipeline hash (",
          current, "). Run $process_skeletons() to regenerate.",
          call. = FALSE
        )
      }

      invisible(current)
    },

    #' @description Write a one-row TSV snapshot of this host's current
    #'   pipeline state to `data_pipeline_snapshot_dir` / `host_label.tsv`
    #'   (one file per host). The file is OVERWRITTEN (not appended) on
    #'   each call, so concurrent runs from different hosts never
    #'   conflict in git. The chronological audit trail is
    #'   `git log -p dev/pipeline_snapshots/your_host.tsv`.
    #'
    #'   Silently skipped when `self$data_pipeline_snapshot_cp` is NULL
    #'   (feature not configured) or when the candidate directory does
    #'   not exist on the current host (e.g. hosts without the git repo
    #'   mounted).
    #'
    #'   The `host_label` defaults to `Sys.info()[["nodename"]]` but can
    #'   be overridden by setting `self$host_label` when hostnames are
    #'   ambiguous.
    #' @return Invisibly: the written path, or NULL if skipped.
    write_pipeline_snapshot = function() {
      if (is.null(self$data_pipeline_snapshot_cp)) {
        return(invisible(NULL))
      }
      dir <- tryCatch(
        self$data_pipeline_snapshot_cp$resolve(),
        error = function(e) NULL
      )
      if (is.null(dir)) {
        message(
          "Pipeline snapshot dir not found on any host, skipping ",
          "($write_pipeline_snapshot)"
        )
        return(invisible(NULL))
      }

      host <- self$host_label %||% Sys.info()[["nodename"]]
      file <- file.path(dir, paste0(host, ".tsv"))

      hashes <- self$skeleton_pipeline_hashes()
      current_hash <- self$pipeline_hash()
      all_consistent <- nrow(hashes) > 0L &&
        !any(is.na(hashes$pipeline_hash)) &&
        all(hashes$pipeline_hash == current_hash)

      row <- data.table::data.table(
        host = host,
        updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        pipeline_hash = current_hash,
        framework_fn_hash = if (is.null(self$framework_fn)) {
          NA_character_
        } else {
          .hash_function(self$framework_fn)
        },
        randvars_steps = paste(names(self$randvars_fns), collapse = ","),
        n_code_entries = length(self$code_registry),
        n_batches = nrow(hashes),
        all_batches_consistent = all_consistent,
        batches_at_current_hash = .format_batch_range(
          hashes[pipeline_hash == current_hash, batch]
        )
      )
      data.table::fwrite(row, file, sep = "\t")
      message("Pipeline snapshot written: ", file)
      message(
        "  git add ", shQuote(file),
        " && git commit -m 'pipeline snapshot: ", host,
        " @ ", substr(current_hash, 1, 8), "'"
      )
      invisible(file)
    },

    #' @description Orchestrate the three-phase skeleton pipeline per batch.
    #'
    #'   Reads `self$framework_fn` (phase 1), `self$randvars_fns` (phase 3),
    #'   and `self$code_registry` (phase 2) from the study and applies them
    #'   via the incremental logic on [Skeleton]. Exact per-batch work:
    #'
    #'   1. Load existing skeleton via `self$load_skeleton(i)`. If missing
    #'      OR its `framework_fn_hash` doesn't match the current
    #'      framework's hash, rebuild the base skeleton from scratch by
    #'      calling `self$framework_fn(batch_data, self)` and wrapping in
    #'      a fresh [Skeleton]. (Phase 1.)
    #'   2. Call `sk$sync_randvars()` with the current ordered
    #'      `self$randvars_fns` and their body/formals hashes. Divergence-
    #'      point rewind-and-replay semantics drop and re-run the
    #'      affected phase-3 steps only. (Phase 3.)
    #'   3. Call `sk$sync_with_registry()` with
    #'      `self$code_registry_fingerprints()`. Entries present on disk
    #'      but not in the current registry are dropped (via
    #'      `.entry_columns()` on the stored descriptor); entries present
    #'      in the current registry but not on disk are applied fresh.
    #'      (Phase 2.)
    #'   4. Save via `self$save_skeleton(sk)`.
    #'
    #'   `batch_data` is loaded lazily -- exactly once per batch, by
    #'   whichever phase needs it first. If no phase needs it (everything
    #'   already in sync), the rawbatch read is skipped entirely and the
    #'   per-batch work is just load → save.
    #'
    #'   At the end of the full batch loop, `self$write_pipeline_snapshot()`
    #'   is called (silently no-ops when `data_pipeline_snapshot_cp` is
    #'   NULL).
    #'
    #' @param batches Integer vector of batch indices to process, or
    #'   `NULL` (default) for all batches in `self$batch_id_list`.
    #' @param n_workers Integer. Number of parallel workers (1 = sequential).
    #'   When `> 1`, each batch runs in a fresh callr subprocess.
    #' @param ... Additional arguments (unused; reserved for future use).
    #' @return `invisible(self)`.
    process_skeletons = function(
      batches = NULL,
      n_workers = 1L,
      ...
    ) {
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

      if (is.null(batches)) {
        batches <- seq_len(self$n_batches)
      }

      framework_hash <- .hash_function(self$framework_fn)
      randvars_hashes <- if (length(self$randvars_fns) == 0L) {
        character(0)
      } else {
        vapply(self$randvars_fns, .hash_function, character(1))
      }
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
                study           = self,
                i               = i,
                framework_hash  = framework_hash,
                randvars_hashes = randvars_hashes,
                current_fps     = current_fps
              ),
              error = function(e) {
                stop(
                  sprintf(
                    "process_skeletons() halted on batch %d: %s\n\nSuccessful batches up to this point are persisted on disk; rerun with `batches = ...` to retry from this one.",
                    i, conditionMessage(e)
                  ),
                  call. = FALSE
                )
              }
            )
            gc()
            p(message = sprintf(
              "%s batch %d", format(Sys.time(), "%H:%M:%S"), i
            ))
          }
        })
      } else {
        threads_per_worker <- max(
          1L,
          floor(parallel::detectCores() / n_workers)
        )
        cat(sprintf(
          "Running %d batches: %d workers x %d threads each\n",
          length(batches),
          n_workers,
          threads_per_worker
        ))
        dev_path <- .swereg_dev_path()

        .launch_batch <- function(batch_idx, study_snapshot) {
          callr::r_bg(
            func = function(
              study_snapshot,
              batch_idx,
              framework_hash,
              randvars_hashes,
              current_fps,
              threads_per_worker,
              dev_path
            ) {
              requireNamespace("data.table")
              data.table::setDTthreads(threads_per_worker)
              if (!is.null(dev_path)) {
                getExportedValue("devtools", "load_all")(dev_path, quiet = TRUE)
              } else {
                requireNamespace("swereg")
              }
              # Use the non-exported file-level helper via getFromNamespace
              # so the worker subprocess resolves it via the swereg namespace
              # (rather than the caller's environment).
              .process_one_batch <- getFromNamespace(
                ".process_one_batch", "swereg"
              )
              # No worker-level session: .process_one_batch() opens its
              # own per-batch session, snapshots into the meta sidecar,
              # and closes it. The parent reads every batch's meta after
              # the loop finishes and emits one consolidated warning
              # covering all workers.
              .process_one_batch(
                study           = study_snapshot,
                i               = batch_idx,
                framework_hash  = framework_hash,
                randvars_hashes = randvars_hashes,
                current_fps     = current_fps
              )
              NULL
            },
            args = list(
              study_snapshot     = study_snapshot,
              batch_idx          = batch_idx,
              framework_hash     = framework_hash,
              randvars_hashes    = randvars_hashes,
              current_fps        = current_fps,
              threads_per_worker = threads_per_worker,
              dev_path           = dev_path
            ),
            package = FALSE,
            supervise = TRUE
          )
        }

        progressr::with_progress({
          p <- progressr::progressor(steps = length(batches))
          active <- list()
          next_i <- 1L

          while (length(active) < n_workers && next_i <= length(batches)) {
            slot <- as.character(length(active) + 1L)
            active[[slot]] <- list(
              proc = .launch_batch(batches[next_i], self),
              idx = next_i
            )
            next_i <- next_i + 1L
          }

          while (length(active) > 0) {
            Sys.sleep(0.5)
            finished_slots <- c()
            for (slot in names(active)) {
              if (!active[[slot]]$proc$is_alive()) {
                finished_slots <- c(finished_slots, slot)
                idx <- active[[slot]]$idx
                tick_msg <- tryCatch(
                  {
                    active[[slot]]$proc$get_result()
                    sprintf(
                      "%s batch %d",
                      format(Sys.time(), "%H:%M:%S"),
                      batches[idx]
                    )
                  },
                  error = function(e) {
                    # Kill any other in-flight workers before raising
                    # so we don't keep burning compute on what is
                    # likely a systematic failure.
                    for (other_slot in names(active)) {
                      tryCatch(
                        active[[other_slot]]$proc$kill_tree(),
                        error = function(e2) NULL
                      )
                    }
                    stop(
                      sprintf(
                        "process_skeletons() halted on batch %d: %s\n\nIn-flight workers were killed. Successful batches are persisted on disk; rerun with `batches = ...` to retry from this one.",
                        batches[idx], conditionMessage(e)
                      ),
                      call. = FALSE
                    )
                  }
                )
                p(message = tick_msg)
              }
            }
            for (slot in finished_slots) {
              active[[slot]] <- NULL
              if (next_i <= length(batches)) {
                active[[slot]] <- list(
                  proc = .launch_batch(batches[next_i], self),
                  idx = next_i
                )
                next_i <- next_i + 1L
              }
            }
          }
        })
      }

      self$write_pipeline_snapshot()
      invisible(self)
    },

    #' @description Compute a population table from saved skeleton files.
    #'
    #' Loads each skeleton file, counts unique persons per
    #' \code{isoyear} and user-specified structural variables,
    #' and returns a complete grid with all combinations
    #' (missing cells filled with zero).
    #'
    #' Both annual (\code{is_isoyear == TRUE}) and weekly
    #' (\code{is_isoyear == FALSE}) rows are handled: each person
    #' is counted once per \code{isoyear} per unique combination
    #' of \code{by} variables via \code{uniqueN(id)}.
    #'
    #' @param by Character vector of column names to group by
    #'   in addition to \code{isoyear}. For example,
    #'   \code{c("saab", "age")} for sex by 1-year age groups.
    #' @param batches Integer vector of batch numbers to include.
    #'   Default \code{NULL} uses all available skeleton files.
    #' @return A data.table with columns: \code{isoyear}, the
    #'   \code{by} columns, and \code{n} (person count). Also
    #'   saved as \code{population.qs2} in the skeleton directory.
    compute_population = function(by, batches = NULL) {
      files <- self$skeleton_files
      if (length(files) == 0) stop("No skeleton files found")

      if (!is.null(batches)) {
        expected <- sprintf("skeleton_%05d.qs2", batches)
        files <- files[basename(files) %in% expected]
        if (length(files) == 0) {
          stop("No skeleton files matched the specified batches")
        }
      }

      group_cols <- c("isoyear", by)

      pop_list <- vector("list", length(files))
      for (i in seq_along(files)) {
        obj <- qs2::qs_read(files[i])
        if (!inherits(obj, "Skeleton")) {
          stop(
            "Skeleton file ", basename(files[i]),
            " is not a Skeleton R6 object. Delete and re-run ",
            "$process_skeletons() to regenerate.",
            call. = FALSE
          )
        }
        skeleton <- obj$data
        missing <- setdiff(group_cols, names(skeleton))
        if (length(missing) > 0) {
          stop(
            "Skeleton file ", basename(files[i]),
            " is missing columns: ", paste(missing, collapse = ", ")
          )
        }
        pop_list[[i]] <- skeleton[,
          .(n = data.table::uniqueN(id)),
          by = group_cols
        ]
        rm(skeleton, obj)
        gc()
      }

      population <- data.table::rbindlist(pop_list)
      population <- population[, .(n = sum(n)), by = group_cols]

      # Complete grid: CJ of all observed values, fill missing with 0
      unique_vals <- lapply(
        group_cols,
        function(col) sort(unique(population[[col]]))
      )
      names(unique_vals) <- group_cols
      complete_grid <- do.call(data.table::CJ, unique_vals)
      population <- population[complete_grid, on = group_cols]
      population[is.na(n), n := 0L]

      data.table::setorderv(population, group_cols)

      out_path <- file.path(self$data_skeleton_dir, "population.qs2")
      qs2::qs_save(population, out_path)
      cat(sprintf(
        "Population table saved: %s (%d rows)\n",
        out_path, nrow(population)
      ))

      invisible(population)
    },

    #' @description Aggregate per-batch counts from `meta_NNNNN.qs2`
    #'   sidecars into a study-wide sanity summary and write it to disk.
    #'
    #'   Writes three artefacts:
    #'   \itemize{
    #'     \item `summary.qs2` in `data_skeleton_dir` -- always written,
    #'       partial or full. The binary form for programmatic reload
    #'       via `qs2::qs_read()`.
    #'     \item `status.txt` in `data_skeleton_dir` -- always written.
    #'       Plain-text human-readable flag report (variables that never
    #'       matched, rare variables, totals).
    #'     \item `summary_<UTC>_<git-sha-or-NA>_<swereg-ver>.tsv` in
    #'       `data_summaries_dir` -- **only** written when every expected
    #'       batch has a meta sidecar on disk (`length(skeleton_files) ==
    #'       n_batches`). Partial runs explicitly skip the TSV; the
    #'       git-tracked audit format is full-run only.
    #'   }
    #'
    #'   Counts in the TSV below `suppress_below` are displayed as
    #'   `"<N"` (Swedish registry data convention). The `summary.qs2`
    #'   preserves exact counts.
    #'
    #'   This method reads only the meta sidecars (few KB each) and
    #'   never touches the heavy skeleton data files.
    #' @param suppress_below Integer. Counts in the TSV strictly less
    #'   than this are masked. Default 5L. The qs2 keeps exact values.
    #' @param write_tsv Logical. Whether to write the TSV when complete.
    #'   Default TRUE.
    #' @param write_status_txt Logical. Whether to write status.txt.
    #'   Default TRUE.
    #' @return The in-memory summary list, invisibly.
    compute_summary = function(suppress_below = 5L,
                               write_tsv = TRUE,
                               write_status_txt = TRUE) {
      n_expected <- as.integer(self$n_batches %||% 0L)
      candidates <- if (n_expected > 0L) {
        seq_len(n_expected)
      } else {
        # Fall back to whatever skeletons are on disk if n_batches is unknown
        as.integer(sub("^.*meta_(\\d+)\\.qs2$", "\\1",
                       list.files(self$data_skeleton_dir,
                                  pattern = "^meta_\\d+\\.qs2$")))
      }
      meta_paths <- file.path(
        self$data_skeleton_dir,
        sprintf("meta_%05d.qs2", candidates)
      )
      meta_paths <- meta_paths[file.exists(meta_paths)]
      n_present <- length(meta_paths)
      if (n_expected == 0L) n_expected <- n_present
      is_complete <- (n_present == n_expected && n_expected > 0L)

      # ---- Aggregate per-column counts across all present batches ----
      n_persons_total <- 0L
      n_person_weeks_total <- 0L
      col_n_persons <- list()
      col_n_weeks   <- list()
      col_label     <- list()
      col_entry_fp  <- list()
      missing_counts_batches <- integer(0)

      for (i in seq_along(meta_paths)) {
        m <- qs2::qs_read(meta_paths[i])
        n_persons_total      <- n_persons_total + (m$n_persons %||% 0L)
        n_person_weeks_total <- n_person_weeks_total + (m$n_rows %||% 0L)
        for (fp in names(m$applied_registry %||% list())) {
          entry <- m$applied_registry[[fp]]
          counts <- entry$counts
          if (is.null(counts)) {
            missing_counts_batches <- c(missing_counts_batches, i)
            next
          }
          for (col in names(counts)) {
            c <- counts[[col]]
            col_n_persons[[col]] <- (col_n_persons[[col]] %||% 0L) +
              as.integer(c$n_persons_with %||% 0L)
            col_n_weeks[[col]]   <- (col_n_weeks[[col]]   %||% 0L) +
              as.integer(c$n_person_weeks_with %||% 0L)
            col_label[[col]]     <- entry$label %||% NA_character_
            col_entry_fp[[col]]  <- fp
          }
        }
      }

      cols <- sort(names(col_n_persons))
      columns_dt <- data.table::data.table(
        column_name        = cols,
        entry_label        = unlist(col_label[cols]) %||% character(0),
        entry_fingerprint  = unlist(col_entry_fp[cols]) %||% character(0),
        n_persons_with     = vapply(cols, function(k) col_n_persons[[k]], integer(1)),
        n_person_weeks_with= vapply(cols, function(k) col_n_weeks[[k]],   integer(1))
      )

      summary <- list(
        meta = list(
          built_at        = Sys.time(),
          swereg_version  = as.character(utils::packageVersion("swereg")),
          n_batches_present  = n_present,
          n_batches_expected = n_expected,
          is_complete        = is_complete,
          missing_counts_batches = unique(missing_counts_batches)
        ),
        registry_wide = list(
          n_persons_total       = n_persons_total,
          n_person_weeks_total  = n_person_weeks_total
        ),
        columns = columns_dt
      )

      # ---- Always: write summary.qs2 ----
      qs2_path <- file.path(self$data_skeleton_dir, "summary.qs2")
      qs2::qs_save(summary, qs2_path)
      cat(sprintf("Summary (qs2) written: %s\n", qs2_path))

      # ---- Always: write status.txt ----
      if (isTRUE(write_status_txt)) {
        txt_path <- file.path(self$data_skeleton_dir, "status.txt")
        .write_status_txt(summary, txt_path)
        cat(sprintf("Status report written: %s\n", txt_path))
      }

      # ---- Only on full runs: write the TSV into data_summaries_dir ----
      tsv_written <- FALSE
      if (isTRUE(write_tsv)) {
        if (!is_complete) {
          cat(sprintf(
            "TSV skipped: partial run (%d / %d batches present).\n",
            n_present, n_expected
          ))
        } else if (is.null(self$data_summaries_cp)) {
          cat("TSV skipped: data_summaries_dir not configured on RegistryStudy.\n")
        } else {
          dir_summaries <- self$data_summaries_cp$resolve()
          ts <- format(Sys.time(), "%Y-%m-%dT%H-%MZ", tz = "UTC")
          sha <- .swereg_git_short_sha(dir_summaries) %||% "NA"
          tsv_name <- sprintf(
            "summary_%s_%s_swereg-%s.tsv",
            ts, sha, summary$meta$swereg_version
          )
          tsv_path <- file.path(dir_summaries, tsv_name)
          .write_summary_tsv(summary, tsv_path, suppress_below)
          cat(sprintf("Summary TSV written (audit-track): %s\n", tsv_path))
          tsv_written <- TRUE
        }
      }
      summary$meta$tsv_written <- tsv_written

      invisible(summary)
    },

    #' @description Delete all rawbatch files from disk.
    delete_rawbatches = function() {
      files <- list.files(
        self$data_rawbatch_dir,
        pattern = "\\d+_rawbatch_.*\\.qs2$",
        full.names = TRUE
      )
      if (length(files) > 0) {
        cat("Deleting", length(files), "rawbatch files\n")
        file.remove(files)
      }
      self$groups_saved <- character(0)
      invisible(self)
    },

    #' @description Delete all skeleton output files (and their meta
    #'   sidecars) from disk.
    delete_skeletons = function() {
      files <- list.files(
        self$data_skeleton_dir,
        pattern = "^(skeleton|meta)_\\d+\\.qs2$",
        full.names = TRUE
      )
      if (length(files) > 0) {
        cat("Deleting", length(files), "skeleton/meta files\n")
        file.remove(files)
      }
      invisible(self)
    },

    #' @description Delete the metadata file from disk.
    delete_meta_file = function() {
      if (file.exists(self$meta_file)) {
        cat("Deleting", self$meta_file, "\n")
        file.remove(self$meta_file)
      }
      invisible(self)
    },

    #' @description Save this study object as metadata. Captures the
    #'   destination path first, then clears host-specific [CandidatePath]
    #'   caches before writing, so the on-disk file never carries a resolved
    #'   path from the saving host.
    save_meta = function() {
      dest <- self$meta_file  # resolves dir_rawbatch before invalidation
      invalidate_candidate_paths(self)
      qs2::qs_save(self, dest)
      cat("Saved", dest, "\n")
      invisible(self)
    },

    #' @description Print method for RegistryStudy.
    #' @param ... Ignored.
    print = function(...) {
      cat("<RegistryStudy>\n")

      # Created timestamp
      if (!is.null(self$created_at)) {
        cat("  Created:", format(self$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
      }

      cat("  IDs:", format(self$n_ids, big.mark = ","), "total\n")

      # Code registry
      if (length(self$code_registry) > 0) {
        parts <- vapply(self$code_registry, function(reg) {
          sprintf("%d %s", length(reg$codes), reg$label)
        }, character(1))
        cat("  Code registry:", paste(parts, collapse = ", "), "\n")
        # Count generated columns
        n_cols <- sum(vapply(self$code_registry, function(reg) {
          n_codes <- length(reg$codes)
          n_groups <- length(reg$groups)
          n_combine <- if (!is.null(reg$combine_as)) 1L else 0L
          n_codes * (n_groups + n_combine)
        }, integer(1)))
        cat("  Generated columns:", n_cols, "\n")
      }

      # Pipeline status: batches → rawbatch → skeleton (grouped together)
      cat(
        "  Batches:",
        self$n_batches,
        "(",
        format(self$batch_size, big.mark = ","),
        "IDs each)\n"
      )

      # Rawbatch info
      if (length(self$groups_saved) > 0) {
        rb_files <- list.files(
          self$data_rawbatch_dir,
          pattern = "\\d+_rawbatch_.*\\.qs2$",
          full.names = TRUE
        )
        rb_size <- sum(file.size(rb_files), na.rm = TRUE)
        cat(
          "  Rawbatch groups saved:",
          paste(self$groups_saved, collapse = ", "),
          "(",
          length(rb_files),
          "files,",
          .format_bytes(rb_size),
          ")\n"
        )
      } else {
        cat("  Rawbatch groups saved: (none)\n")
      }

      # Skeleton info
      n_observed <- length(self$skeleton_files)
      n_expected <- self$expected_skeleton_file_count
      if (n_observed > 0) {
        sk_size <- sum(file.size(self$skeleton_files), na.rm = TRUE)
        cat(
          "  Skeleton files:",
          n_observed,
          "/",
          n_expected,
          "expected",
          "(",
          .format_bytes(sk_size),
          ")\n"
        )
      } else {
        cat("  Skeleton files: 0 /", n_expected, "expected\n")
      }

      # Dirs -- show all candidates, mark resolved one with >
      .print_dir_candidates <- function(label, cp) {
        cat("  ", label, ":\n", sep = "")
        resolved <- tryCatch(cp$resolve(), error = function(e) NULL)
        for (p in cp$candidates) {
          prefix <- if (!is.null(resolved) && identical(p, resolved)) "  > " else "    "
          cat(prefix, p, "\n", sep = "")
        }
      }

      .print_dir_candidates("Rawbatch", self$data_rawbatch_cp)
      if (
        !identical(
          self$data_skeleton_cp$candidates,
          self$data_rawbatch_cp$candidates
        )
      ) {
        .print_dir_candidates("Skeleton", self$data_skeleton_cp)
      }
      if (!is.null(self$data_raw_cp)) {
        .print_dir_candidates("Data raw", self$data_raw_cp)
      }

      invisible(self)
    }
  ),

  active = list(
    #' @field data_rawbatch_dir Character (read-only). Resolved rawbatch
    #'   directory for the current host. Lazily resolved from
    #'   `self$data_rawbatch_cp`.
    data_rawbatch_dir = function(value) {
      if (!missing(value)) {
        stop("data_rawbatch_dir is read-only; set via constructor")
      }
      self$data_rawbatch_cp$resolve()
    },

    #' @field data_skeleton_dir Character (read-only). Resolved skeleton
    #'   directory for the current host.
    data_skeleton_dir = function(value) {
      if (!missing(value)) {
        stop("data_skeleton_dir is read-only; set via constructor")
      }
      self$data_skeleton_cp$resolve()
    },

    #' @field data_meta_dir Character (read-only). Resolved metadata
    #'   directory for the current host (where `registrystudy.qs2` lives).
    data_meta_dir = function(value) {
      if (!missing(value)) {
        stop("data_meta_dir is read-only; set via constructor")
      }
      self$data_meta_cp$resolve()
    },

    #' @field data_raw_dir Character or NULL (read-only). Resolved raw-registry
    #'   directory, or NULL if not configured.
    data_raw_dir = function(value) {
      if (!missing(value)) {
        stop("data_raw_dir is read-only; set via constructor")
      }
      if (is.null(self$data_raw_cp)) return(NULL)
      self$data_raw_cp$resolve()
    },

    #' @field data_pipeline_snapshot_dir Character or NULL (read-only).
    #'   Resolved pipeline-snapshot directory for the current host, or
    #'   NULL if not configured (snapshot feature disabled).
    data_pipeline_snapshot_dir = function(value) {
      if (!missing(value)) {
        stop("data_pipeline_snapshot_dir is read-only; set via constructor")
      }
      if (is.null(self$data_pipeline_snapshot_cp)) return(NULL)
      self$data_pipeline_snapshot_cp$resolve()
    },

    #' @field data_summaries_dir Character or NULL (read-only). Resolved
    #'   audit-track summaries directory for the current host, or NULL if
    #'   not configured.
    data_summaries_dir = function(value) {
      if (!missing(value)) {
        stop("data_summaries_dir is read-only; set via constructor")
      }
      if (is.null(self$data_summaries_cp)) return(NULL)
      self$data_summaries_cp$resolve()
    },

    #' @field skeleton_files Character vector (read-only). Skeleton output file
    #'   paths detected on disk. Scans `skeleton_dir` on each access.
    skeleton_files = function(value) {
      if (!missing(value)) {
        stop("skeleton_files is read-only; populated from disk")
      }
      .detect_skeleton_files(self$data_skeleton_dir)
    },

    #' @field expected_skeleton_file_count Integer (read-only). Expected number
    #'   of skeleton files (one per batch).
    expected_skeleton_file_count = function() {
      as.integer(self$n_batches)
    },

    #' @field meta_file Character. Path to the on-disk metadata file
    #'   (`registrystudy.qs2`) inside `data_meta_dir`.
    meta_file = function() {
      file.path(self$data_meta_dir, "registrystudy.qs2")
    }
  ),

  private = list(
    .schema_version = NULL
  )
)
