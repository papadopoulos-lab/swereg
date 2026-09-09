# The meta sidecar written beside every skeleton, and the pipeline match it records.

# True iff every currently-registered spec is already present in the
# meta's population_aggregations list. Lets .process_one_batch
# distinguish a clean fast-path from a meta-only refresh: if specs are
# missing but the pipeline still matches, we only need to reload the
# skeleton and rewrite the meta -- not re-run framework / randvars /
# codes.
.meta_has_all_specs <- function(meta, specs) {
  if (length(specs) == 0L) {
    return(TRUE)
  }
  if (is.null(meta)) {
    return(FALSE)
  }
  agg <- meta$population_aggregations %||% list()
  required <- vapply(specs, .population_spec_key, character(1))
  return(all(required %in% names(agg)))
}

# A randvars column of type character or factor is counted per level, so
# one column with many distinct values would write one entry per level
# into every batch meta and one row per level into the summary. A column
# with more distinct values than this is skipped and named in
# `randvars_counts_skipped`. An exposure or a demographic column stays far
# below the limit. An identifier or a free-text column does not, and value
# distributions of that size belong in `$compute_population()`.
.RANDVARS_MAX_LEVELS <- 100L

# Person, person-week and person-year counts for one logical mask over the
# skeleton. The three field names are the ones
# `.compute_entry_column_counts()` writes for a code entry, so
# `$compute_summary()` reads both sources with one accumulation.
.count_skeleton_mask <- function(mask, ids, is_weekly, is_annual) {
  n_persons <- if (is.null(ids) || !any(mask)) {
    0L
  } else {
    data.table::uniqueN(ids[mask])
  }
  return(list(
    n_persons_with = as.integer(n_persons),
    n_person_weeks_with = as.integer(sum(mask & is_weekly)),
    n_person_years_with = as.integer(sum(mask & is_annual))
  ))
}

# Count every column that a phase-3 step added to this skeleton. One entry
# per step, keyed by step name, shaped like an `applied_registry` record so
# the summary loop treats a step and a code entry alike.
#
# A logical column is counted under its own name. A character or factor
# column is counted per level, under the key `<column>=<level>`. Any other
# type carries no presence to count, so it is named in `$skipped` instead.
#
# A column is counted once. `Skeleton$sync_randvars()` records the columns
# a step added by a before-and-after diff, so two steps cannot both claim
# one column, and the guard below holds if that ever changes.
.compute_randvars_counts <- function(sk) {
  d <- sk$data
  ids <- d[["id"]]
  has_isoyear_flag <- "is_isoyear" %in% names(d)
  is_weekly <- if (has_isoyear_flag) !d$is_isoyear else rep(FALSE, nrow(d))
  is_annual <- if (has_isoyear_flag) d$is_isoyear else rep(FALSE, nrow(d))

  out <- list()
  skipped <- character(0)
  seen <- character(0)
  for (nm in names(sk$randvars_state %||% list())) {
    state <- sk$randvars_state[[nm]]
    cols <- intersect(
      setdiff(state$added_columns %||% character(0), seen),
      names(d)
    )
    seen <- c(seen, cols)
    counts <- list()
    for (col in cols) {
      v <- d[[col]]
      if (is.logical(v)) {
        counts[[col]] <- .count_skeleton_mask(
          !is.na(v) & v,
          ids,
          is_weekly,
          is_annual
        )
        next
      }
      # The values the column holds, never the levels a factor declares.
      # A factor that declares 300 levels and holds 2 is a two-level
      # column, and a declared level nothing holds is not a count.
      levels_present <- if (is.factor(v) || is.character(v)) {
        sort(unique(as.character(v[!is.na(v)])))
      } else {
        NULL
      }
      if (
        is.null(levels_present) ||
          length(levels_present) > .RANDVARS_MAX_LEVELS
      ) {
        skipped <- c(skipped, col)
        next
      }
      for (lv in levels_present) {
        counts[[paste0(col, "=", lv)]] <- .count_skeleton_mask(
          !is.na(v) & v == lv,
          ids,
          is_weekly,
          is_annual
        )
      }
    }
    # `entry_label` and `entry_fingerprint` are the summary's own column
    # names, so `$compute_summary()` reads this record and an
    # `applied_registry` record through one accumulation.
    out[[nm]] <- list(
      entry_label = nm,
      entry_fingerprint = state$fn_hash %||% NA_character_,
      counts = counts
    )
  }
  return(list(counts = out, skipped = skipped))
}

# Build the meta sidecar payload from a fully-built skeleton + the per-batch
# code-check accumulator snapshot. Stored next to the skeleton file as
# meta_%05d.qs2 by RegistryStudy$save_skeleton(). The meta-only fast path
# in .process_one_batch() reads this and skips loading the heavy skeleton
# entirely if every hash matches.
#
# `framework_removals` is the one field the skeleton cannot supply. The
# framework function reports it once per rebuild, and `.process_one_batch()`
# hands it over. Every other field is derived here from `sk`.
.build_skeleton_meta <- function(
  sk,
  population_by_specs = list(),
  framework_removals = NULL
) {
  # Skeleton convention: the row-key column is always "id" (set by
  # create_skeleton()). The study's `id_col` refers to the rawbatch's
  # id column (typically "lopnr"), used by add_*() to join against the
  # skeleton -- not the skeleton's own row-key.
  d <- sk$data
  ids <- d[["id"]]
  has_isoyear_flag <- "is_isoyear" %in% names(d)
  is_weekly <- if (has_isoyear_flag) !d$is_isoyear else rep(FALSE, nrow(d))
  is_annual <- if (has_isoyear_flag) d$is_isoyear else rep(FALSE, nrow(d))
  weekly_iyw <- if (has_isoyear_flag && "isoyearweek" %in% names(d)) {
    d$isoyearweek[is_weekly]
  } else {
    character(0)
  }
  annual_iy <- if (has_isoyear_flag && "isoyear" %in% names(d)) {
    d$isoyear[is_annual]
  } else {
    integer(0)
  }

  meta <- list(
    schema_version = .REGISTRY_STUDY_SCHEMA_VERSION,
    swereg_version = as.character(utils::packageVersion("swereg")),
    framework_fn_hash = sk$framework_fn_hash,
    trim_fn_hash = sk$trim_fn_hash,
    phase_order = sk$phase_order,
    randvars_state = sk$randvars_state,
    applied_registry = sk$applied_registry,
    n_rows = nrow(d),
    n_rows_weekly = as.integer(sum(is_weekly)),
    n_rows_annual = as.integer(sum(is_annual)),
    n_persons = if (is.null(ids)) NA_integer_ else data.table::uniqueN(ids),
    n_persons_weekly = if (is.null(ids) || !any(is_weekly)) {
      0L
    } else {
      data.table::uniqueN(ids[is_weekly])
    },
    n_persons_annual = if (is.null(ids) || !any(is_annual)) {
      0L
    } else {
      data.table::uniqueN(ids[is_annual])
    },
    weekly_min_isoyearweek = if (length(weekly_iyw) == 0L) {
      NA_character_
    } else {
      min(weekly_iyw, na.rm = TRUE)
    },
    weekly_max_isoyearweek = if (length(weekly_iyw) == 0L) {
      NA_character_
    } else {
      max(weekly_iyw, na.rm = TRUE)
    },
    annual_min_isoyear = if (length(annual_iy) == 0L) {
      NA_integer_
    } else {
      as.integer(min(annual_iy, na.rm = TRUE))
    },
    annual_max_isoyear = if (length(annual_iy) == 0L) {
      NA_integer_
    } else {
      as.integer(max(annual_iy, na.rm = TRUE))
    },
    population_aggregations = .compute_population_aggregations(
      d,
      population_by_specs
    ),
    built_at = Sys.time()
  )

  # Three optional fields. Each one is written only when it carries
  # something, so a study with no phase-3 step and a framework that reports
  # no removals keeps the field set it had before these counts existed.
  # `.process_one_batch()` reads the absence of `randvars_counts` as its
  # signal to backfill a meta written by an earlier swereg.
  rc <- .compute_randvars_counts(sk)
  if (length(rc$counts) > 0L) {
    meta$randvars_counts <- rc$counts
  }
  if (length(rc$skipped) > 0L) {
    meta$randvars_counts_skipped <- rc$skipped
  }
  # A framework that removes nothing MAY still return a zero-row table.
  # Treat that as no report at all, so the field never reaches a meta and
  # `$compute_summary()` never counts the batch as one that reported.
  if (!is.null(framework_removals) && nrow(framework_removals) > 0L) {
    meta$framework_removals <- framework_removals
  }
  return(meta)
}

# Write one batch's meta sidecar. `RegistryStudy$save_skeleton()` and the
# meta-only refresh path in `.process_one_batch()` both call it.
#
# `framework_removals` is not derivable from the skeleton, so each caller
# supplies it: the slow path from the framework function's own report, and
# the refresh path by carrying forward what the previous meta held.
.write_skeleton_meta <- function(study, sk, framework_removals = NULL) {
  meta <- .build_skeleton_meta(
    sk,
    population_by_specs = study$population_by_specs %||% list(),
    framework_removals = framework_removals
  )
  qs2_write_atomic(meta, study$skeleton_meta_path(sk$batch_number))
  return(invisible(NULL))
}

# True iff the meta entry is structurally valid AND the schema version
# matches AND every persisted hash matches the corresponding "current"
# hash from the run-wide pipeline state. The fast-path skip is all-or-
# nothing: any field that disagrees forces a load_skeleton() + per-phase
# replay through the existing logic.
.meta_matches_pipeline <- function(
  meta,
  framework_hash,
  trim_hash,
  phase_order,
  randvars_hashes,
  current_fps
) {
  if (is.null(meta)) {
    return(FALSE)
  }
  if (!identical(meta$schema_version, .REGISTRY_STUDY_SCHEMA_VERSION)) {
    return(FALSE)
  }
  if (!identical(meta$framework_fn_hash, framework_hash)) {
    return(FALSE)
  }

  # A meta written before the trim phase existed has no trim_fn_hash, so
  # this reads NULL. The current hash is never NULL: a study with no trim
  # carries .TRIM_NONE. So a pre-trim meta always falls through to the
  # slow path and the base is rebuilt once.
  if (!identical(meta$trim_fn_hash, trim_hash)) {
    return(FALSE)
  }

  # A meta written before the code registry moved ahead of randvars has no
  # phase_order, so this reads NULL. The slow path is necessary here and it
  # is not sufficient. The rebuild gate in .process_one_batch() compares the
  # same field again, and that gate reconstructs the data. Without the
  # second comparison the slow path finds every hash unchanged, no-ops both
  # syncs, and writes a current meta over a stale skeleton.
  if (!identical(meta$phase_order, phase_order)) {
    return(FALSE)
  }

  # Randvars: compare values + names. Empty cases need to compare as
  # empty regardless of representation (NULL list vs named character(0)).
  stored_randvars_hashes <- vapply(
    meta$randvars_state %||% list(),
    function(x) x$fn_hash %||% NA_character_,
    character(1)
  )
  if (
    !identical(
      unname(stored_randvars_hashes),
      unname(as.character(randvars_hashes))
    )
  ) {
    return(FALSE)
  }
  if (
    !identical(
      names(meta$randvars_state) %||% character(0),
      names(randvars_hashes) %||% character(0)
    )
  ) {
    return(FALSE)
  }

  # Code registry fingerprints: compare as character vectors. Empty list's
  # names() is NULL; empty fingerprint set is character(0); coerce both
  # to character(0) before comparing.
  stored_fp <- names(meta$applied_registry) %||% character(0)
  if (!identical(stored_fp, unname(as.character(current_fps)))) {
    return(FALSE)
  }

  return(TRUE)
}
