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

# Build the meta sidecar payload from a fully-built skeleton + the per-batch
# code-check accumulator snapshot. Stored next to the skeleton file as
# meta_%05d.qs2 by RegistryStudy$save_skeleton(). The meta-only fast path
# in .process_one_batch() reads this and skips loading the heavy skeleton
# entirely if every hash matches.
.build_skeleton_meta <- function(sk, population_by_specs = list()) {
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

  return(list(
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
  ))
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
