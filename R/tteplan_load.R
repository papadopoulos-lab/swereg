# =============================================================================
# tteplan_load
# =============================================================================

#' Load a TTEPlan from disk with the current class definition
#'
#' R6 objects serialized with [qs2::qs_save()] retain the method bindings from
#' the class version at save time. After a package upgrade that adds new methods
#' or fields, [qs2::qs_read()] returns a stale object. This function reads the
#' file, then copies all public fields into a fresh [TTEPlan] instance so that
#' new methods are available.
#'
#' @param path Path to a `tteplan.qs2` file.
#' @return A [TTEPlan] object with the current class definition.
#'
#' @family tte_plan
#' @export
tteplan_load <- function(path) {
  old <- qs2_read(path)
  if (!inherits(old, "TTEPlan")) {
    stop("File does not contain a TTEPlan object: ", path)
  }
  plan <- TTEPlan$new(
    project_prefix = old$project_prefix,
    skeleton_files = old$skeleton_files,
    global_max_isoyearweek = old$global_max_isoyearweek,
    ett = old$ett
  )
  # Copy all additional public fields (use get() not [[ - R6 [[ doesn't
  # reliably access fields, only $ and environment get() do)
  fields <- c(
    "spec",
    "enrollment_counts",
    "period_width",
    "expected_skeleton_file_count",
    "code_registry",
    "expected_n_ids",
    "created_at",
    "registry_study_created_at",
    "skeleton_created_at",
    "output_dir",
    "results_enrollment",
    "results_ett",
    "spec_reloaded_at",
    "spec_reload_skipped_diffs",
    # New fields added by the CandidatePath migration
    "spec_version",
    "dir_tteplan_cp",
    "dir_spec_cp",
    "dir_results_cp",
    "registrystudy",
    "n_skeleton_files_limit"
  )
  for (f in fields) {
    val <- tryCatch(get(f, envir = old), error = function(e) NULL)
    if (!is.null(val)) plan[[f]] <- val
  }

  # Schema-version guard -- fails loudly on pre-migration plans with a
  # pointer at the renamer + s0_init re-run.
  saved_schema <- tryCatch(
    get(".schema_version", envir = old$.__enclos_env__$private),
    error = function(e) 0L
  )
  if (is.null(saved_schema)) {
    saved_schema <- 0L
  }
  assign(".schema_version", saved_schema, envir = plan$.__enclos_env__$private)
  plan$check_version() # errors if saved_schema is too old

  # Refresh skeleton_files from the embedded registrystudy so file paths are
  # valid on the current host. Falls back to the serialized list for plans
  # without an embedded study (legacy; blocked by check_version() above).
  if (
    !is.null(plan$registrystudy) &&
      inherits(plan$registrystudy, "RegistryStudy")
  ) {
    files <- plan$registrystudy$skeleton_files
    if (!is.null(plan$n_skeleton_files_limit)) {
      files <- utils::head(files, plan$n_skeleton_files_limit)
    }
    plan$skeleton_files <- files
  }

  # Older cached plans did not persist output_dir. Infer it from the plan
  # file's own directory, which is the standard layout (the plan and its
  # companion .qs2 files sit in the same folder).
  if (is.null(plan$output_dir) || !nzchar(plan$output_dir)) {
    inferred_dir <- dirname(path)
    if (dir.exists(inferred_dir)) {
      plan$output_dir <- inferred_dir
    }
  }

  # Backfill enrollment counts from per-enrollment sidecar files
  if (!is.null(plan$output_dir) && dir.exists(plan$output_dir)) {
    if (is.null(plan$enrollment_counts)) {
      plan$enrollment_counts <- list()
    }
    .restore_enrollment_counts(
      plan,
      plan$output_dir,
      unique(plan$ett$enrollment_id)
    )
  }

  plan
}


# =============================================================================
# tteplan_locate_and_load + registrystudy_load
# =============================================================================

#' Locate and load a TTEPlan from candidate directories
#'
#' Walks `candidate_dir_tteplan` to find the first directory that exists on
#' the current host, then loads `tteplan.qs2` from inside it via
#' [tteplan_load()]. The one-line convenience that `s1.R` / `s2.R` / `s3.R` /
#' `s4_export.R` stage scripts call to obtain a plan with all directories
#' already resolved.
#'
#' @param candidate_dir_tteplan Character vector of candidate directories,
#'   in priority order, where `tteplan.qs2` might live.
#' @return A [TTEPlan] with CandidatePath caches cleared and
#'   `skeleton_files` refreshed from the embedded `registrystudy`.
#' @seealso [tteplan_load()], [first_existing_path()]
#' @family tte_plan
#' @export
tteplan_locate_and_load <- function(candidate_dir_tteplan) {
  dir <- first_existing_path(candidate_dir_tteplan, "dir_tteplan")
  tteplan_load(file.path(dir, FILENAME_TTEPLAN))
}

#' Locate and load a RegistryStudy from candidate metadata directories
#'
#' Walks `candidate_dir_meta` to find the first directory that exists on
#' the current host, then reads `registrystudy.qs2` from inside it. Used in
#' `s0_init.R` to pass a pre-loaded `study` object to
#' [tteplan_from_spec_and_registrystudy()].
#'
#' @param candidate_dir_meta Character vector of candidate metadata
#'   directories (where `registrystudy.qs2` lives). Pass the same path you
#'   gave to `RegistryStudy$new(data_meta_dir = ...)` -- typically either
#'   the rawbatch directory (legacy default) or its parent.
#' @return A [RegistryStudy] R6 object.
#' @seealso [first_existing_path()],
#'   [tteplan_from_spec_and_registrystudy()]
#' @family tte_plan
#' @export
registrystudy_load <- function(candidate_dir_meta) {
  dir <- first_existing_path(candidate_dir_meta, "dir_meta")
  qs2_read(file.path(dir, "registrystudy.qs2"))
}
