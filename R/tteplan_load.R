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
    stop("File does not contain a TTEPlan object: ", path, call. = FALSE)
  }
  plan <- TTEPlan$new(
    project_prefix = old$project_prefix,
    skeleton_files = old$skeleton_files,
    global_max_isoyearweek = old$global_max_isoyearweek,
    ett = old$ett
  )
  # Copy all additional public fields. The read uses `get()` because swereg
  # defines `[[.TTEPlan` (R/tteplan_operators.R), and that method returns an
  # enrollment spec, never a field. The write below is safe: no
  # `[[<-.TTEPlan` exists, so it reaches the object's environment.
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
    "sparse_decision",
    "sparse_rule_hash",
    "s1_run_id",
    # Set by $s3_feasibility(). A plan saved before 27.2.15 has none.
    "feasibility",
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
  # A plan built before swereg 27.3.10 stores a spec without the two design
  # keys. swereg keeps no code that reads such a plan, so refuse it here
  # rather than let a later step misread it.
  .tte_stop_on_pre_27_3_10_plan(plan$spec, path)
  # A plan saved by 27.3.0 or earlier has no feasibility-horizon column. None
  # of its ETTs is a feasibility-horizon ETT.
  plan$ett <- .tte_ett_backfill_feasibility(plan$ett)

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

  return(plan)
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
#' The plan stores the spec and results candidates that were passed when it
#' was built. Those name the checkout that built it. A later stage that runs
#' from another checkout MUST pass its own, or it reads that checkout's spec
#' and writes its results there. Both replacements live on the loaded plan,
#' so a later `$save()` writes them into `tteplan.qs2`.
#'
#' @param candidate_dir_tteplan Character vector of candidate directories,
#'   in priority order, where `tteplan.qs2` might live.
#' @param candidate_dir_spec Optional character vector. When given, it
#'   replaces the spec candidates stored in the plan.
#' @param candidate_dir_results Optional character vector. When given, it
#'   replaces the results candidates stored in the plan.
#' @return A [TTEPlan] with CandidatePath caches cleared and
#'   `skeleton_files` refreshed from the embedded `registrystudy`.
#' @seealso [tteplan_load()], [first_existing_path()]
#' @family tte_plan
#' @export
tteplan_locate_and_load <- function(
  candidate_dir_tteplan,
  candidate_dir_spec = NULL,
  candidate_dir_results = NULL
) {
  dir <- first_existing_path(candidate_dir_tteplan, "dir_tteplan")
  plan <- tteplan_load(file.path(dir, FILENAME_TTEPLAN))
  if (!is.null(candidate_dir_spec)) {
    plan$dir_spec_cp <- CandidatePath$new(candidate_dir_spec, "dir_spec")
  }
  if (!is.null(candidate_dir_results)) {
    plan$dir_results_cp <- CandidatePath$new(
      candidate_dir_results,
      "dir_results"
    )
  }
  return(plan)
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
  return(qs2_read(file.path(dir, "registrystudy.qs2")))
}


#' Give an ETT grid without a `feasibility_horizon` column that column
#'
#' A plan saved by swereg 27.3.0 or earlier has no `feasibility_horizon`
#' column in `plan$ett`. Every ETT of such a plan is an ETT of the spec, so
#' the column is `FALSE` on every row.
#'
#' @param ett `plan$ett`, or `NULL`.
#' @return `ett` unchanged when it is `NULL` or already holds the column.
#'   Otherwise a copy of `ett` with the column added.
#' @noRd
.tte_ett_backfill_feasibility <- function(ett) {
  if (is.null(ett) || "feasibility_horizon" %in% names(ett)) {
    return(ett)
  }
  ett <- data.table::copy(ett)
  data.table::set(ett, j = "feasibility_horizon", value = rep(FALSE, nrow(ett)))
  return(ett)
}


#' Refuse a plan built before swereg 27.3.10
#'
#' From 27.3.10, every specification carries `period_width` and
#' `admin_censoring_isoyearweek`, so every plan built since stores both in
#' `plan$spec`. A stored spec without them comes from an older swereg. Such a
#' plan may also hold the rule keys and the result statuses that swereg 27.2.14
#' wrote, and swereg no longer translates either.
#'
#' @param spec The stored specification, or `NULL`.
#' @param path The plan file, for the message.
#' @return `invisible(NULL)`, called for the error.
#' @noRd
.tte_stop_on_pre_27_3_10_plan <- function(spec, path) {
  if (!is.list(spec) || all(.TTE_DESIGN_KEYS %in% names(spec))) {
    return(invisible(NULL))
  }
  stop(
    path,
    " was built by a swereg older than 27.3.10. Its stored spec has no ",
    "period_width and no admin_censoring_isoyearweek. Cut a spec version ",
    "that declares both keys, point s0_init.R at it, and rerun s0_init.R ",
    "to build a new plan.",
    call. = FALSE
  )
}
