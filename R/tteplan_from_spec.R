# =============================================================================
# tteplan_from_spec_and_registrystudy
# =============================================================================

#' Create a TTEPlan from a study specification
#'
#' Builds a [TTEPlan] with a full ETT grid (enrollments x outcomes x
#' follow-up) from the parsed study specification and a pre-loaded
#' [RegistryStudy]. Also stores each enrollment's treatment implementation
#' details in the ETT data.table so they are available via
#' `plan[[i]]$treatment_impl`.
#'
#' It carries each enrollment's observation encoding and both arm tolerances
#' from the spec into the ETT data.table. `plan[[i]]$design` reads them back
#' onto a [TTEDesign]. See the observation contract section of
#' [tteplan_read_spec()].
#'
#' Directory-resolution fields (`dir_tteplan_cp`, `dir_spec_cp`,
#' `dir_results_cp`) are stored on the plan as [CandidatePath] instances.
#' Stage scripts (`s1.R`, `s2.R`, `s3.R`, `s4_export.R`) can then re-load
#' the plan on any host with [tteplan_locate_and_load()] and call
#' `plan$save()`, `plan$s1_generate_enrollments_and_ipw()`, etc. without
#' re-specifying any paths.
#'
#' @param study A [RegistryStudy] R6 object, typically loaded via
#'   [registrystudy_load()]. Owns the rawbatch and skeleton path candidates.
#' @param candidate_dir_spec Character vector of candidate directories that
#'   contain the spec YAML `spec_vXXX.yaml`. The first existing directory is
#'   used to locate the spec.
#' @param candidate_dir_tteplan Character vector of candidate directories
#'   where `tteplan.qs2` lives (or will be created by `plan$save()`).
#' @param candidate_dir_results Character vector of candidate directories for
#'   the results BASE directory (without the version suffix -- the plan
#'   appends `spec_version` internally).
#' @param spec_version Optional character scalar like `"v003"` selecting the
#'   spec YAML. When `NULL`, read from `spec$study$implementation$version`.
#' @param project_id Optional character scalar for display/logging. When
#'   `NULL`, read from `spec$study$implementation$project_prefix`.
#' @param n_skeleton_files Optional integer: if not NULL, only the first
#'   `n_skeleton_files` files are used (for faster dev iterations). Stored
#'   on the plan as `n_skeleton_files_limit` so [tteplan_load()] can
#'   re-apply it after a host transfer.
#' @param global_max_isoyearweek Administrative censoring boundary
#'   (isoyearweek string, e.g., "2023-52"). If `NULL` (default), auto-detected
#'   from `max(isoyearweek)` in the first skeleton file. Also runs
#'   [tteplan_validate_spec()] on that skeleton.
#' @param period_width Integer, band width in weeks for enrollment and
#'   time aggregation (default: 4L). Stored on the plan and passed through
#'   to TTEDesign.
#' @return A [TTEPlan] object with the full ETT grid, embedded
#'   `registrystudy`, and CandidatePath fields populated.
#'
#' @family tte_spec
#' @seealso [registrystudy_load()], [tteplan_locate_and_load()]
#' @export
tteplan_from_spec_and_registrystudy <- function(
  study,
  candidate_dir_spec,
  candidate_dir_tteplan,
  candidate_dir_results,
  spec_version = NULL,
  project_id = NULL,
  n_skeleton_files = NULL,
  global_max_isoyearweek = NULL,
  period_width = 4L
) {
  isoyearweek <- treatment_impl <- comparator_to_intervention_ratio <- seed <- NULL

  if (is.null(study) || is.null(study$skeleton_files)) {
    stop(
      "`study` must provide a `$skeleton_files` accessor (use registrystudy_load() to load a RegistryStudy)."
    )
  }

  # Wrap candidate-dir vectors in CandidatePath instances.
  dir_spec_cp <- CandidatePath$new(candidate_dir_spec, "dir_spec")
  dir_tteplan_cp <- CandidatePath$new(candidate_dir_tteplan, "dir_tteplan")
  dir_results_cp <- CandidatePath$new(candidate_dir_results, "dir_results")

  # Read the spec YAML from the resolved spec directory. If spec_version
  # wasn't supplied, we don't yet know which file to read -- require it.
  if (is.null(spec_version)) {
    stop(
      "`spec_version` must be supplied (e.g. \"v003\") so the spec YAML filename can be built."
    )
  }
  spec_dir <- dir_spec_cp$resolve()
  spec_path <- file.path(spec_dir, filename_spec(spec_version))
  if (!file.exists(spec_path)) {
    stop("Spec YAML not found: ", spec_path)
  }
  spec <- tteplan_read_spec(spec_path)
  yaml_version <- spec$study$implementation$version
  if (!identical(yaml_version, spec_version)) {
    stop(
      "spec_version mismatch: argument was '",
      spec_version,
      "' but the YAML at ",
      spec_path,
      " has implementation.version = '",
      yaml_version %||% "NULL",
      "'"
    )
  }

  if (is.null(project_id)) {
    project_id <- spec$study$implementation$project_prefix
  }

  # Resolve skeleton_files from RegistryStudy
  skeleton_files <- study$skeleton_files

  # Apply n_skeleton_files limit
  if (!is.null(n_skeleton_files)) {
    skeleton_files <- utils::head(skeleton_files, n_skeleton_files)
  }
  skeleton_created_at <- NULL
  # Extract the batch number from the first skeleton file so we can go
  # through the study's load_skeleton() API (which unwraps Skeleton R6,
  # falls back to legacy bare-dt files, and restores over-allocation).
  .first_batch_number <- function(path) {
    m <- regmatches(
      basename(path),
      regexec("skeleton_(\\d+)\\.qs2$", basename(path))
    )[[1]]
    if (length(m) < 2L) {
      return(NA_integer_)
    }
    as.integer(m[[2]])
  }
  .load_first_skeleton_dt <- function() {
    batch_num <- .first_batch_number(skeleton_files[1])
    if (is.na(batch_num)) {
      # Unusual filename -- fall back to a raw qs2_read + unwrap
      obj <- qs2_read(skeleton_files[1])
      if (inherits(obj, "Skeleton")) {
        return(list(data = obj$data, created_at = obj$created_at))
      }
      return(list(data = obj, created_at = attr(obj, "created_at")))
    }
    sk <- study$load_skeleton(batch_num)
    if (is.null(sk)) {
      stop("Skeleton file not found: ", skeleton_files[1], call. = FALSE)
    }
    list(data = sk$data, created_at = sk$created_at)
  }

  if (is.null(global_max_isoyearweek)) {
    first <- .load_first_skeleton_dt()
    skeleton <- first$data
    tteplan_validate_spec(spec, skeleton)
    global_max_isoyearweek <- skeleton[, max(isoyearweek, na.rm = TRUE)]
    message("Admin censoring cutoff from skeleton: ", global_max_isoyearweek)
    skeleton_created_at <- first$created_at
    rm(skeleton, first)
  } else if (file.exists(skeleton_files[1])) {
    first <- .load_first_skeleton_dt()
    skeleton_created_at <- first$created_at
    rm(first)
  }

  # Extract confounder variable names
  confounder_vars <- vapply(
    spec$confounders,
    function(c) c$implementation$variable,
    character(1)
  )

  # Extract subgroup (effect-modifier) variable names (optional)
  subgroup_vars <- if (!is.null(spec$subgroups)) {
    vapply(
      spec$subgroups,
      function(s) s$implementation$variable,
      character(1)
    )
  } else {
    NULL
  }

  plan <- TTEPlan$new(
    project_prefix = project_id,
    skeleton_files = skeleton_files,
    global_max_isoyearweek = global_max_isoyearweek
  )
  plan$period_width <- as.integer(period_width)
  plan$spec <- spec
  plan$spec_version <- spec_version

  # CandidatePath fields + embedded study
  plan$dir_spec_cp <- dir_spec_cp
  plan$dir_tteplan_cp <- dir_tteplan_cp
  plan$dir_results_cp <- dir_results_cp
  plan$registrystudy <- study
  plan$n_skeleton_files_limit <- if (is.null(n_skeleton_files)) {
    NULL
  } else {
    as.integer(n_skeleton_files)
  }

  if (!is.null(study$expected_skeleton_file_count)) {
    plan$expected_skeleton_file_count <- study$expected_skeleton_file_count
  }
  if (is.function(study$summary_table)) {
    plan$code_registry <- study$summary_table()
  }
  if (!is.null(study$n_ids)) {
    plan$expected_n_ids <- study$n_ids
  }

  # Provenance timestamps
  plan$registry_study_created_at <- study$created_at
  plan$skeleton_created_at <- skeleton_created_at

  for (enrollment in spec$enrollments) {
    # Extract age range from additional_inclusion
    age_min <- NULL
    age_max <- NULL
    age_group <- NULL
    if (!is.null(enrollment$additional_inclusion)) {
      for (ae in enrollment$additional_inclusion) {
        if (identical(ae$type, "age_range")) {
          age_min <- ae$min
          age_max <- ae$max
          age_group <- paste0(ae$min, "_", ae$max)
        }
      }
    }
    if (is.null(age_min) || is.null(age_max)) {
      stop(
        "Enrollment '",
        enrollment$id,
        "' has no age_range in additional_inclusion"
      )
    }

    for (outcome in spec$outcomes) {
      for (fu in spec$follow_up) {
        plan$add_one_ett(
          enrollment_id = enrollment$id,
          outcome_var = outcome$implementation$variable_combined,
          outcome_name = outcome$name,
          follow_up = fu$weeks,
          confounder_vars = confounder_vars,
          subgroup_vars = subgroup_vars,
          time_treatment_var = "rd_intervention",
          eligible_var = "eligible",
          observed_var = enrollment$observed_var,
          intervention_tolerance_weeks = enrollment$intervention_tolerance_weeks,
          comparator_tolerance_weeks = enrollment$comparator_tolerance_weeks,
          argset = list(
            age_group = age_group,
            age_min = age_min,
            age_max = age_max,
            outcome_description = outcome$description %||% NA_character_,
            outcome_role = outcome$role %||% NA_character_
          )
        )
      }
    }

    # Store treatment implementation in the ETT for this enrollment
    impl <- enrollment$treatment$implementation
    rows <- plan$ett$enrollment_id == enrollment$id
    plan$ett[rows, treatment_impl := list(list(impl))]
    plan$ett[
      rows,
      comparator_to_intervention_ratio := impl$comparator_to_intervention_ratio
    ]
    plan$ett[rows, seed := impl$seed]
  }

  plan
}


#' Format a window spec as human-readable text
#'
#' @param impl Implementation list with `window` field.
#' @return Character string.
#' @noRd
.format_window_human <- function(impl) {
  w <- impl$window
  if (is.null(w)) {
    return("(not specified)")
  }
  if (identical(w, "lifetime_before_and_after_baseline")) {
    return("lifetime before and after baseline")
  }
  if (identical(w, "lifetime_before_baseline")) {
    return("lifetime before baseline")
  }
  if (is.numeric(w)) {
    years <- w / 52
    if (years == as.integer(years) && years >= 1) {
      return(paste0(
        as.integer(years),
        if (years == 1) " year" else " years",
        " before baseline"
      ))
    }
    return(paste0(w, " weeks before baseline"))
  }
  as.character(w)
}
