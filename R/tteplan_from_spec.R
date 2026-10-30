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
#'   A list MAY stand in for it, as the package tests do. The list MUST carry
#'   `skeleton_files`. Without a `work_root`, s1 reads `data_meta_dir` from
#'   it for its work directory. A list has no `$assert_skeletons_consistent()`, so pass
#'   `check_skeletons = FALSE` here and to
#'   `$s1_generate_enrollments_and_ipw()`. With
#'   `global_max_isoyearweek = NULL` and a first file named
#'   `skeleton_<n>.qs2`, the list also needs `$load_skeleton()`.
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
#' @param period_width Integer, the width in weeks of the enrollment period
#'   (default: 4L). The same width sets each follow-up interval. Stored on the
#'   plan and passed through to TTEDesign.
#' @param check_skeletons Logical (default `TRUE`). Run
#'   [RegistryStudy]`$assert_skeletons_consistent()` before reading any
#'   skeleton, so a half-rebuilt or stale skeleton store stops here rather
#'   than flowing into a TTE plan. It scans every sidecar in the skeleton
#'   directory, which on a large store is minutes over a network mount.
#'   Pass `FALSE` only for a dev iteration you do not intend to trust,
#'   typically alongside `n_skeleton_files`.
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
  period_width = 4L,
  check_skeletons = TRUE
) {
  isoyearweek <- NULL

  if (is.null(study) || is.null(study$skeleton_files)) {
    stop(
      "`study` must provide a `$skeleton_files` accessor (use registrystudy_load() to load a RegistryStudy).",
      call. = FALSE
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
      "`spec_version` must be supplied (e.g. \"v003\") so the spec YAML filename can be built.",
      call. = FALSE
    )
  }
  spec_dir <- dir_spec_cp$resolve()
  spec_path <- file.path(spec_dir, filename_spec(spec_version))
  if (!file.exists(spec_path)) {
    stop("Spec YAML not found: ", spec_path, call. = FALSE)
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
      "'",
      call. = FALSE
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

  # Pre-flight: the skeleton store must be one generation, and that
  # generation must be the one this study describes. It runs AFTER the
  # n_skeleton_files limit, so it validates the files this plan will
  # actually consume rather than the whole directory.
  #
  # Per-batch replay answers "does THIS batch need work". It never looks
  # across batches, so $process_skeletons(batches = 1:100) after a code
  # edit leaves a mixed store with every filename present, and only a
  # cross-batch check sees it.
  #
  # This is an EARLY check, not the definitive one. The store can change
  # between here and s1, and s1 re-resolves skeleton_files after a load.
  # $s1_generate_enrollments_and_ipw() runs the same assertion on the
  # files it is about to read.
  .assert_skeleton_store(study, skeleton_files, check_skeletons)
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
    return(as.integer(m[[2]]))
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
    return(list(data = sk$data, created_at = sk$created_at))
  }

  if (is.null(global_max_isoyearweek)) {
    first <- .load_first_skeleton_dt()
    skeleton <- first$data
    tteplan_validate_spec(spec, skeleton)
    global_max_isoyearweek <- skeleton[, max(isoyearweek, na.rm = TRUE)]
    message("Admin censoring cutoff from skeleton: ", global_max_isoyearweek)
    skeleton_created_at <- first$created_at
    rm(skeleton, first)
  } else {
    # A supplied cutoff means no skeleton is validated, so the prevalent-user
    # check in tteplan_validate_spec() does not run. Say so, rather than let
    # a silent build read as a clean one.
    if (file.exists(skeleton_files[1])) {
      first <- .load_first_skeleton_dt()
      skeleton_created_at <- first$created_at
      rm(first)
    }
    message("prevalent-user check skipped: no skeleton loaded")
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

  .tteplan_add_spec_etts(plan, spec)

  return(plan)
}


#' Add the full ETT grid of a spec to a plan
#'
#' One ETT for each enrollment, outcome and follow-up horizon of the spec, in
#' that nesting order. A horizon with `only_enrollments` gives ETTs only to the
#' enrollments it names. A dropped ETT takes no ETT id. Each ETT row also carries the treatment implementation,
#' the comparator-to-intervention ratio and the seed of its enrollment.
#' [tteplan_from_spec_and_registrystudy()] and
#' [tteplan_excel_spec_summary()] both build their grid here.
#'
#' `.tte_follow_up_grid()` MAY add a feasibility horizon to an enrollment. A
#' second pass then adds one feasibility-horizon ETT for each such enrollment
#' and each outcome, in the same nesting order. Their ETT ids follow the ids of the spec's own ETTs, so
#' the spec's own ETTs keep the ids of a grid without the key.
#'
#' A third pass adds the horizon-only ETTs of `.tte_ceiling_follow_ups()`.
#' Each is the whole year that decides a part-year horizon of an enrollment
#' whose grid does not hold that year. Their ETT ids follow every other ETT
#' id.
#'
#' @param plan A [TTEPlan] with no ETTs.
#' @param spec A spec list from [tteplan_read_spec()].
#' @return The plan, invisibly. The plan is changed in place.
#' @noRd
.tteplan_add_spec_etts <- function(plan, spec) {
  treatment_impl <- comparator_to_intervention_ratio <- seed <- NULL

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

  # The follow-up horizons of one enrollment, then the feasibility horizon of
  # `feasibility_follow_up_weeks` when it is longer than every one of them.
  # `feasible = TRUE` keeps only the feasibility horizon, and `FALSE` drops it.
  enrollment_grid <- function(enrollment, feasible) {
    return(Filter(
      function(fu) isTRUE(fu$feasibility_horizon) == feasible,
      .tte_follow_up_grid(spec, enrollment$id)
    ))
  }

  # One ETT per outcome and follow-up of one enrollment, then the treatment
  # implementation on every ETT of that enrollment.
  add_enrollment <- function(enrollment, follow_ups) {
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
        "' has no age_range in additional_inclusion",
        call. = FALSE
      )
    }
    # `only_enrollments` can leave an enrollment without a horizon.
    if (length(follow_ups) == 0L) {
      return(invisible(NULL))
    }

    for (outcome in spec$outcomes) {
      for (fu in follow_ups) {
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
          ),
          feasibility_horizon = isTRUE(fu$feasibility_horizon)
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
    return(invisible(NULL))
  }

  for (enrollment in spec$enrollments) {
    add_enrollment(enrollment, enrollment_grid(enrollment, FALSE))
  }
  for (enrollment in spec$enrollments) {
    feasibility <- enrollment_grid(enrollment, TRUE)
    if (length(feasibility) > 0L) {
      add_enrollment(enrollment, feasibility)
    }
  }
  # The whole year that decides each part-year horizon, when the enrollment
  # has no ETT at that year. These ETTs come last, so no ETT id above moves.
  for (enrollment in spec$enrollments) {
    ceilings <- .tte_ceiling_follow_ups(spec, enrollment$id)
    if (length(ceilings) > 0L) {
      add_enrollment(enrollment, ceilings)
    }
  }

  return(invisible(plan))
}


#' The whole-year horizons that decide the part-year horizons of an enrollment
#'
#' A part-year horizon is a follow-up that is not a multiple of 52 weeks. Its
#' estimate is reported when the minimum-events rule passes at the next whole
#' year, `ceiling(weeks / 52) * 52` weeks. The function returns one entry for
#' each such whole year that `.tte_follow_up_grid()` does not hold for the
#' enrollment. The feasibility horizon counts as held. A feasibility horizon
#' that is not a whole number of years is a part-year horizon too, so it gets
#' a whole year as well.
#'
#' @param spec A spec list from [tteplan_read_spec()].
#' @param enrollment_id The id of one enrollment.
#' @return A list of follow-up entries, in the order of the first part-year
#'   horizon that needs each one. Each holds `label` (`"1 year"` or
#'   `"<k> years"`), `weeks`, `feasibility_horizon = TRUE`, and `decides`,
#'   the weeks of the part-year horizons it decides. The list is empty when
#'   the grid holds the whole year of every part-year horizon.
#' @noRd
.tte_ceiling_follow_ups <- function(spec, enrollment_id) {
  width <- .HORIZON_WINDOW_WEEKS
  held <- vapply(
    .tte_follow_up_grid(spec, enrollment_id),
    function(f) as.numeric(f$weeks),
    numeric(1)
  )
  out <- list()
  for (w in held[held %% width != 0]) {
    weeks <- ceiling(w / width) * width
    if (weeks %in% held) {
      next
    }
    key <- as.character(weeks)
    if (is.null(out[[key]])) {
      years <- weeks / width
      out[[key]] <- list(
        label = paste(years, if (years == 1) "year" else "years"),
        weeks = as.integer(weeks),
        feasibility_horizon = TRUE,
        decides = numeric(0)
      )
    }
    out[[key]]$decides <- c(out[[key]]$decides, w)
  }
  return(unname(out))
}


#' The follow-up horizons of the ETT grid of a spec
#'
#' The spec's own `follow_up` entries, in spec order. With `enrollment_id`,
#' only the entries that apply to that enrollment: an entry without
#' `only_enrollments`, or an entry that lists the id. When the spec sets
#' `feasibility_follow_up_weeks` to W, and W is longer than every entry kept,
#' one entry follows them:
#' \itemize{
#'   \item `weeks` is W;
#'   \item `label` is `"<W/52> years"` when W is a multiple of 52, `"1 year"`
#'     for 52, and `"<W> weeks"` otherwise;
#'   \item `feasibility_horizon` is `TRUE`.
#' }
#' Otherwise the spec adds no entry.
#'
#' @param spec A spec list from [tteplan_read_spec()].
#' @param enrollment_id The id of one enrollment, or `NULL` for every entry
#'   of the spec.
#' @return A list of follow-up entries. Each holds `label` and `weeks`. The
#'   feasibility horizon also holds `feasibility_horizon = TRUE`.
#' @noRd
.tte_follow_up_grid <- function(spec, enrollment_id = NULL) {
  fu <- spec$follow_up %||% list()
  if (!is.null(enrollment_id)) {
    fu <- Filter(function(f) .tte_follow_up_applies(f, enrollment_id), fu)
  }
  w <- spec[["feasibility_follow_up_weeks"]]
  if (is.null(w)) {
    return(fu)
  }
  longest <- max(
    vapply(fu, function(f) as.numeric(f$weeks), numeric(1)),
    -Inf
  )
  if (as.numeric(w) <= longest) {
    return(fu)
  }
  years <- as.numeric(w) / 52
  label <- if (as.numeric(w) %% 52 == 0) {
    paste(format(years, trim = TRUE), if (years == 1) "year" else "years")
  } else {
    paste(format(as.numeric(w), trim = TRUE), "weeks")
  }
  return(c(
    fu,
    list(list(
      label = label,
      weeks = as.integer(w),
      feasibility_horizon = TRUE
    ))
  ))
}


#' `TRUE` when a follow-up entry applies to an enrollment
#'
#' @param fu One `follow_up` entry of the spec.
#' @param enrollment_id The id of one enrollment.
#' @return `TRUE` when `fu` has no `only_enrollments`, or lists the id.
#' @noRd
.tte_follow_up_applies <- function(fu, enrollment_id) {
  ids <- fu[["only_enrollments"]]
  if (is.null(ids)) {
    return(TRUE)
  }
  return(as.character(enrollment_id) %in% as.character(unlist(ids)))
}


#' `TRUE` when a spec restricts any follow-up entry with `only_enrollments`
#'
#' @param spec A spec list from [tteplan_read_spec()].
#' @return Logical(1).
#' @noRd
.tte_follow_up_restricted <- function(spec) {
  return(any(vapply(
    spec$follow_up %||% list(),
    function(f) !is.null(f[["only_enrollments"]]),
    logical(1)
  )))
}


#' The enrollment ids a follow-up entry applies to, as text
#'
#' @param fu One `follow_up` entry of the spec.
#' @param prefix Character scalar, the text before the ids.
#' @return `""` for an entry without `only_enrollments`. Otherwise `prefix`,
#'   then `"enrollment <id>"` or `"enrollments <id>, <id>"`.
#' @noRd
.tte_follow_up_scope <- function(fu, prefix = ", only for ") {
  ids <- fu[["only_enrollments"]]
  if (is.null(ids)) {
    return("")
  }
  ids <- as.character(unlist(ids))
  return(paste0(
    prefix,
    if (length(ids) == 1L) "enrollment " else "enrollments ",
    paste(ids, collapse = ", ")
  ))
}


#' The feasibility horizon of a spec, and the enrollments that get it
#'
#' @param spec A spec list from [tteplan_read_spec()].
#' @return `NULL` when no enrollment gets a feasibility horizon. Otherwise a
#'   list: `fu`, the feasibility entry of `.tte_follow_up_grid()`, and `ids`,
#'   the ids of the enrollments that get it, in spec order.
#' @noRd
.tte_feasibility_enrollments <- function(spec) {
  out <- NULL
  for (enr in spec$enrollments) {
    feasible <- Filter(
      function(fu) isTRUE(fu$feasibility_horizon),
      .tte_follow_up_grid(spec, enr$id)
    )
    if (length(feasible) > 0L) {
      out <- list(fu = feasible[[1]], ids = c(out$ids, as.character(enr$id)))
    }
  }
  return(out)
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
  return(as.character(w))
}
