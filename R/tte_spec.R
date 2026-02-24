# =============================================================================
# Spec-driven study configuration for TTE projects
# =============================================================================
# Functions for reading a YAML study specification and applying it to skeleton
# data. The spec serves as an auditable bridge between the clinical protocol
# (written by doctors) and the data pipeline (maintained by programmers).
#
# Exported functions:
#   tte_read_spec()                — parse + validate YAML spec
#   tte_apply_exclusions()         — apply eligibility criteria from spec
#   tte_apply_derived_confounders()— compute derived confounder columns
#   tte_plan_from_spec()           — create TTEPlan + ETT grid from spec
# =============================================================================


# =============================================================================
# tte_read_spec
# =============================================================================

#' Read and validate a YAML study specification
#'
#' Parses a YAML study specification file, converts human-readable windows
#' (e.g., "lifetime", "3 years") to weeks, and validates that all required
#' fields are present.
#'
#' @param spec_path Path to the YAML specification file.
#' @return A nested list representing the parsed specification, with
#'   `window_weeks` fields added to exclusion criteria and confounders.
#'
#' @details
#' Window conversion rules:
#' \itemize{
#'   \item `"lifetime"` -> `Inf`
#'   \item `"N year"` or `"N years"` -> `N * 52`
#' }
#'
#' Validation checks:
#' \itemize{
#'   \item Required sections: study, population, enrollments, outcomes, follow_up
#'   \item `study$implementation$project_prefix` must exist
#'   \item Each exclusion criterion must have `implementation$variable`
#'   \item Each outcome must have `implementation$variable`
#'   \item Each enrollment must have `id` and `exposure$implementation$variable`
#'   \item Computed confounders must have `implementation$source_variable`
#' }
#'
#' Warns about open questions with `status: "open"`.
#'
#' @family tte_spec
#' @export
tte_read_spec <- function(spec_path) {
  if (!file.exists(spec_path)) {
    stop("Spec file not found: ", spec_path)
  }

  spec <- yaml::read_yaml(spec_path)

  # Validate required sections
  required_sections <- c(
    "study", "population", "enrollments", "outcomes", "follow_up"
  )
  missing <- setdiff(required_sections, names(spec))
  if (length(missing) > 0) {
    stop(
      "Missing required sections in spec: ",
      paste(missing, collapse = ", ")
    )
  }

  # Validate study has implementation$project_prefix
  if (is.null(spec$study$implementation$project_prefix)) {
    stop("study$implementation$project_prefix is required")
  }

  # Validate and convert exclusion_criteria
  if (!is.null(spec$exclusion_criteria)) {
    for (i in seq_along(spec$exclusion_criteria)) {
      ec <- spec$exclusion_criteria[[i]]

      if (is.null(ec$implementation$variable)) {
        stop(
          "exclusion_criteria[", i, "] '", ec$name,
          "' is missing implementation$variable"
        )
      }

      if (is.null(ec$window)) {
        stop(
          "exclusion_criteria[", i, "] '", ec$name,
          "' is missing 'window'"
        )
      }
      spec$exclusion_criteria[[i]]$window_weeks <- .convert_window(ec$window)
    }
  }

  # Validate outcomes
  for (i in seq_along(spec$outcomes)) {
    if (is.null(spec$outcomes[[i]]$implementation$variable)) {
      stop(
        "outcomes[", i, "] '", spec$outcomes[[i]]$name,
        "' is missing implementation$variable"
      )
    }
  }

  # Validate enrollments
  for (i in seq_along(spec$enrollments)) {
    enr <- spec$enrollments[[i]]
    if (is.null(enr$id)) {
      stop("enrollments[", i, "] is missing 'id'")
    }
    if (is.null(enr$exposure$implementation$variable)) {
      stop(
        "enrollments[", i, "] '", enr$name %||% enr$id,
        "' is missing exposure$implementation$variable"
      )
    }
    if (is.null(enr$exposure$matching_ratio)) {
      stop(
        "enrollments[", i, "] '", enr$name %||% enr$id,
        "' is missing exposure$matching_ratio"
      )
    }
  }

  # Convert confounder windows and validate computed confounders
  if (!is.null(spec$confounders)) {
    for (i in seq_along(spec$confounders)) {
      conf <- spec$confounders[[i]]
      if (!is.null(conf$window)) {
        spec$confounders[[i]]$window_weeks <- .convert_window(conf$window)
      }
      if (isTRUE(conf$implementation$computed)) {
        if (is.null(conf$implementation$source_variable)) {
          stop(
            "confounders[", i, "] '", conf$name,
            "' is computed but missing implementation$source_variable"
          )
        }
        if (is.null(conf$window)) {
          stop(
            "confounders[", i, "] '", conf$name,
            "' is computed but missing 'window'"
          )
        }
      }
    }
  }

  # Warn about open questions
  if (!is.null(spec$open_questions)) {
    open <- Filter(
      function(q) is.null(q$status) || q$status == "open",
      spec$open_questions
    )
    if (length(open) > 0) {
      warning(
        length(open), " open question(s) in spec:\n",
        paste0(
          "  - ",
          vapply(open, function(q) q$question, character(1)),
          collapse = "\n"
        )
      )
    }
  }

  spec
}


#' Convert a human-readable window string to weeks
#'
#' @param window Character: "lifetime", "N year", or "N years".
#' @return Numeric: `Inf` for lifetime, `N * 52` for year-based windows.
#' @noRd
.convert_window <- function(window) {
  if (identical(window, "lifetime")) return(Inf)

  m <- regmatches(window, regexec("^(\\d+)\\s+years?$", window))[[1]]
  if (length(m) == 2) return(as.integer(m[2]) * 52L)

  stop("Cannot parse window: '", window, "'. Expected 'lifetime' or 'N year(s)'.")
}


# =============================================================================
# tte_apply_exclusions
# =============================================================================

#' Apply exclusion criteria from a study spec to a skeleton
#'
#' Applies calendar year eligibility, global exclusion criteria, and
#' enrollment-specific additional eligibility (e.g., age range) from the
#' parsed study specification. Calls [tte_eligible_combine()] at the end
#' to AND all criteria into a single `eligible` column.
#'
#' @param skeleton A data.table skeleton (person-week panel).
#' @param spec Parsed study specification from [tte_read_spec()].
#' @param enrollment_spec Enrollment spec from the plan (must contain
#'   `enrollment_id`), as returned by `plan[[i]]` or passed to the
#'   `process_fn` callback.
#' @return The skeleton (modified by reference), with eligibility columns
#'   added and a combined `eligible` column.
#'
#' @family tte_spec
#' @export
tte_apply_exclusions <- function(skeleton, spec, enrollment_spec) {
  enrollment_id <- enrollment_spec$enrollment_id

  # Find enrollment definition in the spec
  enrollment_def <- NULL
  for (enr in spec$enrollments) {
    if (enr$id == enrollment_id) {
      enrollment_def <- enr
      break
    }
  }
  if (is.null(enrollment_def)) {
    stop("Enrollment ID '", enrollment_id, "' not found in spec$enrollments")
  }

  # 1. Calendar years
  years <- seq(
    spec$population$calendar_years[1],
    spec$population$calendar_years[2]
  )
  skeleton <- tte_eligible_isoyears(skeleton, years)
  eligible_cols <- "eligible_isoyears"

  # 2. Enrollment-specific additional eligibility (before global exclusions)
  if (!is.null(enrollment_def$additional_eligibility)) {
    for (ae in enrollment_def$additional_eligibility) {
      if (identical(ae$type, "age_range")) {
        skeleton <- tte_eligible_age_range(
          skeleton,
          age_var = ae$implementation$variable,
          min_age = ae$min,
          max_age = ae$max
        )
        eligible_cols <- c(eligible_cols, "eligible_age")
      }
    }
  }

  # 3. Global exclusion criteria
  for (ec in spec$exclusion_criteria) {
    impl <- ec$implementation
    window <- ec$window_weeks

    if (identical(impl$type, "no_prior_exposure")) {
      col_name <- paste0(
        "eligible_no_", impl$variable, "_",
        .window_label(window)
      )
      skeleton <- tte_eligible_no_observation_in_window_excluding_wk0(
        skeleton,
        var = impl$variable,
        value = impl$exposure_value,
        window = window,
        col_name = col_name
      )
    } else {
      col_name <- paste0(
        "eligible_no_", impl$variable, "_",
        .window_label(window)
      )
      skeleton <- tte_eligible_no_events_in_window_excluding_wk0(
        skeleton,
        event_var = impl$variable,
        window = window,
        col_name = col_name
      )
    }
    eligible_cols <- c(eligible_cols, col_name)
  }

  # 4. Combine all criteria
  skeleton <- tte_eligible_combine(skeleton, eligible_cols)

  skeleton
}


#' Format a window value as a label for column names
#'
#' @param window_weeks Numeric: weeks or Inf.
#' @return Character: "ever" for Inf, "{weeks}wk" otherwise.
#' @noRd
.window_label <- function(window_weeks) {
  if (is.infinite(window_weeks)) "ever" else paste0(window_weeks, "wk")
}


# =============================================================================
# tte_apply_derived_confounders
# =============================================================================

#' Compute derived confounder columns from a study spec
#'
#' For confounders with `implementation$computed: true`, computes rolling
#' window indicators using [tte_eligible_no_events_in_window_excluding_wk0()].
#' Requires `implementation$source_variable` and `window` to be set.
#'
#' @param skeleton A data.table skeleton (person-week panel).
#' @param spec Parsed study specification from [tte_read_spec()].
#' @return The skeleton (modified by reference), with derived confounder
#'   columns added.
#'
#' @family tte_spec
#' @export
tte_apply_derived_confounders <- function(skeleton, spec) {
  if (is.null(spec$confounders)) return(skeleton)

  for (conf in spec$confounders) {
    impl <- conf$implementation
    if (isTRUE(impl$computed)) {
      skeleton <- tte_eligible_no_events_in_window_excluding_wk0(
        skeleton,
        event_var = impl$source_variable,
        window = conf$window_weeks,
        col_name = impl$variable
      )
    }
  }

  skeleton
}


# =============================================================================
# tte_plan_from_spec
# =============================================================================

#' Create a TTEPlan from a study specification
#'
#' Builds a [TTEPlan] with a full ETT grid (enrollments x outcomes x
#' follow-up) from the parsed study specification. Also stores each
#' enrollment's exposure implementation details in the ETT data.table so
#' they are available via `plan[[i]]$exposure_impl`.
#'
#' @param spec Parsed study specification from [tte_read_spec()].
#' @param skeleton_files Character vector of skeleton file paths.
#' @param global_max_isoyearweek Administrative censoring boundary
#'   (isoyearweek string, e.g., "2023-52").
#' @return A [TTEPlan] object with the full ETT grid.
#'
#' @family tte_spec
#' @export
tte_plan_from_spec <- function(spec, skeleton_files, global_max_isoyearweek) {
  project_prefix <- spec$study$implementation$project_prefix

  # Extract confounder variable names
  confounder_vars <- vapply(
    spec$confounders,
    function(c) c$implementation$variable,
    character(1)
  )

  plan <- tte_plan(
    project_prefix = project_prefix,
    skeleton_files = skeleton_files,
    global_max_isoyearweek = global_max_isoyearweek
  )

  for (enrollment in spec$enrollments) {
    # Extract age range from additional_eligibility
    age_min <- NULL
    age_max <- NULL
    age_group <- NULL
    if (!is.null(enrollment$additional_eligibility)) {
      for (ae in enrollment$additional_eligibility) {
        if (identical(ae$type, "age_range")) {
          age_min <- ae$min
          age_max <- ae$max
          age_group <- paste0(ae$min, "_", ae$max)
        }
      }
    }
    if (is.null(age_min) || is.null(age_max)) {
      stop(
        "Enrollment '", enrollment$id,
        "' has no age_range in additional_eligibility"
      )
    }

    for (outcome in spec$outcomes) {
      for (fu in spec$follow_up) {
        plan$add_one_ett(
          enrollment_id = enrollment$id,
          outcome_var = outcome$implementation$variable,
          outcome_name = outcome$name,
          follow_up = fu$weeks,
          confounder_vars = confounder_vars,
          time_exposure_var = "rd_exposed",
          eligible_var = "eligible",
          argset = list(
            age_group = age_group,
            age_min = age_min,
            age_max = age_max
          )
        )
      }
    }

    # Store exposure implementation in the ETT for this enrollment
    impl <- enrollment$exposure$implementation
    rows <- plan$ett$enrollment_id == enrollment$id
    plan$ett[rows, exposure_impl := list(list(impl))]
    plan$ett[rows, matching_ratio := enrollment$exposure$matching_ratio]
    plan$ett[rows, seed := impl$seed]
  }

  plan
}
