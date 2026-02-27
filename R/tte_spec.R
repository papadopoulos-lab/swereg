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
#   tte_plan_from_spec_and_skeleton_meta()           — create TTEPlan + ETT grid from spec
# =============================================================================


# =============================================================================
# tte_read_spec
# =============================================================================

#' Read and validate a YAML study specification
#'
#' Parses a YAML study specification file, converts human-readable windows
#' (e.g., "lifetime_before_baseline", "3 years") to weeks, and validates that all required
#' fields are present.
#'
#' @param spec_path Path to the YAML specification file.
#' @return A nested list representing the parsed specification, with
#'   `window_weeks` fields added to exclusion criteria and confounders.
#'
#' @details
#' Window conversion rules:
#' \itemize{
#'   \item `"lifetime_before_baseline"` -> `Inf`
#'   \item `"N year"` or `"N years"` -> `N * 52`
#' }
#'
#' Validation checks:
#' \itemize{
#'   \item Required sections: study, enrollments, outcomes, follow_up
#'   \item `study$implementation$project_prefix` must exist
#'   \item Each exclusion criterion must have `implementation$source_variable`
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
    "study", "enrollments", "outcomes", "follow_up"
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

      if (is.null(ec$implementation$source_variable)) {
        stop(
          "exclusion_criteria[", i, "] '", ec$name,
          "' is missing implementation$source_variable"
        )
      }

      if (identical(ec$implementation$window, "lifetime_before_and_after_baseline")) {
        # Person-level: no window_weeks conversion needed
      } else {
        if (is.null(ec$implementation$window)) {
          stop(
            "exclusion_criteria[", i, "] '", ec$name,
            "' is missing implementation$window"
          )
        }
        spec$exclusion_criteria[[i]]$implementation$window_weeks <-
          .convert_window(ec$implementation$window)
      }
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

    # Validate and convert additional_exclusion entries
    if (!is.null(enr$additional_exclusion)) {
      for (j in seq_along(enr$additional_exclusion)) {
        ae <- enr$additional_exclusion[[j]]
        if (is.null(ae$implementation$source_variable)) {
          stop(
            "enrollments[", i, "] '", enr$name %||% enr$id,
            "' additional_exclusion[", j, "] '", ae$name,
            "' is missing implementation$source_variable"
          )
        }
        if (identical(ae$implementation$window, "lifetime_before_and_after_baseline")) {
          # Person-level: no window_weeks conversion needed
        } else {
          if (is.null(ae$implementation$window)) {
            stop(
              "enrollments[", i, "] '", enr$name %||% enr$id,
              "' additional_exclusion[", j, "] '", ae$name,
              "' is missing implementation$window"
            )
          }
          spec$enrollments[[i]]$additional_exclusion[[j]]$implementation$window_weeks <-
            .convert_window(ae$implementation$window)
        }
      }
    }
  }

  # Convert confounder windows and validate computed confounders
  if (!is.null(spec$confounders)) {
    for (i in seq_along(spec$confounders)) {
      conf <- spec$confounders[[i]]
      if (!is.null(conf$implementation$window)) {
        spec$confounders[[i]]$implementation$window_weeks <-
          .convert_window(conf$implementation$window)
      }
      if (isTRUE(conf$implementation$computed)) {
        if (is.null(conf$implementation$source_variable)) {
          stop(
            "confounders[", i, "] '", conf$name,
            "' is computed but missing implementation$source_variable"
          )
        }
        if (is.null(conf$implementation$window)) {
          stop(
            "confounders[", i, "] '", conf$name,
            "' is computed but missing implementation$window"
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


#' Convert a window value to weeks
#'
#' @param window Numeric weeks, or character: "lifetime_before_baseline",
#'   "N year", "N years".
#' @return Numeric: `Inf` for lifetime_before_baseline, integer weeks otherwise.
#' @noRd
.convert_window <- function(window) {
  if (identical(window, "lifetime_before_baseline")) return(Inf)
  if (is.numeric(window)) return(as.integer(window))

  # Legacy string support
  m <- regmatches(window, regexec("^(\\d+)\\s+years?$", window))[[1]]
  if (length(m) == 2) return(as.integer(m[2]) * 52L)

  stop(
    "Cannot parse window: '", window,
    "'. Expected 'lifetime_before_baseline', numeric weeks, or 'N year(s)'."
  )
}


# =============================================================================
# tte_apply_exclusions
# =============================================================================

#' Apply exclusion criteria from a study spec to a skeleton
#'
#' Applies calendar year eligibility, enrollment-specific additional inclusion
#' (e.g., age range), global exclusion criteria, and enrollment-specific
#' additional exclusion criteria from the parsed study specification. Calls
#' [tte_eligible_combine()] at the end to AND all criteria into a single
#' `eligible` column.
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
    spec$inclusion_criteria$isoyears[1],
    spec$inclusion_criteria$isoyears[2]
  )
  skeleton <- tte_eligible_isoyears(skeleton, years)
  eligible_cols <- "eligible_isoyears"

  # 2. Enrollment-specific additional inclusion (before global exclusions)
  if (!is.null(enrollment_def$additional_inclusion)) {
    for (ae in enrollment_def$additional_inclusion) {
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

    if (identical(impl$window, "lifetime_before_and_after_baseline")) {
      col_name <- paste0(
        "eligible_no_", impl$source_variable,
        "_lifetime_before_and_after_baseline"
      )
      skeleton <- tte_eligible_no_events_lifetime_before_and_after_baseline(
        skeleton,
        event_var = impl$source_variable,
        col_name = col_name
      )
    } else if (identical(impl$type, "no_prior_exposure")) {
      window <- impl$window_weeks
      col_name <- paste0(
        "eligible_no_", impl$source_variable, "_",
        .window_label(window)
      )
      skeleton <- tte_eligible_no_observation_in_window_excluding_wk0(
        skeleton,
        var = impl$source_variable,
        value = impl$exposure_value,
        window = window,
        col_name = col_name
      )
    } else {
      window <- impl$window_weeks
      col_name <- paste0(
        "eligible_no_", impl$source_variable, "_",
        .window_label(window)
      )
      skeleton <- tte_eligible_no_events_in_window_excluding_wk0(
        skeleton,
        event_var = impl$source_variable,
        window = window,
        col_name = col_name
      )
    }
    eligible_cols <- c(eligible_cols, col_name)
  }

  # 4. Enrollment-specific additional exclusion criteria
  if (!is.null(enrollment_def$additional_exclusion)) {
    for (ec in enrollment_def$additional_exclusion) {
      impl <- ec$implementation

      if (identical(impl$window, "lifetime_before_and_after_baseline")) {
        col_name <- paste0(
          "eligible_no_", impl$source_variable,
          "_lifetime_before_and_after_baseline"
        )
        skeleton <- tte_eligible_no_events_lifetime_before_and_after_baseline(
          skeleton,
          event_var = impl$source_variable,
          col_name = col_name
        )
      } else if (identical(impl$type, "no_prior_exposure")) {
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_no_", impl$source_variable, "_",
          .window_label(window)
        )
        skeleton <- tte_eligible_no_observation_in_window_excluding_wk0(
          skeleton,
          var = impl$source_variable,
          value = impl$exposure_value,
          window = window,
          col_name = col_name
        )
      } else {
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_no_", impl$source_variable, "_",
          .window_label(window)
        )
        skeleton <- tte_eligible_no_events_in_window_excluding_wk0(
          skeleton,
          event_var = impl$source_variable,
          window = window,
          col_name = col_name
        )
      }
      eligible_cols <- c(eligible_cols, col_name)
    }
  }

  # 5. Combine all criteria
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
#' Requires `implementation$source_variable` and `implementation$window` to be set.
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
        window = impl$window_weeks,
        col_name = impl$variable
      )
    }
  }

  skeleton
}


# =============================================================================
# .tte_process_skeleton (internal default callback for Loop 1)
# =============================================================================

#' Process one skeleton file into an enrollment (internal)
#'
#' Default callback for `TTEPlan$generate_enrollments_and_ipw()` when the plan
#' has a stored `spec`. Applies exclusions, derives confounders, sets exposure,
#' enrolls, and prefixes trial_id.
#'
#' @param enrollment_spec Enrollment spec list from `plan$enrollment_spec(i)`.
#' @param file_path Path to a skeleton `.qs2` file.
#' @param spec Parsed study spec from [tte_read_spec()].
#' @return A [TTEEnrollment] object.
#' @noRd
.tte_process_skeleton <- function(enrollment_spec, file_path, spec) {
  data.table::setDTthreads(enrollment_spec$n_threads)
  skeleton <- qs2_read(file_path, nthreads = enrollment_spec$n_threads)

  skeleton <- tte_apply_exclusions(skeleton, spec, enrollment_spec)
  skeleton <- tte_apply_derived_confounders(skeleton, spec)

  x_exp <- enrollment_spec$exposure_impl
  skeleton[,
    rd_exposed := data.table::fcase(
      get(x_exp$variable) == x_exp$exposed_value, TRUE,
      get(x_exp$variable) == x_exp$comparator_value, FALSE,
      default = NA
    )
  ]
  skeleton[, baseline_exposed := rd_exposed]

  enrollment <- tte_enrollment(
    data = skeleton,
    design = enrollment_spec$design,
    ratio = enrollment_spec$matching_ratio,
    seed = enrollment_spec$seed,
    extra_cols = "isoyearweek"
  )
  rm(skeleton)

  enrollment$data[,
    trial_id := paste0(enrollment_spec$enrollment_id, ".", trial_id)
  ]
  enrollment
}


# =============================================================================
# tte_plan_from_spec_and_skeleton_meta
# =============================================================================

# =============================================================================
# tte_validate_spec
# =============================================================================

#' Validate spec variables against skeleton data
#'
#' Checks that all `implementation$variable` references in the spec actually
#' exist as columns in the skeleton data.table. For categorical confounders,
#' also checks that the declared categories match the data. Collects all
#' issues before reporting.
#'
#' @param spec Parsed study specification from [tte_read_spec()].
#' @param skeleton A data.table skeleton (person-week panel) to validate
#'   against.
#' @return `invisible(TRUE)` on success; stops with a numbered issue list
#'   if any checks fail.
#'
#' @family tte_spec
#' @export
tte_validate_spec <- function(spec, skeleton) {
  if (!data.table::is.data.table(skeleton)) {
    stop("skeleton must be a data.table, got ", class(skeleton)[1])
  }

  issues <- character(0)
  n_checked <- 0L
  skel_cols <- names(skeleton)

  # --- Exclusion criteria ---
  for (i in seq_along(spec$exclusion_criteria)) {
    ec <- spec$exclusion_criteria[[i]]
    var <- ec$implementation$source_variable
    n_checked <- n_checked + 1L
    if (!var %in% skel_cols) {
      issues <- c(issues, paste0(
        "exclusion_criteria '", ec$name, "': source_variable '", var,
        "' not found in skeleton"
      ))
    }
  }

  # --- Outcomes ---
  for (i in seq_along(spec$outcomes)) {
    out <- spec$outcomes[[i]]
    var <- out$implementation$variable
    n_checked <- n_checked + 1L
    if (!var %in% skel_cols) {
      issues <- c(issues, paste0(
        "outcomes '", out$name, "': variable '", var,
        "' not found in skeleton"
      ))
    }
  }

  # --- Confounders ---
  for (i in seq_along(spec$confounders)) {
    conf <- spec$confounders[[i]]
    impl <- conf$implementation

    if (isTRUE(impl$computed)) {
      # Computed: check source_variable exists, skip variable (created later)
      n_checked <- n_checked + 1L
      if (!impl$source_variable %in% skel_cols) {
        issues <- c(issues, paste0(
          "confounders '", conf$name, "': source_variable '",
          impl$source_variable, "' not found in skeleton"
        ))
      }
    } else {
      # Non-computed: check variable exists
      n_checked <- n_checked + 1L
      if (!impl$variable %in% skel_cols) {
        issues <- c(issues, paste0(
          "confounders '", conf$name, "': variable '", impl$variable,
          "' not found in skeleton"
        ))
      } else if (!is.null(conf$categories)) {
        # Category check
        data_values <- unique(stats::na.omit(skeleton[[impl$variable]]))
        spec_values <- unlist(conf$categories)
        in_data_not_spec <- setdiff(data_values, spec_values)
        in_spec_not_data <- setdiff(spec_values, data_values)
        if (length(in_data_not_spec) > 0) {
          issues <- c(issues, paste0(
            "confounders '", conf$name, "': values in data but not spec: ",
            paste(in_data_not_spec, collapse = ", ")
          ))
        }
        if (length(in_spec_not_data) > 0) {
          issues <- c(issues, paste0(
            "confounders '", conf$name,
            "': values in spec but not data (may be absent in this batch): ",
            paste(in_spec_not_data, collapse = ", ")
          ))
        }
      }
    }
  }

  # --- Enrollments ---
  for (i in seq_along(spec$enrollments)) {
    enr <- spec$enrollments[[i]]
    exp_impl <- enr$exposure$implementation

    # Exposure variable
    n_checked <- n_checked + 1L
    if (!exp_impl$variable %in% skel_cols) {
      issues <- c(issues, paste0(
        "enrollments '", enr$name %||% enr$id, "': exposure variable '",
        exp_impl$variable, "' not found in skeleton"
      ))
    } else {
      # Check exposed_value and comparator_value are present in data
      data_values <- unique(skeleton[[exp_impl$variable]])
      if (!exp_impl$exposed_value %in% data_values) {
        issues <- c(issues, paste0(
          "enrollments '", enr$name %||% enr$id, "': exposed_value '",
          exp_impl$exposed_value, "' not found in column '",
          exp_impl$variable, "'"
        ))
      }
      if (!exp_impl$comparator_value %in% data_values) {
        issues <- c(issues, paste0(
          "enrollments '", enr$name %||% enr$id, "': comparator_value '",
          exp_impl$comparator_value, "' not found in column '",
          exp_impl$variable, "'"
        ))
      }
    }

    # Additional inclusion variables
    if (!is.null(enr$additional_inclusion)) {
      for (ae in enr$additional_inclusion) {
        if (!is.null(ae$implementation$variable)) {
          n_checked <- n_checked + 1L
          if (!ae$implementation$variable %in% skel_cols) {
            issues <- c(issues, paste0(
              "enrollments '", enr$name %||% enr$id,
              "': additional_inclusion variable '",
              ae$implementation$variable, "' not found in skeleton"
            ))
          }
        }
      }
    }

    # Additional exclusion variables
    if (!is.null(enr$additional_exclusion)) {
      for (ae in enr$additional_exclusion) {
        var <- ae$implementation$source_variable
        n_checked <- n_checked + 1L
        if (!var %in% skel_cols) {
          issues <- c(issues, paste0(
            "enrollments '", enr$name %||% enr$id,
            "': additional_exclusion source_variable '",
            var, "' not found in skeleton"
          ))
        }
      }
    }
  }

  if (length(issues) > 0) {
    stop(
      "Spec validation failed with ", length(issues), " issue(s):\n",
      paste0("  ", seq_along(issues), ". ", issues, collapse = "\n")
    )
  }

  message(
    "Spec validation passed: ", n_checked,
    " entries checked against ", length(skel_cols), " columns"
  )
  invisible(TRUE)
}


# =============================================================================
# tte_plan_from_spec_and_skeleton_meta
# =============================================================================

#' Create a TTEPlan from a study specification
#'
#' Builds a [TTEPlan] with a full ETT grid (enrollments x outcomes x
#' follow-up) from the parsed study specification. Also stores each
#' enrollment's exposure implementation details in the ETT data.table so
#' they are available via `plan[[i]]$exposure_impl`.
#'
#' @param spec Character path to a YAML spec file, or a parsed spec list
#'   from [tte_read_spec()]. When a path is given, the version extracted
#'   from the filename (`_vNNN.yaml`) is validated against
#'   `spec$study$implementation$version`.
#' @param skeleton_files Character vector of skeleton file paths. Either
#'   this or `skeleton_meta_path` must be provided.
#' @param skeleton_meta_path Path to a `skeleton_meta.qs2` file. The
#'   `@skeleton_files` slot is extracted from the [SkeletonMeta] object.
#' @param n_skeleton_files Optional integer: if not NULL, only the first
#'   `n_skeleton_files` files are used (for faster dev iterations).
#' @param global_max_isoyearweek Administrative censoring boundary
#'   (isoyearweek string, e.g., "2023-52"). If `NULL` (default), auto-detected
#'   from `max(isoyearweek)` in the first skeleton file. Also runs
#'   [tte_validate_spec()] on that skeleton.
#' @return A [TTEPlan] object with the full ETT grid.
#'
#' @family tte_spec
#' @export
tte_plan_from_spec_and_skeleton_meta <- function(
  spec,
  skeleton_files = NULL,
  skeleton_meta_path = NULL,
  n_skeleton_files = NULL,
  global_max_isoyearweek = NULL
) {
  # Resolve spec: path -> parsed list
  if (is.character(spec) && length(spec) == 1) {
    spec_path <- spec
    # Extract version from filename (_vNNN.yaml)
    filename_version <- regmatches(
      basename(spec_path),
      regexec("_(v\\d+)\\.yaml$", basename(spec_path))
    )[[1]][2]
    spec <- tte_read_spec(spec_path)
    if (!is.na(filename_version)) {
      spec_version <- spec$study$implementation$version
      if (!identical(spec_version, filename_version)) {
        stop(
          "Version mismatch: filename has '", filename_version,
          "' but spec YAML has '", spec_version %||% "NULL", "'"
        )
      }
    }
  }

  # Resolve skeleton_files
  if (is.null(skeleton_files)) {
    if (is.null(skeleton_meta_path)) {
      stop("Either skeleton_files or skeleton_meta_path must be provided")
    }
    skel_meta <- qs2_read(skeleton_meta_path)
    skeleton_files <- skel_meta@skeleton_files
  }

  # Apply n_skeleton_files limit
  if (!is.null(n_skeleton_files)) {
    skeleton_files <- utils::head(skeleton_files, n_skeleton_files)
  }
  if (is.null(global_max_isoyearweek)) {
    skeleton <- qs2_read(skeleton_files[1])
    tte_validate_spec(spec, skeleton)
    global_max_isoyearweek <- skeleton[, max(isoyearweek, na.rm = TRUE)]
    message("Admin censoring cutoff from skeleton: ", global_max_isoyearweek)
    rm(skeleton)
  }

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
  plan$spec <- spec

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
        "Enrollment '", enrollment$id,
        "' has no age_range in additional_inclusion"
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
