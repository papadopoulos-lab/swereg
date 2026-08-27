# These TTEPlan methods read and change the study specification. They cover
# the ETT grid, one enrollment's spec, the spec workbook, and the reload.

#' @include r6_tteplan.R
#' @description Add one ETT to the plan.
#'
#' TTE (target trial emulation) names the method and the class prefix. An
#' ETT (emulated target trial) names one cell of the grid: one outcome x
#' one follow_up x one enrollment_id, and one row of `$ett`. ETT is always
#' countable. Write "one ETT" or "12 ETTs", never "the ETT approach".
#'
#' ETTs sharing an enrollment_id use the same trial panels
#' (same comparator draw, same age group, same confounders). They differ only
#' in outcome and/or follow-up duration. This avoids redundant
#' re-enrollment for each outcome/follow-up combo.
#'
#' @param enrollment_id Character, enrollment group identifier (e.g., "01").
#' @param outcome_var Character, name of the outcome column.
#' @param outcome_name Character, short human-readable outcome label
#'   (used in forest plot rows and Table S10).
#' @param follow_up Integer, follow-up duration in weeks.
#' @param confounder_vars Character vector of confounder column names.
#' @param subgroup_vars Character vector or NULL, baseline subgroup columns
#'   for effect-modification analyses (default: NULL).
#' @param time_treatment_var Character or NULL, time-varying treatment column.
#' @param eligible_var Character or NULL, eligibility column.
#' @param observed_var The observation encoding, or NULL. Give a list with
#'   exactly one of `column` and `sentinel`. See the observation contract
#'   section of [tteplan_read_spec()].
#' @param intervention_tolerance_weeks Integer, the tolerance in weeks for
#'   the intervention arm (default: 0L).
#' @param comparator_tolerance_weeks Integer, the tolerance in weeks for
#'   the comparator arm (default: 0L).
#' @param argset Named list with age_group, age_min, age_max (and optional
#'   person_id_var, outcome_description).
TTEPlan$set(
  "public",
  "add_one_ett",
  function(
    enrollment_id,
    outcome_var,
    outcome_name,
    follow_up,
    confounder_vars,
    subgroup_vars = NULL,
    time_treatment_var,
    eligible_var,
    observed_var = NULL,
    intervention_tolerance_weeks = 0L,
    comparator_tolerance_weeks = 0L,
    argset = list()
  ) {
    outcome_description <- argset$outcome_description %||% NA_character_
    outcome_role <- argset$outcome_role %||% NA_character_
    age_group <- argset$age_group
    age_min <- argset$age_min
    age_max <- argset$age_max
    if (is.null(age_group) || is.null(age_min) || is.null(age_max)) {
      stop(
        "argset must contain 'age_group', 'age_min', and 'age_max'",
        call. = FALSE
      )
    }
    person_id_var <- if (!is.null(argset$person_id_var)) {
      argset$person_id_var
    } else {
      "id"
    }
    treatment_var <- "baseline_intervention"

    tv_intervention <- if (is.null(time_treatment_var)) {
      NA_character_
    } else {
      time_treatment_var
    }
    elig <- if (is.null(eligible_var)) NA_character_ else eligible_var
    observed_var <- .tte_observed_var(observed_var, "observed_var")
    intervention_tolerance_weeks <- .tte_tolerance_weeks(
      intervention_tolerance_weeks,
      "intervention_tolerance_weeks"
    )
    comparator_tolerance_weeks <- .tte_tolerance_weeks(
      comparator_tolerance_weeks,
      "comparator_tolerance_weeks"
    )

    # Validate: if this enrollment_id already exists, design params must match
    if (!is.null(self$ett) && nrow(self$ett) > 0) {
      rows_match <- self$ett$enrollment_id == enrollment_id
      existing <- self$ett[rows_match]
      if (nrow(existing) > 0) {
        first <- existing[1]
        if (first$person_id_var != person_id_var) {
          stop(
            "person_id_var mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
        if (first$treatment_var != treatment_var) {
          stop(
            "treatment_var mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
        first_tv <- first$time_treatment_var
        if (
          !identical(is.na(first_tv), is.na(tv_intervention)) ||
            (!is.na(first_tv) && first_tv != tv_intervention)
        ) {
          stop(
            "time_treatment_var mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
        first_el <- first$eligible_var
        if (
          !identical(is.na(first_el), is.na(elig)) ||
            (!is.na(first_el) && first_el != elig)
        ) {
          stop(
            "eligible_var mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
        if (!identical(first$confounder_vars[[1]], confounder_vars)) {
          stop(
            "confounder_vars mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
        if (
          "observed_var" %in%
            names(existing) &&
            !identical(first$observed_var[[1]], observed_var)
        ) {
          stop(
            "observed_var mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
        if (
          "intervention_tolerance_weeks" %in%
            names(existing) &&
            !identical(
              first$intervention_tolerance_weeks,
              intervention_tolerance_weeks
            )
        ) {
          stop(
            "intervention_tolerance_weeks mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
        if (
          "comparator_tolerance_weeks" %in%
            names(existing) &&
            !identical(
              first$comparator_tolerance_weeks,
              comparator_tolerance_weeks
            )
        ) {
          stop(
            "comparator_tolerance_weeks mismatch within enrollment_id ",
            enrollment_id,
            call. = FALSE
          )
        }
      }
    }

    ett_num <- if (is.null(self$ett)) 1L else nrow(self$ett) + 1L
    ett_id <- paste0("ETT", sprintf("%05d", ett_num))
    description <- paste0(
      ett_id,
      ": ",
      outcome_name,
      " (",
      follow_up,
      "w, age ",
      stringr::str_replace(age_group, "_", "-"),
      ")"
    )
    prefix <- self$project_prefix
    file_raw <- paste0(prefix, "_raw_", enrollment_id, ".qs2")
    file_imp <- paste0(prefix, "_imp_", enrollment_id, ".qs2")
    file_analysis <- paste0(prefix, "_analysis_", ett_id, ".qs2")
    file_analysis_itt <- paste0(prefix, "_analysis_itt_", ett_id, ".qs2")

    new_row <- data.table::data.table(
      enrollment_id = enrollment_id,
      ett_id = ett_id,
      age_group = age_group,
      age_min = age_min,
      age_max = age_max,
      follow_up = follow_up,
      outcome_var = outcome_var,
      outcome_name = outcome_name,
      outcome_description = outcome_description,
      outcome_role = outcome_role,
      description = description,
      file_raw = file_raw,
      file_imp = file_imp,
      file_analysis = file_analysis,
      file_analysis_itt = file_analysis_itt,
      confounder_vars = list(confounder_vars),
      subgroup_vars = list(subgroup_vars),
      person_id_var = person_id_var,
      treatment_var = treatment_var,
      time_treatment_var = tv_intervention,
      eligible_var = elig,
      observed_var = list(observed_var),
      intervention_tolerance_weeks = intervention_tolerance_weeks,
      comparator_tolerance_weeks = comparator_tolerance_weeks
    )

    if (is.null(self$ett)) {
      self$ett <- new_row
    } else {
      self$ett <- data.table::rbindlist(
        list(self$ett, new_row),
        use.names = TRUE,
        fill = TRUE
      )
    }
    return(invisible(self))
  }
)

#' @description Extract enrollment spec for the i-th enrollment_id group.
#'
#' @param i Integer index (1-based).
#' @return A list with:
#'   \describe{
#'     \item{design}{A [TTEDesign] object with column mappings. It carries
#'       the observation encoding and both arm tolerances that the spec
#'       declared for this enrollment.}
#'     \item{enrollment_id}{Character, the enrollment group ID}
#'     \item{age_range}{Numeric vector of length 2: c(min, max)}
#'     \item{n_threads}{Integer, number of data.table threads to use}
#'     \item{treatment_impl}{List with variable, intervention_value, comparator_value
#'       (present when plan was built from a spec)}
#'     \item{comparator_to_intervention_ratio}{Numeric. The draw takes
#'       this many times a trial's count of intervention individuals,
#'       capped at the comparators that trial holds. Present when the
#'       plan was built from a spec.}
#'     \item{seed}{Integer. It makes the comparator draw reproducible.
#'       Present when the plan was built from a spec.}
#'   }
TTEPlan$set("public", "enrollment_spec", function(i = 1L) {
  enrollment_ids <- unique(self$ett$enrollment_id)
  eid <- enrollment_ids[i]
  rows <- self$ett[self$ett$enrollment_id == eid]
  first <- rows[1]

  x_person_id <- first$person_id_var
  x_time_treatment <- first$time_treatment_var
  if (is.na(x_time_treatment)) {
    x_time_treatment <- NULL
  }
  x_eligible <- first$eligible_var
  if (is.na(x_eligible)) {
    x_eligible <- NULL
  }
  # A plan saved before the observation contract has no such column, so
  # read it defensively. A missing column means "not declared", which is
  # what a pre-landmark plan meant.
  x_observed <- if ("observed_var" %in% names(self$ett)) {
    first$observed_var[[1]]
  } else {
    NULL
  }
  x_tol_intervention <- if (
    "intervention_tolerance_weeks" %in% names(self$ett)
  ) {
    first$intervention_tolerance_weeks
  } else {
    0L
  }
  x_tol_comparator <- if ("comparator_tolerance_weeks" %in% names(self$ett)) {
    first$comparator_tolerance_weeks
  } else {
    0L
  }

  result <- list(
    design = TTEDesign$new(
      person_id_var = x_person_id,
      treatment_var = first$treatment_var,
      time_treatment_var = x_time_treatment,
      eligible_var = x_eligible,
      observed_var = x_observed,
      intervention_tolerance_weeks = x_tol_intervention,
      comparator_tolerance_weeks = x_tol_comparator,
      outcome_vars = rows$outcome_var,
      confounder_vars = first$confounder_vars[[1]],
      subgroup_vars = if ("subgroup_vars" %in% names(self$ett)) {
        first$subgroup_vars[[1]]
      } else {
        NULL
      },
      follow_up_time = as.integer(max(rows$follow_up)),
      admin_censor_isoyearweek = self$global_max_isoyearweek,
      period_width = self$period_width
    ),
    enrollment_id = eid,
    age_range = c(first$age_min, first$age_max),
    n_threads = .safe_n_cores()
  )

  # Pass through spec-derived fields if present in ETT
  if ("treatment_impl" %in% names(self$ett)) {
    result$treatment_impl <- first$treatment_impl[[1]]
  }
  if ("comparator_to_intervention_ratio" %in% names(self$ett)) {
    result$comparator_to_intervention_ratio <- first$comparator_to_intervention_ratio
  }
  if ("seed" %in% names(self$ett)) {
    result$seed <- first$seed
  }

  return(result)
})

#' @description Export the study specification to a standalone Excel file.
#'
#' Writes a formatted summary of the spec (design, criteria, confounders,
#' outcomes, enrollments) with ICD-10/ATC code annotations from the code
#' registry. No analysis results required.
#'
#' @param path Optional output path override. If `NULL` (default), writes
#'   to `self$spec_xlsx` (that is, `spec_<version>.xlsx` inside
#'   `self$dir_results`, where `<version>` is `self$spec_version`).
#' @return `invisible(self)`
TTEPlan$set("public", "excel_spec_summary", function(path = NULL) {
  return(.plan_excel_spec_summary(self, path))
})

#' @description Refresh cosmetic spec fields (enrollment names, treatment
#' arm labels, outcome names, ETT descriptions) on a cached plan without
#' re-running the upstream pipeline.
#'
#' Structural fields (confounders, exclusion criteria, follow-up windows,
#' comparator-draw parameters, etc.) are *not* applied - they would invalidate
#' the cached results. The differences are surfaced via a loud warning
#' and recorded in `self$spec_reload_skipped_diffs`.
#'
#' @param spec_path Optional path to a `.yaml` study spec file. If `NULL`
#'   (default), uses `self$spec_path` (resolved from `dir_spec_cp` +
#'   `filename_spec(spec_version)`).
#' @param quiet Logical, suppress the success message (default FALSE).
#' @return `invisible(self)`.
TTEPlan$set("public", "reload_spec", function(spec_path = NULL, quiet = FALSE) {
  if (is.null(self$spec)) {
    stop("This plan has no existing spec to reload against.", call. = FALSE)
  }
  if (is.null(spec_path)) {
    spec_path <- self$spec_path
  }
  new_spec <- tteplan_read_spec(spec_path)
  diffs <- .diff_specs(self$spec, new_spec)
  if (length(diffs$structural) > 0L) {
    warning(
      "Spec has structural changes that were NOT applied (cached results ",
      "are still bound to the old definitions):\n  ",
      paste(diffs$structural, collapse = "\n  "),
      call. = FALSE
    )
    self$spec_reload_skipped_diffs <- diffs$structural
  } else {
    self$spec_reload_skipped_diffs <- NULL
  }
  .apply_cosmetic_spec_updates(self, new_spec)
  self$spec_reloaded_at <- Sys.time()
  if (!quiet) {
    n_cosm <- length(diffs$cosmetic)
    message(
      "Spec reloaded: ",
      n_cosm,
      " cosmetic field(s) updated. Call $export_tables() to regenerate ",
      "the workbook with the new labels."
    )
  }
  return(invisible(self))
})
