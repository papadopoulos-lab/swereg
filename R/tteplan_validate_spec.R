# =============================================================================
# tteplan_validate_spec
# =============================================================================

#' Validate spec variables against skeleton data
#'
#' Checks that all `implementation$variable` references in the spec actually
#' exist as columns in the skeleton data.table. For categorical confounders,
#' also checks that the declared categories match the data. Collects all
#' issues before reporting.
#'
#' It also checks the observation column of every enrollment that names one:
#' the column MUST exist in the skeleton, and it MUST be logical.
#' [tteplan_read_spec()] cannot run that check, because it reads no data. An
#' enrollment that declares the `row_presence` sentinel names no column, so
#' there is nothing to check.
#'
#' @param spec Parsed study specification from [tteplan_read_spec()].
#' @param skeleton A data.table skeleton (person-week panel) to validate
#'   against.
#' @return `invisible(TRUE)` on success; emits a warning with a numbered
#'   issue list if any checks fail.
#'
#' @family tte_spec
#' @export
tteplan_validate_spec <- function(spec, skeleton) {
  if (!data.table::is.data.table(skeleton)) {
    stop(
      "skeleton must be a data.table, got ",
      class(skeleton)[1],
      call. = FALSE
    )
  }

  errors <- character(0)
  warnings <- character(0)
  n_checked <- 0L
  skel_cols <- names(skeleton)

  # --- Global inclusion criteria ---
  # `tteplan_read_spec()` cannot run this check, because it reads no data.
  global_inclusion <- spec[["inclusion_criteria"]][["criteria"]] %||% list()
  for (i in seq_along(global_inclusion)) {
    ic <- global_inclusion[[i]]
    vars <- ic$implementation$source_variable
    n_checked <- n_checked + 1L
    missing <- vars[!vars %in% skel_cols]
    if (length(missing) > 0) {
      errors <- c(
        errors,
        paste0(
          "inclusion_criteria$criteria '",
          ic$name,
          "': source_variable '",
          paste(missing, collapse = "', '"),
          "' not found in skeleton"
        )
      )
    }
  }

  # --- Exclusion criteria ---
  for (i in seq_along(spec$exclusion_criteria)) {
    ec <- spec$exclusion_criteria[[i]]
    vars <- ec$implementation$source_variable
    n_checked <- n_checked + 1L
    missing <- vars[!vars %in% skel_cols]
    if (length(missing) > 0) {
      errors <- c(
        errors,
        paste0(
          "exclusion_criteria '",
          ec$name,
          "': source_variable '",
          paste(missing, collapse = "', '"),
          "' not found in skeleton"
        )
      )
    }
  }

  # --- Outcomes ---
  for (i in seq_along(spec$outcomes)) {
    out <- spec$outcomes[[i]]
    vars <- out$implementation$variable
    n_checked <- n_checked + 1L
    missing <- vars[!vars %in% skel_cols]
    if (length(missing) > 0) {
      errors <- c(
        errors,
        paste0(
          "outcomes '",
          out$name,
          "': variable '",
          paste(missing, collapse = "', '"),
          "' not found in skeleton"
        )
      )
    }
  }

  # --- Confounders ---
  for (i in seq_along(spec$confounders)) {
    conf <- spec$confounders[[i]]
    impl <- conf$implementation

    if (isTRUE(impl$computed)) {
      # Computed: check source_variable exists, skip variable (created later)
      n_checked <- n_checked + 1L
      missing <- impl$source_variable[!impl$source_variable %in% skel_cols]
      if (length(missing) > 0) {
        errors <- c(
          errors,
          paste0(
            "confounders '",
            conf$name,
            "': source_variable '",
            paste(missing, collapse = "', '"),
            "' not found in skeleton"
          )
        )
      }
    } else {
      # Non-computed: check variable exists
      n_checked <- n_checked + 1L
      if (!impl$variable %in% skel_cols) {
        errors <- c(
          errors,
          paste0(
            "confounders '",
            conf$name,
            "': variable '",
            impl$variable,
            "' not found in skeleton"
          )
        )
      } else if (!is.null(conf$categories)) {
        # Category check (soft: categories may be absent in small batches)
        data_values <- unique(stats::na.omit(skeleton[[impl$variable]]))
        spec_values <- unlist(conf$categories)
        in_data_not_spec <- setdiff(data_values, spec_values)
        in_spec_not_data <- setdiff(spec_values, data_values)
        if (length(in_data_not_spec) > 0) {
          errors <- c(
            errors,
            paste0(
              "confounders '",
              conf$name,
              "': values in data but not spec: ",
              paste(in_data_not_spec, collapse = ", ")
            )
          )
        }
        if (length(in_spec_not_data) > 0) {
          warnings <- c(
            warnings,
            paste0(
              "confounders '",
              conf$name,
              "': values in spec but not data (may be absent in this batch): ",
              paste(in_spec_not_data, collapse = ", ")
            )
          )
        }
      }
    }
  }

  # --- Enrollments ---
  for (i in seq_along(spec$enrollments)) {
    enr <- spec$enrollments[[i]]
    tx_impl <- enr$treatment$implementation

    # Treatment variable
    n_checked <- n_checked + 1L
    if (!tx_impl$variable %in% skel_cols) {
      errors <- c(
        errors,
        paste0(
          "enrollments '",
          enr$name %||% enr$id,
          "': treatment variable '",
          tx_impl$variable,
          "' not found in skeleton"
        )
      )
    } else {
      # Check intervention_value and comparator_value are present in data
      data_values <- unique(skeleton[[tx_impl$variable]])
      if (!tx_impl$intervention_value %in% data_values) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': intervention_value '",
            tx_impl$intervention_value,
            "' not found in column '",
            tx_impl$variable,
            "'"
          )
        )
      }
      if (!tx_impl$comparator_value %in% data_values) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': comparator_value '",
            tx_impl$comparator_value,
            "' not found in column '",
            tx_impl$variable,
            "'"
          )
        )
      }
    }

    # Observation column. The parser cannot run this check, because it reads
    # no data. A sentinel names no column, so there is nothing to check.
    obs_col <- .tte_observed_column(enr$observed_var)
    if (!is.null(obs_col)) {
      n_checked <- n_checked + 1L
      if (!obs_col %in% skel_cols) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': observed_var column '",
            obs_col,
            "' not found in skeleton"
          )
        )
      } else if (!is.logical(skeleton[[obs_col]])) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': observed_var column '",
            obs_col,
            "' must be logical, and it is ",
            class(skeleton[[obs_col]])[1]
          )
        )
      }
    }

    # Additional inclusion variables
    if (!is.null(enr$additional_inclusion)) {
      for (ae in enr$additional_inclusion) {
        if (identical(ae$type, "has_event")) {
          vars <- ae$implementation$source_variable
          n_checked <- n_checked + 1L
          missing <- vars[!vars %in% skel_cols]
          if (length(missing) > 0) {
            errors <- c(
              errors,
              paste0(
                "enrollments '",
                enr$name %||% enr$id,
                "': additional_inclusion source_variable '",
                paste(missing, collapse = "', '"),
                "' not found in skeleton"
              )
            )
          }
        } else if (!is.null(ae$implementation$variable)) {
          n_checked <- n_checked + 1L
          if (!ae$implementation$variable %in% skel_cols) {
            errors <- c(
              errors,
              paste0(
                "enrollments '",
                enr$name %||% enr$id,
                "': additional_inclusion variable '",
                ae$implementation$variable,
                "' not found in skeleton"
              )
            )
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
          errors <- c(
            errors,
            paste0(
              "enrollments '",
              enr$name %||% enr$id,
              "': additional_exclusion source_variable '",
              var,
              "' not found in skeleton"
            )
          )
        }
      }
    }
  }

  # --- Subgroups: each must exist in the skeleton AND be a confounder. The
  # within-stratum validity of the marginal weights requires the subgroup to
  # be in the PS / IPCW models (i.e. among the confounders).
  if (!is.null(spec$subgroups)) {
    confounder_vars <- vapply(
      spec$confounders %||% list(),
      function(cf) cf$implementation$variable %||% NA_character_,
      character(1)
    )
    for (i in seq_along(spec$subgroups)) {
      sv <- spec$subgroups[[i]]$implementation$variable
      if (!sv %in% skel_cols) {
        errors <- c(
          errors,
          paste0(
            "subgroups[",
            i,
            "] variable '",
            sv,
            "' not found in skeleton columns"
          )
        )
      }
      if (!sv %in% confounder_vars) {
        errors <- c(
          errors,
          paste0(
            "subgroups[",
            i,
            "] variable '",
            sv,
            "' must also be a confounder (effect-modifier weights are only ",
            "valid within strata when the subgroup is in the PS/IPCW models)"
          )
        )
      }
    }
  }

  # --- Report results ---
  # Warnings are soft issues (e.g. category absent in this batch)
  if (length(warnings) > 0) {
    warning(
      "Spec validation: ",
      length(warnings),
      " warning(s):\n",
      paste0("  ", seq_along(warnings), ". ", warnings, collapse = "\n"),
      call. = FALSE
    )
  }

  # Errors are hard failures (missing variables that will break the pipeline)
  if (length(errors) > 0) {
    stop(
      "Spec validation failed: ",
      length(errors),
      " error(s):\n",
      paste0("  ", seq_along(errors), ". ", errors, collapse = "\n"),
      call. = FALSE
    )
  }

  message(
    "Spec validation passed: ",
    n_checked,
    " entries checked against ",
    length(skel_cols),
    " columns"
  )
  return(invisible(TRUE))
}
