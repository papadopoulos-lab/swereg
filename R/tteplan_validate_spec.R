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
#' @section The prevalent-user check:
#'
#' An enrollment gets its incident-user design from a washout exclusion, an
#' exclusion of type `no_prior_intervention`. The check warns when no washout
#' covers the enrollment's intervention level.
#'
#' Coverage is containment, measured on the weekly rows of `skeleton`. A
#' washout names a level in `intervention_value` and one or more columns in
#' `source_variable`. It covers the enrollment when every weekly row at the
#' intervention level holds that level, in one of those columns. A missing
#' value counts as uncovered. A multi-source washout covers through the union
#' of its sources. The enrollment passes when at least one washout covers it.
#'
#' The washout column and the treatment column often differ. A specification
#' can wash out a whole drug class, then compare one sub-type of that class
#' against no treatment. A test on the two column names cannot answer that,
#' and it cannot see a washout that names the right column at the wrong
#' level.
#'
#' Set `options(swereg.warn_prevalent_user = FALSE)` to silence the warning.
#' Use it for a discontinuation or switching study, which enrols prevalent
#' users by design.
#'
#' @param spec Parsed study specification from [tteplan_read_spec()].
#' @param skeleton A data.table skeleton (person-week panel) to validate
#'   against.
#' @param skeleton_batch Batch number of `skeleton`. The prevalent-user
#'   warning reports it, so a reader knows which batch the check measured.
#' @return `invisible(TRUE)` on success; emits a warning with a numbered
#'   issue list if any checks fail.
#'
#' @family tte_spec
#' @export
tteplan_validate_spec <- function(spec, skeleton, skeleton_batch = 1L) {
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

    # Additional exclusion variables. `source_variable` holds one column or
    # several, so the check is vectorised, as the global exclusion_criteria
    # check above is. A scalar test would stop on a multi-source washout with
    # "the condition has length > 1", and the prevalent-user guard below
    # reads exactly those washouts.
    if (!is.null(enr$additional_exclusion)) {
      for (ae in enr$additional_exclusion) {
        vars <- ae$implementation$source_variable
        n_checked <- n_checked + 1L
        missing <- vars[!vars %in% skel_cols]
        if (length(missing) > 0) {
          errors <- c(
            errors,
            paste0(
              "enrollments '",
              enr$name %||% enr$id,
              "': additional_exclusion source_variable '",
              paste(missing, collapse = "', '"),
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

  # --- Prevalent-user guard ---
  # It runs after the error gate, so every column it reads exists in the
  # skeleton. [tteplan_read_spec()] cannot run this check, because it reads
  # no data.
  .tte_warn_prevalent_user(spec, skeleton, skeleton_batch)

  message(
    "Spec validation passed: ",
    n_checked,
    " entries checked against ",
    length(skel_cols),
    " columns"
  )
  return(invisible(TRUE))
}


#' Collect the washout exclusions that apply to one enrollment
#'
#' A washout is an exclusion of type `no_prior_intervention`. It names its
#' target columns in `source_variable` and the level it looks for in
#' `intervention_value`. Both the global `exclusion_criteria` and the
#' enrollment's own `additional_exclusion` entries apply.
#'
#' @param spec Parsed study specification.
#' @param enr One entry of `spec$enrollments`.
#' @return A list with one entry per washout. Each entry holds `vars`, the
#'   `source_variable` columns, and `level`, the `intervention_value`.
#' @noRd
.tte_washouts <- function(spec, enr) {
  excls <- c(
    spec$exclusion_criteria %||% list(),
    enr$additional_exclusion %||% list()
  )
  out <- list()
  for (ec in excls) {
    # `[[` is exact. `$type` would partial-match another key.
    impl <- ec$implementation %||% list()
    if (!identical(impl[["type"]], "no_prior_intervention")) {
      next
    }
    vars <- as.character(unlist(impl[["source_variable"]] %||% character()))
    level <- impl[["intervention_value"]]
    if (length(vars) == 0L || is.null(level)) {
      next
    }
    out[[length(out) + 1L]] <- list(vars = vars, level = level)
  }
  return(out)
}


#' Mark the rows one washout does not cover
#'
#' @param skeleton The skeleton data.table.
#' @param idx Integer row numbers of the weekly rows at the intervention
#'   level.
#' @param washout One entry of [.tte_washouts()].
#' @return A logical vector, one element per entry of `idx`. `TRUE` marks a
#'   row that no source of `washout` covers. `%in%` makes a missing value
#'   uncovered.
#' @noRd
.tte_washout_outside <- function(skeleton, idx, washout) {
  outside <- rep(TRUE, length(idx))
  for (v in washout$vars) {
    outside <- outside & !(skeleton[[v]][idx] %in% washout$level)
  }
  return(outside)
}


#' Warn when no washout covers an enrollment's intervention level
#'
#' See the prevalent-user section of [tteplan_validate_spec()] for the rule.
#'
#' @param spec Parsed study specification.
#' @param skeleton The skeleton data.table.
#' @param skeleton_batch Batch number of `skeleton`, for the message.
#' @return `invisible(NULL)`, called for the warning.
#' @noRd
.tte_warn_prevalent_user <- function(spec, skeleton, skeleton_batch) {
  if (!isTRUE(getOption("swereg.warn_prevalent_user", TRUE))) {
    return(invisible(NULL))
  }
  # A weekly row is one person-week. An annual row summarises a year, so it
  # is not a week the person could initiate in.
  weekly <- if ("is_isoyear" %in% names(skeleton)) {
    which(skeleton[["is_isoyear"]] %in% FALSE)
  } else {
    seq_len(nrow(skeleton))
  }
  # Enrollments repeat the same treatment column and level, once per age
  # band. Index the rows at each level once.
  at_level <- list()
  for (enr in spec$enrollments) {
    tx_impl <- enr$treatment$implementation
    tx_var <- tx_impl$variable
    tx_level <- tx_impl$intervention_value
    key <- paste0(tx_var, "\r", as.character(tx_level))
    if (!key %in% names(at_level)) {
      at_level[[key]] <- weekly[skeleton[[tx_var]][weekly] %in% tx_level]
    }
    idx <- at_level[[key]]

    washouts <- .tte_washouts(spec, enr)
    covered <- FALSE
    outside_all <- rep(TRUE, length(idx))
    for (washout in washouts) {
      outside <- .tte_washout_outside(skeleton, idx, washout)
      if (!any(outside)) {
        covered <- TRUE
        break
      }
      outside_all <- outside_all & outside
    }
    if (covered) {
      next
    }

    warning(
      "enrollment '",
      enr$id %||% enr$name,
      "' has no new-user/washout exclusion covering its intervention level ('",
      tx_var,
      "' == \"",
      tx_level,
      "\"): prevalent users will enrol as intervention at every eligible ",
      "trial period (prevalent-user design). If an incident-user design ",
      "is intended, add an exclusion that covers that level -- either ",
      "a finite washout window (e.g. window: 104 weeks, as in Danaei ",
      "2013) or window: 'lifetime_before_baseline' for a never-user ",
      "design (implementation type 'no_prior_intervention'). On skeleton ",
      "batch ",
      skeleton_batch,
      ", ",
      sum(outside_all),
      " of ",
      length(idx),
      " weeks at ",
      tx_var,
      " == \"",
      tx_level,
      "\" are outside every washout.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}
