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
#' An enrollment gets its incident-user design from a washout rule, of type
#' `no_prior_value` or `only_prior_value`. The check warns when no washout
#' covers the enrollment's intervention level. All four rule blocks are
#' searched: `inclusion_criteria$criteria`, `exclusion_criteria`, and the
#' enrollment's `additional_inclusion` and `additional_exclusion`.
#'
#' Coverage is measured on the weekly rows of `skeleton`. A prevalent week is a
#' week at the intervention level that follows an earlier week of the same
#' person at that level. A washout covers the enrollment when it makes every
#' prevalent week ineligible. The check evaluates the eligibility expression
#' the compiler builds, so it measures the column the skeleton will hold. A
#' first initiation stays eligible and is not an uncovered week. The enrollment
#' passes when at least one washout covers it.
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

    # Additional inclusion variables. Every rule type that compiles to an
    # eligibility column names its columns in `source_variable`, so all three
    # take the same check. `age_range` names one column in `variable`.
    if (!is.null(enr$additional_inclusion)) {
      for (ae in enr$additional_inclusion) {
        if (isTRUE(.tte_entry_type(ae) %in% .TTE_INCLUSION_RULE_TYPES)) {
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


#' Collect the washout rules that apply to one enrollment
#'
#' A washout declares `implementation$type`, and the two types are
#' `no_prior_value` and `only_prior_value`. All four rule blocks can hold one:
#' the global `inclusion_criteria$criteria` and `exclusion_criteria`, and the
#' enrollment's own `additional_inclusion` and `additional_exclusion`.
#'
#' Every washout's source columns are checked in the error gate of
#' [tteplan_validate_spec()], which stops before the guard runs. So this
#' function needs no column check of its own.
#'
#' @param spec Parsed study specification.
#' @param enr One entry of `spec$enrollments`.
#' @return A list with one entry per washout. Each entry holds `name`, `vars`,
#'   the `source_variable` columns, and `impl`, the implementation list.
#' @noRd
.tte_washouts <- function(spec, enr) {
  entries <- c(
    spec[["inclusion_criteria"]][["criteria"]] %||% list(),
    spec$exclusion_criteria %||% list(),
    enr$additional_inclusion %||% list(),
    enr$additional_exclusion %||% list()
  )
  out <- list()
  for (ec in entries) {
    # `[[` is exact. `$type` would partial-match another key.
    impl <- ec$implementation %||% list()
    if (!isTRUE(impl[["type"]] %in% .TTE_WASHOUT_TYPES)) {
      next
    }
    vars <- as.character(unlist(impl[["source_variable"]] %||% character()))
    window <- impl[["window_weeks"]]
    if (length(vars) == 0L || is.null(impl[["value"]])) {
      next
    }
    if (length(window) != 1L || is.na(window)) {
      next
    }
    impl$source_variable <- vars
    impl$source_variable_combined <- impl[["source_variable_combined"]] %||%
      paste(vars, collapse = "__")
    out[[length(out) + 1L]] <- list(
      name = ec$name %||% .tte_washout_col_name(impl),
      vars = vars,
      impl = impl
    )
  }
  return(out)
}


#' The weeks one washout makes ineligible
#'
#' It evaluates the eligibility expression [tteplan_apply_exclusions()] builds
#' for that washout, on the weekly rows of the skeleton. The guard therefore
#' measures the column the skeleton will hold, and not a second reading of the
#' rule.
#'
#' Row order inside a person decides what "prior" means, so the subset keeps
#' the skeleton's own row order. An eligibility value of `NA` is not a week the
#' washout makes ineligible.
#'
#' @param skeleton The skeleton data.table.
#' @param weekly Integer row numbers of the weekly rows.
#' @param washout One entry of `.tte_washouts()`.
#' @return A logical vector, one element per entry of `weekly`.
#' @noRd
.tte_washout_ineligible <- function(skeleton, weekly, washout) {
  impl <- washout$impl
  sv <- impl$source_variable_combined
  dtx <- skeleton[weekly, unique(c("id", washout$vars)), with = FALSE]
  dtx <- .grow_dt_alloc(dtx, 2L)
  if (length(washout$vars) > 1L) {
    dtx[, (sv) := Reduce(`|`, .SD), .SDcols = washout$vars]
  }
  sp <- .tte_washout_batch_spec(impl)
  sp$col_name <- ".tte_washout_eligible"
  dtx <- .tte_apply_eligibility_batch(dtx, list(sp), id_col = "id")
  return(dtx[[".tte_washout_eligible"]] %in% FALSE)
}


#' The prevalent weeks at one treatment level
#'
#' A prevalent week is a week at the intervention level that follows an earlier
#' week of the same person at that level. A person's first week at the level is
#' an initiation, and an incident-user design keeps it.
#'
#' @param skeleton The skeleton data.table.
#' @param weekly Integer row numbers of the weekly rows.
#' @param tx_var The treatment column.
#' @param tx_level The intervention level.
#' @return Integer positions into `weekly`.
#' @noRd
.tte_prevalent_positions <- function(skeleton, weekly, tx_var, tx_level) {
  at <- which(skeleton[[tx_var]][weekly] %in% tx_level)
  if (length(at) == 0L) {
    return(integer(0))
  }
  return(at[duplicated(skeleton[["id"]][weekly][at])])
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
  # band. Index the prevalent weeks at each level once.
  prevalent_at <- list()
  for (enr in spec$enrollments) {
    tx_impl <- enr$treatment$implementation
    tx_var <- tx_impl$variable
    tx_level <- tx_impl$intervention_value
    key <- paste0(tx_var, "\r", as.character(tx_level))
    if (!key %in% names(prevalent_at)) {
      prevalent_at[[key]] <- .tte_prevalent_positions(
        skeleton,
        weekly,
        tx_var,
        tx_level
      )
    }
    prevalent <- prevalent_at[[key]]

    washouts <- .tte_washouts(spec, enr)
    covered <- FALSE
    parts <- character()
    for (washout in washouts) {
      ineligible <- .tte_washout_ineligible(skeleton, weekly, washout)
      n_uncovered <- sum(!ineligible[prevalent])
      if (n_uncovered == 0L) {
        covered <- TRUE
        break
      }
      parts <- c(
        parts,
        paste0(
          "Washout '",
          washout$name,
          "' leaves ",
          n_uncovered,
          " uncovered."
        )
      )
    }
    if (covered) {
      next
    }
    if (length(parts) == 0L) {
      parts <- "No washout applies to this enrollment."
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
      "is intended, add a rule that covers that level -- either ",
      "a finite washout window (e.g. window: 104 weeks, as in Danaei ",
      "2013) or window: 'lifetime_before_baseline' for a never-user ",
      "design (implementation type 'no_prior_value' or 'only_prior_value'). ",
      "On skeleton batch ",
      skeleton_batch,
      ", ",
      length(prevalent),
      " ",
      if (length(prevalent) == 1L) "prevalent week" else "prevalent weeks",
      " at ",
      tx_var,
      " == \"",
      tx_level,
      "\". ",
      paste(parts, collapse = " "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

