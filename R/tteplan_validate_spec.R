# =============================================================================
# tteplan_validate_spec
# =============================================================================

#' Validate spec variables against skeleton data
#'
#' Checks each column, treatment value and subgroup that a parsed spec names
#' against one skeleton batch. It collects every issue before it reports, then
#' warns on the soft issues and stops on the hard issues.
#'
#' @section The checks:
#'
#' A hard issue is an error, and a soft issue is a warning. Each issue below
#' is an error, except a declared category absent from the batch.
#'
#' - Global rules: every `source_variable` column of each
#'   `inclusion_criteria$criteria` and `exclusion_criteria` rule MUST be in the
#'   skeleton.
#' - `additional_exclusion` rules: every `source_variable` column MUST be in
#'   the skeleton.
#' - `additional_inclusion` rules: a rule of type `has_event`,
#'   `no_prior_value` or `only_prior_value` MUST have every `source_variable`
#'   column in the skeleton. A rule of type `isoyear_range` names no column,
#'   so the skeleton MUST have the column `isoyear`. Another rule that names
#'   `implementation$variable`, such as `age_range`, MUST have that column in
#'   the skeleton.
#' - Outcomes: every `variable` column MUST be in the skeleton.
#' - Confounders that are not computed: the `variable` MUST be a skeleton
#'   column. When the confounder declares `categories`, a non-missing data
#'   value outside `categories` is an error. A declared category absent from
#'   this batch is a warning, because a small batch can lack it.
#' - Computed confounders (`computed: true`): every `source_variable` column
#'   MUST be in the skeleton. The function does not check the `variable`,
#'   because [tteplan_apply_derived_confounders()] creates that column in s1.
#' - Enrollments: the treatment `variable` MUST be a skeleton column. That
#'   column MUST hold the `intervention_value` and the `comparator_value`.
#' - Observation columns: when an enrollment's `observed_var` names a column,
#'   that column MUST be in the skeleton, and it MUST be logical.
#'   [tteplan_read_spec()] cannot run this check, because it reads no data.
#'   The `row_presence` sentinel names no column, so there is nothing to
#'   check.
#' - Subgroups: the `variable` MUST be a skeleton column or the `variable` of
#'   a computed confounder. It MUST also be the `variable` of a confounder.
#'   The marginal weights are valid within a stratum only when the PS and
#'   IPCW models include the subgroup.
#'
#' @section How it fails:
#'
#' The function stops at once when `skeleton` is not a data.table. Otherwise
#' it runs every check above, then it reports in this order:
#'
#' 1. When there are soft issues, it warns with a numbered list, under
#'    `Spec validation: N warning(s)`.
#' 2. When there are hard issues, it stops with a numbered list, under
#'    `Spec validation failed: N error(s)`. So one call can warn and then
#'    stop.
#' 3. It runs the prevalent-user check below. That check can warn, and it can
#'    stop.
#' 4. It shows the message
#'    `Spec validation passed: N entries checked against M columns`.
#'
#' @section The prevalent-user check:
#'
#' An enrollment gets its incident-user design from a washout rule, of type
#' `no_prior_value` or `only_prior_value`. The check warns when no washout
#' covers the enrollment's intervention level. All four rule blocks are
#' searched: `inclusion_criteria$criteria`, `exclusion_criteria`, and the
#' enrollment's `additional_inclusion` and `additional_exclusion`. A washout
#' counts only when it names a `source_variable`, a `value` and one
#' `window_weeks`.
#'
#' Coverage is measured on the weekly rows of `skeleton`. A weekly row has
#' `is_isoyear` equal to `FALSE`, and a skeleton without `is_isoyear` has only
#' weekly rows. A prevalent week is a week at the intervention level that
#' follows an earlier week of the same person at that level. A washout covers
#' the enrollment when it makes every prevalent week ineligible. The check
#' evaluates the eligibility expression the compiler builds, so it measures
#' the column the skeleton will hold. A first initiation stays eligible and is
#' not an uncovered week. The enrollment passes when at least one washout
#' covers it.
#'
#' The washout column and the treatment column often differ. A specification
#' can wash out a whole drug class, then compare one sub-type of that class
#' against no treatment. A test on the two column names cannot answer that,
#' and it cannot see a washout that names the right column at the wrong
#' level.
#'
#' The check stops in three cases:
#'
#' - The skeleton has no `id` column.
#' - The skeleton has no `isoyearweek` column. The stop names the column when
#'   a weekly row is at the intervention level. Otherwise a washout that
#'   applies stops in data.table, with the message
#'   `column not found: [isoyearweek]`.
#' - The weekly rows of one person at an intervention level run backwards in
#'   `isoyearweek`. Sort the skeleton by `id` and `isoyearweek` before you
#'   validate it.
#'
#' Set `options(swereg.warn_prevalent_user = FALSE)` to skip the check, with
#' its warning and its stops. Use it for a discontinuation or switching study,
#' which enrols prevalent users by design.
#'
#' @param spec Parsed study specification from [tteplan_read_spec()].
#' @param skeleton A data.table skeleton (person-week panel) to validate
#'   against.
#' @param skeleton_batch Batch number of `skeleton`. The prevalent-user
#'   warning reports it, so a reader knows which batch the check measured.
#' @return `invisible(TRUE)` when no check stops. A warning does not change
#'   the return value.
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
    # `isoyear_range` names none and reads `isoyear`.
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
        } else if (identical(.tte_entry_type(ae), "isoyear_range")) {
          # The entry names no column. skeleton_eligible_isoyears() reads
          # `isoyear`.
          n_checked <- n_checked + 1L
          if (!"isoyear" %in% skel_cols) {
            errors <- c(
              errors,
              paste0(
                "enrollments '",
                enr$name %||% enr$id,
                "': additional_inclusion isoyear_range needs the column ",
                "'isoyear', which is not in the skeleton"
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

  # --- Subgroups: each must be a confounder, and its column must exist in the
  # skeleton or be derived by s1. The within-stratum validity of the marginal
  # weights requires the subgroup to be in the PS / IPCW models (i.e. among
  # the confounders). A computed confounder has no skeleton column: s1
  # derives its `rd_no_<source>_<window>` column, so the subgroup check
  # accepts that derived name.
  if (!is.null(spec$subgroups)) {
    confounder_vars <- vapply(
      spec$confounders %||% list(),
      function(cf) cf$implementation$variable %||% NA_character_,
      character(1)
    )
    computed_vars <- vapply(
      spec$confounders %||% list(),
      function(cf) {
        if (!isTRUE(cf$implementation$computed)) {
          return(NA_character_)
        }
        return(cf$implementation$variable %||% NA_character_)
      },
      character(1)
    )
    computed_vars <- computed_vars[!is.na(computed_vars)]
    for (i in seq_along(spec$subgroups)) {
      sv <- spec$subgroups[[i]]$implementation$variable
      if (!sv %in% skel_cols && !sv %in% computed_vars) {
        errors <- c(
          errors,
          paste0(
            "subgroups[",
            i,
            "] variable '",
            sv,
            "' not found in skeleton columns, and no computed confounder ",
            "derives it"
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
  dtx <- skeleton[
    weekly,
    unique(c("id", "isoyearweek", washout$vars)),
    with = FALSE
  ]
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
#' `duplicated()` marks every occurrence after the first ROW, and the rule
#' needs every week after the earliest WEEK. The two agree only while the rows
#' of one person at the level run forward in time. So this stops on a person
#' whose rows run backwards, rather than report an initiation as a prevalent
#' week.
#'
#' The order test groups by `id` and compares each person's own consecutive
#' rows. One person's rows can be separated by another person's, and a test on
#' neighbouring rows of the whole vector cannot see a backwards run there.
#'
#' @param skeleton The skeleton data.table.
#' @param weekly Integer row numbers of the weekly rows.
#' @param tx_var The treatment column.
#' @param tx_level The intervention level.
#' @return Integer positions into `weekly`.
#' @noRd
.tte_prevalent_positions <- function(skeleton, weekly, tx_var, tx_level) {
  pu_id <- pu_week <- pu_prev <- NULL # nolint
  at <- which(skeleton[[tx_var]][weekly] %in% tx_level)
  if (length(at) == 0L) {
    return(integer(0))
  }
  if (!"isoyearweek" %in% names(skeleton)) {
    stop(
      "tteplan_validate_spec(): the skeleton has no `isoyearweek` column; ",
      "the prevalent-user check reads it to verify that the weeks at the ",
      "intervention level run forward in time.",
      call. = FALSE
    )
  }
  ids <- skeleton[["id"]][weekly][at]
  order_check <- data.table::data.table(
    pu_id = ids,
    pu_week = skeleton[["isoyearweek"]][weekly][at]
  )
  # `by = pu_id` shifts inside each person's own rows, so another person's row
  # between two of hers changes nothing.
  order_check[
    !is.na(pu_week),
    pu_prev := data.table::shift(pu_week),
    by = pu_id
  ]
  back <- order_check[!is.na(pu_prev) & pu_week < pu_prev]
  if (nrow(back) > 0L) {
    stop(
      "tteplan_validate_spec(): person '",
      back$pu_id[[1L]],
      "' has rows at ",
      tx_var,
      " == \"",
      as.character(tx_level)[[1L]],
      "\" that run backwards in time (",
      back$pu_prev[[1L]],
      " then ",
      back$pu_week[[1L]],
      "). The prevalent-user check keeps the first such ROW of each person, ",
      "so those rows must run forward in time. Sort the skeleton by `id` and ",
      "`isoyearweek` before you validate it.",
      call. = FALSE
    )
  }
  return(at[duplicated(ids)])
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
  # The check groups by `id`, and the eligibility compiler reads that column
  # by name. Without this stop, a skeleton that lacks it dies inside the
  # compiler with data.table's "column not found: [id]".
  if (!"id" %in% names(skeleton)) {
    stop(
      "tteplan_validate_spec(): the skeleton has no `id` column; the ",
      "prevalent-user check groups by it.",
      call. = FALSE
    )
  }
  # A weekly row is one person-week. An annual row summarises a year, so it
  # is not a week the person could initiate in.
  weekly <- if ("is_isoyear" %in% names(skeleton)) {
    which(skeleton[["is_isoyear"]] %in% FALSE)
  } else {
    seq_len(nrow(skeleton))
  }
  # Enrollments repeat the same treatment column and level, once per
  # age band. Index the prevalent weeks at each level once.
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
