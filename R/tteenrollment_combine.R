# =============================================================================
# S3 method: summary.TTEEnrollment
# =============================================================================

#' @export
summary.TTEEnrollment <- function(object, ..., pretty = FALSE) {
  object$summary(pretty = pretty)
}


# =============================================================================
# Standalone helpers (operate on lists of trials/results)
# =============================================================================

#' Combine multiple enrollment objects
#'
#' Combines multiple [TTEEnrollment] objects by row-binding their data. Used for
#' batched processing where data is too large to fit in memory at once.
#'
#' @param trials A list of [TTEEnrollment] objects to combine.
#'
#' @return A new [TTEEnrollment] object with combined data.
#'
#' @details
#' All trials must have the same design and data_level. The combined trial inherits:
#' - The design and data_level from the first trial
#' - The intersection of steps_completed from all trials
#' - The union of weight_cols from all trials
#'
#' @examples
#' \dontrun{
#' trials <- lapply(files, function(f) {
#'   TTEEnrollment$new(load_data(f), design, ratio = 2)
#' })
#' combined <- tteenrollment_rbind(trials)
#' combined$s2_ipw()
#' }
#'
#' @family tte_methods
#' @export
tteenrollment_rbind <- function(trials) {
  if (!is.list(trials) || length(trials) == 0) {
    stop("trials must be a non-empty list")
  }

  for (i in seq_along(trials)) {
    if (!inherits(trials[[i]], "TTEEnrollment")) {
      stop("All elements must be TTEEnrollment objects")
    }
  }

  data_level <- trials[[1]]$data_level
  for (i in seq_along(trials)[-1]) {
    if (trials[[i]]$data_level != data_level) {
      stop(
        "All trials must have the same data_level.\n",
        "First trial: '",
        data_level,
        "', trial ",
        i,
        ": '",
        trials[[i]]$data_level,
        "'"
      )
    }
  }

  design <- trials[[1]]$design

  combined_data <- data.table::rbindlist(
    lapply(trials, function(t) t$data),
    use.names = TRUE,
    fill = TRUE
  )

  steps <- trials[[1]]$steps_completed
  for (t in trials[-1]) {
    steps <- intersect(steps, t$steps_completed)
  }

  weight_cols <- unique(unlist(lapply(trials, function(t) t$weight_cols)))

  # Preserve the estimand tag (set by s4_prepare_for_analysis). Without this,
  # a combined ITT object would lose its tag and the irr() guard would wrongly
  # block its valid IPW-only weight. Combining different estimands is an error;
  # NULL (unprepared, the usual pre-s4 rbind case) is fine.
  estimands <- unique(Filter(
    Negate(is.null),
    lapply(trials, function(t) t$estimand)
  ))
  if (length(estimands) > 1L) {
    stop(
      "Cannot rbind TTEEnrollment objects with different estimands: ",
      paste(unlist(estimands), collapse = ", ")
    )
  }

  result <- TTEEnrollment$new(
    data = combined_data,
    design = design,
    data_level = data_level,
    steps_completed = steps,
    weight_cols = weight_cols
  )
  if (length(estimands) == 1L) {
    result$estimand <- estimands[[1]]
  }
  result
}


#' Combine and format multiple rates outputs into a publication-ready table
#'
#' @param results Named list of per-ETT result lists.
#' @param slot Character scalar: name of the slot with `$rates()` output.
#' @param descriptions Optional named character vector mapping ett_id to descriptions.
#'
#' @return A data.table in wide format.
#'
#' @family tte_methods
#' @export
tteenrollment_rates_combine <- function(results, slot, descriptions = NULL) {
  ett_id <- arm <- events_weighted <- py_weighted <- rate_per_100000py <- description <- NULL
  rates_list <- lapply(results, `[[`, slot)

  first_non_null <- Find(Negate(is.null), rates_list)
  treatment_col <- attr(first_non_null, "treatment_var")
  if (is.null(treatment_col)) {
    stop(
      "results$*$",
      slot,
      " must be $rates() outputs (missing 'treatment_var' attribute)"
    )
  }

  dt <- rbindlist(rates_list, idcol = "ett_id")
  dt[, arm := fifelse(get(treatment_col), "Intervention", "Comparator")]
  dt[, (treatment_col) := NULL]

  dt[, `:=`(
    events_weighted = format(round(events_weighted, 1), nsmall = 1),
    py_weighted = format(round(py_weighted, 0), big.mark = ","),
    rate_per_100000py = format(round(rate_per_100000py, 1), nsmall = 1)
  )]

  if (!is.null(descriptions)) {
    dt[, description := descriptions[ett_id]]
    cast_formula <- stats::as.formula("ett_id + description ~ arm")
  } else {
    cast_formula <- stats::as.formula("ett_id ~ arm")
  }

  dcast(
    dt,
    cast_formula,
    value.var = c("events_weighted", "py_weighted", "rate_per_100000py")
  )
}


#' Combine and format multiple irr outputs into a publication-ready table
#'
#' @param results Named list of per-ETT result lists.
#' @param slot Character scalar: name of the slot with `$irr()` output.
#' @param descriptions Optional named character vector mapping ett_id to descriptions.
#'
#' @return A data.table with formatted IRR estimates.
#'
#' @family tte_methods
#' @export
tteenrollment_irr_combine <- function(results, slot, descriptions = NULL) {
  ett_id <- warn <- IRR <- IRR_lower <- IRR_upper <- IRR_pvalue <- description <- . <- NULL
  irr_list <- lapply(results, `[[`, slot)
  dt <- rbindlist(irr_list, idcol = "ett_id")

  warn_ids <- dt[warn == TRUE, ett_id]
  if (length(warn_ids) > 0L) {
    message("Convergence warnings in: ", paste(warn_ids, collapse = ", "))
  }

  result <- dt[, .(
    ett_id,
    IRR = format(round(IRR, 2), nsmall = 2),
    `95% CI` = paste0(
      format(round(IRR_lower, 2), nsmall = 2),
      " to ",
      format(round(IRR_upper, 2), nsmall = 2)
    ),
    `p-value` = format.pval(IRR_pvalue, digits = 3)
  )]

  # Flag convergence warnings
  if (any(dt$warn)) {
    warn_flags <- dt[, fifelse(warn, "*", "")]
    result[, IRR := paste0(IRR, warn_flags)]
  }

  if (!is.null(descriptions)) {
    result[, description := descriptions[ett_id]]
    setcolorder(result, c("ett_id", "description"))
  }

  result
}


#' Combine rates + IRR outputs into a single wide publication-ready table
#'
#' Calls [tteenrollment_rates_combine()] and [tteenrollment_irr_combine()]
#' with shared `descriptions`, then left-joins on `ett_id` so that each row
#' carries per-arm event counts, person-years, rates, and the incidence rate
#' ratio (with 95% CI and p-value) in one place.
#'
#' The returned data.table still uses the generic `_Intervention`/`_Comparator`
#' column suffixes from [tteenrollment_rates_combine()]. The workbook writer
#' in `.write_combined_rates_irr()` applies `.rename_treatment_columns()`
#' afterwards when the featured ETTs share a single enrollment.
#'
#' @param results Named list of per-ETT result lists.
#' @param rates_slot Character scalar, name of the slot with `$rates()` output
#'   (e.g. `"rates_pp_trunc"`).
#' @param irr_slot Character scalar, name of the slot with `$irr()` output
#'   (e.g. `"irr_pp_trunc"`).
#' @param descriptions Optional named character vector mapping `ett_id` to
#'   descriptions.
#'
#' @return A wide `data.table` with one row per ETT.
#'
#' @family tte_methods
#' @export
tteenrollment_combined_combine <- function(
  results,
  rates_slot,
  irr_slot,
  descriptions = NULL
) {
  ett_id <- `95% CI` <- `p-value` <- IRR <- NULL
  rates_dt <- tteenrollment_rates_combine(results, rates_slot, descriptions)
  irr_dt <- tteenrollment_irr_combine(results, irr_slot, descriptions)
  irr_slim <- irr_dt[, .(ett_id, IRR, `95% CI`, `p-value`)]
  merge(rates_dt, irr_slim, by = "ett_id", all.x = TRUE, sort = FALSE)
}


#' Impute missing confounders by sampling from observed values
#'
#' Thin standalone wrapper that delegates to `trial$s1_impute_confounders()`.
#' Exists as a standalone function so it can be used as the default
#' `impute_fn` callback in `$s1_generate_enrollments_and_ipw()`.
#'
#' @param trial A [TTEEnrollment] object.
#' @param confounder_vars Character vector of confounder column names to impute.
#' @param seed Integer seed for reproducibility (default: 4L).
#' @return The modified [TTEEnrollment] object (invisibly).
#' @export
tteenrollment_impute_confounders <- function(
  trial,
  confounder_vars,
  seed = 4L
) {
  trial$s1_impute_confounders(confounder_vars, seed)
  invisible(trial)
}
