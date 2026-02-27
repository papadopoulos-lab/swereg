# =============================================================================
# Trial Eligibility Helper Functions
# =============================================================================
# Small, composable functions for defining trial eligibility criteria.
# All functions add eligibility columns to data.tables (modifying by reference).
# =============================================================================

#' Check eligibility based on ISO years
#'
#' Adds a logical column indicating whether each row falls within the specified
#' ISO years. Typically used to restrict analysis to a calendar period.
#'
#' @param dt A data.table with an `isoyear` column.
#' @param isoyears Integer vector of eligible ISO years (e.g., 2007:2020).
#' @param col_name Character. Name of the eligibility column to create.
#'   Default: "eligible_isoyears".
#'
#' @return The input data.table (invisibly), modified by reference with the new
#'   eligibility column.
#'
#' @family tte_eligibility
#' @seealso [tte_eligible_combine()] to combine multiple eligibility criteria
#'
#' @examples
#' \dontrun{
#' temp |>
#'   tte_eligible_isoyears(2007:2020)
#' }
#'
#' @export
tte_eligible_isoyears <- function(dt, isoyears, col_name = "eligible_isoyears") {
  # Declare variable for data.table non-standard evaluation
  isoyear <- NULL

  if (!"isoyear" %in% names(dt)) {
    stop("dt must have an 'isoyear' column")
  }
  dt[, (col_name) := isoyear %in% isoyears]
  invisible(dt)
}


#' Check eligibility based on age range
#'
#' Adds a logical column indicating whether each row's age falls within the
#' specified range (inclusive on both ends).
#'
#' @param dt A data.table with the specified age variable.
#' @param age_var Character. Name of the column containing continuous age.
#' @param min_age Numeric. Minimum eligible age (inclusive).
#' @param max_age Numeric. Maximum eligible age (inclusive).
#' @param col_name Character. Name of the eligibility column to create.
#'   Default: "eligible_age".
#'
#' @return The input data.table (invisibly), modified by reference with the new
#'   eligibility column.
#'
#' @family tte_eligibility
#' @seealso [tte_eligible_combine()] to combine multiple eligibility criteria
#'
#' @examples
#' \dontrun{
#' temp |>
#'   tte_eligible_age_range("rd_age_continuous", min_age = 50, max_age = 60)
#' }
#'
#' @export
tte_eligible_age_range <- function(dt, age_var, min_age, max_age,
                                   col_name = "eligible_age") {
  if (!age_var %in% names(dt)) {
    stop("age_var '", age_var, "' not found in dt")
  }
  dt[, (col_name) := get(age_var) >= min_age & get(age_var) <= max_age]
  invisible(dt)
}


#' Check eligibility based on no events in prior window (excluding baseline week)
#'
#' Adds a logical column indicating whether there were NO TRUE values in the
#' specified event variable within the prior window, EXCLUDING the current
#' (baseline) week. This is the correct semantic for trial eligibility: we want
#' to know if someone had the event BEFORE they could potentially enter the trial.
#'
#' Uses [any_events_prior_to()] internally with `.after = -1L`, which always
#' excludes the current row.
#'
#' @param dt A data.table with the specified event variable. Must be grouped
#'   by person ID for proper calculation.
#' @param event_var Character. Name of a logical column indicating event
#'   occurrence.
#' @param window Integer or Inf. Number of prior weeks to check (excluding
#'   current week). Use `Inf` to check entire lifetime history.
#'   Default: 52 (one year).
#' @param col_name Character or NULL. Name of the eligibility column to create.
#'   If NULL, auto-generates as "eligible_no_<event_var>_<window>wk" or
#'   "eligible_no_<event_var>_ever" for Inf window.
#'
#' @return The input data.table (invisibly), modified by reference with the new
#'   eligibility column.
#'
#' @details
#' **Why exclude baseline week?**
#'
#' In target trial emulation, eligibility is assessed at the moment of potential
#' trial entry. An event occurring IN the baseline week cannot be used to
#' determine eligibility because it happens simultaneously with (or after)
#' the eligibility assessment. The semantic is: "Did this person have the
#' event BEFORE this week?"
#'
#' Using `cumsum(x) == 0` is INCORRECT because it INCLUDES the current week.
#' This function correctly excludes the current week.
#'
#' @family tte_eligibility
#' @seealso [any_events_prior_to()] for the underlying function,
#'   [tte_eligible_combine()] to combine multiple eligibility criteria
#'
#' @examples
#' \dontrun{
#' # No psychosis diagnosis ever (excluding baseline week)
#' temp |>
#'   tte_eligible_no_events_in_window_excluding_wk0("icd10_f20_f29", window = Inf)
#'
#' # No antipsychotic prescriptions in prior year (excluding baseline week)
#' temp |>
#'   tte_eligible_no_events_in_window_excluding_wk0("rx_n05a", window = 52)
#' }
#'
#' @export
tte_eligible_no_events_in_window_excluding_wk0 <- function(dt, event_var,
                                                           window = 52,
                                                           col_name = NULL) {
  # Declare variable for data.table non-standard evaluation
  id <- NULL

  if (!event_var %in% names(dt)) {
    stop("event_var '", event_var, "' not found in dt")
  }

  # Convert Inf to large number for any_events_prior_to
  window_weeks <- if (is.infinite(window)) 99999L else as.integer(window)

  if (is.null(col_name)) {
    window_label <- if (is.infinite(window)) "ever" else paste0(window, "wk")
    col_name <- paste0("eligible_no_", event_var, "_", window_label)
  }

  dt[, (col_name) := !any_events_prior_to(get(event_var),
                                          window_excluding_wk0 = window_weeks),
     by = list(id)]
  invisible(dt)
}


#' Check eligibility based on no observation of a specific value (excluding baseline week)
#'
#' Adds a logical column indicating whether a specific value was NOT observed
#' in the specified variable within the prior window, EXCLUDING the current
#' (baseline) week. Useful for categorical variables where you want to exclude
#' people who had a specific value (e.g., "systemic_mht") in their history.
#'
#' This is a convenience wrapper around [tte_eligible_no_events_in_window_excluding_wk0()]
#' that first creates a temporary logical column for whether the value was observed.
#'
#' @param dt A data.table with the specified variable. Must be grouped by
#'   person ID for proper calculation.
#' @param var Character. Name of the column to check.
#' @param value The specific value to look for (will be compared with `==`).
#' @param window Integer or Inf. Number of prior weeks to check (excluding
#'   current week). Use `Inf` to check entire lifetime history. Default: Inf.
#' @param col_name Character or NULL. Name of the eligibility column to create.
#'   If NULL, auto-generates as "eligible_no_<var>_<window>wk" or
#'   "eligible_no_<var>_ever" for Inf window.
#'
#' @return The input data.table (invisibly), modified by reference with the new
#'   eligibility column.
#'
#' @family tte_eligibility
#' @seealso [tte_eligible_no_events_in_window_excluding_wk0()] for the underlying logic,
#'   [tte_eligible_combine()] to combine multiple eligibility criteria
#'
#' @examples
#' \dontrun{
#' # No prior systemic MHT use (excluding baseline week)
#' temp |>
#'   tte_eligible_no_observation_in_window_excluding_wk0(
#'     "rd_approach1_single", "systemic_mht", window = Inf
#'   )
#' }
#'
#' @export
tte_eligible_no_observation_in_window_excluding_wk0 <- function(dt, var, value,
                                                                window = Inf,
                                                                col_name = NULL) {
  if (!var %in% names(dt)) {
    stop("var '", var, "' not found in dt")
  }

  if (is.null(col_name)) {
    window_label <- if (is.infinite(window)) "ever" else paste0(window, "wk")
    col_name <- paste0("eligible_no_", var, "_", window_label)
  }

  # Create temp logical column, run no_events check, clean up
  temp_col <- paste0(".temp_obs_", var)
  dt[, (temp_col) := get(var) == value]
  tte_eligible_no_events_in_window_excluding_wk0(dt, temp_col,
                                                 window = window,
                                                 col_name = col_name)
  dt[, (temp_col) := NULL]
  invisible(dt)
}


#' Check eligibility based on no events ever (person-level, before and after baseline)
#'
#' Adds a logical column indicating whether the person has NO TRUE values in the
#' specified event variable at ANY time point. Unlike
#' [tte_eligible_no_events_in_window_excluding_wk0()] which checks a rolling
#' window relative to each week, this is a person-level flag: if the event is
#' TRUE at any row for a person, ALL rows for that person are marked ineligible.
#'
#' Use case: excluding people with a condition diagnosed at any time (e.g.,
#' gender dysphoria F64) where future diagnoses also indicate the person
#' should never have been eligible.
#'
#' @param dt A data.table with an `id` column and the specified event variable.
#' @param event_var Character. Name of a logical column indicating event
#'   occurrence.
#' @param col_name Character or NULL. Name of the eligibility column to create.
#'   If NULL, auto-generates as
#'   "eligible_no_<event_var>_lifetime_before_and_after_baseline".
#'
#' @return The input data.table (invisibly), modified by reference with the new
#'   eligibility column.
#'
#' @family tte_eligibility
#' @seealso [tte_eligible_no_events_in_window_excluding_wk0()] for
#'   time-relative eligibility, [tte_eligible_combine()] to combine criteria
#'
#' @examples
#' \dontrun{
#' # Exclude anyone who EVER has gender dysphoria diagnosis
#' temp |>
#'   tte_eligible_no_events_lifetime_before_and_after_baseline("icd10_f64")
#' }
#'
#' @export
tte_eligible_no_events_lifetime_before_and_after_baseline <- function(
  dt, event_var, col_name = NULL
) {
  # Declare variable for data.table non-standard evaluation
  id <- NULL

  if (!event_var %in% names(dt)) {
    stop("event_var '", event_var, "' not found in dt")
  }

  if (is.null(col_name)) {
    col_name <- paste0(
      "eligible_no_", event_var, "_lifetime_before_and_after_baseline"
    )
  }

  dt[, (col_name) := !any(get(event_var), na.rm = TRUE), by = list(id)]
  invisible(dt)
}


#' Combine multiple eligibility criteria
#'
#' Combines multiple eligibility columns into a single logical column using
#' AND logic (all criteria must be TRUE for overall eligibility).
#'
#' @param dt A data.table with the specified eligibility columns.
#' @param eligible_cols Character vector. Names of the eligibility columns to
#'   combine.
#' @param col_name Character. Name of the combined eligibility column to create.
#'   Default: "eligible".
#'
#' @return The input data.table (invisibly), modified by reference with the new
#'   combined eligibility column.
#'
#' @family tte_eligibility
#' @seealso [tte_eligible_isoyears()], [tte_eligible_age_range()],
#'   [tte_eligible_no_events_in_window_excluding_wk0()],
#'   [tte_eligible_no_observation_in_window_excluding_wk0()]
#'
#' @examples
#' \dontrun{
#' # First define individual criteria
#' temp |>
#'   tte_eligible_isoyears(2007:2020) |>
#'   tte_eligible_age_range("rd_age_continuous", min_age = 50, max_age = 60) |>
#'   tte_eligible_no_events_in_window_excluding_wk0("icd10_f20_f29", window = Inf) |>
#'   tte_eligible_no_observation_in_window_excluding_wk0(
#'     "rd_approach1_single", "systemic_mht", window = Inf
#'   ) |>
#'   tte_eligible_no_events_in_window_excluding_wk0("rx_n05a", window = 52) |>
#'   tte_eligible_no_events_in_window_excluding_wk0("rx_g03aa", window = 52) |>
#'   tte_eligible_combine(c(
#'     "eligible_isoyears",
#'     "eligible_age",
#'     "eligible_no_icd10_f20_f29_ever",
#'     "eligible_no_rd_approach1_single_ever",
#'     "eligible_no_rx_n05a_52wk",
#'     "eligible_no_rx_g03aa_52wk"
#'   ))
#' }
#'
#' @export
tte_eligible_combine <- function(dt, eligible_cols, col_name = "eligible") {
  # Check all columns exist
  missing_cols <- setdiff(eligible_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("The following eligibility columns are not in dt: ",
         paste(missing_cols, collapse = ", "))
  }

  # Build expression: col1 == TRUE & col2 == TRUE & ...
  expr <- paste0("`", eligible_cols, "` == TRUE", collapse = " & ")
  dt[, (col_name) := eval(parse(text = expr))]
  invisible(dt)
}
