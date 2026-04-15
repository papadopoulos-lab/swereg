# =============================================================================
# Eligibility helpers (skeleton-level operations)
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
#' @family skeleton_eligibility
#' @seealso [skeleton_eligible_combine()] to combine multiple eligibility criteria
#'
#' @examples
#' \dontrun{
#' temp |>
#'   skeleton_eligible_isoyears(2007:2020)
#' }
#'
#' @export
skeleton_eligible_isoyears <- function(dt, isoyears, col_name = "eligible_isoyears") {
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
#' @family skeleton_eligibility
#' @seealso [skeleton_eligible_combine()] to combine multiple eligibility criteria
#'
#' @examples
#' \dontrun{
#' temp |>
#'   skeleton_eligible_age_range("rd_age_continuous", min_age = 50, max_age = 60)
#' }
#'
#' @export
skeleton_eligible_age_range <- function(dt, age_var, min_age, max_age,
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
#' (baseline) week.
#'
#' @param dt A data.table with the specified event variable.
#' @param event_var Character. Name of a logical column indicating event occurrence.
#' @param window Integer or Inf. Number of prior weeks to check. Default: 52.
#' @param col_name Character or NULL. Name of the eligibility column to create.
#'
#' @return The input data.table (invisibly), modified by reference.
#'
#' @family skeleton_eligibility
#' @seealso [any_events_prior_to()], [skeleton_eligible_combine()]
#'
#' @examples
#' \dontrun{
#' temp |>
#'   skeleton_eligible_no_events_in_window_excluding_wk0("icd10_f20_f29", window = Inf)
#' }
#'
#' @export
skeleton_eligible_no_events_in_window_excluding_wk0 <- function(dt, event_var,
                                                           window = 52,
                                                           col_name = NULL) {
  id <- NULL
  if (!event_var %in% names(dt)) {
    stop("event_var '", event_var, "' not found in dt")
  }
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
#' @param dt A data.table with the specified variable.
#' @param var Character. Name of the column to check.
#' @param value The specific value to look for.
#' @param window Integer or Inf. Default: Inf.
#' @param col_name Character or NULL.
#'
#' @return The input data.table (invisibly), modified by reference.
#'
#' @family skeleton_eligibility
#' @seealso [skeleton_eligible_no_events_in_window_excluding_wk0()],
#'   [skeleton_eligible_combine()]
#'
#' @export
skeleton_eligible_no_observation_in_window_excluding_wk0 <- function(dt, var, value,
                                                                window = Inf,
                                                                col_name = NULL) {
  if (!var %in% names(dt)) {
    stop("var '", var, "' not found in dt")
  }
  if (is.null(col_name)) {
    window_label <- if (is.infinite(window)) "ever" else paste0(window, "wk")
    col_name <- paste0("eligible_no_", var, "_", window_label)
  }
  temp_col <- paste0(".temp_obs_", var)
  dt[, (temp_col) := get(var) == value]
  skeleton_eligible_no_events_in_window_excluding_wk0(dt, temp_col,
                                                 window = window,
                                                 col_name = col_name)
  dt[, (temp_col) := NULL]
  invisible(dt)
}


#' Check eligibility based on no events ever (person-level, before and after baseline)
#'
#' @param dt A data.table with an `id` column and the specified event variable.
#' @param event_var Character. Name of a logical column.
#' @param col_name Character or NULL.
#'
#' @return The input data.table (invisibly), modified by reference.
#'
#' @family skeleton_eligibility
#' @seealso [skeleton_eligible_no_events_in_window_excluding_wk0()],
#'   [skeleton_eligible_combine()]
#'
#' @export
skeleton_eligible_no_events_lifetime_before_and_after_baseline <- function(
  dt, event_var, col_name = NULL
) {
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
#' @param dt A data.table with the specified eligibility columns.
#' @param eligible_cols Character vector of eligibility column names.
#' @param col_name Character. Default: "eligible".
#'
#' @return The input data.table (invisibly), modified by reference.
#'
#' @family skeleton_eligibility
#' @seealso [skeleton_eligible_isoyears()], [skeleton_eligible_age_range()],
#'   [skeleton_eligible_no_events_in_window_excluding_wk0()],
#'   [skeleton_eligible_no_observation_in_window_excluding_wk0()]
#'
#' @export
skeleton_eligible_combine <- function(dt, eligible_cols, col_name = "eligible") {
  missing_cols <- setdiff(eligible_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("The following eligibility columns are not in dt: ",
         paste(missing_cols, collapse = ", "))
  }
  expr <- paste0("`", eligible_cols, "` == TRUE", collapse = " & ")
  dt[, (col_name) := eval(parse(text = expr))]
  invisible(dt)
}
