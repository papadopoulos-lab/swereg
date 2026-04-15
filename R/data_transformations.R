#' Transform a row-dependent variable to a row-independent variable using first occurrence
#'
#' Creates a row-independent (`ri_`) variable by finding the first occurrence
#' where a condition is TRUE and extracting the corresponding value. This is a
#' common pattern in longitudinal registry data analysis for creating stable
#' person-level characteristics from time-varying skeleton columns.
#'
#' @param dt A data.table with longitudinal data
#' @param condition Character string representing the logical condition to evaluate
#' @param value_var Character string naming the column to extract values from
#' @param new_var Character string naming the new `ri_*` variable to create
#'
#' @return The data.table is modified by reference (invisibly returned)
#'
#' @details
#' swereg distinguishes two variable shapes in longitudinal skeleton data:
#' \describe{
#'   \item{**row-dependent** (prefix `rd_`)}{Values that can change over time
#'     for a person. Examples: `rd_age_continuous`, `rd_education`,
#'     `rd_income_inflation_adjusted`.}
#'   \item{**row-independent** (prefix `ri_`)}{Values that are fixed
#'     person-level. Examples: `ri_birthcountry`, `ri_age_first_diagnosis`,
#'     `ri_isoyear_first_diagnosis`, `ri_register_tag`.}
#' }
#'
#' This function automates the common `rd_` -> `ri_` transformation of
#' capturing "the value at the first time something became true". The
#' transformation follows these steps:
#' 1. Create a temporary column where `condition` is TRUE
#' 2. Use `first_non_na()` to find the first occurrence for each person
#' 3. Clean up the temporary column automatically
#'
#' Equivalent to the manual pattern:
#' \code{dt[condition, temp := value_var]}
#' \code{dt[, new_var := first_non_na(temp), by = .(id)]}
#' \code{dt[, temp := NULL]}
#'
#' @examples
#' \dontrun{
#' # Create example skeleton with diagnosis data
#' skeleton <- create_skeleton(c(1,2,3), "2020-01-01", "2020-12-31")
#'
#' # Add some example diagnosis data
#' add_diagnoses(skeleton, diagnosis_data, "lopnr",
#'               codes = list("example_diag" = "^F64"))
#'
#' # Transform: age at first example diagnosis
#' make_rowind_first_occurrence(skeleton,
#'                              condition = "example_diag == TRUE",
#'                              value_var = "age",
#'                              new_var = "ri_age_first_example_diag")
#' }
#' @family data_transformation
#' @seealso \code{\link{first_non_na}} for the aggregation function used internally
#' @export
make_rowind_first_occurrence <- function(dt, condition, value_var, new_var) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  temp <- id <- NULL

  # Validate inputs
  if (!is.character(condition) || length(condition) != 1) {
    stop("condition must be a single character string")
  }
  if (!is.character(value_var) || length(value_var) != 1) {
    stop("value_var must be a single character string")
  }
  if (!is.character(new_var) || length(new_var) != 1) {
    stop("new_var must be a single character string")
  }
  if (!value_var %in% names(dt)) {
    stop("value_var '", value_var, "' not found in data.table")
  }

  # Create temporary variable where condition is TRUE
  eval_condition <- paste0("dt[", condition, ", temp := ", value_var, "]")
  eval(parse(text = eval_condition))

  # Aggregate using first_non_na and create new variable
  dt[, (new_var) := first_non_na(temp), by = .(id)]

  # Clean up temporary variable
  dt[, temp := NULL]

  # Return invisibly (data.table is modified by reference)
  invisible(dt)
}
