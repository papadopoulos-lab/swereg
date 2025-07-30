#' Transform rowdep variable to rowind variable using first occurrence
#'
#' Creates a row-independent (rowind) variable by finding the first occurrence 
#' where a condition is TRUE and extracting the corresponding value. This is a 
#' common pattern in longitudinal registry data analysis for creating stable 
#' person-level characteristics.
#'
#' @param dt A data.table with longitudinal data
#' @param condition Character string representing the logical condition to evaluate
#' @param value_var Character string naming the column to extract values from
#' @param new_var Character string naming the new rowind variable to create
#'
#' @return The data.table is modified by reference (invisibly returned)
#'
#' @details
#' This function implements the common pattern of transforming time-varying 
#' (rowdep) variables into time-invariant (rowind) variables by capturing the 
#' value at the first occurrence of a condition.
#' 
#' The transformation follows these steps:
#' 1. Create temporary variable where condition is TRUE
#' 2. Use first_non_na() to find the first occurrence for each person
#' 3. Clean up temporary variables automatically
#' 
#' This is equivalent to the manual pattern:
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
#'               diags = list("example_diag" = "^F64"))
#' 
#' # Transform: Age at first example diagnosis
#' make_rowind_first_occurrence(skeleton,
#'                              condition = "diag_example_diag == TRUE",
#'                              value_var = "age", 
#'                              new_var = "rowind_age_first_example_diag")
#' }
#' @family data_transformation
#' @seealso \code{\link{first_non_na}} for the aggregation function used internally
#' @export
make_rowind_first_occurrence <- function(dt, condition, value_var, new_var) {
  # Declare variables for data.table non-standard evaluation
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