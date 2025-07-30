#' Calculate minimum while treating infinite values as NA
#'
#' Computes the minimum value of a numeric vector, converting any infinite
#' values to NA. Useful for robust statistical calculations.
#'
#' @param x Numeric vector
#' @param na.rm Logical, whether to remove NA values before calculation (default: TRUE)
#' @return Minimum value with infinite values converted to NA
#' @examples
#' x <- c(1, 2, 4)
#' min_with_infinite_as_na(x)  # Returns 1
#' 
#' y <- c(1, 2, Inf, 4, -Inf)
#' min_with_infinite_as_na(y)  # Returns NA (because of infinite values)
#' @export
min_with_infinite_as_na <- function(x, na.rm=T){
  suppressWarnings(retval <- min(x, na.rm=na.rm))
  retval[is.infinite(retval)] <- NA
  return(retval)
}

#' Calculate maximum while treating infinite values as NA
#'
#' Computes the maximum value of a numeric vector, converting any infinite
#' values to NA. Useful for robust statistical calculations.
#'
#' @param x Numeric vector
#' @param na.rm Logical, whether to remove NA values before calculation (default: TRUE)
#' @return Maximum value with infinite values converted to NA
#' @examples
#' x <- c(1, 2, 4)
#' max_with_infinite_as_na(x)  # Returns 4
#' 
#' y <- c(1, 2, Inf, 4, -Inf)
#' max_with_infinite_as_na(y)  # Returns NA (because of infinite values)
#' @export
max_with_infinite_as_na <- function(x, na.rm=T){
  suppressWarnings(retval <- max(x, na.rm=na.rm))
  retval[is.infinite(retval)] <- NA
  return(retval)
}

#' Convert minimum to logical while treating infinite values as NA
#'
#' Computes the minimum value and converts it to logical, treating infinite
#' values as NA. Useful for aggregating boolean data.
#'
#' @param x Numeric vector
#' @param na.rm Logical, whether to remove NA values before calculation (default: TRUE)
#' @return Logical value (minimum converted to logical) with infinite values as NA
#' @seealso \code{\link{min_with_infinite_as_na}}
#' @export
as_logical_min_with_infinite_as_na <- function(x, na.rm=T){
  as.logical(min_with_infinite_as_na(x, na.rm=na.rm))
}

#' Convert maximum to logical while treating infinite values as NA
#'
#' Computes the maximum value and converts it to logical, treating infinite
#' values as NA. Useful for aggregating boolean data.
#'
#' @param x Numeric vector
#' @param na.rm Logical, whether to remove NA values before calculation (default: TRUE)
#' @return Logical value (maximum converted to logical) with infinite values as NA
#' @seealso \code{\link{max_with_infinite_as_na}}
#' @export
as_logical_max_with_infinite_as_na <- function(x, na.rm=T){
  as.logical(max_with_infinite_as_na(x, na.rm=na.rm))
}

#' Get first non-NA value from vector
#'
#' Returns the first non-missing value from a vector, useful for data cleaning
#' and summarization tasks.
#'
#' @param x Vector of any type
#' @return First non-NA value in the vector
#' @examples
#' x <- c(NA, NA, 3, 4, 5)
#' first_non_na(x)  # Returns 3
#' @export
first_non_na <- function(x){
  dplyr::first(na.omit(x))
}

#' Get last non-NA value from vector
#'
#' Returns the last non-missing value from a vector, useful for data cleaning
#' and summarization tasks.
#'
#' @param x Vector of any type
#' @return Last non-NA value in the vector
#' @examples
#' x <- c(1, 2, 3, NA, NA)
#' last_non_na(x)  # Returns 3
#' @export
last_non_na <- function(x){
  dplyr::last(na.omit(x))
}

#' Create row-independent variable from first occurrence of a condition
#'
#' Transforms a row-dependent (rowdep) variable into a row-independent (rowind) 
#' variable by finding the first occurrence where a condition is TRUE and 
#' spreading that value across all rows for each person. This is a common 
#' pattern in longitudinal registry data analysis.
#'
#' @param dt data.table containing the skeleton data
#' @param condition String expression defining when to capture the value 
#'   (e.g., "diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE")
#' @param value_var Character string naming the column to extract the value from
#' @param new_var Character string naming the new rowind variable to create
#' @return The input data.table (modified by reference) with the new rowind variable added
#' @examples
#' \dontrun{
#' # Load fake data
#' data("fake_demographics")
#' 
#' # Create skeleton 
#' skeleton <- create_skeleton(
#'   fake_demographics$lopnr[1:10], 
#'   "2020-01-01", "2020-12-31"
#' )
#' 
#' # Add some diagnosis data
#' add_diagnoses(skeleton, fake_inpatient_diagnoses, "lopnr", 
#'               diags = list("example_diag" = "^F64"))
#' 
#' # Create rowind variable for age at first diagnosis
#' make_rowind_first_occurrence(skeleton,
#'                              condition = "diag_example_diag == TRUE",
#'                              value_var = "age", 
#'                              new_var = "rowind_age_first_example_diag")
#' }
#' @family data_integration
#' @seealso \code{\link{first_non_na}} for the aggregation function used internally
#' @export
make_rowind_first_occurrence <- function(dt, condition, value_var, new_var) {
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

