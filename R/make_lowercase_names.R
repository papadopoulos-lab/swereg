#' Convert column names to lowercase and optionally clean date columns
#'
#' Generic function to convert all column names in an object to lowercase.
#' Optionally parses and cleans a specified date column using Swedish registry
#' date format handling. Methods are provided for data.table and default objects.
#'
#' @param x An object with named columns (data.frame, data.table, etc.)
#' @param date_column Character string specifying the name of a date column to clean.
#'   If provided, this column will be parsed using \code{\link{parse_swedish_date}}
#'   and the result saved in a new column called 'date'
#' @param ... Additional arguments passed to \code{\link{parse_swedish_date}}
#' @return The object with all column names converted to lowercase, and optionally
#'   a cleaned 'date' column if date_column was specified
#' @examples
#' # Load fake data
#' data("fake_demographics", package = "swereg")
#' 
#' # Basic usage - convert column names to lowercase
#' swereg::make_lowercase_names(fake_demographics)
#' 
#' # With date cleaning - clean birth dates
#' swereg::make_lowercase_names(fake_demographics, date_column = "fodelseman")
#' 
#' # Check that 'date' column was created
#' head(fake_demographics$date)
#' 
#' # For diagnosis data
#' data("fake_inpatient_diagnoses", package = "swereg")
#' swereg::make_lowercase_names(fake_inpatient_diagnoses, date_column = "indatum")
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_onetime}} for merging data,
#'   \code{\link{add_diagnoses}} for diagnosis data,
#'   \code{\link{parse_swedish_date}} for date parsing
#' @family data_preprocessing
#' @export
make_lowercase_names <- function(x, date_column = NULL, ...) {
  UseMethod("make_lowercase_names", x)
}

#' @rdname make_lowercase_names
#' @export
make_lowercase_names.default <- function(x, date_column = NULL, ...) {
  # Convert column names to lowercase
  names(x) <- tolower(names(x))
  
  # Process date column if specified
  if (!is.null(date_column)) {
    date_column_lower <- tolower(date_column)
    
    if (date_column_lower %in% names(x)) {
      x$date <- parse_swedish_date(x[[date_column_lower]], ...)
    } else {
      warning("date_column '", date_column, "' not found in data after lowercase conversion")
    }
  }
  
  x
}

#' @rdname make_lowercase_names
#' @export
make_lowercase_names.data.table <- function(x, date_column = NULL, ...) {
  # Convert column names to lowercase
  setnames(x, tolower(names(x)))
  
  # Process date column if specified
  if (!is.null(date_column)) {
    date_column_lower <- tolower(date_column)
    
    if (date_column_lower %in% names(x)) {
      x[, date := parse_swedish_date(get(date_column_lower), ...)]
    } else {
      warning("date_column '", date_column, "' not found in data after lowercase conversion")
    }
  }
  
  invisible(x)
}
