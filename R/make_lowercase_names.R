#' Convert column names to lowercase and optionally clean date columns
#'
#' Generic function to convert all column names in an object to lowercase.
#' Optionally parses and cleans specified date columns using Swedish registry
#' date format handling. Methods are provided for data.table and default objects.
#'
#' The function automatically detects common Swedish registry date columns
#' (indatum, utdatum, edatum, dodsdat, fodelseman) and provides helpful messages
#' suggesting their inclusion in the date_columns parameter.
#'
#' @param x An object with named columns (data.frame, data.table, etc.)
#' @param date_columns Character vector specifying the names of date columns to clean.
#'   Should use lowercase names since column names are converted to lowercase first.
#'   If uppercase names are provided, a warning is issued and lowercase versions are used.
#'   If provided, these columns will be parsed using Swedish date format handling
#'   and converted to Date class in place (keeping original column names).
#'   If NULL, the function will suggest commonly found Swedish registry date columns.
#' @param ... Additional arguments for date parsing (default_month_day, default_day, na_strings)
#' @return The object with all column names converted to lowercase, and optionally
#'   cleaned date columns if date_columns was specified (converted to Date class in place)
#' @examples
#' # Load fake data
#' data("fake_demographics", package = "swereg")
#'
#' # Basic usage - convert column names to lowercase
#' # This will show a message suggesting to include 'fodelseman' in date_columns
#' swereg::make_lowercase_names(fake_demographics)
#'
#' # With date cleaning - clean birth dates (use lowercase column names)
#' swereg::make_lowercase_names(fake_demographics, date_columns = "fodelseman")
#'
#' # Check that fodelseman column was converted to Date class
#' head(fake_demographics$fodelseman)
#'
#' # For diagnosis data with multiple date columns (use lowercase column names)
#' data("fake_diagnoses", package = "swereg")
#' swereg::make_lowercase_names(fake_diagnoses, date_columns = c("indatum", "utdatum"))
#'
#' # The function suggests missing date columns
#' swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")
#' # Message: "Found additional date columns not in date_columns: utdatum"
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_onetime}} for merging data,
#'   \code{\link{add_diagnoses}} for diagnosis data,
#'   \code{\link{parse_swedish_date}} for date parsing details
#' @family data_preprocessing
#' @export
make_lowercase_names <- function(x, date_columns = NULL, ...) {
  UseMethod("make_lowercase_names", x)
}

#' @rdname make_lowercase_names
#' @export
make_lowercase_names.default <- function(x, date_columns = NULL, ...) {
  # Convert column names to lowercase
  names(x) <- tolower(names(x))

  # Check for common Swedish registry date columns and provide suggestions
  common_date_cols <- c("indatum", "utdatum", "edatum", "dodsdat", "fodelseman")
  found_date_cols <- intersect(names(x), common_date_cols)

  if (length(found_date_cols) > 0) {
    if (is.null(date_columns)) {
      # No date_columns specified but date columns found
      message("Found potential date columns: ", paste(found_date_cols, collapse = ", "),
              ". Consider adding them to date_columns parameter for automatic date parsing.")
    } else {
      # Check if any common date columns are missing from specified date_columns
      date_columns_lower <- tolower(date_columns)
      missing_date_cols <- setdiff(found_date_cols, date_columns_lower)
      if (length(missing_date_cols) > 0) {
        message("Found additional date columns not in date_columns: ",
                paste(missing_date_cols, collapse = ", "),
                ". Consider adding them for automatic date parsing.")
      }
    }
  }

  # Process date columns if specified
  if (!is.null(date_columns)) {
    # Check for uppercase date_columns and warn user
    uppercase_cols <- date_columns[date_columns != tolower(date_columns)]
    if (length(uppercase_cols) > 0) {
      warning("date_columns contains uppercase names: ", paste(uppercase_cols, collapse = ", "), 
              ". Since make_lowercase_names() converts all column names to lowercase, ",
              "date_columns should use lowercase names. Using lowercase versions.")
    }
    
    for (date_col in date_columns) {
      date_col_lower <- tolower(date_col)

      if (date_col_lower %in% names(x)) {
        # If already a Date object, keep it; otherwise parse it
        if (!inherits(x[[date_col_lower]], "Date")) {
          x[[date_col_lower]] <- parse_swedish_date(x[[date_col_lower]], ...)
        }
      } else {
        warning("date_column '", date_col, "' not found in data after lowercase conversion")
      }
    }
  }

  x
}

#' @rdname make_lowercase_names
#' @export
make_lowercase_names.data.table <- function(x, date_columns = NULL, ...) {
  # Convert column names to lowercase
  setnames(x, tolower(names(x)))

  # Check for common Swedish registry date columns and provide suggestions
  common_date_cols <- c("indatum", "utdatum", "edatum", "dodsdat", "fodelseman")
  found_date_cols <- intersect(names(x), common_date_cols)

  if (length(found_date_cols) > 0) {
    if (is.null(date_columns)) {
      # No date_columns specified but date columns found
      message("Found potential date columns: ", paste(found_date_cols, collapse = ", "),
              ". Consider adding them to date_columns parameter for automatic date parsing.")
    } else {
      # Check if any common date columns are missing from specified date_columns
      date_columns_lower <- tolower(date_columns)
      missing_date_cols <- setdiff(found_date_cols, date_columns_lower)
      if (length(missing_date_cols) > 0) {
        message("Found additional date columns not in date_columns: ",
                paste(missing_date_cols, collapse = ", "),
                ". Consider adding them for automatic date parsing.")
      }
    }
  }

  # Process date columns if specified
  if (!is.null(date_columns)) {
    # Check for uppercase date_columns and warn user
    uppercase_cols <- date_columns[date_columns != tolower(date_columns)]
    if (length(uppercase_cols) > 0) {
      warning("date_columns contains uppercase names: ", paste(uppercase_cols, collapse = ", "), 
              ". Since make_lowercase_names() converts all column names to lowercase, ",
              "date_columns should use lowercase names. Using lowercase versions.")
    }
    
    for (date_col in date_columns) {
      date_col_lower <- tolower(date_col)

      if (date_col_lower %in% names(x)) {
        # If already a Date object, keep it; otherwise parse it
        if (!inherits(x[[date_col_lower]], "Date")) {
          x[, (date_col_lower) := parse_swedish_date(get(date_col_lower), ...)]
        }
      } else {
        warning("date_column '", date_col, "' not found in data after lowercase conversion")
      }
    }
  }

  invisible(x)
}
