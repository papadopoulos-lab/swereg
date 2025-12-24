# Internal validation helper functions for swereg package
# These functions are not exported and are used internally by add_* functions

#' Validate skeleton structure
#' @param skeleton A data.table that should be a valid skeleton
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_skeleton_structure <- function(skeleton) {
  if (!is.data.table(skeleton)) {
    stop("skeleton must be a data.table. Did you forget to run setDT(skeleton)?")
  }
  
  required_cols <- c("id", "isoyear", "isoyearweek", "is_isoyear")
  missing_cols <- setdiff(required_cols, names(skeleton))
  
  if (length(missing_cols) > 0) {
    stop("skeleton is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
         "Did you create it with create_skeleton()?")
  }
  
  # Check for reasonable data types
  if (!is.logical(skeleton$is_isoyear)) {
    stop("skeleton$is_isoyear must be logical (TRUE/FALSE)")
  }
  
  if (!is.integer(skeleton$isoyear)) {
    stop("skeleton$isoyear must be integer")
  }
}

#' Validate ID column exists in dataset
#' @param data A data.table containing the data
#' @param id_name Character string with the ID column name
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_id_column <- function(data, id_name) {
  if (!is.data.table(data)) {
    stop("data must be a data.table. Did you forget to run setDT(data)?")
  }
  
  if (!id_name %in% names(data)) {
    stop("Column '", id_name, "' not found in data.\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(data)?")
  }
}

#' Validate pattern list for diagnoses/operations/prescriptions/cods
#' @param pattern_list Named list of patterns
#' @param pattern_type Character string describing the pattern type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_pattern_list <- function(pattern_list, pattern_type = "patterns") {
  if (!is.list(pattern_list)) {
    stop(pattern_type, " must be a list")
  }
  
  if (length(pattern_list) == 0) {
    stop(pattern_type, " list is empty. Please provide patterns like:\n",
         "list('depression' = c('F32', 'F33'), 'anxiety' = c('F40', 'F41'))")
  }
  
  if (is.null(names(pattern_list)) || any(names(pattern_list) == "")) {
    stop("All items in ", pattern_type, " list must be named. Example:\n",
         "list('depression' = c('F32', 'F33'), 'anxiety' = c('F40', 'F41'))")
  }
  
  # Check each pattern is a character vector
  for (i in seq_along(pattern_list)) {
    if (!is.character(pattern_list[[i]])) {
      stop("Pattern '", names(pattern_list)[i], "' must be a character vector")
    }
    if (length(pattern_list[[i]]) == 0) {
      stop("Pattern '", names(pattern_list)[i], "' is empty")
    }
  }
}

#' Validate data structure for add_* functions
#' @param data A data.table containing the data
#' @param required_cols Character vector of required column names
#' @param data_type Character string describing the data type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_data_structure <- function(data, required_cols = NULL, data_type = "data") {
  if (!is.data.table(data)) {
    stop(data_type, " must be a data.table. Did you forget to run setDT(", data_type, ")?")
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(data_type, " is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
           "Available columns: ", paste(names(data), collapse = ", "), "\n",
           "Did you forget to run make_lowercase_names(", data_type, ")?")
    }
  }
  
  if (nrow(data) == 0) {
    warning(data_type, " has 0 rows. No data will be added to skeleton.")
  }
}

#' Validate isoyear parameter
#' @param isoyear Integer year value
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_isoyear <- function(isoyear) {
  if (!is.numeric(isoyear) || length(isoyear) != 1) {
    stop("isoyear must be a single numeric value")
  }
  
  if (isoyear != round(isoyear)) {
    stop("isoyear must be an integer")
  }
  
  if (isoyear < 1900 || isoyear > 2050) {
    stop("isoyear must be between 1900 and 2050, got: ", isoyear)
  }
}

#' Validate date column exists and is proper format
#' @param data A data.table containing the data
#' @param date_col Character string with the date column name
#' @param data_type Character string describing the data type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_date_column <- function(data, date_col, data_type = "data") {
  if (!date_col %in% names(data)) {
    stop(data_type, " is missing required date column: ", date_col, "\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(", data_type, ")?")
  }
  
  # Check if date column has reasonable values
  if (all(is.na(data[[date_col]]))) {
    warning("Date column '", date_col, "' in ", data_type, " contains only NA values")
  }
}

#' Validate prescription-specific columns
#' @param data A data.table containing prescription data
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_prescription_data <- function(data) {
  validate_data_structure(data, data_type = "prescription data")
  
  # Check for required prescription columns
  required_cols <- c("edatum", "fddd")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Prescription data is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
         "Required columns: edatum (prescription date), fddd (treatment duration)\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(prescription_data)?")
  }
  
  # Check for either atc or produkt column
  if (!any(c("atc", "produkt") %in% names(data))) {
    stop("Prescription data must have either 'atc' or 'produkt' column for drug codes.\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(prescription_data)?")
  }
}

#' Validate death registry data columns
#' @param data A data.table containing death registry data
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_death_data <- function(data) {
  validate_data_structure(data, data_type = "death registry data")
  
  # Check for required death columns
  required_cols <- c("dodsdat")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Death registry data is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
         "Required columns: dodsdat (death date)\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(death_data)?")
  }
  
  # Check for cause of death columns
  cause_cols <- c(
    stringr::str_subset(names(data), "^ulorsak"),
    stringr::str_subset(names(data), "^morsak")
  )
  
  if (length(cause_cols) == 0) {
    stop("Death registry data must have cause of death columns (ulorsak or morsak).\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(death_data)?")
  }
}


#' Validate date columns exist and are proper format
#' @param data A data.table containing the data
#' @param expected_date_cols Character vector of expected date column names
#' @param data_type Character string describing the data type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_date_columns <- function(data, expected_date_cols, data_type = "data") {
  missing_cols <- setdiff(expected_date_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Date columns not found in ", data_type, ": ", paste(missing_cols, collapse = ", "), "\n",
         "Please clean the data first using:\n",
         "swereg::make_lowercase_names(", data_type, ", date_columns = c('", paste(expected_date_cols, collapse = "', '"), "'))")
  }
  
  # Check if date columns have reasonable values and are Date objects
  for (date_col in expected_date_cols) {
    if (all(is.na(data[[date_col]]))) {
      warning("Date column '", date_col, "' in ", data_type, " contains only NA values")
    }
    
    if (!inherits(data[[date_col]], "Date")) {
      stop("Column '", date_col, "' in ", data_type, " is not a Date object. Please clean the data first using:\n",
           "swereg::make_lowercase_names(", data_type, ", date_columns = c('", paste(expected_date_cols, collapse = "', '"), "'))")
    }
  }
}

#' Validate source column in diagnosis data
#' @param data A data.table containing diagnosis data
#' @param valid_sources Character vector of valid source values (default: inpatient, outpatient, cancer)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_source_column <- function(data, valid_sources = c("inpatient", "outpatient", "cancer")) {
  if (!"source" %in% names(data)) {
    stop("Diagnosis data is missing required 'source' column.\n",
         "The source column should contain one of: ", paste(valid_sources, collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(data)?")
  }

  unique_sources <- unique(data$source)
  invalid_sources <- setdiff(unique_sources, valid_sources)

  if (length(invalid_sources) > 0) {
    stop("Invalid source values found: ", paste(invalid_sources, collapse = ", "), "\n",
         "Valid source values are: ", paste(valid_sources, collapse = ", "))
  }
}