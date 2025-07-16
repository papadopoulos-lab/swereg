#' Parse Swedish registry dates
#'
#' Parses Swedish registry dates that may have varying precision (year only, 
#' year-month, or full date) and converts them to proper Date objects. Handles 
#' common Swedish registry date formats and missing date patterns.
#'
#' @param date_string Character vector of dates in Swedish registry format
#' @param default_month_day Character string for default month-day when only year is provided (default: "0701" for July 1st)
#' @param default_day Character string for default day when only year-month is provided (default: "15" for 15th)
#' @param na_strings Character vector of strings to treat as NA (default: c("", "NA", "9999", "99999999"))
#'
#' @return Date vector with parsed dates
#'
#' @details
#' Swedish registry dates often come in different formats:
#' \itemize{
#'   \item 4 characters (YYYY): Only year known - adds default_month_day
#'   \item 6 characters (YYYYMM): Year and month known - adds default_day  
#'   \item 8 characters (YYYYMMDD): Full date known - uses as-is
#' }
#'
#' Special handling:
#' \itemize{
#'   \item "0000" endings are replaced with default_month_day
#'   \item "00" endings are replaced with default_day
#'   \item Invalid dates return NA with warnings
#' }
#'
#' @examples
#' # Different date formats
#' dates <- c("2020", "202003", "20200315", "19990000", "199900", "")
#' parse_swedish_date(dates)
#' 
#' # Custom defaults
#' parse_swedish_date(dates, default_month_day = "0101", default_day = "01")
#' 
#' @keywords internal
parse_swedish_date <- function(date_string, 
                               default_month_day = "0701",
                               default_day = "15",
                               na_strings = c("", "NA", "9999", "99999999")) {
  
  # Input validation
  if (!is.character(date_string)) {
    date_string <- as.character(date_string)
  }
  
  # Handle NA values and empty strings
  result <- rep(as.Date(NA), length(date_string))
  
  # Identify non-NA values to process
  valid_indices <- !is.na(date_string) & !date_string %in% na_strings
  
  if (!any(valid_indices)) {
    return(result)
  }
  
  # Process valid dates
  valid_dates <- date_string[valid_indices]
  
  # Remove any whitespace
  valid_dates <- stringr::str_trim(valid_dates)
  
  # Initialize processed dates
  processed_dates <- character(length(valid_dates))
  
  for (i in seq_along(valid_dates)) {
    date_str <- valid_dates[i]
    
    # Handle different string lengths
    if (nchar(date_str) == 4) {
      # Year only (YYYY) - add default month/day
      processed_dates[i] <- paste0(date_str, default_month_day)
      
    } else if (nchar(date_str) == 6) {
      # Year-month (YYYYMM) - add default day
      processed_dates[i] <- paste0(date_str, default_day)
      
    } else if (nchar(date_str) == 8) {
      # Full date (YYYYMMDD) - use as-is
      processed_dates[i] <- date_str
      
    } else {
      # Invalid length - will become NA
      processed_dates[i] <- NA_character_
      next
    }
  }
  
  # Handle special cases for "0000" and "00" endings
  processed_dates <- stringr::str_replace(processed_dates, "0000$", default_month_day)
  processed_dates <- stringr::str_replace(processed_dates, "00$", default_day)
  
  # Convert to Date objects
  parsed_dates <- suppressWarnings(lubridate::ymd(processed_dates))
  
  # Check for parsing failures
  failed_indices <- is.na(parsed_dates) & !is.na(processed_dates)
  if (any(failed_indices)) {
    failed_dates <- valid_dates[failed_indices]
    warning("Failed to parse dates: ", paste(failed_dates, collapse = ", "))
  }
  
  # Put results back in original positions
  result[valid_indices] <- parsed_dates
  
  return(result)
}