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


