min_with_infinite_as_na <- function(x, na.rm=T){
  suppressWarnings(retval <- min(x, na.rm=na.rm))
  retval[is.infinite(retval)] <- NA
  return(retval)
}

max_with_infinite_as_na <- function(x, na.rm=T){
  suppressWarnings(retval <- max(x, na.rm=na.rm))
  retval[is.infinite(retval)] <- NA
  return(retval)
}

as_logical_min_with_infinite_as_na <- function(x, na.rm=T){
  as.logical(min_with_infinite_as_na(x, na.rm=na.rm))
}

as_logical_max_with_infinite_as_na <- function(x, na.rm=T){
  as.logical(max_with_infinite_as_na(x, na.rm=na.rm))
}

first_non_na <- function(x){
  dplyr::first(na.omit(x))
}

