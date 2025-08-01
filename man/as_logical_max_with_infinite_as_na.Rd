% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{as_logical_max_with_infinite_as_na}
\alias{as_logical_max_with_infinite_as_na}
\title{Convert maximum to logical while treating infinite values as NA}
\usage{
as_logical_max_with_infinite_as_na(x, na.rm = T)
}
\arguments{
\item{x}{Numeric vector}

\item{na.rm}{Logical, whether to remove NA values before calculation (default: TRUE)}
}
\value{
Logical value (maximum converted to logical) with infinite values as NA
}
\description{
Computes the maximum value and converts it to logical, treating infinite
values as NA. Useful for aggregating boolean data.
}
\seealso{
\code{\link{max_with_infinite_as_na}}
}
