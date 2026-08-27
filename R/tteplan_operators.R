# =============================================================================
# S3 methods for TTEPlan operator overloading
# =============================================================================

#' @export
`[[.TTEPlan` <- function(x, i) {
  return(x$enrollment_spec(i))
}

#' @export
length.TTEPlan <- function(x) {
  if (is.null(x$ett) || nrow(x$ett) == 0) {
    return(0L)
  }
  return(data.table::uniqueN(x$ett$enrollment_id))
}
