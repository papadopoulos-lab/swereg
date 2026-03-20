#' Read a qs2 file (auto-detecting format)
#'
#' Reads files saved with either `qs2::qd_save` (qdata format) or
#' `qs2::qs_save` (standard format). Tries qdata first, falls back to standard.
#'
#' @param file Path to the .qs2 file.
#' @param nthreads Number of threads for decompression.
#' @return The deserialized R object.
#' @export
qs2_read <- function(file, nthreads = 1L) {
  obj <- tryCatch(
    qs2::qd_read(file, nthreads = nthreads),
    error = function(e) {
      if (grepl("qs2 format", conditionMessage(e))) {
        qs2::qs_read(file, nthreads = nthreads)
      } else {
        stop(e)
      }
    }
  )

  # Auto-check schema version for R6 objects
  if (is.environment(obj) && !is.null(obj$check_version)) {
    obj$check_version()
  }

  obj
}
