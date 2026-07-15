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

#' Atomically write an object to a qs2 file
#'
#' Writes to a temporary file in the same directory, then renames it into place.
#' Rename-into-place is atomic on POSIX filesystems (and server-side atomic on
#' SMB/CIFS), so an interrupted write -- SIGKILL, crash, dropped mount -- leaves
#' the destination either absent (a later resume rebuilds that batch) or complete,
#' never a truncated file that `qs2_read()` would halt on. `...` is forwarded to
#' [qs2::qs_save()].
#'
#' @param object Object to serialize.
#' @param path Destination path.
#' @param ... Passed to `qs2::qs_save()` (e.g. `nthreads`).
#' @return `path`, invisibly.
#' @export
qs2_write_atomic <- function(object, path, ...) {
  tmp <- paste0(path, ".tmp", Sys.getpid())
  qs2::qs_save(object, tmp, ...)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("qs2_write_atomic(): could not rename ", tmp, " -> ", path)
  }
  invisible(path)
}
