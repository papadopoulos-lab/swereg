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
#' Writes to a uniquely-named temporary file in the same directory, then renames
#' it into place. Rename-into-place is atomic on POSIX filesystems (and
#' server-side atomic on SMB/CIFS), so an interrupted write -- SIGKILL, crash,
#' dropped mount -- leaves the destination either absent (a later resume
#' rebuilds that batch) or complete, never a truncated file that `qs2_read()`
#' would halt on. `...` is forwarded to [qs2::qs_save()].
#'
#' What this does **not** promise, stated because the tempting reading is wrong:
#'
#' * **It is not durability.** `file.rename()` is atomic with respect to other
#'   *readers*; it is not an `fsync`. A power loss can still lose a renamed file
#'   whose data has not reached the disk. This protects against a killed
#'   process, not a killed machine.
#' * **It is not a lock.** Two concurrent writers of the same `path` each
#'   produce a complete file and the last rename wins. No reader sees a torn
#'   file, but nothing here decides *which* writer should have won.
#'
#' The temporary file is created with [tempfile()] in the destination directory
#' rather than `paste0(path, ".tmp", Sys.getpid())`. The PID suffix was not
#' collision-proof: PIDs are unique only among *live processes on one host*, and
#' this package's data lives on a share that two hosts mount at once -- so the
#' same PID on two machines could pick the same temp path for the same target.
#' Same directory is required: `file.rename()` is not atomic across filesystems.
#'
#' @param object Object to serialize.
#' @param path Destination path.
#' @param ... Passed to `qs2::qs_save()` (e.g. `nthreads`).
#' @return `path`, invisibly.
#' @export
qs2_write_atomic <- function(object, path, ...) {
  dir <- dirname(path)
  tmp <- tempfile(pattern = paste0(basename(path), ".tmp"), tmpdir = dir)

  # Clean up the partial temp file on ANY failure -- a serialization error used
  # to leave it behind next to the real data, where the next run would see an
  # unrecognised file rather than nothing.
  ok <- FALSE
  on.exit(if (!ok) unlink(tmp, force = TRUE), add = TRUE)

  qs2::qs_save(object, tmp, ...)
  if (!file.rename(tmp, path)) {
    stop("qs2_write_atomic(): could not rename ", tmp, " -> ", path)
  }
  ok <- TRUE
  invisible(path)
}
