# =============================================================================
# s1 scratch root: validation, and the age-based sweep that keeps it clean
# =============================================================================
#
# The s1 work directory holds transient dataflow between the four s1 sub-steps.
# On an SMB share the delete of ~276,500 small files runs for about 30 minutes
# and can leave residue behind. On a local NVMe disk the same delete takes
# seconds. The `work_root` argument of $s1_generate_enrollments_and_ipw() moves
# the directory to local disk.
#
# A run that Slurm kills never reaches its own cleanup, so the root needs a
# janitor. `.sweep_scratch_root()` is that janitor. It deletes by age alone,
# which is safe because `--exclusive` serializes the s1 runs on one box.

#' Validate one scratch root.
#'
#' A leading `~` is expanded. `path.expand()` needs no directory to exist, and
#' the caller MUST receive the path the filesystem will use.
#'
#' The root MUST already exist. s1 creates the work directory under it, and a
#' mistyped root would become a new directory that nobody looks in.
#'
#' The root is NOT normalized beyond the expansion.
#' `normalizePath(mustWork = FALSE)` returns a non-existent relative path
#' unchanged. Normalizing would hide a relative value instead of refusing it.
#'
#' @param value The value the caller gave.
#' @param source The name of the argument, for the error message.
#' @return `value`, with any leading `~` expanded.
#' @noRd
.validate_scratch_root <- function(value, source) {
  ok <- is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
  if (!ok) {
    stop(
      source,
      ": the s1 work root must be a single non-empty string, got: ",
      paste(utils::capture.output(utils::str(value)), collapse = " "),
      call. = FALSE
    )
  }
  # Expand BEFORE the absolute check. `~` is absolute to a user, and
  # `file.path()` never expands it. A work directory built from an unexpanded
  # `~` lands in a literal "~" directory beside the working directory.
  value <- path.expand(value)
  # The absoluteness test from output_dir in R/r6_tteplan_pipeline.R, WITHOUT
  # its `~` alternative. path.expand() runs above, so a value that still starts
  # with `~` names no user that exists, and it is not absolute.
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", value)) {
    stop(
      source,
      ": the s1 work root must be an absolute path, got: ",
      encodeString(value, quote = "\""),
      call. = FALSE
    )
  }
  # The root MUST exist. Creating it would turn a typo into a work directory
  # on some other disk, and the sweep would then never see the real root.
  if (!dir.exists(value)) {
    stop(
      source,
      ": the s1 work root must be an existing directory, got: ",
      encodeString(value, quote = "\""),
      call. = FALSE
    )
  }
  return(value)
}

#' Delete stale entries directly under the scratch root.
#'
#' Age is the only rule. Every entry directly under `root` whose modification
#' time is older than `max_age_days` goes, whether it is a file or a
#' directory. Dotfiles go too, and no keep-marker file protects an entry. The
#' sweep never recurses past the first level, and it never stops the run. A
#' directory that survives the delete raises a warning. The sweep then carries
#' on to the next entry.
#'
#' @param root The scratch root, or `NULL`.
#' @param max_age_days Delete an entry older than this many days. The default
#'   is 14 days.
#' @param .unlink The deleter. Test-only: a test passes a no-op here to make an
#'   entry survive. Production callers never pass it.
#' @return The number of entries removed, invisibly. `0L` when `root` is `NULL`
#'   or does not exist, and the function then prints nothing.
#' @noRd
.sweep_scratch_root <- function(
  root,
  max_age_days = 14,
  .unlink = unlink
) {
  if (is.null(root) || !dir.exists(root)) {
    return(invisible(0L))
  }
  cutoff <- Sys.time() - max_age_days * 86400
  entries <- list.files(root, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  k <- 0L
  for (entry in entries) {
    mtime <- file.mtime(entry)
    # `isTRUE()`, because file.mtime() is NA for a broken symlink, or for an
    # entry that vanished between the listing and here. `if (NA < x)` is an
    # error. An NA age is skipped silently.
    if (!isTRUE(mtime < cutoff)) {
      next
    }
    if (dir.exists(entry)) {
      files <- list.files(
        entry,
        recursive = TRUE,
        all.files = TRUE,
        no.. = TRUE,
        full.names = TRUE
      )
      n <- length(files)
      bytes <- sum(file.size(files), na.rm = TRUE)
    } else {
      n <- 1L
      bytes <- file.size(entry)
    }
    age_days <- as.integer(floor(
      as.numeric(difftime(Sys.time(), mtime, units = "days"))
    ))
    .unlink(entry, recursive = TRUE, force = TRUE)
    if (file.exists(entry)) {
      warning("Could not sweep ", entry, call. = FALSE)
      next
    }
    k <- k + 1L
    cat(sprintf(
      "Swept %s: %d files, %s, age %d days\n",
      basename(entry),
      n,
      format(structure(bytes, class = "object_size"), units = "auto"),
      age_days
    ))
  }
  if (k > 0L) {
    cat(sprintf(
      "Scratch sweep: %d entries removed under %s (older than %s days)\n",
      k,
      root,
      format(max_age_days)
    ))
  }
  return(invisible(k))
}
