# =============================================================================
# s1 scratch root: relocation, and the age-based sweep that keeps it clean
# =============================================================================
#
# The s1 work directory holds transient dataflow between the four s1 sub-steps.
# On an SMB share the delete of ~276,500 small files runs for about 30 minutes
# and can leave residue behind. On a local NVMe disk the same delete takes
# seconds. `swereg.s1_work_root` moves the directory to local disk.
#
# A run that Slurm kills never reaches its own cleanup, so the root needs a
# janitor. `.sweep_scratch_root()` is that janitor. It deletes by age alone,
# which is safe because `--exclusive` serializes the s1 runs on one box.

#' Resolve the scratch root for the s1 work directory.
#'
#' Reads `options(swereg.s1_work_root)` first, then `SWEREG_S1_WORK_ROOT`, then
#' returns `NULL`. The order and the validate-do-not-repair rule follow
#' `.default_n_workers_impl()` in R/default_n_workers.R.
#'
#' A scalar `NA` option and an empty environment variable both read as unset,
#' which is what `.default_n_workers_impl()` does. So a caller can clear either
#' setting without removing it.
#'
#' @return A single absolute path, with any leading `~` expanded, or `NULL`
#'   when neither source is set. The path need not exist.
#' @noRd
.s1_scratch_root <- function() {
  opt <- getOption("swereg.s1_work_root", default = NULL)
  if (!is.null(opt) && !(length(opt) == 1L && is.na(opt))) {
    return(.validate_scratch_root(opt, "options(swereg.s1_work_root)"))
  }
  env <- Sys.getenv("SWEREG_S1_WORK_ROOT", unset = "")
  if (nzchar(env)) {
    return(.validate_scratch_root(env, "SWEREG_S1_WORK_ROOT"))
  }
  return(NULL)
}

#' Validate one configured scratch root.
#'
#' A leading `~` is expanded. `path.expand()` needs no directory to exist, and
#' the caller MUST receive the path the filesystem will use.
#'
#' The root is NOT normalized beyond that. It names a directory that need not
#' exist yet. `normalizePath(mustWork = FALSE)` returns a non-existent relative
#' path unchanged. Normalizing would hide a relative value instead of refusing
#' it.
#'
#' @param value The configured value.
#' @param source The name of the option or the environment variable, for the
#'   error message.
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
  return(value)
}

#' Resolve the sweep age, in days.
#'
#' Reads `options(swereg.scratch_max_age_days)` first, then
#' `SWEREG_SCRATCH_MAX_AGE_DAYS`, then returns 14. A scalar `NA` option and an
#' empty environment variable both read as unset.
#'
#' @return A single finite number greater than or equal to 0.
#' @noRd
.scratch_max_age_days <- function() {
  opt <- getOption("swereg.scratch_max_age_days", default = NULL)
  if (!is.null(opt) && !(length(opt) == 1L && is.na(opt))) {
    return(.validate_max_age_days(opt, "options(swereg.scratch_max_age_days)"))
  }
  env <- Sys.getenv("SWEREG_SCRATCH_MAX_AGE_DAYS", unset = "")
  if (nzchar(env)) {
    num <- suppressWarnings(as.numeric(env))
    if (is.na(num)) {
      stop(
        "SWEREG_SCRATCH_MAX_AGE_DAYS",
        ": the sweep age must be a number of days >= 0, got: ",
        encodeString(env, quote = "\""),
        call. = FALSE
      )
    }
    return(.validate_max_age_days(num, "SWEREG_SCRATCH_MAX_AGE_DAYS"))
  }
  return(14)
}

#' Validate one configured sweep age.
#'
#' @param value The configured value.
#' @param source The name of the option or the environment variable, for the
#'   error message.
#' @return `value` as a double.
#' @noRd
.validate_max_age_days <- function(value, source) {
  ok <- is.numeric(value) &&
    length(value) == 1L &&
    is.finite(value) &&
    value >= 0
  if (!ok) {
    stop(
      source,
      ": the sweep age must be a single finite number of days >= 0, got: ",
      paste(utils::capture.output(utils::str(value)), collapse = " "),
      call. = FALSE
    )
  }
  return(as.numeric(value))
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
#' `max_age_days` is a lazy default on purpose. A caller with no root
#' configured passes `NULL` and never reaches it. A misconfigured age then
#' cannot fail a run that would sweep nothing.
#'
#' @param root The scratch root, or `NULL`.
#' @param max_age_days Delete an entry older than this many days.
#' @param .unlink The deleter. Test-only: a test passes a no-op here to make an
#'   entry survive. Production callers never pass it.
#' @return The number of entries removed, invisibly. `0L` when `root` is `NULL`
#'   or does not exist, and the function then prints nothing.
#' @noRd
.sweep_scratch_root <- function(
  root,
  max_age_days = .scratch_max_age_days(),
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
