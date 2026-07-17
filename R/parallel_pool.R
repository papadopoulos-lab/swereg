#' Run a function on each work item in parallel via processx
#'
#' Dispatches items across `n_workers` concurrent R subprocesses. Each item's
#' arguments are saved to a qs2 tempfile and passed to a standalone worker R
#' script (in `inst/`) via command-line arguments. Results are read back from
#' qs2 output tempfiles. This avoids R's IPC serialization overhead entirely.
#'
#' @param items List of argument lists, one per work item. Each element is
#'   saved to a qs2 tempfile and read by the worker script.
#' @param worker_script Basename of the worker R script under `inst/`
#'   (e.g. `"worker_s1a.R"`).
#' @param n_workers Integer number of concurrent subprocesses.
#' @param swereg_dev_path Path to local swereg dev copy (for
#'   `devtools::load_all()`), or `NULL` to use installed swereg.
#' @param p Progressor function from [progressr::progressor()], or `NULL`.
#'   Called once per completed work item in the main process.
#' @param collect If `TRUE` (default), collect and return worker results from
#'   output tempfiles. If `FALSE`, discard (useful when workers save output
#'   directly to their final location).
#' @param label Optional short stage tag (e.g. `"s1c"`) prefixed to the
#'   per-item progress message so the live bar's `(last: ...)` slot self-
#'   identifies which sub-stage is running. `NULL` (default) = timestamp only.
#' Bounded tail of a worker's log file
#'
#' Reads at most the last `max_bytes` of `path` and returns its last `n` lines.
#'
#' Bounded on the way IN, which is the whole point: the first version of this
#' called `readLines()` on the entire file and only then took the tail, so a
#' worker that died after emitting a multi-GB log would OOM the **parent** while
#' its error was being reported -- turning one worker's failure into the whole
#' run's. Never more than `max_bytes` enters memory.
#'
#' Worker output is not guaranteed to be text. A C library can emit a NUL, and
#' seeking into the middle of a file can slice a multi-byte character in half.
#' `rawToChar()` errors on an embedded NUL and `strsplit()` errors on an invalid
#' multibyte string, so an unscrubbed version would report "(no output
#' captured)" for a worker that had in fact said exactly what was wrong. Bytes
#' are therefore scrubbed, not trusted.
#'
#' @param path Log file path.
#' @param n Maximum lines to return.
#' @param max_bytes Maximum bytes to read from the end of the file.
#' @return A single string, `""` if there is nothing readable to report.
#' @noRd
.pp_log_tail <- function(path, n = 100L, max_bytes = 64000) {
  if (!file.exists(path)) return("")
  size <- file.size(path)
  if (is.na(size) || size == 0L) return("")

  from <- max(0, size - max_bytes)
  txt <- tryCatch(
    {
      con <- file(path, "rb")
      on.exit(close(con), add = TRUE)
      if (from > 0) seek(con, where = from, origin = "start")
      bytes <- readBin(con, "raw", n = min(size, max_bytes))
      bytes <- bytes[bytes != as.raw(0L)]
      raw_txt <- rawToChar(bytes)
      Encoding(raw_txt) <- "UTF-8"
      iconv(raw_txt, from = "UTF-8", to = "UTF-8", sub = "?")
    },
    error = function(e) ""
  )
  if (length(txt) != 1L || is.na(txt) || !nzchar(txt)) return("")

  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  # A mid-line seek makes the first fragment partial; drop it rather than report
  # a truncated line as though it were real output.
  if (from > 0 && length(lines) > 1L) lines <- lines[-1L]
  clipped <- from > 0 || length(lines) > n
  if (length(lines) > n) lines <- utils::tail(lines, n)

  paste(
    c(
      if (clipped) sprintf("... (tail of %s; %s bytes total)", path, format(size)),
      lines
    ),
    collapse = "\n"
  )
}

#' @param ... Ignored (absorbs unused arguments from callers).
#' @return If `collect = TRUE`, a list of results. If `collect = FALSE`,
#'   `invisible(NULL)`.
#' @export
parallel_pool <- function(
  items,
  worker_script,
  n_workers,
  swereg_dev_path = NULL,
  p = NULL,
  collect = TRUE,
  label = NULL,
  ...
) {
  # Validate BEFORE anything is divided by, launched, or written. n_workers = 0
  # used to give floor(n_cores / 0) -> n_threads = Inf and then an infinite BUSY
  # loop: the inner while never launches (length(active) < 0 is never true),
  # `active` stays empty, and the Sys.sleep(0.1) sits inside
  # `if (length(active) > 0L)`. 100% of a core, forever, silently.
  n_workers <- .validate_n_workers(n_workers, "parallel_pool()")

  n_items <- length(items)
  if (n_items == 0L) return(if (collect) list() else invisible(NULL))

  # A dev path that was ASKED FOR but does not exist is a mistake, not a
  # preference. Falling through to the installed package (the old behaviour)
  # silently runs stale code and reports success -- the worst outcome of the
  # three, and impossible to notice from the results.
  is_dev <- !is.null(swereg_dev_path)
  if (is_dev) {
    swereg_dev_path <- normalizePath(swereg_dev_path, mustWork = FALSE)
    if (!dir.exists(swereg_dev_path)) {
      stop(
        "parallel_pool(): swereg_dev_path was given but does not exist: ",
        swereg_dev_path,
        "\n  Refusing to fall back to the installed package, which would ",
        "silently run different code than you asked for.\n  Pass ",
        "swereg_dev_path = NULL to use the installed package deliberately."
      )
    }
    # Existing is not the same as correct. The bootstrap load_all()s this tree
    # into the worker, so a mistargeted or renamed directory loads the WRONG
    # package and mixes it with the installed swereg -- the same class of
    # silently-running-different-code as the missing-path case above, just
    # harder to notice. Cheap to rule out: the tree says what it is.
    dcf_path <- file.path(swereg_dev_path, "DESCRIPTION")
    if (!file.exists(dcf_path)) {
      stop(
        "parallel_pool(): swereg_dev_path is not an R package source tree ",
        "(no DESCRIPTION): ", swereg_dev_path
      )
    }
    dev_pkg <- tryCatch(
      unname(read.dcf(dcf_path, fields = "Package")[1L, 1L]),
      error = function(e) NA_character_
    )
    if (is.na(dev_pkg) || !identical(dev_pkg, "swereg")) {
      stop(
        "parallel_pool(): swereg_dev_path points at package '",
        dev_pkg, "', not 'swereg': ", swereg_dev_path
      )
    }
    script_path <- file.path(swereg_dev_path, "inst", worker_script)
  } else {
    script_path <- system.file(worker_script, package = "swereg")
  }
  if (!file.exists(script_path)) {
    stop("Worker script not found: ", script_path)
  }

  bootstrap_path <- file.path(dirname(script_path), "worker_bootstrap.R")
  if (!file.exists(bootstrap_path)) {
    stop("Bootstrap script not found: ", bootstrap_path)
  }

  rscript_bin <- file.path(R.home("bin"), "Rscript")

  n_threads <- .threads_per_worker(n_workers)

  input_paths <- vapply(seq_len(n_items), function(i) {
    tempfile(pattern = paste0("pp_in_", i, "_"), fileext = ".qs2")
  }, character(1))

  output_paths <- if (collect) {
    vapply(seq_len(n_items), function(i) {
      tempfile(pattern = paste0("pp_out_", i, "_"), fileext = ".qs2")
    }, character(1))
  } else {
    rep_len(NA_character_, n_items)
  }

  # Per-item log files. Workers write stdout/stderr to disk rather than to a
  # pipe: a pipe has a fixed OS buffer (64 KB on Linux) and this pool only
  # reads it AFTER the child exits, so a child that out-writes the buffer
  # blocks forever on write() and stays is_alive() == TRUE. That deadlock is
  # indistinguishable from a hung worker. Reproduced before this change: 1 KB
  # per stream finished in 0.7s, 100 KB never returned. Files are also bounded
  # by disk rather than RAM, and survive for the error message.
  log_paths <- vapply(seq_len(n_items), function(i) {
    tempfile(pattern = paste0("pp_log_", i, "_"), fileext = ".log")
  }, character(1))

  on.exit({
    unlink(input_paths, force = TRUE)
    unlink(log_paths, force = TRUE)
    if (collect) unlink(output_paths, force = TRUE)
  }, add = TRUE)

  for (i in seq_len(n_items)) {
    item <- items[[i]]
    item$swereg_dev_path <- swereg_dev_path
    item$n_threads <- n_threads
    qs2::qs_save(item, input_paths[i])
  }

  active <- list()
  n_done <- 0L
  next_item <- 1L
  results <- if (collect) vector("list", n_items) else NULL

  on.exit({
    for (entry in active) {
      tryCatch(entry$proc$kill_tree(), error = function(e) NULL)
    }
  }, add = TRUE, after = FALSE)

  if (is.null(p)) message(sprintf("  [0/%d] dispatching workers...", n_items))

  .launch_worker <- function(idx) {
    cmd_args <- c("--vanilla", script_path, bootstrap_path, input_paths[idx])
    if (collect) cmd_args <- c(cmd_args, output_paths[idx])

    processx::process$new(
      command = rscript_bin,
      args = cmd_args,
      stdout = log_paths[idx],
      stderr = "2>&1",
      cleanup_tree = TRUE
    )
  }

  # Read one item's output, enforcing the output contract while its log is still
  # on disk. A zero exit status is not the same as a result: a worker can exit
  # cleanly having written nothing, or having been killed after opening the file.
  .collect_output <- function(entry) {
    idx <- entry$idx
    path <- output_paths[idx]
    if (!file.exists(path)) {
      .fail_worker(entry, sprintf("produced no output file: %s", path))
    }
    tryCatch(
      qs2_read(path),
      error = function(e) {
        .fail_worker(
          entry,
          sprintf("wrote an unreadable output file (%s): %s", path, conditionMessage(e))
        )
      }
    )
  }

  # One place that reports a worker failure, so the log tail is attached however
  # the item failed -- nonzero exit, or a violated output contract.
  .fail_worker <- function(entry, what) {
    tail_txt <- .pp_log_tail(log_paths[entry$idx])
    if (nzchar(trimws(tail_txt))) {
      message(sprintf(
        "\n--- Worker %d failed ---\nOUTPUT (stdout+stderr):\n%s\n---",
        entry$idx, tail_txt
      ))
    }
    stop(sprintf("Worker %d %s", entry$idx, what), call. = FALSE)
  }

  .check_worker_error <- function(entry) {
    exit_status <- entry$proc$get_exit_status()
    if (is.null(exit_status) || exit_status == 0L) return(invisible(NULL))

    # stdout and stderr are interleaved into one per-item log file (see
    # .launch_worker); read a bounded tail of it rather than the old pipes.
    all_output <- .pp_log_tail(log_paths[entry$idx])
    if (nchar(trimws(all_output)) > 0L) {
      all_output <- paste0("OUTPUT (stdout+stderr):\n", all_output)
    }
    if (nchar(trimws(all_output)) == 0L) {
      all_output <- sprintf(
        paste0(
          "(no output captured --worker died before producing output)\n",
          "  Command: %s %s\n",
          "  Input file exists: %s (%s bytes)\n",
          "  Bootstrap exists: %s"
        ),
        rscript_bin,
        paste(c("--vanilla", script_path, bootstrap_path, input_paths[entry$idx]), collapse = " "),
        file.exists(input_paths[entry$idx]),
        if (file.exists(input_paths[entry$idx])) file.size(input_paths[entry$idx]) else "N/A",
        file.exists(bootstrap_path)
      )
    }
    message(sprintf(
      "\n--- Worker %d failed (exit %d) ---\n%s\n---",
      entry$idx, exit_status, all_output
    ))
    stop(sprintf(
      "Worker %d failed (exit %d). See output above.",
      entry$idx, exit_status
    ))
  }

  while (next_item <= n_items || length(active) > 0L) {
    while (length(active) < n_workers && next_item <= n_items) {
      proc <- .launch_worker(next_item)
      active[[length(active) + 1L]] <- list(proc = proc, idx = next_item)
      next_item <- next_item + 1L
    }

    still_active <- list()
    for (entry in active) {
      if (!entry$proc$is_alive()) {
        .check_worker_error(entry)
        # Read and validate this item's output BEFORE its log is reclaimed. A
        # worker can exit 0 and still violate the output contract (no file, or
        # an unreadable one); that used to be discovered in a second pass after
        # the loop, by which point every successful log had been deleted -- so
        # the parent reported "produced no output file" having just destroyed
        # the only evidence of why.
        if (collect) results[[entry$idx]] <- .collect_output(entry)
        # Only now is the log genuinely spent. Reclaim it per item rather than
        # at pool exit: s1c dispatches 39,492 items over ~10h, so deferring to
        # on.exit() would sit on ~39k files and their bytes for the whole stage.
        # The failure paths never reach here -- both stop() above, leaving the
        # log in place for the message they are about to print.
        unlink(log_paths[entry$idx], force = TRUE)
        n_done <- n_done + 1L
        if (!is.null(p)) {
          ts <- format(Sys.time(), "%H:%M:%S")
          p(message = if (is.null(label)) ts else paste(label, ts))
        } else if (n_done == n_items || n_done %% max(1L, n_items %/% 20L) == 0L) {
          message(sprintf(
            "  [%d/%d] complete  %s",
            n_done, n_items, format(Sys.time(), "%H:%M:%S")
          ))
        }
      } else {
        still_active[[length(still_active) + 1L]] <- entry
      }
    }
    active <- still_active

    if (length(active) > 0L) Sys.sleep(0.1)
  }

  if (collect) results else invisible(NULL)
}
