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
  ...
) {
  n_items <- length(items)
  if (n_items == 0L) return(if (collect) list() else invisible(NULL))

  if (!is.null(swereg_dev_path)) {
    swereg_dev_path <- normalizePath(swereg_dev_path, mustWork = FALSE)
  }
  is_dev <- !is.null(swereg_dev_path) && dir.exists(swereg_dev_path)
  if (is_dev) {
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

  n_cores <- parallel::detectCores()
  n_threads <- max(1L, floor(n_cores / n_workers))

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

  on.exit({
    unlink(input_paths, force = TRUE)
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
      stdout = "|",
      stderr = "|",
      cleanup_tree = TRUE
    )
  }

  .check_worker_error <- function(entry) {
    exit_status <- entry$proc$get_exit_status()
    if (is.null(exit_status) || exit_status == 0L) return(invisible(NULL))

    stderr_text <- entry$proc$read_all_error()
    stdout_text <- entry$proc$read_all_output()
    all_output <- paste(
      c(
        if (nchar(stderr_text) > 0L) paste0("STDERR:\n", stderr_text),
        if (nchar(stdout_text) > 0L) paste0("STDOUT:\n", stdout_text)
      ),
      collapse = "\n"
    )
    if (nchar(trimws(all_output)) == 0L) {
      all_output <- sprintf(
        paste0(
          "(no output captured — worker died before producing output)\n",
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
        n_done <- n_done + 1L
        if (!is.null(p)) {
          p(message = format(Sys.time(), "%H:%M:%S"))
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

  if (collect) {
    results <- vector("list", n_items)
    for (i in seq_len(n_items)) {
      if (!file.exists(output_paths[i])) {
        stop(sprintf("Worker %d produced no output file: %s", i, output_paths[i]))
      }
      results[[i]] <- qs2_read(output_paths[i])
    }
    results <- Filter(Negate(is.null), results)
    if (length(results) == 0L) stop("All items failed in parallel_pool()")
    results
  } else {
    invisible(NULL)
  }
}
