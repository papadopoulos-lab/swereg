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
  collect = TRUE,
  ...
) {
  n_items <- length(items)
  if (n_items == 0L) return(if (collect) list() else invisible(NULL))

  is_dev <- !is.null(swereg_dev_path) && dir.exists(swereg_dev_path)
  if (is_dev) {
    script_path <- file.path(swereg_dev_path, "inst", worker_script)
  } else {
    script_path <- system.file(worker_script, package = "swereg")
  }
  if (!file.exists(script_path)) {
    stop("Worker script not found: ", script_path)
  }

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

  stderr_paths <- vapply(seq_len(n_items), function(i) {
    tempfile(pattern = paste0("pp_err_", i, "_"), fileext = ".txt")
  }, character(1))

  on.exit({
    unlink(input_paths, force = TRUE)
    if (collect) unlink(output_paths, force = TRUE)
    unlink(stderr_paths, force = TRUE)
  }, add = TRUE)

  bootstrap_path <- file.path(dirname(script_path), "worker_bootstrap.R")
  if (!file.exists(bootstrap_path)) {
    stop("Bootstrap script not found: ", bootstrap_path)
  }

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

  cat(sprintf("  [0/%d] dispatching workers...\r", n_items))

  while (next_item <= n_items || length(active) > 0L) {
    while (length(active) < n_workers && next_item <= n_items) {
      cmd_args <- c("--vanilla", script_path, bootstrap_path, input_paths[next_item])
      if (collect) cmd_args <- c(cmd_args, output_paths[next_item])

      proc <- processx::process$new(
        command = "Rscript",
        args = cmd_args,
        stdout = NULL,
        stderr = stderr_paths[next_item],
        cleanup_tree = TRUE
      )
      active[[length(active) + 1L]] <- list(proc = proc, idx = next_item)
      next_item <- next_item + 1L
    }

    still_active <- list()
    for (entry in active) {
      if (!entry$proc$is_alive()) {
        exit_status <- entry$proc$get_exit_status()
        if (!is.null(exit_status) && exit_status != 0L) {
          stderr_text <- tryCatch(
            readLines(stderr_paths[entry$idx], warn = FALSE),
            error = function(e) "(could not read stderr)"
          )
          message(sprintf(
            "\n--- Worker %d stderr (exit %d) ---\n%s\n---",
            entry$idx, exit_status, paste(stderr_text, collapse = "\n")
          ))
          stop(sprintf(
            "Worker %d failed (exit %d). See stderr above.",
            entry$idx, exit_status
          ))
        }
        n_done <- n_done + 1L
        cat(sprintf(
          "  [%d/%d] complete  %s\r",
          n_done, n_items, format(Sys.time(), "%H:%M:%S")
        ))
      } else {
        still_active[[length(still_active) + 1L]] <- entry
      }
    }
    active <- still_active

    if (length(active) > 0L) Sys.sleep(0.1)
  }

  cat("\n")

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
