# =============================================================================
# callr_pool: Persistent callr::r_session worker pool
# =============================================================================

# PID file glob pattern (used by cleanup and kill functions)
.PID_FILE_PATTERN <- "^swereg_callr_.*\\.txt$"

#' Check whether a process with the given PID is alive
#'
#' Sends signal 0 (no-op) to test existence.
#'
#' @param pid Integer process ID.
#' @return Logical.
#' @noRd
.pid_alive <- function(pid) {
  tryCatch({ tools::pskill(pid, signal = 0L); TRUE }, error = function(e) FALSE)
}

#' Read and parse a PID file
#'
#' @param path Path to PID file.
#' @return List with `parent_pid` (integer) and `child_pids` (integer vector),
#'   or NULL if the file is empty or malformed.
#' @noRd
.read_pid_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0L) return(NULL)
  pids <- suppressWarnings(as.integer(lines))
  parent <- pids[1L]
  if (is.na(parent)) return(NULL)
  children <- pids[-1L]
  list(parent_pid = parent, child_pids = children[!is.na(children)])
}

#' Kill orphaned callr worker sessions from previous crashed runs
#'
#' Scans PID files written by [callr_pool()]. Only kills workers whose parent
#' R process is dead (i.e. was OOM-killed or crashed).
#'
#' @return Invisible NULL.
#' @noRd
.cleanup_orphaned_workers <- function() {
  files <- list.files("/tmp", pattern = .PID_FILE_PATTERN, full.names = TRUE)
  for (f in files) {
    info <- .read_pid_file(f)
    if (is.null(info)) { unlink(f); next }
    if (!.pid_alive(info$parent_pid)) {
      for (pid in info$child_pids) {
        tryCatch(tools::pskill(pid), error = function(e) NULL)
      }
      unlink(f)
    }
  }
  invisible(NULL)
}

#' Kill orphaned swereg callr worker sessions
#'
#' Scans PID files in `/tmp` written by [callr_pool()]. Only kills workers
#' whose parent R process is dead (i.e. was OOM-killed or crashed). Workers
#' owned by live R sessions are left untouched. Stale or empty PID files are
#' removed.
#'
#' Normal cleanup is handled automatically by [callr_pool()]'s `on.exit()`
#' handler. This function is only needed to clean up after hard crashes
#' (SIGKILL, OOM) where `on.exit()` never fires.
#'
#' @return Invisible integer: number of orphaned workers killed.
#' @export
callr_kill_workers <- function() {
  files <- list.files("/tmp", pattern = .PID_FILE_PATTERN, full.names = TRUE)
  n_killed <- 0L
  n_removed <- 0L
  for (f in files) {
    info <- .read_pid_file(f)
    if (is.null(info) || length(info$child_pids) == 0L) { unlink(f); n_removed <- n_removed + 1L; next }
    if (.pid_alive(info$parent_pid)) next
    for (pid in info$child_pids) {
      if (.pid_alive(pid)) {
        tryCatch(tools::pskill(pid), error = function(e) NULL)
        n_killed <- n_killed + 1L
      }
    }
    unlink(f)
    n_removed <- n_removed + 1L
  }
  message("Killed ", n_killed, " orphaned worker(s), removed ", n_removed, " stale PID file(s)")
  invisible(n_killed)
}

#' Initialize persistent callr sessions with swereg namespace (parallel)
#'
#' Spawns `n` sessions concurrently, then loads the namespace in each.
#'
#' @param n Number of sessions.
#' @param swereg_dev_path Dev path or NULL.
#' @return List of `callr::r_session` objects.
#' @noRd
.init_sessions <- function(n, swereg_dev_path) {
  opts <- callr::r_session_options(supervise = TRUE)
  # Start all sessions in parallel (non-blocking)
  sessions <- lapply(seq_len(n), function(i) {
    callr::r_session$new(options = opts, wait = FALSE)
  })
  # Wait for startup, then load namespace in each (non-blocking calls)
  for (s in sessions) {
    s$poll_process(30000L)
    s$read()
    s$call(function(swereg_dev_path) {
      requireNamespace("data.table")
      if (!is.null(swereg_dev_path) && dir.exists(swereg_dev_path)) {
        getExportedValue("devtools", "load_all")(swereg_dev_path)
      } else {
        requireNamespace("swereg")
      }
    }, args = list(swereg_dev_path = swereg_dev_path))
  }
  # Wait for all namespace loads to complete
  for (s in sessions) {
    s$poll_process(60000L)
    s$read()
  }
  sessions
}

#' Initialize a single persistent callr session with swereg namespace
#'
#' Used for crash recovery (single session respawn).
#'
#' @param swereg_dev_path Dev path or NULL.
#' @return A `callr::r_session` object.
#' @noRd
.init_session <- function(swereg_dev_path) {
  opts <- callr::r_session_options(supervise = TRUE)
  s <- callr::r_session$new(options = opts, wait = TRUE)
  s$run(function(swereg_dev_path) {
    requireNamespace("data.table")
    if (!is.null(swereg_dev_path) && dir.exists(swereg_dev_path)) {
      getExportedValue("devtools", "load_all")(swereg_dev_path)
    } else {
      requireNamespace("swereg")
    }
  }, args = list(swereg_dev_path = swereg_dev_path))
  s
}

#' Bind a worker function into a session's global environment
#'
#' @param session A `callr::r_session` object.
#' @param worker_fn The function to bind.
#' @param is_dev Logical; if TRUE, rebinds the function's environment to
#'   the swereg namespace.
#' @noRd
.bind_worker_fn <- function(session, worker_fn, is_dev) {
  session$run(function(fn, dev) {
    if (dev) environment(fn) <- asNamespace("swereg")
    assign(".worker_fn", fn, envir = globalenv())
  }, args = list(fn = worker_fn, dev = is_dev))
}

#' Write a PID file tracking parent and worker PIDs
#'
#' @param pid_file Path to write.
#' @param sessions List of `callr::r_session` objects.
#' @return Invisible NULL.
#' @noRd
.write_pid_file <- function(pid_file, sessions) {
  writeLines(
    c(
      as.character(Sys.getpid()),
      vapply(sessions, function(s) as.character(s$get_pid()), character(1))
    ),
    pid_file
  )
  invisible(NULL)
}

#' Run a function on each work item via a pool of persistent callr sessions
#'
#' Creates `n_workers` persistent [callr::r_session] workers, loads the swereg
#' namespace once per worker, then dispatches items across workers as they
#' become idle. This avoids the per-item startup cost of [callr::r_bg()].
#'
#' On entry, kills any orphaned workers from previous crashed runs (detected
#' via PID files in `/tmp`).
#'
#' @param items List of argument lists, one per work item. Each element is
#'   passed to `worker_fn` via [do.call()].
#' @param worker_fn Function to call in each subprocess. Its signature must
#'   match the names in each element of `items`.
#' @param n_workers Integer number of concurrent subprocesses.
#' @param swereg_dev_path Path to local swereg dev copy (for
#'   `devtools::load_all()`), or `NULL` to use installed swereg.
#' @param p Progressor function from [progressr::progressor()], or `NULL`.
#' @param item_labels Character vector of labels for error messages (same
#'   length as `items`). Defaults to `"1"`, `"2"`, etc.
#' @param collect If `TRUE` (default), collect and return worker results. If
#'   `FALSE`, discard results (useful when workers save output directly).
#' @return If `collect = TRUE`, a list of results (failures excluded with
#'   warning). If `collect = FALSE`, `invisible(NULL)`.
#' @export
callr_pool <- function(
  items,
  worker_fn,
  n_workers,
  swereg_dev_path = NULL,
  p = NULL,
  item_labels = NULL,
  collect = TRUE
) {
  n_items <- length(items)
  if (n_items == 0L) return(if (collect) list() else invisible(NULL))

  if (is.null(item_labels)) {
    item_labels <- as.character(seq_len(n_items))
  }

  # --- Orphan cleanup --------------------------------------------------------
  .cleanup_orphaned_workers()

  # --- Initialize persistent sessions (parallel startup) ---------------------
  n_workers <- min(n_workers, n_items)
  sessions <- .init_sessions(n_workers, swereg_dev_path)
  pid_file <- file.path(
    "/tmp",
    paste0("swereg_callr_", Sys.getpid(), "_", as.integer(Sys.time()), ".txt")
  )
  .write_pid_file(pid_file, sessions)
  on.exit({
    lapply(sessions, function(s) tryCatch(s$close(), error = function(e) NULL))
    unlink(pid_file)
  }, add = TRUE)

  # --- Bind worker_fn into each session --------------------------------------
  is_dev <- !is.null(swereg_dev_path) && dir.exists(swereg_dev_path)
  for (s in sessions) .bind_worker_fn(s, worker_fn, is_dev)

  # --- Dispatch loop ---------------------------------------------------------
  results <- if (collect) vector("list", n_items) else NULL

  # slot_item[i] = item index assigned to session i, or NA if idle
  slot_item <- rep(NA_integer_, n_workers)
  next_idx <- 1L
  n_done <- 0L

  # Dispatch closure (hoisted to avoid repeated serialization)
  .dispatch_fn <- function(item_args) do.call(.worker_fn, item_args)

  # Seed: assign first batch
  for (i in seq_len(n_workers)) {
    if (next_idx > n_items) break
    sessions[[i]]$call(.dispatch_fn, args = list(item_args = items[[next_idx]]))
    slot_item[i] <- next_idx
    next_idx <- next_idx + 1L
  }

  # Poll until all items complete
  while (n_done < n_items) {
    # Poll all active sessions simultaneously via processx::poll()
    active_idx <- which(!is.na(slot_item))
    if (length(active_idx) == 0L) { Sys.sleep(0.1); next }

    conns <- lapply(active_idx, function(i) sessions[[i]]$get_poll_connection())
    poll_res <- processx::poll(conns, 200L)

    did_work <- FALSE
    for (j in seq_along(active_idx)) {
      if (poll_res[[j]] != "ready") next
      i <- active_idx[j]
      did_work <- TRUE

      idx <- slot_item[i]
      slot_item[i] <- NA_integer_
      n_done <- n_done + 1L
      crashed <- FALSE

      tryCatch(
        {
          msg <- sessions[[i]]$read()
          if (!is.null(msg$error)) {
            warning(
              "Item ", item_labels[idx], " failed: ",
              conditionMessage(msg$error),
              call. = FALSE
            )
          } else {
            if (collect) results[[idx]] <- msg$result
          }
          if (!is.null(p)) p()
        },
        error = function(e) {
          crashed <<- TRUE
          warning(
            "Item ", item_labels[idx], " failed: worker session crashed (",
            conditionMessage(e), ")",
            call. = FALSE
          )
          if (!is.null(p)) p()
        }
      )

      # Respawn crashed session
      if (crashed) {
        tryCatch(sessions[[i]]$close(), error = function(e) NULL)
        sessions[[i]] <- .init_session(swereg_dev_path)
        .bind_worker_fn(sessions[[i]], worker_fn, is_dev)
        .write_pid_file(pid_file, sessions)
      }

      # Assign next item
      if (next_idx <= n_items) {
        sessions[[i]]$call(.dispatch_fn, args = list(item_args = items[[next_idx]]))
        slot_item[i] <- next_idx
        next_idx <- next_idx + 1L
      }
    }
    if (!did_work) Sys.sleep(0.05)
  }

  if (collect) {
    results <- Filter(Negate(is.null), results)
    if (length(results) == 0) {
      stop("All items failed in callr_pool()")
    }
    results
  } else {
    invisible(NULL)
  }
}
