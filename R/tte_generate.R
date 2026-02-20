# =============================================================================
# Trial generation helpers
# =============================================================================
# tte_impute_confounders(): Thin standalone wrapper that delegates to
#   trial$impute_confounders(). Needed as default for the impute_fn callback
#   parameter in TTEPlan$generate_enrollments_and_ipw().
#
# .tte_callr_pool(): Internal callr::r_bg() worker pool for parallel processing.
# =============================================================================

#' Impute missing confounders by sampling from observed values
#'
#' Thin standalone wrapper that delegates to `trial$impute_confounders()`.
#' Exists as a standalone function so it can be used as the default
#' `impute_fn` callback in `$generate_enrollments_and_ipw()`.
#'
#' @param trial A [TTETrial] object.
#' @param confounder_vars Character vector of confounder column names to impute.
#' @param seed Integer seed for reproducibility (default: 4L).
#' @return The modified [TTETrial] object (invisibly).
#' @export
tte_impute_confounders <- function(trial, confounder_vars, seed = 4L) {
  trial$impute_confounders(confounder_vars, seed)
  invisible(trial)
}

# =============================================================================
# .tte_callr_pool  (internal helper)
# =============================================================================
#' Run process_fn on each skeleton file via a pool of callr::r_bg() workers
#'
#' Launches up to n_workers concurrent subprocesses. Each subprocess loads
#' data.table + swereg in a fresh R session (clean OpenMP state), reads one
#' skeleton file, applies process_fn, and returns the TTETrial object.
#'
#' @param files character vector of skeleton file paths
#' @param process_fn callback with signature `function(enrollment_spec, file_path)`
#' @param enrollment_spec list from `$enrollment_spec()` with design, enrollment_id, age_range, n_threads
#' @param n_workers integer number of concurrent subprocesses
#' @param swereg_dev_path path to local swereg dev copy, or NULL
#' @param p progressor function from [progressr::progressor()]
#' @return list of TTETrial objects (one per file, failures excluded with warning)
#' @keywords internal
.tte_callr_pool <- function(
  files,
  process_fn,
  enrollment_spec,
  n_workers,
  swereg_dev_path,
  p
) {
  n_files <- length(files)
  results <- vector("list", n_files)
  active <- list()   # slot -> list(proc, idx)
  next_idx <- 1L
  done <- 0L

  .launch_one <- function(idx) {
    f <- files[idx]
    proc <- callr::r_bg(
      func = function(enrollment_spec, file_path, process_fn, swereg_dev_path) {
        requireNamespace("data.table")
        if (!is.null(swereg_dev_path) && dir.exists(swereg_dev_path)) {
          getExportedValue("devtools", "load_all")(swereg_dev_path)
        } else {
          requireNamespace("swereg")
        }
        process_fn(enrollment_spec, file_path)
      },
      args = list(
        enrollment_spec = enrollment_spec,
        file_path = f,
        process_fn = process_fn,
        swereg_dev_path = swereg_dev_path
      ),
      package = FALSE,
      supervise = TRUE
    )
    proc
  }

  # Seed the pool
  while (length(active) < n_workers && next_idx <= n_files) {
    slot <- as.character(length(active) + 1L)
    active[[slot]] <- list(proc = .launch_one(next_idx), idx = next_idx)
    next_idx <- next_idx + 1L
  }

  # Poll until all done
  while (length(active) > 0) {
    Sys.sleep(0.5)
    finished_slots <- c()
    for (slot in names(active)) {
      proc <- active[[slot]]$proc
      idx <- active[[slot]]$idx
      if (!proc$is_alive()) {
        finished_slots <- c(finished_slots, slot)
        tryCatch(
          {
            results[[idx]] <- proc$get_result()
            done <- done + 1L
            p()
          },
          error = function(e) {
            warning(
              "File ", files[idx], " failed: ", conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    }
    # Remove finished, launch replacements
    for (slot in finished_slots) {
      active[[slot]] <- NULL
      if (next_idx <= n_files) {
        active[[slot]] <- list(proc = .launch_one(next_idx), idx = next_idx)
        next_idx <- next_idx + 1L
      }
    }
  }

  # Drop NULLs (failed files)
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) {
    stop("All skeleton files failed in Loop 1")
  }
  results
}
