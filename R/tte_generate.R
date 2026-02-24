# =============================================================================
# Trial generation helpers
# =============================================================================
# tte_impute_confounders(): Thin standalone wrapper that delegates to
#   trial$impute_confounders(). Needed as default for the impute_fn callback
#   parameter in TTEPlan$generate_enrollments_and_ipw().
#
# tte_callr_pool(): Generic callr::r_bg() worker pool for parallel processing.
# =============================================================================

#' Impute missing confounders by sampling from observed values
#'
#' Thin standalone wrapper that delegates to `trial$impute_confounders()`.
#' Exists as a standalone function so it can be used as the default
#' `impute_fn` callback in `$generate_enrollments_and_ipw()`.
#'
#' @param trial A [TTEEnrollment] object.
#' @param confounder_vars Character vector of confounder column names to impute.
#' @param seed Integer seed for reproducibility (default: 4L).
#' @return The modified [TTEEnrollment] object (invisibly).
#' @export
tte_impute_confounders <- function(trial, confounder_vars, seed = 4L) {
  trial$impute_confounders(confounder_vars, seed)
  invisible(trial)
}

# =============================================================================
# tte_callr_pool  (generic worker pool)
# =============================================================================
#' Run a function on each work item via a pool of callr::r_bg() workers
#'
#' Launches up to `n_workers` concurrent subprocesses. Each subprocess loads
#' data.table + swereg in a fresh R session (clean OpenMP state), then calls
#' `do.call(worker_fn, items[[i]])`.
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
tte_callr_pool <- function(
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

  results <- if (collect) vector("list", n_items) else NULL
  active <- list()
  next_idx <- 1L

  .launch_one <- function(idx) {
    callr::r_bg(
      func = function(worker_fn, item_args, swereg_dev_path) {
        requireNamespace("data.table")
        if (!is.null(swereg_dev_path) && dir.exists(swereg_dev_path)) {
          getExportedValue("devtools", "load_all")(swereg_dev_path)
        } else {
          requireNamespace("swereg")
        }
        do.call(worker_fn, item_args)
      },
      args = list(
        worker_fn = worker_fn,
        item_args = items[[idx]],
        swereg_dev_path = swereg_dev_path
      ),
      package = FALSE,
      supervise = TRUE
    )
  }

  # Seed the pool
  while (length(active) < n_workers && next_idx <= n_items) {
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
            res <- proc$get_result()
            if (collect) results[[idx]] <- res
            if (!is.null(p)) p()
          },
          error = function(e) {
            warning(
              "Item ", item_labels[idx], " failed: ", conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    }
    # Remove finished, launch replacements
    for (slot in finished_slots) {
      active[[slot]] <- NULL
      if (next_idx <= n_items) {
        active[[slot]] <- list(proc = .launch_one(next_idx), idx = next_idx)
        next_idx <- next_idx + 1L
      }
    }
  }

  if (collect) {
    results <- Filter(Negate(is.null), results)
    if (length(results) == 0) {
      stop("All items failed in tte_callr_pool()")
    }
    results
  } else {
    invisible(NULL)
  }
}
