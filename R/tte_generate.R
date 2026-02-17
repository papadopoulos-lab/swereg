# =============================================================================
# Trial generation pipeline
# =============================================================================
# Infrastructure for the two-loop generate pattern:
#   Loop 1 (tte_generate_enrollments): skeleton files → trial panels (parallel via callr)
#   Loop 2 (project-specific): IPW/IPCW weighting chain
#
# tte_impute_confounders() handles missing confounder imputation.
# .tte_callr_pool() is the internal callr::r_bg() worker pool.
# =============================================================================

# =============================================================================
# tte_impute_confounders
# =============================================================================
#' Impute missing confounders by sampling from observed values
#'
#' For each confounder variable, identifies trials with missing values and
#' replaces them by sampling (with replacement) from observed values across
#' all trials. Operates at the trial level (one value per trial_id), then
#' merges imputed values back into the full panel data.
#'
#' @param trial A TTETrial object
#' @param confounder_vars character vector of confounder column names to impute
#' @param seed integer seed for reproducibility (default: 4L)
#' @return Modified TTETrial object with imputed confounder values
#' @export
tte_impute_confounders <- function(trial, confounder_vars, seed = 4L) {
  trial_level <- trial@data[,
    lapply(.SD, data.table::first),
    by = trial_id,
    .SDcols = confounder_vars
  ]

  set.seed(seed)
  for (var in confounder_vars) {
    missing_trials <- trial_level[is.na(get(var)), trial_id]
    if (length(missing_trials) > 0) {
      observed_vals <- trial_level[!is.na(get(var)), get(var)]
      sampled_vals <- sample(observed_vals, length(missing_trials), replace = TRUE)
      trial_level[trial_id %in% missing_trials, (var) := sampled_vals]
    }
  }

  # Merge imputed values back
  trial@data[, (confounder_vars) := NULL]
  data.table::setkey(trial@data, trial_id)
  data.table::setkey(trial_level, trial_id)
  trial@data <- merge(
    trial@data,
    trial_level[, .SD, .SDcols = c("trial_id", confounder_vars)],
    by = "trial_id",
    all.x = TRUE
  )

  trial
}

# =============================================================================
# tte_generate_enrollments
# =============================================================================
#' Loop 1: Create trial panels from skeleton files
#'
#' For each enrollment_id (follow-up x age group combination), processes all
#' skeleton files in parallel using callr::r_bg() subprocesses. Each subprocess
#' starts with a clean OpenMP state, avoiding the fork+data.table segfault
#' problem.
#'
#' After all files are processed, combines trial objects via tte_rbind +
#' tte_collapse, optionally imputes missing confounders, and saves raw + imp files.
#'
#' @param plan A [TTEPlan] object bundling ETT grid, files, confounders,
#'   and design column names.
#' @param process_fn callback function with signature
#'   `function(task, file_path)` returning a [TTETrial]. `task` is a list with
#'   components: `design` ([TTEDesign]), `enrollment_id`, `age_range`,
#'   `n_threads`.
#' @param output_dir directory for output files
#' @param period_width integer collapse period width (default: 4L)
#' @param impute_fn imputation callback, or NULL to skip imputation.
#'   Default: [tte_impute_confounders]. Called as
#'   `impute_fn(trial, confounder_vars)`.
#' @param n_workers integer number of concurrent subprocesses (default: 3L)
#' @param swereg_dev_path path to local swereg dev copy, or NULL for library(swereg)
#' @export
tte_generate_enrollments <- function(
  plan,
  process_fn,
  output_dir,
  period_width = 4L,
  impute_fn = tte_impute_confounders,
  n_workers = 3L,
  swereg_dev_path = NULL
) {
  if (is.null(plan@ett) || nrow(plan@ett) == 0) {
    stop("plan has no ETTs. Use tte_plan_add_one_ett() to add ETTs first.")
  }

  ett <- plan@ett
  files <- plan@skeleton_files
  n_cores <- parallel::detectCores()
  n_threads <- max(1L, floor(n_cores / n_workers))

  # Aggregate ETT to one row per enrollment_id (for file_raw/file_imp/logging)
  ett_loop1 <- ett[,
    .(
      follow_up = follow_up[1],
      age_grp = age_group[1],
      file_raw = file_raw[1],
      file_imp = file_imp[1]
    ),
    by = enrollment_id
  ]

  cat(sprintf(
    "Creating enrollment files: %d enrollment(s) x %d skeleton files\n",
    nrow(ett_loop1), length(files)
  ))

  p <- progressr::progressor(steps = nrow(ett_loop1) * length(files))

  for (i in seq_len(nrow(ett_loop1))) {
    x_file_raw <- ett_loop1$file_raw[i]
    x_file_imp <- ett_loop1$file_imp[i]

    task <- tte_plan_task(plan, i)
    task$n_threads <- n_threads  # override for worker partitioning

    # Launch callr::r_bg() subprocesses as a worker pool
    results <- .tte_callr_pool(
      files = files,
      process_fn = process_fn,
      task = task,
      n_workers = n_workers,
      swereg_dev_path = swereg_dev_path,
      p = p
    )

    # =========================================================================
    # STEP 2: Combine trial objects and apply collapse
    # =========================================================================
    data.table::setDTthreads(n_cores)
    trial <- tte_rbind(results) |>
      tte_collapse(period_width = period_width)

    # Save trial object (before imputation)
    .qs_save(
      trial,
      file.path(output_dir, x_file_raw),
      preset = "fast",
      nthreads = n_cores
    )

    # =========================================================================
    # STEP 3: Impute missing confounders
    # =========================================================================
    if (!is.null(impute_fn)) {
      trial <- impute_fn(trial, task$design@confounder_vars)
    }

    # Save trial object (after imputation)
    .qs_save(
      trial,
      file.path(output_dir, x_file_imp),
      preset = "fast",
      nthreads = n_cores
    )

    rm(results, trial)
    gc()
  }
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
#' @param process_fn callback with signature `function(task, file_path)`
#' @param task list from [tte_plan_task()] with design, enrollment_id, age_range, n_threads
#' @param n_workers integer number of concurrent subprocesses
#' @param swereg_dev_path path to local swereg dev copy, or NULL
#' @param p progressor function from [progressr::progressor()]
#' @return list of TTETrial objects (one per file, failures excluded with warning)
#' @keywords internal
.tte_callr_pool <- function(
  files,
  process_fn,
  task,
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
      func = function(task, file_path, process_fn, swereg_dev_path) {
        requireNamespace("data.table")
        if (!is.null(swereg_dev_path) && dir.exists(swereg_dev_path)) {
          getExportedValue("devtools", "load_all")(swereg_dev_path)
        } else {
          requireNamespace("swereg")
        }
        process_fn(task, file_path)
      },
      args = list(
        task = task,
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
