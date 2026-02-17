# =============================================================================
# Trial generation pipeline
# =============================================================================
# Infrastructure for the two-loop generate pattern:
#   Loop 1 (tte_generate_trials): skeleton files → trial panels (parallel via callr)
#   Loop 2 (project-specific): IPW/IPCW weighting chain
#
# tte_grid() builds the ETT combinatorial grid.
# tte_impute_confounders() handles missing confounder imputation.
# .tte_callr_pool() is the internal callr::r_bg() worker pool.
# =============================================================================

# =============================================================================
# tte_grid
# =============================================================================
#' Build the full ETT (Emulated Target Trial) grid
#'
#' Takes project-specific inputs and returns a data.table with one row per
#' outcome x follow-up x age-group combination, including file naming columns.
#'
#' @param outcomes_dt data.table with columns `outcome_var` and `outcome_name`
#' @param follow_up_weeks integer vector of follow-up durations (e.g., c(52, 104, 156))
#' @param age_groups named list mapping group labels to c(min_age, max_age)
#' @param project_prefix string used for file naming (e.g., "project002_ozel_psychosis")
#' @return data.table with columns: file_id, ett_id, age_group, age_min,
#'   age_max, follow_up, outcome_var, outcome_name, description, file_raw,
#'   file_imp, file_analysis
#' @export
tte_grid <- function(outcomes_dt, follow_up_weeks, age_groups, project_prefix) {
  age_dt <- data.table::data.table(
    age_group = names(age_groups),
    age_min = vapply(age_groups, `[`, numeric(1), 1),
    age_max = vapply(age_groups, `[`, numeric(1), 2)
  )
  ett <- data.table::CJ(
    outcome_var = outcomes_dt$outcome_var,
    follow_up = follow_up_weeks,
    age_group = names(age_groups)
  )
  ett[age_dt, `:=`(age_min = i.age_min, age_max = i.age_max), on = "age_group"]
  ett[outcomes_dt, outcome_name := i.outcome_name, on = "outcome_var"]
  data.table::setorder(ett, age_group, follow_up, outcome_var)
  ett[, file_id := sprintf("%02d", data.table::rleid(follow_up, age_group))]
  ett[, ett_id := paste0("ETT", sprintf("%02d", .I))]
  ett[,
    description := paste0(
      ett_id, ": ", outcome_name,
      " (", follow_up, "w, age ",
      stringr::str_replace(age_group, "_", "-"), ")"
    )
  ]
  ett[, file_raw := paste0(project_prefix, "_raw_", file_id, ".qs")]
  ett[, file_imp := paste0(project_prefix, "_imp_", file_id, ".qs")]
  ett[, file_analysis := paste0(project_prefix, "_analysis_", ett_id, ".qs")]
  data.table::setcolorder(ett, c(
    "file_id", "ett_id", "age_group", "age_min", "age_max", "follow_up",
    "outcome_var", "outcome_name", "description",
    "file_raw", "file_imp", "file_analysis"
  ))
  ett
}

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
# tte_generate_trials
# =============================================================================
#' Loop 1: Create trial panels from skeleton files
#'
#' For each file_id (follow-up x age group combination), processes all skeleton
#' files in parallel using callr::r_bg() subprocesses. Each subprocess starts
#' with a clean OpenMP state, avoiding the fork+data.table segfault problem.
#'
#' After all files are processed, combines trial objects via tte_rbind +
#' tte_collapse, optionally imputes missing confounders, and saves raw + imp files.
#'
#' @param ett data.table from tte_grid()
#' @param files character vector of skeleton file paths
#' @param confounder_vars character vector of confounder column names
#' @param global_max_isoyearweek integer administrative censoring boundary
#' @param process_fn callback function with signature
#'   function(file_path, design, file_id, age_range, n_threads) returning a TTETrial
#' @param output_dir directory for output files
#' @param period_width integer collapse period width (default: 4L)
#' @param impute_fn imputation callback, or NULL to skip imputation.
#'   Default: [tte_impute_confounders]. Called as
#'   `impute_fn(trial, confounder_vars)`.
#' @param n_workers integer number of concurrent subprocesses (default: 3L)
#' @param swereg_dev_path path to local swereg dev copy, or NULL for library(swereg)
#' @export
tte_generate_trials <- function(
  ett,
  files,
  confounder_vars,
  global_max_isoyearweek,
  process_fn,
  output_dir,
  period_width = 4L,
  impute_fn = tte_impute_confounders,
  n_workers = 3L,
  swereg_dev_path = NULL
) {
  n_cores <- parallel::detectCores()
  n_threads <- max(1L, floor(n_cores / n_workers))

  # Aggregate ETT to one row per file_id
  ett_loop1 <- ett[,
    .(
      outcome_vars = list(outcome_var),
      follow_up = follow_up[1],
      age_grp = age_group[1],
      age_min = age_min[1],
      age_max = age_max[1],
      file_raw = file_raw[1],
      file_imp = file_imp[1]
    ),
    by = file_id
  ]

  cat(sprintf("Loop 1: Creating **  %d  ** trial panels\n", nrow(ett_loop1)))

  for (i in seq_len(nrow(ett_loop1))) {
    x_file_id <- ett_loop1$file_id[i]
    x_outcome_vars <- ett_loop1$outcome_vars[[i]]
    x_follow_up <- ett_loop1$follow_up[i]
    x_age_grp <- ett_loop1$age_grp[i]
    x_file_raw <- ett_loop1$file_raw[i]
    x_file_imp <- ett_loop1$file_imp[i]
    x_age_range <- c(ett_loop1$age_min[i], ett_loop1$age_max[i])

    # Define S7 design (once per file_id)
    x_design <- tte_design(
      person_id_var = "id",
      exposure_var = "baseline_exposed",
      time_exposure_var = "rd_exposed",
      eligible_var = "eligible",
      outcome_vars = x_outcome_vars,
      confounder_vars = confounder_vars,
      follow_up_time = as.integer(x_follow_up),
      admin_censor_isoyearweek = global_max_isoyearweek
    )

    # =========================================================================
    # STEP 1: Create trial panels from skeleton files (parallel via callr)
    # =========================================================================
    cat(sprintf(
      "  file_id %s: %d files (%dw, age %s)\n",
      x_file_id, length(files), x_follow_up, x_age_grp
    ))

    # Launch callr::r_bg() subprocesses as a worker pool
    results <- .tte_callr_pool(
      files = files,
      process_fn = process_fn,
      design = x_design,
      file_id = x_file_id,
      age_range = x_age_range,
      n_threads = n_threads,
      n_workers = n_workers,
      swereg_dev_path = swereg_dev_path
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
      trial <- impute_fn(trial, confounder_vars)
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
#' @return list of TTETrial objects (one per file, failures excluded with warning)
#' @keywords internal
.tte_callr_pool <- function(
  files,
  process_fn,
  design,
  file_id,
  age_range,
  n_threads,
  n_workers,
  swereg_dev_path
) {
  n_files <- length(files)
  results <- vector("list", n_files)
  active <- list()   # slot -> list(proc, idx)
  next_idx <- 1L
  done <- 0L

  # Progress bar via progressr (if handler is registered)
  p <- progressr::progressor(steps = n_files)

  .launch_one <- function(idx) {
    f <- files[idx]
    proc <- callr::r_bg(
      func = function(file_path, process_fn, design, file_id, age_range,
                       n_threads, swereg_dev_path) {
        requireNamespace("data.table")
        if (!is.null(swereg_dev_path) && dir.exists(swereg_dev_path)) {
          getExportedValue("devtools", "load_all")(swereg_dev_path)
        } else {
          requireNamespace("swereg")
        }
        process_fn(file_path, design, file_id, age_range, n_threads)
      },
      args = list(
        file_path = f,
        process_fn = process_fn,
        design = design,
        file_id = file_id,
        age_range = age_range,
        n_threads = n_threads,
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
