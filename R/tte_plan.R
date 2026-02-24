# =============================================================================
# TTEPlan: Builder pattern for trial generation planning (R6 class)
# =============================================================================
# Bundles the ETT grid, skeleton file paths, and design column names.
# Inline methods: add_one_ett(), save(), enrollment_spec(), generate_enrollments_and_ipw(),
#   generate_analysis_files_and_ipcw_pp().
# Also includes constructors, S3 methods, and tte_plan_load().
# =============================================================================

#' TTEPlan class for trial generation planning
#'
#' Bundles the ETT grid, skeleton file paths, and design column names into a
#' single object using a builder pattern. Create an empty plan with
#' [tte_plan()], then add ETTs one at a time with `$add_one_ett()`.
#' Supports `plan[[i]]` to extract the i-th enrollment spec for
#' interactive testing.
#'
#' Design parameters (confounder_vars, person_id_var, exposure_var, etc.) are
#' stored per-ETT in the `ett` data.table, allowing different ETTs to use
#' different confounders or design columns. Within an enrollment_id (same
#' follow_up + age_group), design params must match.
#'
#' @param project_prefix Character, string used for file naming.
#' @param ett NULL or a data.table with per-ETT columns including design params.
#' @param skeleton_files Character vector of skeleton file paths.
#' @param global_max_isoyearweek Administrative censoring boundary (isoyearweek string).
#'
#' @section Computed properties:
#' \describe{
#'   \item{max_follow_up}{(read-only) The maximum `follow_up` across all ETTs.
#'     Used by `$enrollment_spec()` to set `design$follow_up_time` so that
#'     enrollment covers the longest follow-up per enrollment group.
#'     Returns `NA` when no ETTs have been added.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{`$add_one_ett(...)`}{Add one ETT row to the plan. Returns `invisible(self)`.}
#'   \item{`$save(dir)`}{Save the plan to disk as `.qs2`. Returns `invisible(path)`.}
#'   \item{`$enrollment_spec(i)`}{Extract the i-th enrollment spec as a list with design, age_range, etc.}
#'   \item{`$generate_enrollments_and_ipw(...)`}{Run Loop 1: skeleton files to trial panels + IPW.}
#'   \item{`$generate_analysis_files_and_ipcw_pp(...)`}{Run Loop 2: per-ETT IPCW-PP + analysis file generation.}
#' }
#'
#' @examples
#' \dontrun{
#' plan <- tte_plan(
#'   project_prefix = "project002",
#'   skeleton_files = skeleton_files,
#'   global_max_isoyearweek = "2023-52"
#' )
#' plan$add_one_ett(
#'   outcome_var = "death",
#'   outcome_name = "Death",
#'   follow_up = 52,
#'   confounder_vars = c("age", "education"),
#'   time_exposure_var = "rd_exposed",
#'   eligible_var = "eligible",
#'   argset = list(age_group = "50_60", age_min = 50, age_max = 60)
#' )
#'
#' # Extract first enrollment spec for interactive testing
#' enrollment_spec <- plan[[1]]
#' enrollment_spec$design
#' enrollment_spec$age_range
#' }
#'
#' @family tte_classes
#' @seealso [tte_plan()] for the constructor, [tte_plan_load()] to load from disk
#' @export
TTEPlan <- R6::R6Class("TTEPlan",
  public = list(
    #' @field project_prefix Character, string used for file naming.
    project_prefix = NULL,
    #' @field ett NULL or a data.table with per-ETT columns.
    ett = NULL,
    #' @field skeleton_files Character vector of skeleton file paths.
    skeleton_files = NULL,
    #' @field global_max_isoyearweek Admin censoring boundary.
    global_max_isoyearweek = NULL,

    #' @description Create a new TTEPlan object.
    initialize = function(project_prefix, skeleton_files,
                          global_max_isoyearweek, ett = NULL) {
      if (length(project_prefix) != 1) stop("project_prefix must be length 1")
      if (length(skeleton_files) == 0) stop("skeleton_files cannot be empty")
      if (!is.null(ett)) {
        if (!data.table::is.data.table(ett)) {
          stop("ett must be a data.table or NULL")
        }
        if (nrow(ett) > 0) {
          required_cols <- c(
            "enrollment_id", "outcome_var", "follow_up", "age_min", "age_max",
            "confounder_vars", "person_id_var", "exposure_var"
          )
          missing <- setdiff(required_cols, names(ett))
          if (length(missing) > 0) {
            stop(paste(
              "ett missing required columns:", paste(missing, collapse = ", ")
            ))
          }
        }
      }

      self$project_prefix <- project_prefix
      self$skeleton_files <- skeleton_files
      self$global_max_isoyearweek <- global_max_isoyearweek
      self$ett <- ett
    },

    #' @description Print the TTEPlan object.
    print = function(...) {
      cat("<TTEPlan>", self$project_prefix, "\n")
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        cat("  ETTs: (none)\n")
      } else {
        n_enrollments <- length(self)
        n_etts <- nrow(self$ett)
        n_skeletons <- length(self$skeleton_files)

        cat(sprintf(
          "  %d enrollment(s) x %d skeleton files -> %d ETT(s)\n\n",
          n_enrollments, n_skeletons, n_etts
        ))

        # Enrollment grid
        enroll_grid <- self$ett[,
          .(
            max_follow_up = paste0(max(follow_up), "w"),
            n_ett = .N
          ),
          by = enrollment_id
        ]
        cat("  Enrollments:\n")
        print(enroll_grid, row.names = FALSE, class = FALSE)

        # ETT grid
        ett_grid <- self$ett[, .(
          ett_id,
          outcome_name = fifelse(
            nchar(outcome_name) > 45,
            paste0(substr(outcome_name, 1, 42), "..."),
            outcome_name
          ),
          follow_up = paste0(follow_up, "w"),
          enrollment_id
        )]
        cat("\n  ETTs:\n")
        print(ett_grid, row.names = FALSE, class = FALSE)
      }
      invisible(self)
    },

    # =========================================================================
    # Methods
    # =========================================================================

    #' @description Add one ETT to the plan.
    #' Each ETT represents one outcome x follow_up x age_group combination.
    #' ETTs with the same `enrollment_id` share trial panels and must have
    #' matching design parameters.
    #' @param enrollment_id Character, enrollment group identifier (e.g., "01").
    #' @param outcome_var Character, name of the outcome column.
    #' @param outcome_name Character, human-readable outcome name.
    #' @param follow_up Integer, follow-up duration in weeks.
    #' @param confounder_vars Character vector of confounder column names.
    #' @param time_exposure_var Character or NULL, time-varying exposure column.
    #' @param eligible_var Character or NULL, eligibility column.
    #' @param argset Named list with age_group, age_min, age_max (and optional person_id_var).
    add_one_ett = function(
        enrollment_id,
        outcome_var,
        outcome_name,
        follow_up,
        confounder_vars,
        time_exposure_var,
        eligible_var,
        argset = list()
    ) {
      age_group <- argset$age_group
      age_min <- argset$age_min
      age_max <- argset$age_max
      if (is.null(age_group) || is.null(age_min) || is.null(age_max)) {
        stop("argset must contain 'age_group', 'age_min', and 'age_max'")
      }
      person_id_var <- if (!is.null(argset$person_id_var)) argset$person_id_var else "id"
      exposure_var <- "baseline_exposed"

      tv_exp <- if (is.null(time_exposure_var)) NA_character_ else time_exposure_var
      elig <- if (is.null(eligible_var)) NA_character_ else eligible_var

      # Validate: if this enrollment_id already exists, design params must match
      if (!is.null(self$ett) && nrow(self$ett) > 0) {
        rows_match <- self$ett$enrollment_id == enrollment_id
        existing <- self$ett[rows_match]
        if (nrow(existing) > 0) {
          first <- existing[1]
          if (first$person_id_var != person_id_var) {
            stop("person_id_var mismatch within enrollment_id ", enrollment_id)
          }
          if (first$exposure_var != exposure_var) {
            stop("exposure_var mismatch within enrollment_id ", enrollment_id)
          }
          first_tv <- first$time_exposure_var
          if (!identical(is.na(first_tv), is.na(tv_exp)) ||
              (!is.na(first_tv) && first_tv != tv_exp)) {
            stop("time_exposure_var mismatch within enrollment_id ", enrollment_id)
          }
          first_el <- first$eligible_var
          if (!identical(is.na(first_el), is.na(elig)) ||
              (!is.na(first_el) && first_el != elig)) {
            stop("eligible_var mismatch within enrollment_id ", enrollment_id)
          }
          if (!identical(first$confounder_vars[[1]], confounder_vars)) {
            stop("confounder_vars mismatch within enrollment_id ", enrollment_id)
          }
        }
      }

      ett_num <- if (is.null(self$ett)) 1L else nrow(self$ett) + 1L
      ett_id <- paste0("ETT", sprintf("%02d", ett_num))
      description <- paste0(
        ett_id, ": ", outcome_name,
        " (", follow_up, "w, age ",
        stringr::str_replace(age_group, "_", "-"), ")"
      )
      prefix <- self$project_prefix
      file_raw <- paste0(prefix, "_raw_", enrollment_id, ".qs2")
      file_imp <- paste0(prefix, "_imp_", enrollment_id, ".qs2")
      file_analysis <- paste0(prefix, "_analysis_", ett_id, ".qs2")

      new_row <- data.table::data.table(
        enrollment_id = enrollment_id,
        ett_id = ett_id,
        age_group = age_group,
        age_min = age_min,
        age_max = age_max,
        follow_up = follow_up,
        outcome_var = outcome_var,
        outcome_name = outcome_name,
        description = description,
        file_raw = file_raw,
        file_imp = file_imp,
        file_analysis = file_analysis,
        confounder_vars = list(confounder_vars),
        person_id_var = person_id_var,
        exposure_var = exposure_var,
        time_exposure_var = tv_exp,
        eligible_var = elig
      )

      if (is.null(self$ett)) {
        self$ett <- new_row
      } else {
        self$ett <- data.table::rbindlist(list(self$ett, new_row), use.names = TRUE)
      }
      invisible(self)
    },

    #' @description Save the plan to disk.
    #' File is named `{project_prefix}_plan.qs2` inside `dir`.
    #' @param dir Directory to save into.
    #' @return Invisibly returns the file path.
    save = function(dir) {
      meta <- list(
        project_prefix = self$project_prefix,
        skeleton_files = self$skeleton_files,
        ett = self$ett,
        global_max_isoyearweek = self$global_max_isoyearweek
      )
      path <- file.path(dir, paste0(self$project_prefix, "_plan.qs2"))
      .qs_save(meta, path, nthreads = parallel::detectCores())
      invisible(path)
    },

    #' @description Extract enrollment spec for the i-th enrollment_id group.
    #' Returns a list with design, enrollment_id, age_range, n_threads.
    #' @param i Integer index (1-based).
    enrollment_spec = function(i = 1L) {
      enrollment_ids <- unique(self$ett$enrollment_id)
      eid <- enrollment_ids[i]
      rows <- self$ett[self$ett$enrollment_id == eid]
      first <- rows[1]

      x_person_id <- first$person_id_var
      x_time_exp <- first$time_exposure_var
      if (is.na(x_time_exp)) x_time_exp <- NULL
      x_eligible <- first$eligible_var
      if (is.na(x_eligible)) x_eligible <- NULL

      result <- list(
        design = tte_design(
          person_id_var = x_person_id,
          exposure_var = first$exposure_var,
          time_exposure_var = x_time_exp,
          eligible_var = x_eligible,
          outcome_vars = rows$outcome_var,
          confounder_vars = first$confounder_vars[[1]],
          follow_up_time = as.integer(max(rows$follow_up)),
          admin_censor_isoyearweek = self$global_max_isoyearweek
        ),
        enrollment_id = eid,
        age_range = c(first$age_min, first$age_max),
        n_threads = parallel::detectCores()
      )

      # Pass through spec-derived fields if present in ETT
      if ("exposure_impl" %in% names(self$ett)) {
        result$exposure_impl <- first$exposure_impl[[1]]
      }
      if ("matching_ratio" %in% names(self$ett)) {
        result$matching_ratio <- first$matching_ratio
      }
      if ("seed" %in% names(self$ett)) {
        result$seed <- first$seed
      }

      result
    },

    #' @description Loop 1: Create trial panels from skeleton files and compute IPW.
    #' For each enrollment_id, processes skeleton files in parallel using
    #' callr::r_bg() subprocesses. Combines, collapses, optionally imputes,
    #' computes IPW + truncation, and saves raw + imp files.
    #' @param process_fn Callback with signature `function(enrollment_spec, file_path)`.
    #' @param output_dir Directory for output files.
    #' @param period_width Integer, collapse period width (default: 4L).
    #' @param impute_fn Imputation callback or NULL (default: [tte_impute_confounders]).
    #' @param stabilize Logical, stabilize IPW (default: TRUE).
    #' @param n_workers Integer, concurrent subprocesses (default: 3L).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    generate_enrollments_and_ipw = function(
        process_fn,
        output_dir,
        period_width = 4L,
        impute_fn = tte_impute_confounders,
        stabilize = TRUE,
        n_workers = 3L,
        swereg_dev_path = NULL
    ) {
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
      }

      ett <- self$ett
      files <- self$skeleton_files
      n_cores <- parallel::detectCores()
      n_threads <- max(1L, floor(n_cores / n_workers))

      ett_loop1 <- ett[,
        .(
          max_follow_up = max(follow_up),
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

        enrollment_spec <- self$enrollment_spec(i)
        enrollment_spec$n_threads <- n_threads

        items <- lapply(files, \(f) list(
          enrollment_spec = enrollment_spec,
          file_path = f
        ))
        results <- tte_callr_pool(
          items = items,
          worker_fn = process_fn,
          n_workers = n_workers,
          swereg_dev_path = swereg_dev_path,
          p = p,
          item_labels = basename(files)
        )

        data.table::setDTthreads(n_cores)
        trial <- tte_rbind(results)
        trial$collapse(period_width = period_width)

        .qs_save(
          trial,
          file.path(output_dir, x_file_raw),
          nthreads = n_cores
        )

        if (!is.null(impute_fn)) {
          trial <- impute_fn(trial, enrollment_spec$design$confounder_vars)
        }

        trial$ipw(stabilize = stabilize)
        trial$truncate(weight_cols = "ipw")

        .qs_save(
          trial,
          file.path(output_dir, x_file_imp),
          nthreads = n_cores
        )

        rm(results, trial)
        gc()
      }
    },

    #' @description Loop 2: Per-ETT IPCW-PP calculation and analysis file generation.
    #' For each ETT, loads the imputed enrollment file, calls
    #' `$prepare_for_analysis()` (outcome + IPCW-PP + weight combination +
    #' truncation), and saves the analysis-ready file.
    #' @param output_dir Directory containing imp files and where analysis files
    #'   are saved.
    #' @param estimate_ipcw_pp_separately_by_exposure Logical, estimate IPCW-PP
    #'   separately by exposure group (default: TRUE).
    #' @param estimate_ipcw_pp_with_gam Logical, use GAM for IPCW-PP estimation
    #'   (default: TRUE).
    #' @param n_workers Integer, concurrent subprocesses (default: 1L).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    generate_analysis_files_and_ipcw_pp = function(
        output_dir,
        estimate_ipcw_pp_separately_by_exposure = TRUE,
        estimate_ipcw_pp_with_gam = TRUE,
        n_workers = 1L,
        swereg_dev_path = NULL
    ) {
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
      }

      ett <- self$ett
      n_cores <- parallel::detectCores()
      n_threads <- max(1L, floor(n_cores / n_workers))

      cat(sprintf(
        "Loop 2: Calculating per-ETT weights - IPCW-PP (%d ETT(s), %d worker(s), %d threads each)\n",
        nrow(ett), n_workers, n_threads
      ))

      sep_by_exp <- estimate_ipcw_pp_separately_by_exposure
      with_gam <- estimate_ipcw_pp_with_gam
      items <- lapply(seq_len(nrow(ett)), function(i) {
        list(
          outcome = ett$outcome_var[i],
          follow_up = ett$follow_up[i],
          file_imp_path = file.path(output_dir, ett$file_imp[i]),
          file_analysis_path = file.path(output_dir, ett$file_analysis[i]),
          n_threads = n_threads,
          sep_by_exp = sep_by_exp,
          with_gam = with_gam
        )
      })

      labels <- sprintf(
        "ETT %d (%s, %dw)",
        seq_len(nrow(ett)), ett$outcome_var, ett$follow_up
      )

      p <- progressr::progressor(steps = length(items))
      tte_callr_pool(
        items = items,
        worker_fn = .loop2_worker,
        n_workers = n_workers,
        swereg_dev_path = swereg_dev_path,
        p = p,
        item_labels = labels,
        collect = FALSE
      )
    }
  ),
  active = list(
    #' @field max_follow_up (read-only) Maximum follow_up across all ETTs.
    max_follow_up = function() {
      if (is.null(self$ett) || nrow(self$ett) == 0) return(NA_integer_)
      as.integer(max(self$ett$follow_up))
    }
  )
)


# =============================================================================
# Package-level worker for Loop 2 (not exported)
# =============================================================================

.loop2_worker <- function(
    outcome, follow_up, file_imp_path, file_analysis_path, n_threads,
    sep_by_exp, with_gam
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(file_imp_path, nthreads = n_threads)
  enrollment$prepare_for_analysis(
    outcome = outcome,
    follow_up = follow_up,
    estimate_ipcw_pp_separately_by_exposure = sep_by_exp,
    estimate_ipcw_pp_with_gam = with_gam
  )
  qs2::qs_save(enrollment, file_analysis_path, nthreads = n_threads)
  TRUE
}


# =============================================================================
# S3 methods for TTEPlan operator overloading
# =============================================================================

#' @export
`[[.TTEPlan` <- function(x, i) {
  x$enrollment_spec(i)
}

#' @export
length.TTEPlan <- function(x) {
  if (is.null(x$ett) || nrow(x$ett) == 0) return(0L)
  data.table::uniqueN(x$ett$enrollment_id)
}


# =============================================================================
# Constructors
# =============================================================================

#' Create an empty TTE plan
#'
#' Constructor for [TTEPlan] objects. Creates an empty plan with just
#' infrastructure parameters. Use `$add_one_ett()` to add ETTs.
#'
#' @param project_prefix string used for file naming (e.g., "project002_ozel_psychosis").
#' @param skeleton_files Character vector of skeleton file paths.
#' @param global_max_isoyearweek Administrative censoring boundary (isoyearweek string).
#'
#' @return A [TTEPlan] object with no ETTs.
#'
#' @examples
#' \dontrun{
#' plan <- tte_plan(
#'   project_prefix = "project002",
#'   skeleton_files = skeleton_files,
#'   global_max_isoyearweek = "2023-52"
#' )
#' plan$add_one_ett(
#'   outcome_var = "death",
#'   outcome_name = "Death",
#'   follow_up = 52,
#'   confounder_vars = c("age", "sex", "education"),
#'   time_exposure_var = "rd_exposed",
#'   eligible_var = "eligible",
#'   argset = list(age_group = "50_60", age_min = 50, age_max = 60)
#' )
#' }
#'
#' @family tte_classes
#' @seealso [TTEPlan] for class details
#' @export
tte_plan <- function(project_prefix, skeleton_files, global_max_isoyearweek) {
  TTEPlan$new(
    project_prefix = project_prefix,
    skeleton_files = skeleton_files,
    global_max_isoyearweek = global_max_isoyearweek,
    ett = NULL
  )
}


#' Load a TTE plan from disk
#'
#' Reads a `.qs2` file saved by `$save()` and reconstructs a
#' [TTEPlan] object.
#'
#' @param path File path to the `.qs2` plan file.
#' @return A [TTEPlan] object.
#'
#' @family tte_classes
#' @seealso [TTEPlan] for class details
#' @export
tte_plan_load <- function(path) {
  meta <- .qs_read(path, nthreads = parallel::detectCores())
  TTEPlan$new(
    project_prefix = meta$project_prefix,
    skeleton_files = meta$skeleton_files,
    global_max_isoyearweek = meta$global_max_isoyearweek,
    ett = meta$ett
  )
}
