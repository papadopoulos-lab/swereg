# =============================================================================
# TTEPlan R6 class
# =============================================================================
# This file holds the class definition, its fields, the lifecycle methods and
# the path active bindings. Six sibling files carry the rest of the class
# through $set():
#
#   r6_tteplan_print.R      print() and print_spec_summary()
#   r6_tteplan_checklist.R  print_target_checklist()
#   r6_tteplan_spec.R       the spec and ETT-grid methods
#   r6_tteplan_pipeline.R   s1, s2 and s3
#   r6_tteplan_results.R    the stored-result accessors
#   r6_tteplan_export.R     export_tables(), export() and the two exhibit
#                           producers
#
# The standalone helpers and the spec functions moved to the tteplan_*.R files.
# =============================================================================

.TTE_PLAN_SCHEMA_VERSION <- 3L

# On-disk filename constants. The directory is the scope; the filename is
# the role. See "stub-free filenames" in the refactor plan.
FILENAME_TTEPLAN <- "tteplan.qs2"
filename_spec_xlsx <- function(version) sprintf("spec_%s.xlsx", version)
FILENAME_TABLES_XLSX <- "tables.xlsx"

filename_spec <- function(version) sprintf("spec_%s.yaml", version)

#' TTEPlan class for trial generation planning
#'
#' Bundles the ETT grid, skeleton file paths, and design column names into a
#' single object using a builder pattern. Create an empty plan with
#' [TTEPlan$new()], then add ETTs one at a time with `$add_one_ett()`.
#' Supports `plan[[i]]` to extract the i-th enrollment spec for
#' interactive testing.
#'
#' Design parameters (confounder_vars, person_id_var, treatment_var, etc.) are
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
#' @inheritSection TTEDesign The interval convention
#'
#' @section Methods:
#' \describe{
#'   \item{`$add_one_ett(...)`}{Add one ETT row to the plan. Returns `invisible(self)`.}
#'   \item{`$save(dir)`}{Save the plan to disk as `.qs2`. Returns `invisible(path)`.}
#'   \item{`$enrollment_spec(i)`}{Extract the i-th enrollment spec as a list with design, age_range, etc.}
#'   \item{`$s1_generate_enrollments_and_ipw(...)`}{Run Loop 1: skeleton files to trial panels + IPW.}
#'   \item{`$s2_generate_analysis_files_and_ipcw_pp(...)`}{Run Loop 2: per-ETT IPCW-PP + analysis file generation.}
#' }
#'
#' @examples
#' \dontrun{
#' plan <- TTEPlan$new(
#'   project_prefix = "myproject",
#'   skeleton_files = skeleton_files,
#'   global_max_isoyearweek = "2023-52"
#' )
#' plan$add_one_ett(
#'   outcome_var = "death",
#'   outcome_name = "Death",
#'   follow_up = 52,
#'   confounder_vars = c("age", "education"),
#'   time_treatment_var = "rd_intervention",
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
#' @seealso [qs2_read()] to load from disk
#' @export
TTEPlan <- R6::R6Class(
  "TTEPlan",
  lock_objects = FALSE,
  public = list(
    #' @field project_prefix Character, string used for file naming.
    project_prefix = NULL,
    #' @field ett NULL or a data.table with per-ETT columns.
    ett = NULL,
    #' @field skeleton_files Character vector of skeleton file paths.
    skeleton_files = NULL,
    #' @field global_max_isoyearweek Admin censoring boundary.
    global_max_isoyearweek = NULL,
    #' @field spec Parsed study spec (from [tteplan_read_spec()]), or NULL.
    spec = NULL,
    #' @field expected_skeleton_file_count Expected number of skeleton files, or NULL.
    expected_skeleton_file_count = NULL,
    #' @field code_registry data.table from [RegistryStudy]`$summary_table()`, or NULL.
    code_registry = NULL,
    #' @field expected_n_ids Total number of individuals across all batches, or NULL.
    expected_n_ids = NULL,
    #' @field created_at POSIXct. When this plan was created.
    created_at = NULL,
    #' @field registry_study_created_at POSIXct or NULL. When the source RegistryStudy was created.
    registry_study_created_at = NULL,
    #' @field skeleton_created_at POSIXct or NULL. When skeleton files were created (from first file's attribute).
    skeleton_created_at = NULL,
    #' @field period_width Integer, band width in weeks for enrollment (default: 4L).
    period_width = 4L,
    #' @field enrollment_counts Named list of per-enrollment TARGET Item 8 data.
    #'   Each element is a list with:
    #'   \describe{
    #'     \item{attrition}{Long-format data.table (trial_id, criterion,
    #'       n_persons, n_person_trials, n_intervention, n_comparator) showing
    #'       cumulative attrition at each eligibility step. Includes a
    #'       \code{"before_exclusions"} row with pre-filtering counts.}
    #'     \item{matching}{data.table (trial_id, n_intervention_total,
    #'       n_comparator_total, n_intervention_enrolled, n_comparator_enrolled).}
    #'   }
    enrollment_counts = NULL,
    #' @field output_dir Character. Directory where enrollment/analysis files are stored.
    output_dir = NULL,
    #' @field results_enrollment Named list of per-enrollment analysis results (keyed by enrollment_id).
    results_enrollment = NULL,
    #' @field results_ett Named list of per-ETT analysis results (keyed by ett_id).
    results_ett = NULL,
    #' @field spec_reloaded_at POSIXct or NULL. When `$reload_spec()` was last
    #'   called to refresh cosmetic labels.
    spec_reloaded_at = NULL,
    #' @field spec_reload_skipped_diffs Character vector of structural spec
    #'   differences that `$reload_spec()` chose not to apply, or NULL.
    spec_reload_skipped_diffs = NULL,

    # --- Directory candidates and embedded study ---

    #' @field spec_version Character. Spec version tag (e.g. `"v003"`) that
    #'   selects the YAML filename and the results sub-directory.
    spec_version = NULL,

    #' @field dir_tteplan_cp [CandidatePath] for the directory where
    #'   `tteplan.qs2` and its companion enrollment/analysis files live.
    dir_tteplan_cp = NULL,

    #' @field dir_spec_cp [CandidatePath] for the directory containing the
    #'   spec YAML (`spec_vXXX.yaml`).
    dir_spec_cp = NULL,

    #' @field dir_results_cp [CandidatePath] for the results base directory.
    #'   `dir_results` (active binding) appends `spec_version` to this.
    dir_results_cp = NULL,

    #' @field registrystudy Embedded [RegistryStudy] R6 object. Owns the
    #'   rawbatch and skeleton directory candidates; accessed via
    #'   `plan$data_skeleton` and `plan$data_rawbatch`.
    registrystudy = NULL,

    #' @field n_skeleton_files_limit Optional integer. When non-NULL,
    #'   `tteplan_load()` caps `self$skeleton_files` to this many entries
    #'   after refreshing them from `self$registrystudy`. Used for dev
    #'   configs that only want a subset of skeletons.
    n_skeleton_files_limit = NULL,

    #' @description Create a new TTEPlan object.
    initialize = function(
      project_prefix,
      skeleton_files,
      global_max_isoyearweek,
      ett = NULL
    ) {
      if (length(project_prefix) != 1) {
        stop("project_prefix must be length 1")
      }
      if (length(skeleton_files) == 0) {
        stop("skeleton_files cannot be empty")
      }
      if (!is.null(ett)) {
        if (!data.table::is.data.table(ett)) {
          stop("ett must be a data.table or NULL")
        }
        if (nrow(ett) > 0) {
          required_cols <- c(
            "enrollment_id",
            "outcome_var",
            "follow_up",
            "age_min",
            "age_max",
            "confounder_vars",
            "person_id_var",
            "treatment_var"
          )
          missing <- setdiff(required_cols, names(ett))
          if (length(missing) > 0) {
            stop(paste(
              "ett missing required columns:",
              paste(missing, collapse = ", ")
            ))
          }
        }
      }

      self$project_prefix <- project_prefix
      self$skeleton_files <- skeleton_files
      self$global_max_isoyearweek <- global_max_isoyearweek
      self$ett <- ett
      self$created_at <- Sys.time()

      private$.schema_version <- .TTE_PLAN_SCHEMA_VERSION
    },

    #' @description Check if this object's schema version matches the current
    #' class version. Errors if the object was saved with an older schema.
    #' @return `invisible(TRUE)` if versions match. Errors otherwise with an
    #'   actionable migration message.
    check_version = function() {
      current <- .TTE_PLAN_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        stop(
          class(self)[1],
          " on disk has schema version ",
          saved,
          " but this swereg requires version ",
          current,
          ".\n",
          "Regenerate by re-running the project's s0_init.R (or the old ",
          "1_generate.R equivalent) against the new tteplan_from_spec_and_registrystudy() ",
          "signature, and update any on-disk filenames via dev/rename_r6_files.sh.",
          call. = FALSE
        )
      }
      invisible(TRUE)
    },

    #' @description Save the plan to disk as `tteplan.qs2`.
    #'
    #' Writes to `self$tteplan` by default -- that is, `tteplan.qs2` inside
    #' the directory resolved from `self$dir_tteplan_cp`. Supply `dir` to
    #' override the destination (deprecated; used only by in-flight scripts
    #' that don't yet have a `dir_tteplan_cp`).
    #'
    #' Captures the destination path FIRST, then invalidates every
    #' [CandidatePath] on the plan (and on its embedded [RegistryStudy]) so
    #' the on-disk file never carries the saving host's resolved paths.
    #' Reload with [tteplan_load()].
    #'
    #' @param dir Optional destination directory override. If `NULL` (default),
    #'   writes to `self$tteplan`.
    #' @return Invisibly returns the file path.
    save = function(dir = NULL) {
      if (is.null(dir)) {
        # Standard path: use the active binding (resolves dir_tteplan_cp).
        dest <- self$tteplan
      } else {
        # Deprecated override: legacy filename used project_prefix; new files
        # always use FILENAME_TTEPLAN regardless.
        dest <- file.path(dir, FILENAME_TTEPLAN)
      }
      invalidate_candidate_paths(self)
      qs2_write_atomic(self, dest, nthreads = .safe_n_cores())
      invisible(dest)
    }
  ),
  active = list(
    #' @field max_follow_up (read-only) Maximum follow_up across all ETTs.
    max_follow_up = function() {
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        return(NA_integer_)
      }
      as.integer(max(self$ett$follow_up))
    },

    #' @field dir_tteplan (read-only) Directory where `tteplan.qs2` is saved,
    #'   resolved from `self$dir_tteplan_cp` on the current host.
    dir_tteplan = function() {
      if (is.null(self$dir_tteplan_cp)) {
        stop(
          "TTEPlan has no dir_tteplan_cp -- was it created with the new tteplan_from_spec_and_registrystudy() signature?"
        )
      }
      self$dir_tteplan_cp$resolve()
    },

    #' @field dir_spec (read-only) Directory containing the spec YAML,
    #'   resolved from `self$dir_spec_cp`.
    dir_spec = function() {
      if (is.null(self$dir_spec_cp)) {
        stop("TTEPlan has no dir_spec_cp")
      }
      self$dir_spec_cp$resolve()
    },

    #' @field dir_results_base (read-only) Results base directory, resolved
    #'   from `self$dir_results_cp`. `dir_results` appends `spec_version`.
    dir_results_base = function() {
      if (is.null(self$dir_results_cp)) {
        stop("TTEPlan has no dir_results_cp")
      }
      self$dir_results_cp$resolve()
    },

    #' @field dir_results (read-only) Results directory with version suffix:
    #'   `file.path(self$dir_results_base, self$spec_version)`.
    dir_results = function() {
      file.path(self$dir_results_base, self$spec_version)
    },

    #' @field tteplan (read-only) Full path to `tteplan.qs2`.
    tteplan = function() {
      file.path(self$dir_tteplan, FILENAME_TTEPLAN)
    },

    #' @field spec_path (read-only) Full path to the spec YAML
    #'   (`spec_vXXX.yaml`) selected by `self$spec_version`.
    spec_path = function() {
      file.path(self$dir_spec, filename_spec(self$spec_version))
    },

    #' @field spec_xlsx (read-only) Full path to `spec_<version>.xlsx`
    #'   inside `self$dir_results`, where `<version>` is `self$spec_version`.
    spec_xlsx = function() {
      file.path(self$dir_results, filename_spec_xlsx(self$spec_version))
    },

    #' @field tables_xlsx (read-only) Full path to `tables.xlsx` inside
    #'   `self$dir_results`.
    tables_xlsx = function() {
      file.path(self$dir_results, FILENAME_TABLES_XLSX)
    },

    #' @field data_skeleton (read-only) Delegates to
    #'   `self$registrystudy$data_skeleton_dir`.
    data_skeleton = function() {
      if (is.null(self$registrystudy)) {
        stop("TTEPlan has no embedded registrystudy")
      }
      self$registrystudy$data_skeleton_dir
    },

    #' @field data_rawbatch (read-only) Delegates to
    #'   `self$registrystudy$data_rawbatch_dir`.
    data_rawbatch = function() {
      if (is.null(self$registrystudy)) {
        stop("TTEPlan has no embedded registrystudy")
      }
      self$registrystudy$data_rawbatch_dir
    }
  ),

  private = list(
    .schema_version = NULL
  )
)
