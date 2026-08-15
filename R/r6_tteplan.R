# =============================================================================
# TTEPlan R6 class and spec functions
# =============================================================================
# This file contains the planning-side R6 class and all standalone functions
# called by its methods:
#
#   1. TTEPlan R6 class
#   2. .s1_prepare_skeleton(), .s1_eligible_tuples() (shared helpers)
#   3. .s1a_worker_multi(), .s1b_worker(), .s1c_worker(), .s1d_worker() (Loop 1
#      sub-step workers; see "Loop 1 sub-steps" section below)
#   4. .s2_worker() (Loop 2 IPCW-PP worker)
#   5. S3 methods: [[.TTEPlan, length.TTEPlan
#   6. Spec functions: tteplan_read_spec, tteplan_apply_exclusions,
#      tteplan_apply_derived_confounders, tteplan_validate_spec,
#      tteplan_from_spec_and_registrystudy
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

    #' @description Print the TTEPlan object.
    #' @param ... Ignored.
    print = function(...) {
      cat("<TTEPlan>", self$project_prefix, "\n")
      if (!is.null(self$created_at)) {
        cat("  Created:", format(self$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
      }
      if (!is.null(self$registry_study_created_at)) {
        cat(
          "  RegistryStudy created:",
          format(self$registry_study_created_at, "%Y-%m-%d %H:%M:%S"),
          "\n"
        )
      }
      if (!is.null(self$skeleton_created_at)) {
        cat(
          "  Skeletons created:",
          format(self$skeleton_created_at, "%Y-%m-%d %H:%M:%S"),
          "\n"
        )
      }
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        cat("  ETTs: (none)\n")
      } else {
        n_enrollments <- length(self)
        n_etts <- nrow(self$ett)
        n_outcomes <- length(unique(self$ett$outcome_var))
        n_follow_up <- length(unique(self$ett$follow_up))
        n_skeletons <- length(self$skeleton_files)

        # Skeleton file count line (before ETT summary)
        n_expected <- self$expected_skeleton_file_count
        if (!is.null(n_expected) && n_skeletons != n_expected) {
          cat(sprintf(
            "  Skeleton files: %d / %d expected \033[31m** WARNING: incomplete **\033[0m\n",
            n_skeletons,
            n_expected
          ))
        } else if (!is.null(n_expected)) {
          cat(sprintf(
            "  Skeleton files: %d / %d expected\n",
            n_skeletons,
            n_expected
          ))
        } else {
          cat(sprintf("  Skeleton files: %d\n", n_skeletons))
        }

        cat(sprintf(
          "  %d outcome(s) x %d follow-up x %d enrollment(s) = %d ETT(s)\n",
          n_outcomes,
          n_follow_up,
          n_enrollments,
          n_etts
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
      cat("\n")
      invisible(self)
    },

    # =========================================================================
    # Methods
    # =========================================================================

    #' @description Print a target trial specification summary.
    #' Console-friendly summary derived from the study specification stored
    #' on this plan. When `$code_registry` is available, variable names are
    #' shown in red and matched code details in blue (ANSI colors).
    #' @return `invisible(NULL)`
    print_spec_summary = function() {
      spec <- self$spec
      if (is.null(spec)) {
        stop("plan has no spec")
      }

      # ANSI color/style helpers
      bold <- function(x) paste0("\033[1m", x, "\033[0m")
      green <- function(x) paste0("\033[92m", x, "\033[0m")
      cyan <- function(x) paste0("\033[36m", x, "\033[0m")
      magenta <- function(x) paste0("\033[95m", x, "\033[0m")
      yellow <- function(x) paste0("\033[93m", x, "\033[0m")

      # Build code lookup if registry available
      cl <- .build_code_lookup(self, colorize = TRUE)
      code_lookup <- cl$lookup
      fmt_var <- cl$fmt_var

      cat("=== Target Trial Specification ===\n")
      if (!is.null(code_lookup)) {
        cat("\n")
        cat("  Color   Meaning\n")
        cat(
          "  ",
          green("green"),
          "   Variable defined by a statistician (hardcoded in skeleton)\n",
          sep = ""
        )
        cat(
          "  ",
          cyan("cyan"),
          "    Variable auto-generated from ",
          magenta("registered codes"),
          "\n",
          sep = ""
        )
        cat(
          "  ",
          magenta("magenta"),
          " Registered diagnosis/medication codes (ICD-10, ATC, etc.)\n",
          sep = ""
        )
        cat(
          "  ",
          yellow("yellow"),
          "  Category levels / arm values\n",
          sep = ""
        )
        cat("\n")
      }
      # Helper: print a bold label padded to 17 chars
      lbl <- function(label) {
        padded <- formatC(label, width = -17, flag = "-")
        bold(padded)
      }

      impl <- spec$study$implementation
      cat(lbl("Title:"), spec$study$title, "\n", sep = "")
      if (!is.null(spec$study$design)) {
        cat(lbl("Design:"), spec$study$design, "\n", sep = "")
      }
      cat(lbl("PI:"), spec$study$principal_investigator, "\n", sep = "")
      if (!is.null(impl$date)) {
        cat(lbl("Date:"), impl$date, "\n", sep = "")
      }
      if (!is.null(impl$status)) {
        cat(lbl("Status:"), impl$status, "\n", sep = "")
      }
      cat(lbl("Version:"), impl$version, "\n", sep = "")
      # RegistryStudy + nested Skeletons + TTEPlan
      if (!is.null(self$registry_study_created_at)) {
        cat(
          lbl("RegistryStudy:"),
          format(self$registry_study_created_at, "%Y-%m-%d %H:%M:%S"),
          "\n",
          sep = ""
        )
      }

      # Skeletons (nested under RegistryStudy)
      n_skeletons <- length(self$skeleton_files)
      n_expected <- self$expected_skeleton_file_count
      skel_detail <- if (!is.null(n_expected) && n_skeletons != n_expected) {
        sprintf(
          "%d / %d expected \033[31m** WARNING: incomplete **\033[0m",
          n_skeletons,
          n_expected
        )
      } else if (!is.null(n_expected)) {
        sprintf("%d / %d expected", n_skeletons, n_expected)
      } else {
        sprintf("%d files", n_skeletons)
      }
      skel_label <- bold(formatC(
        " \u2514\u2500 Skeletons:",
        width = -17,
        flag = "-"
      ))
      if (!is.null(self$skeleton_created_at)) {
        cat(
          skel_label,
          format(self$skeleton_created_at, "%Y-%m-%d %H:%M:%S"),
          " (",
          skel_detail,
          ")\n",
          sep = ""
        )
      } else {
        cat(skel_label, "(", skel_detail, ")\n", sep = "")
      }

      if (!is.null(self$created_at)) {
        cat(
          lbl("TTEPlan:"),
          format(self$created_at, "%Y-%m-%d %H:%M:%S"),
          "\n",
          sep = ""
        )
      }
      if (!is.null(self$expected_n_ids)) {
        cat(
          lbl("Individuals:"),
          format(self$expected_n_ids, big.mark = ","),
          " (expected)\n",
          sep = ""
        )
      }
      if (!is.null(self$global_max_isoyearweek)) {
        cat(
          lbl("Admin censoring:"),
          self$global_max_isoyearweek,
          " (isoyear-isoweek)\n",
          sep = ""
        )
      }

      cat("\n")

      # Follow-up
      cat(bold("Follow-up:"), "\n")
      for (fu in spec$follow_up) {
        cat(sprintf("  - %s (%d weeks)\n", fu$label, fu$weeks))
      }
      cat("\n")

      # Inclusion criteria
      cat(bold("Inclusion criteria (global):"), "\n")
      iso <- spec$inclusion_criteria$isoyears
      cat("  Isoyears: ", iso[1], "-", iso[2], "\n", sep = "")
      cat("\n")

      # Exclusion criteria
      cat(bold("Exclusion criteria (global):"), "\n")
      for (ec in spec$exclusion_criteria) {
        cat("  -", ec$name, "\n")
        cat(
          "    Variable:   ",
          fmt_var(
            ec$implementation$source_variable_combined %||%
              ec$implementation$source_variable
          ),
          "\n"
        )
        cat("    Window:     ", .format_window_human(ec$implementation), "\n")
      }
      cat("\n")

      # Confounders
      cat(bold("Confounders:"), "\n")
      for (conf in spec$confounders) {
        cimpl <- conf$implementation
        cat("  -", conf$name, "\n")
        if (isTRUE(cimpl$computed)) {
          derived <- cimpl$variable %||%
            paste0(
              "rd_no_",
              cimpl$source_variable_combined %||% cimpl$source_variable,
              "_",
              .window_label(cimpl$window_weeks)
            )
          cat(
            "    Variable:   ",
            derived,
            "<-",
            fmt_var(cimpl$source_variable_combined %||% cimpl$source_variable),
            "\n"
          )
          cat("    Window:     ", .format_window_human(cimpl), "\n")
        } else {
          cat("    Variable:   ", fmt_var(cimpl$variable), "\n")
        }
        if (!is.null(conf$categories)) {
          cat(
            "    Categories: ",
            yellow(paste(conf$categories, collapse = ", ")),
            "\n"
          )
        }
      }
      cat("\n")

      # Outcomes
      cat(bold("Outcomes:"), "\n")
      for (out in spec$outcomes) {
        cat("  -", out$name, "\n")
        cat("    Variable:   ", fmt_var(out$implementation$variable), "\n")
      }
      cat("\n")

      # Enrollments
      cat(bold("Enrollments:"), "\n")
      for (enr in spec$enrollments) {
        cat(sprintf("  %s\n", bold(paste0(enr$id, ": ", enr$name))))

        # Treatment sub-block
        tx <- enr$treatment
        cat("    Treatment:\n")
        cat(sprintf(
          "      %-18s%s\n",
          "Variable:",
          fmt_var(tx$implementation$variable)
        ))
        cat(sprintf(
          "      %-18s%s <- %s\n",
          "Intervention:",
          tx$arms$intervention,
          yellow(tx$implementation$intervention_value)
        ))
        cat(sprintf(
          "      %-18s%s <- %s\n",
          "Comparator:",
          tx$arms$comparator,
          yellow(tx$implementation$comparator_value)
        ))
        cat(sprintf(
          "      %-18s1:%d\n",
          "Matching ratio:",
          tx$implementation$matching_ratio
        ))

        # Additional inclusion
        if (!is.null(enr$additional_inclusion)) {
          cat("    Additional inclusion:\n")
          for (ai in enr$additional_inclusion) {
            if (identical(ai$type, "age_range")) {
              cat(sprintf("      %-18s%d-%d\n", "Age range:", ai$min, ai$max))
            } else if (identical(ai$type, "has_event")) {
              cat("      -", ai$name, "\n")
              cat(
                "        Variable:    ",
                fmt_var(
                  ai$implementation$source_variable_combined %||%
                    ai$implementation$source_variable
                ),
                "\n"
              )
              cat(
                "        Window:      ",
                .format_window_human(ai$implementation),
                "\n"
              )
            } else {
              cat("      -", ai$name, "\n")
            }
          }
        }

        # Additional exclusion
        if (!is.null(enr$additional_exclusion)) {
          cat("    Additional exclusion:\n")
          for (ae in enr$additional_exclusion) {
            cat("      -", ae$name, "\n")
            cat(
              "        Variable:    ",
              fmt_var(
                ae$implementation$source_variable_combined %||%
                  ae$implementation$source_variable
              ),
              "\n"
            )
            cat(
              "        Window:      ",
              .format_window_human(ae$implementation),
              "\n"
            )
          }
        }
      }

      cat("\n")

      invisible(NULL)
    },

    #' @description Print a TARGET-aligned reporting checklist.
    #'
    #' Generates a self-contained document following the TARGET Statement
    #' (Cashin et al., JAMA 2025) 21-item checklist for transparent reporting
    #' of target trial emulations. Each item includes the full TARGET
    #' description, auto-filled content from the swereg spec where available,
    #' and `[FILL IN]` placeholders for PI completion.
    #'
    #' @return `invisible(NULL)`
    print_target_checklist = function() {
      spec <- self$spec
      if (is.null(spec)) {
        stop("plan has no spec -- set plan$spec first")
      }

      bold <- function(x) paste0("\033[1m", x, "\033[0m")
      dim <- function(x) paste0("\033[2m", x, "\033[0m")
      red <- function(x) paste0("\033[31m", x, "\033[0m")
      cyan <- function(x) paste0("\033[36m", x, "\033[0m")

      # Header
      cat(strrep("\u2550", 59), "\n")
      cat("          TARGET CHECKLIST \u2014 Transparent Reporting of\n")
      cat("     Observational Studies Emulating a Target Trial (2025)\n")
      cat(strrep("\u2550", 59), "\n")
      cat("\n")
      cat("Reference: Cashin AG, Hansford HJ, Hern\u00e1n MA, et al. TARGET\n")
      cat("Statement. JAMA. 2025;334(12):1084-1093.\n")
      cat("doi:10.1001/jama.2025.13350\n")
      cat("\n")
      if (!is.null(spec$study$title)) {
        cat("Generated from TTEPlan:", spec$study$title, "\n")
      }
      cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
      cat("\n")

      # Helper to print one item
      item <- function(num, sub, title, guidance, auto_content = NULL) {
        label <- if (!is.null(sub)) paste0(num, sub) else as.character(num)
        cat(bold(paste0("Item ", label, ". ")), title, "\n\n", sep = "")
        cat(dim(paste0("   Guidance: ", guidance)), "\n\n")
        if (!is.null(auto_content) && nchar(auto_content) > 0) {
          cat("   From spec:\n")
          lines <- strsplit(auto_content, "\n")[[1]]
          for (l in lines) {
            cat("   ", l, "\n")
          }
          cat("\n")
        }
        cat("   >> [FILL IN]\n\n")
      }

      # --- ABSTRACT ---
      cat(strrep("\u2500", 59), "\n")
      cat(bold("ABSTRACT"), "\n")
      cat(strrep("\u2500", 59), "\n\n")

      item(
        "1",
        "a",
        "Identify that the study attempts to emulate a target trial.",
        "Readers should be able to identify from the abstract that the study used observational data to emulate a target trial."
      )

      item(
        "1",
        "b",
        "Report the data sources used for emulation.",
        "Knowledge of the data sources provides context for assessing robustness and generalizability.",
        if (!is.null(spec$study$title)) spec$study$title
      )

      item(
        "1",
        "c",
        "Key assumptions, methods, and findings.",
        "Summarize the key assumptions, statistical methods, and main findings."
      )

      # --- INTRODUCTION ---
      cat(strrep("\u2500", 59), "\n")
      cat(bold("INTRODUCTION"), "\n")
      cat(strrep("\u2500", 59), "\n\n")

      item(
        "2",
        NULL,
        "Scientific background and rationale.",
        "Describe the scientific background and rationale for the study."
      )

      item(
        "3",
        NULL,
        "Causal question.",
        "State the specific causal question the study aims to address.",
        spec$study$description
      )

      item(
        "4",
        NULL,
        "Rationale for target trial emulation approach.",
        "Explain why a target trial emulation was used instead of a randomized trial."
      )

      # --- METHODS ---
      cat(strrep("\u2500", 59), "\n")
      cat(bold("METHODS \u2014 TARGET TRIAL SPECIFICATION"), "\n")
      cat(strrep("\u2500", 59), "\n\n")

      # 6a: Eligibility
      elig_text <- NULL
      if (!is.null(spec$inclusion_criteria$isoyears)) {
        iso <- spec$inclusion_criteria$isoyears
        parts <- paste0("- ISO years: ", iso[1], "-", iso[2])
        if (!is.null(spec$exclusion_criteria)) {
          for (ec in spec$exclusion_criteria) {
            parts <- c(
              parts,
              paste0(
                "- Exclusion: ",
                ec$name,
                " (variable: ",
                ec$implementation$source_variable_combined %||%
                  ec$implementation$source_variable,
                ", window: ",
                .format_window_human(ec$implementation),
                ")"
              )
            )
          }
        }
        elig_text <- paste(parts, collapse = "\n")
      }
      item(
        "6",
        "a",
        "Describe the eligibility criteria.",
        "The eligibility criteria indicate who would be eligible for the target trial, including any washout or run-in periods.",
        elig_text
      )

      # 6b: Treatment strategies
      treat_text <- NULL
      if (!is.null(spec$enrollments)) {
        parts <- character()
        for (enr in spec$enrollments) {
          tx <- enr$treatment
          parts <- c(
            parts,
            paste0(
              "Enrollment '",
              enr$id,
              "': ",
              tx$arms$intervention,
              " vs ",
              tx$arms$comparator,
              " (variable: ",
              tx$implementation$variable,
              ", ratio: 1:",
              tx$implementation$matching_ratio,
              ")"
            )
          )
        }
        treat_text <- paste(parts, collapse = "\n")
      }
      item(
        "6",
        "b",
        "Describe the treatment strategies being compared.",
        "Clearly describe each treatment strategy, including dose, route, frequency, and duration.",
        treat_text
      )

      # 6c: Assignment
      assign_parts <- character()
      for (enr in spec$enrollments) {
        ratio <- enr$treatment$implementation$matching_ratio
        assign_parts <- c(
          assign_parts,
          sprintf(
            "In enrollment %s, each intervention individual was matched to %d comparator individual%s from the same sequential trial.",
            enr$id,
            ratio,
            if (ratio > 1) "s" else ""
          )
        )
      }
      assign_text <- paste0(
        paste(assign_parts, collapse = " "),
        " Matching was stratified by sequential trial to preserve the temporal structure of the emulation. ",
        "Within each trial, all intervention individuals were retained and comparator individuals were sampled at the specified ratio from the full study population. ",
        "Inverse probability weighting was then applied to adjust for residual confounding by measured baseline covariates within the matched set."
      )
      item(
        "6",
        "c",
        "Describe the assignment procedures.",
        "Describe how individuals were assigned to treatment strategies in the emulated trial.",
        assign_text
      )

      # 6d: Follow-up
      fu_text <- NULL
      if (!is.null(spec$follow_up)) {
        parts <- vapply(
          spec$follow_up,
          function(fu) {
            paste0(fu$label, " (", fu$weeks, " weeks)")
          },
          character(1)
        )
        fu_text <- paste(parts, collapse = "\n")
      }
      item(
        "6",
        "d",
        "Describe the start and end of follow-up.",
        "Define when follow-up begins and the criteria for its end.",
        fu_text
      )

      # 6e: Outcomes
      out_text <- NULL
      if (!is.null(spec$outcomes)) {
        parts <- vapply(
          spec$outcomes,
          function(o) {
            # `variable` may be a multi-source list (e.g. an outcome
            # ascertained from ICD-10 OR a quality registry); collapse
            # so the result is always a length-1 string for vapply.
            paste0(
              o$name,
              " (variable: ",
              paste(unlist(o$implementation$variable), collapse = " + "),
              ")"
            )
          },
          character(1)
        )
        out_text <- paste(parts, collapse = "\n")
      }
      item(
        "6",
        "e",
        "Describe the outcomes.",
        "Define the primary and secondary outcomes.",
        out_text
      )

      # 6f: Causal contrasts
      item(
        "6",
        "f",
        "Describe the causal contrasts (estimands).",
        "Specify the causal estimand (e.g., intention-to-treat, per-protocol).",
        "Supported: Per-protocol (IPW + IPCW-PP). Not supported: ITT (pipeline censors at protocol deviation), As-treated (requires time-varying IPW)."
      )

      # 6g: Confounders
      conf_text <- NULL
      if (!is.null(spec$confounders)) {
        parts <- vapply(
          spec$confounders,
          function(c) {
            impl <- c$implementation
            if (isTRUE(impl$computed)) {
              paste0(
                c$name,
                " (computed from: ",
                impl$source_variable_combined %||% impl$source_variable,
                ", window: ",
                .format_window_human(impl),
                ")"
              )
            } else {
              paste0(c$name, " (variable: ", impl$variable, ")")
            }
          },
          character(1)
        )
        conf_text <- paste(parts, collapse = "\n")
      }
      item(
        "6",
        "g",
        "Describe assumptions and confounders.",
        "Assumptions for valid causal inference include no unmeasured confounding, positivity, consistency, and correct model specification.",
        conf_text
      )

      # 6h: Analysis plan
      item(
        "6",
        "h",
        "Describe the data analysis plan.",
        "Describe the statistical methods, including how weights were estimated, models fitted, and sensitivity analyses planned.",
        paste0(
          "Treatment weights were estimated using stabilized inverse probability weights derived from a logistic regression model ",
          "for the probability of treatment assignment conditional on measured baseline covariates, fitted on baseline rows only. ",
          "Per-protocol effects were estimated by censoring individuals at the time of protocol deviation (treatment switching or loss to follow-up) ",
          "and applying inverse probability of censoring weights to account for informative censoring. ",
          "Censoring probabilities were modelled using a generalized additive model with a smooth function of follow-up time and sequential trial indicators, ",
          "conditional on baseline covariates, and fitted separately for the intervention and comparator arms. ",
          "Stabilization used marginal (population-average) censoring probabilities as the numerator. ",
          "The primary outcome model was a weighted Poisson regression (quasipoisson family) ",
          "with a natural cubic spline for follow-up time (3 degrees of freedom), sequential trial indicators to adjust for calendar time, ",
          "and a person-time offset, fitted via survey-weighted generalized linear models with person-level clustered standard errors. ",
          "Extreme weights were truncated at the 1st and 99th percentiles after each weighting step to reduce the influence of near-violations of the positivity assumption."
        )
      )

      # 7a-7h: Emulation
      cat(strrep("\u2500", 59), "\n")
      cat(bold("METHODS \u2014 EMULATION"), "\n")
      cat(strrep("\u2500", 59), "\n\n")

      item(
        "7",
        "a-h",
        "Describe how each specification element was emulated.",
        "For each element (6a-6h), describe how it was emulated using the observational data, including any deviations from the target trial.",
        paste0(
          "Each element of the target trial specification (items 6a\u2013h) was emulated using the observational registry data as follows. ",
          # 7a: Eligibility
          "Eligibility (6a): Eligibility was assessed in every week of the person-week skeleton. ",
          "Consecutive weeks were then grouped into enrollment periods of period_width weeks, and each period defined one sequential trial. ",
          "A person could be eligible in some weeks of a period and not in others. ",
          "Individuals entered the pool of eligible person-trials if they met the inclusion criteria (calendar year range, age) and had not met any exclusion criterion ",
          "(e.g., no prior intervention within the specified washout window, no prior outcome event within the lookback window or over the lifetime, as defined in the specification). ",
          "Exclusion criteria were evaluated cumulatively, and the number of persons and person-trials remaining after each criterion was recorded for the participant flow diagram. ",
          # 7b: Treatment strategies
          "Treatment strategies (6b): Treatment status was determined from registry data in every week of the person-week skeleton. ",
          "The treatment variable and its values came from the study configuration. ",
          "Arm assignment within a period used only the weeks in which the person was eligible and on one of the two protocol arms. ",
          "A person entered the intervention arm if at least one of those weeks was on the intervention treatment. ",
          "A person entered the comparator arm if all of those weeks were on the comparator treatment. ",
          "A person with no such week was ineligible for that period's trial and entered neither arm. ",
          "Initiation occurring anywhere within the period was attributed to its start. ",
          "The enrollment period width (period_width) determines the granularity of sequential trial entry. ",
          "Narrower periods reduce residual immortal time bias, at the cost of fewer eligible individuals per trial (Caniglia et al., 2023). ",
          "No grace period was implemented. ",
          "The period provides slack for the timing of initiation at enrollment only. ",
          "Deviation from the assigned strategy censored per-protocol follow-up at the first mismatched period. ",
          # 7c: Assignment
          "Assignment (6c): Treatment assignment was emulated through stratified matching of comparator to intervention individuals within each sequential trial, ",
          "rather than including all eligible non-initiators with inverse probability weighting alone (Danaei et al., 2013). ",
          "This approach was chosen for computational tractability with large registry datasets. ",
          "Residual confounding within the matched set was addressed by inverse probability weighting using baseline covariates. ",
          # 7d: Follow-up
          "Follow-up (6d): Follow-up began at the start of the enrollment period in which an individual met eligibility and intervention criteria ",
          "and ended at the earliest of the outcome event, protocol deviation (treatment switching), loss to follow-up, administrative censoring, or the pre-specified maximum follow-up duration. ",
          # 7e: Outcomes
          "Outcomes (6e): Outcome events were identified from registry data using the variables specified in the study configuration. ",
          "An event was recorded at the first time period in which the outcome indicator was observed. ",
          # 7f: Causal contrasts
          "Causal contrasts (6f): The per-protocol effect was estimated by censoring individuals at the time of treatment switching ",
          "and applying inverse probability of censoring weights to adjust for the potential informativeness of this censoring. ",
          "Intention-to-treat and as-treated analyses were not conducted. ",
          # 7g: Confounders
          "Confounders (6g): Baseline confounders were measured at the start of each sequential trial. ",
          "For computed confounders (e.g., rolling-window indicators), values were derived from the specified source variable over the lookback window preceding trial entry. ",
          "Missing confounder values were imputed by sampling from the observed distribution of that confounder across person-trials. ",
          # 7h: Analysis
          "Analysis (6h): The analysis followed the two-stage weighting approach described in items 6c and 6h, ",
          "combining baseline inverse probability of treatment weights with time-varying inverse probability of censoring weights for the per-protocol estimand."
        )
      )

      # --- RESULTS ---
      cat(strrep("\u2500", 59), "\n")
      cat(bold("RESULTS"), "\n")
      cat(strrep("\u2500", 59), "\n\n")

      # Item 8: auto-populate from the stored attrition rows if available.
      # `$get_attrition()` returns every stored row, per-trial and global, so
      # this reads the same rows the raw table held.
      item8_text <- NULL
      {
        item8_all <- self$get_attrition()
        item8_parts <- character()
        for (enr_id in unique(item8_all$enrollment_id)) {
          ec <- .plan_cohort_counts(self, enr_id)
          if (!is.null(ec$attrition)) {
            att <- ec$attrition
            # Aggregate across trial_ids for overall counts
            overall <- att[,
              .(
                n_person_trials = sum(n_person_trials),
                n_intervention = sum(n_intervention),
                n_comparator = sum(n_comparator)
              ),
              by = criterion
            ]
            # Preserve criterion order from attrition (before_exclusions first)
            overall[,
              criterion := factor(criterion, levels = unique(criterion))
            ]
            data.table::setorder(overall, criterion)

            # Compute column widths for right-justified alignment
            all_totals <- overall$n_person_trials
            all_intervention <- overall$n_intervention
            all_comparator <- overall$n_comparator
            deltas_total <- c(0, -diff(all_totals))
            deltas_intervention <- c(0, -diff(all_intervention))
            deltas_comparator <- c(0, -diff(all_comparator))

            fmt_num <- function(x, w) {
              formatC(format(x, big.mark = ","), width = w)
            }
            col_width <- function(vals, deltas) {
              max(nchar(format(c(vals, abs(deltas)), big.mark = ",")))
            }
            w_total <- col_width(all_totals, deltas_total)
            w_intervention <- col_width(all_intervention, deltas_intervention)
            w_comparator <- col_width(all_comparator, deltas_comparator)

            item8_parts <- c(
              item8_parts,
              paste0("Enrollment '", enr_id, "' participant flow:")
            )

            for (j in seq_len(nrow(overall))) {
              tot <- all_totals[j]
              n_int <- all_intervention[j]
              n_cmp <- all_comparator[j]

              if (overall$criterion[j] == "before_exclusions") {
                item8_parts <- c(
                  item8_parts,
                  "  Before exclusions:",
                  sprintf(
                    "    \u21b3 %s person-trials",
                    cyan(fmt_num(tot, w_total))
                  )
                )
              } else {
                d_tot <- all_totals[j - 1] - tot
                d_intervention <- all_intervention[j - 1] - n_int
                d_comparator <- all_comparator[j - 1] - n_cmp
                item8_parts <- c(
                  item8_parts,
                  sprintf(
                    "  Applying %s:",
                    bold(as.character(overall$criterion[j]))
                  ),
                  sprintf(
                    "    \u21b3 Excluding %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                    red(fmt_num(d_tot, w_total)),
                    red(fmt_num(d_intervention, w_intervention)),
                    red(fmt_num(d_comparator, w_comparator))
                  ),
                  sprintf(
                    "    \u21b3 Remaining %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                    cyan(fmt_num(tot, w_total)),
                    cyan(fmt_num(n_int, w_intervention)),
                    cyan(fmt_num(n_cmp, w_comparator))
                  )
                )
              }
            }
          }
          if (!is.null(ec$matching)) {
            m <- ec$matching
            n_int <- sum(m$n_intervention_enrolled, na.rm = TRUE)
            n_cmp <- sum(m$n_comparator_enrolled, na.rm = TRUE)
            n_match_total <- n_int + n_cmp
            item8_parts <- c(
              item8_parts,
              "  Post-matching:",
              sprintf(
                "    \u21b3 %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                cyan(fmt_num(n_match_total, w_total)),
                cyan(fmt_num(n_int, w_intervention)),
                cyan(fmt_num(n_cmp, w_comparator))
              )
            )
          }
        }
        if (length(item8_parts) > 0) {
          item8_text <- paste(item8_parts, collapse = "\n")
        }
      }
      if (is.null(item8_text)) {
        item8_text <- "Run $s1_generate_enrollments_and_ipw() first to populate attrition counts."
      }
      item(
        "8",
        NULL,
        "Participant selection (flow diagram).",
        "Provide a flow diagram or description of participant selection.",
        item8_text
      )

      item(
        "9",
        NULL,
        "Baseline data.",
        "Report baseline characteristics for each treatment group.",
        "Available via TTEEnrollment$table1(ipw_col)."
      )

      item(
        "10",
        NULL,
        "Follow-up summary.",
        "Report summary measures of follow-up time.",
        "Available via TTEEnrollment$summary(pretty = TRUE)."
      )

      item(
        "11",
        NULL,
        "Missing data.",
        "Report the amount of missing data and methods used to handle it.",
        "Confounder imputation via $s1_impute_confounders() (sampling from observed)."
      )

      item(
        "12",
        NULL,
        "Outcome frequencies.",
        "Report outcome event counts and rates.",
        "Available via TTEEnrollment$rates(weight_col)."
      )

      item(
        "13",
        NULL,
        "Effect estimates.",
        "Report estimated effects with confidence intervals.",
        "Available via TTEEnrollment$irr(weight_col)."
      )

      item(
        "14",
        NULL,
        "Sensitivity analyses.",
        "Report results of any sensitivity analyses."
      )

      # --- DISCUSSION ---
      cat(strrep("\u2500", 59), "\n")
      cat(bold("DISCUSSION"), "\n")
      cat(strrep("\u2500", 59), "\n\n")

      item(
        "15",
        NULL,
        "Interpretation.",
        "Interpret results considering the study objectives, limitations, and context."
      )

      item(
        "16",
        NULL,
        "Limitations.",
        "Discuss limitations, including potential sources of bias and unmeasured confounding."
      )

      # --- OTHER ---
      cat(strrep("\u2500", 59), "\n")
      cat(bold("OTHER"), "\n")
      cat(strrep("\u2500", 59), "\n\n")

      for (num in 17:21) {
        titles <- c(
          "Ethics approval.",
          "Study registration.",
          "Data availability.",
          "Funding.",
          "Conflicts of interest."
        )
        item(
          as.character(num),
          NULL,
          titles[num - 16],
          "Report as per standard guidelines."
        )
      }

      invisible(NULL)
    },

    #' @description Add one ETT to the plan.
    #'
    #' An ETT (Emulated Target Trial) is one outcome x follow_up x age_group
    #' combination. ETTs sharing an enrollment_id use the same trial panels
    #' (same matching, same age group, same confounders). They differ only
    #' in outcome and/or follow-up duration. This avoids redundant
    #' re-enrollment for each outcome/follow-up combo.
    #'
    #' @param enrollment_id Character, enrollment group identifier (e.g., "01").
    #' @param outcome_var Character, name of the outcome column.
    #' @param outcome_name Character, short human-readable outcome label
    #'   (used in forest plot rows and Table S10).
    #' @param follow_up Integer, follow-up duration in weeks.
    #' @param confounder_vars Character vector of confounder column names.
    #' @param subgroup_vars Character vector or NULL, baseline subgroup columns
    #'   for effect-modification analyses (default: NULL).
    #' @param time_treatment_var Character or NULL, time-varying treatment column.
    #' @param eligible_var Character or NULL, eligibility column.
    #' @param observed_var The observation encoding, or NULL. Give a list with
    #'   exactly one of `column` and `sentinel`. See the observation contract
    #'   section of [tteplan_read_spec()].
    #' @param intervention_tolerance_weeks Integer, the tolerance in weeks for
    #'   the intervention arm (default: 0L).
    #' @param comparator_tolerance_weeks Integer, the tolerance in weeks for
    #'   the comparator arm (default: 0L).
    #' @param argset Named list with age_group, age_min, age_max (and optional
    #'   person_id_var, outcome_description).
    add_one_ett = function(
      enrollment_id,
      outcome_var,
      outcome_name,
      follow_up,
      confounder_vars,
      subgroup_vars = NULL,
      time_treatment_var,
      eligible_var,
      observed_var = NULL,
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      argset = list()
    ) {
      outcome_description <- argset$outcome_description %||% NA_character_
      outcome_role <- argset$outcome_role %||% NA_character_
      age_group <- argset$age_group
      age_min <- argset$age_min
      age_max <- argset$age_max
      if (is.null(age_group) || is.null(age_min) || is.null(age_max)) {
        stop("argset must contain 'age_group', 'age_min', and 'age_max'")
      }
      person_id_var <- if (!is.null(argset$person_id_var)) {
        argset$person_id_var
      } else {
        "id"
      }
      treatment_var <- "baseline_intervention"

      tv_intervention <- if (is.null(time_treatment_var)) {
        NA_character_
      } else {
        time_treatment_var
      }
      elig <- if (is.null(eligible_var)) NA_character_ else eligible_var
      observed_var <- .tte_observed_var(observed_var, "observed_var")
      intervention_tolerance_weeks <- .tte_tolerance_weeks(
        intervention_tolerance_weeks,
        "intervention_tolerance_weeks"
      )
      comparator_tolerance_weeks <- .tte_tolerance_weeks(
        comparator_tolerance_weeks,
        "comparator_tolerance_weeks"
      )

      # Validate: if this enrollment_id already exists, design params must match
      if (!is.null(self$ett) && nrow(self$ett) > 0) {
        rows_match <- self$ett$enrollment_id == enrollment_id
        existing <- self$ett[rows_match]
        if (nrow(existing) > 0) {
          first <- existing[1]
          if (first$person_id_var != person_id_var) {
            stop("person_id_var mismatch within enrollment_id ", enrollment_id)
          }
          if (first$treatment_var != treatment_var) {
            stop("treatment_var mismatch within enrollment_id ", enrollment_id)
          }
          first_tv <- first$time_treatment_var
          if (
            !identical(is.na(first_tv), is.na(tv_intervention)) ||
              (!is.na(first_tv) && first_tv != tv_intervention)
          ) {
            stop(
              "time_treatment_var mismatch within enrollment_id ",
              enrollment_id
            )
          }
          first_el <- first$eligible_var
          if (
            !identical(is.na(first_el), is.na(elig)) ||
              (!is.na(first_el) && first_el != elig)
          ) {
            stop("eligible_var mismatch within enrollment_id ", enrollment_id)
          }
          if (!identical(first$confounder_vars[[1]], confounder_vars)) {
            stop(
              "confounder_vars mismatch within enrollment_id ",
              enrollment_id
            )
          }
          if (
            "observed_var" %in% names(existing) &&
              !identical(first$observed_var[[1]], observed_var)
          ) {
            stop("observed_var mismatch within enrollment_id ", enrollment_id)
          }
          if (
            "intervention_tolerance_weeks" %in% names(existing) &&
              !identical(
                first$intervention_tolerance_weeks,
                intervention_tolerance_weeks
              )
          ) {
            stop(
              "intervention_tolerance_weeks mismatch within enrollment_id ",
              enrollment_id
            )
          }
          if (
            "comparator_tolerance_weeks" %in% names(existing) &&
              !identical(
                first$comparator_tolerance_weeks,
                comparator_tolerance_weeks
              )
          ) {
            stop(
              "comparator_tolerance_weeks mismatch within enrollment_id ",
              enrollment_id
            )
          }
        }
      }

      ett_num <- if (is.null(self$ett)) 1L else nrow(self$ett) + 1L
      ett_id <- paste0("ETT", sprintf("%05d", ett_num))
      description <- paste0(
        ett_id,
        ": ",
        outcome_name,
        " (",
        follow_up,
        "w, age ",
        stringr::str_replace(age_group, "_", "-"),
        ")"
      )
      prefix <- self$project_prefix
      file_raw <- paste0(prefix, "_raw_", enrollment_id, ".qs2")
      file_imp <- paste0(prefix, "_imp_", enrollment_id, ".qs2")
      file_analysis <- paste0(prefix, "_analysis_", ett_id, ".qs2")
      file_analysis_itt <- paste0(prefix, "_analysis_itt_", ett_id, ".qs2")

      new_row <- data.table::data.table(
        enrollment_id = enrollment_id,
        ett_id = ett_id,
        age_group = age_group,
        age_min = age_min,
        age_max = age_max,
        follow_up = follow_up,
        outcome_var = outcome_var,
        outcome_name = outcome_name,
        outcome_description = outcome_description,
        outcome_role = outcome_role,
        description = description,
        file_raw = file_raw,
        file_imp = file_imp,
        file_analysis = file_analysis,
        file_analysis_itt = file_analysis_itt,
        confounder_vars = list(confounder_vars),
        subgroup_vars = list(subgroup_vars),
        person_id_var = person_id_var,
        treatment_var = treatment_var,
        time_treatment_var = tv_intervention,
        eligible_var = elig,
        observed_var = list(observed_var),
        intervention_tolerance_weeks = intervention_tolerance_weeks,
        comparator_tolerance_weeks = comparator_tolerance_weeks
      )

      if (is.null(self$ett)) {
        self$ett <- new_row
      } else {
        self$ett <- data.table::rbindlist(
          list(self$ett, new_row),
          use.names = TRUE,
          fill = TRUE
        )
      }
      invisible(self)
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
    },

    #' @description Extract enrollment spec for the i-th enrollment_id group.
    #'
    #' @param i Integer index (1-based).
    #' @return A list with:
    #'   \describe{
    #'     \item{design}{A [TTEDesign] object with column mappings. It carries
    #'       the observation encoding and both arm tolerances that the spec
    #'       declared for this enrollment.}
    #'     \item{enrollment_id}{Character, the enrollment group ID}
    #'     \item{age_range}{Numeric vector of length 2: c(min, max)}
    #'     \item{n_threads}{Integer, number of data.table threads to use}
    #'     \item{treatment_impl}{List with variable, intervention_value, comparator_value
    #'       (present when plan was built from a spec)}
    #'     \item{matching_ratio}{Numeric, e.g. 2 for 1:2 matching
    #'       (present when plan was built from a spec)}
    #'     \item{seed}{Integer for reproducible matching
    #'       (present when plan was built from a spec)}
    #'   }
    enrollment_spec = function(i = 1L) {
      enrollment_ids <- unique(self$ett$enrollment_id)
      eid <- enrollment_ids[i]
      rows <- self$ett[self$ett$enrollment_id == eid]
      first <- rows[1]

      x_person_id <- first$person_id_var
      x_time_treatment <- first$time_treatment_var
      if (is.na(x_time_treatment)) {
        x_time_treatment <- NULL
      }
      x_eligible <- first$eligible_var
      if (is.na(x_eligible)) {
        x_eligible <- NULL
      }
      # A plan saved before the observation contract has no such column, so
      # read it defensively. A missing column means "not declared", which is
      # what a pre-landmark plan meant.
      x_observed <- if ("observed_var" %in% names(self$ett)) {
        first$observed_var[[1]]
      } else {
        NULL
      }
      x_tol_intervention <- if (
        "intervention_tolerance_weeks" %in% names(self$ett)
      ) {
        first$intervention_tolerance_weeks
      } else {
        0L
      }
      x_tol_comparator <- if ("comparator_tolerance_weeks" %in% names(self$ett)) {
        first$comparator_tolerance_weeks
      } else {
        0L
      }

      result <- list(
        design = TTEDesign$new(
          person_id_var = x_person_id,
          treatment_var = first$treatment_var,
          time_treatment_var = x_time_treatment,
          eligible_var = x_eligible,
          observed_var = x_observed,
          intervention_tolerance_weeks = x_tol_intervention,
          comparator_tolerance_weeks = x_tol_comparator,
          outcome_vars = rows$outcome_var,
          confounder_vars = first$confounder_vars[[1]],
          subgroup_vars = if ("subgroup_vars" %in% names(self$ett)) {
            first$subgroup_vars[[1]]
          } else {
            NULL
          },
          follow_up_time = as.integer(max(rows$follow_up)),
          admin_censor_isoyearweek = self$global_max_isoyearweek,
          period_width = self$period_width
        ),
        enrollment_id = eid,
        age_range = c(first$age_min, first$age_max),
        n_threads = .safe_n_cores()
      )

      # Pass through spec-derived fields if present in ETT
      if ("treatment_impl" %in% names(self$ett)) {
        result$treatment_impl <- first$treatment_impl[[1]]
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
    #'
    #' Uses a two-pass pipeline to fix cross-batch matching ratio imbalance.
    #' Requires `self$spec` to be set (e.g., via
    #' [tteplan_from_spec_and_registrystudy()]).
    #'
    #' \enumerate{
    #'   \item **Pass 1a (scout)**: Lightweight parallel pass that reads each
    #'     skeleton file, applies exclusions and treatment, and returns eligible
    #'     `(person_id, trial_id, intervention)` tuples. No confounders or enrollment.
    #'   \item **Centralized matching**: Combines all tuples from all batches,
    #'     then per `trial_id` keeps all intervention and samples
    #'     `ratio * n_intervention` comparator globally. Stores counts on
    #'     `self$enrollment_counts` for TARGET Item 8 reporting.
    #'   \item **Pass 1b (full enrollment)**: Parallel pass that re-reads each
    #'     skeleton file with full processing (exclusions + confounders +
    #'     treatment), then enrolls using pre-matched IDs (skipping per-batch
    #'     matching). Produces panel-expanded TTEEnrollment objects.
    #' }
    #'
    #' @param output_dir Optional directory override for output files. If
    #'   `NULL` (default), uses `self$dir_tteplan`.
    #' @param impute_fn Imputation callback or NULL (default:
    #'   [tteenrollment_impute_confounders]). swereg calls it with the panel and
    #'   with the `.tte_entry__` snapshot names, not with the plain confounder
    #'   names. It MUST impute only the columns it is given.
    #' @param stabilize Logical, stabilize IPW (default: TRUE).
    #' @param n_workers Integer, concurrent subprocesses. Default
    #'   [default_n_workers]`("s1")` (1 unless `SWEREG_N_WORKERS_S1` is set).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    s1_generate_enrollments_and_ipw = function(
      output_dir = NULL,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = default_n_workers("s1"),
      swereg_dev_path = NULL
    ) {
      # Validate FIRST, before any self$ mutation or filesystem work. A bad
      # count used to error only after self$output_dir had already been
      # overwritten, leaving the plan half-changed.
      n_workers <- .validate_n_workers(n_workers, "s1_generate_enrollments_and_ipw()")
      if (is.null(output_dir)) {
        output_dir <- self$dir_tteplan
      }
      # All-subprocess s1 dispatcher. The main R process holds only paths,
      # status flags, and progressors -- never a data.table. Four sub-steps
      # (s1a..s1d) communicate via files in
      #   {study$data_meta_dir}/s1_work/{project_prefix}/
      # which is removed on success. See "s1 work directory + path
      # constructors" above for the file-naming contract.
      #
      # Sub-step    Mode                                 Target
      # --------    ----                                 ------
      # s1a         parallel x skeleton                  .s1a_worker_multi()
      # s1b         single x enrollment                  .s1b_worker()
      # s1c         parallel x (enrollment x skeleton)   .s1c_worker()
      # s1d         single x enrollment                  .s1d_worker()
      # All four sub-steps dispatch through .batch_run_and_write(), which
      # commits each item's declared output paths atomically -- all of them,
      # or none. s1b/s1c use style = "return" (the worker returns the objects,
      # batchit serializes them). s1a and s1d use style = "staged_writer" (the
      # worker writes each output itself via .batch_where_to_write_output()):
      #   * s1a because one item writes 2 x n_enrollments files streamed
      #     inside a loop, and holding them all to return at the end would put
      #     every (tuples, attrition) chunk in RAM at once; and
      #   * s1d because its two outputs are two STATES of one by-reference
      #     object and cannot be returned together -- see the s1d dispatch
      #     below.
      # In both staged_writer cases the parent declares every path and the
      # worker names outputs only, so a parent/worker drift is a loud child
      # failure rather than a file written where nothing will read it.
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
      }
      if (is.null(self$spec)) {
        stop(
          "plan has no spec. ",
          "Create the plan with tteplan_from_spec_and_registrystudy()."
        )
      }
      # Declared-output paths must be ABSOLUTE -- batchit's atomic commit
      # rejects a relative `outputs` entry. Create the directory BEFORE
      # normalizing: normalizePath(mustWork = FALSE) returns an absolute path
      # for a path that exists, but returns a non-existent relative path
      # UNCHANGED, so normalizing too early fails silently. Same precedent as
      # `outpaths` in R/r6_registrystudy.R. `output_dir` itself is left alone:
      # it is persisted to self$output_dir just below and s3_analyze falls back
      # to that field, so normalizing it would change what a saved plan reports
      # across a save/load.
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      }
      out_abs <- normalizePath(output_dir, mustWork = FALSE)
      if (!grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", out_abs)) {
        stop(
          "s1_generate_enrollments_and_ipw(): output_dir did not resolve to an ",
          "absolute path (declared outputs must be absolute): ",
          out_abs,
          call. = FALSE
        )
      }

      self$output_dir <- output_dir
      spec <- self$spec

      ett <- self$ett
      files <- self$skeleton_files
      skel_basenames <- basename(files)
      n_threads <- .threads_per_worker(n_workers)

      # Per-enrollment summary (one row per enrollment_id).
      ett_loop1 <- ett[,
        .(
          max_follow_up = max(follow_up),
          age_grp = age_group[1],
          file_raw = file_raw[1],
          file_imp = file_imp[1]
        ),
        by = enrollment_id
      ]
      n_enr <- nrow(ett_loop1)

      cat(sprintf(
        "Creating enrollment files: %d enrollment(s) x %d skeleton files\n",
        n_enr,
        length(files)
      ))

      # Pre-build enrollment_spec objects once (used by all sub-steps).
      all_es <- lapply(seq_len(n_enr), function(i) {
        es <- self$enrollment_spec(i)
        es$n_threads <- n_threads
        es
      })
      enrollment_ids <- ett_loop1$enrollment_id

      work_dir <- .s1_work_dir(self, ensure_exists = FALSE)
      # The work directory is transient dataflow between the four sub-steps,
      # cleared at the start of every run and removed on success. Nothing here
      # persists across runs (Phase 5': s1 has no resume).
      if (dir.exists(work_dir)) {
        unlink(work_dir, recursive = TRUE, force = TRUE)
        if (dir.exists(work_dir)) {
          stop(
            "Could not clear the s1 work directory: ",
            work_dir,
            "\nRemove it by hand and re-run.",
            call. = FALSE
          )
        }
      }
      dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
      cat(sprintf("Work directory: %s\n", work_dir))

      # Restore enrollment_counts from sidecar files on disk (idempotent).
      if (is.null(self$enrollment_counts)) {
        self$enrollment_counts <- list()
      }
      .restore_enrollment_counts(self, output_dir, enrollment_ids)

      # The four sub-steps below each create their progressor right before
      # they run so the handler's "active" bar matches the current phase.

      # ====================================================================
      # s1a -- per skeleton (parallel)
      # ====================================================================
      cat(sprintf(
        "\n[s1a] Eligibility + attrition + tuples + caches (per skeleton, parallel x %d):\n",
        n_workers
      ))
      cat(sprintf(
        "      reading %d canonical skeleton(s) ONCE each across %d enrollments\n",
        length(files),
        n_enr
      ))
      p_s1a <- progressr::progressor(steps = length(files))
      s1a_items <- lapply(seq_along(files), function(j) {
        list(
          file_path = files[j],
          enrollment_specs = all_es,
          spec = spec
        )
      })
      # Stable ids: the skeleton each scout reads, prefixed with the sub-step
      # so a failure among all four Loop 1 dispatches says which one died.
      names(s1a_items) <- paste0("s1a_", skel_basenames)
      # Every file this item will write, declared here and NOWHERE ELSE: 2 x
      # n_enrollments per skeleton. `work_dir` is absolute (.s1_work_dir()),
      # which batchit's atomic commit requires. The worker never sees
      # `work_dir` -- it asks for these names back through
      # .batch_where_to_write_output().
      s1a_outputs <- lapply(skel_basenames, function(bn) {
        .s1a_outputs_for_skeleton(work_dir, enrollment_ids, bn)
      })
      names(s1a_outputs) <- names(s1a_items)
      if (length(s1a_items) > 0L) {
        .batch_run_and_write(
          target = .batch_target("swereg", ".s1a_worker_multi"),
          items = s1a_items,
          outputs = s1a_outputs,
          style = "staged_writer",
          n_workers = n_workers,
          dev_path = swereg_dev_path,
          p = p_s1a,
          label = "s1a"
        )
      }
      rm(s1a_items, s1a_outputs)

      # ====================================================================
      # s1b -- per enrollment (single subworker each, run sequentially)
      # ====================================================================
      cat(sprintf(
        "\n[s1b] Match comparators (per enrollment, single subworker x %d)\n",
        n_enr
      ))
      p_s1b <- progressr::progressor(steps = n_enr)
      for (i in seq_len(n_enr)) {
        eid <- enrollment_ids[i]
        counts_path <- .enrollment_counts_path(
          output_dir,
          self$project_prefix,
          eid
        )
        id <- sprintf("s1b_%s", eid)
        s1b_items <- list(list(
          enrollment_spec = all_es[[i]],
          spec = spec,
          work_dir = work_dir,
          skel_basenames = skel_basenames
        ))
        names(s1b_items) <- id
        # The two objects the worker's return value commits to. They live in
        # DIFFERENT directories -- enrolled_ids in work_dir (transient input
        # to s1c), counts in output_dir (the sidecar the master reads back
        # below) -- which batchit's atomic commit handles as one set. The
        # declared counts path is built from `out_abs`, not `output_dir`,
        # because batchit rejects a relative declared output; it names the
        # same file the read-back below opens via `counts_path`.
        s1b_outputs <- list(c(
          enrolled_ids = .s1b_enrolled_ids_path(work_dir, eid),
          counts = .enrollment_counts_path(out_abs, self$project_prefix, eid)
        ))
        names(s1b_outputs) <- id
        .batch_run_and_write(
          target = .batch_target("swereg", ".s1b_worker"),
          items = s1b_items,
          outputs = s1b_outputs,
          style = "return",
          n_workers = 1L,
          dev_path = swereg_dev_path,
          p = p_s1b,
          label = "s1b"
        )
        # Surface the matching/attrition counts to the plan object.
        if (file.exists(counts_path)) {
          self$enrollment_counts[[eid]] <- qs2_read(counts_path)
        }
      }

      # ====================================================================
      # s1c -- per (enrollment, skeleton) (parallel)
      # ====================================================================
      cat(sprintf(
        "\n[s1c] Build panels (per enrollment x per skeleton, parallel x %d)\n",
        n_workers
      ))
      s1c_steps <- n_enr * length(files)
      s1c_items <- list()
      s1c_outputs <- list()
      for (i in seq_len(n_enr)) {
        eid <- enrollment_ids[i]
        es <- all_es[[i]]
        for (j in seq_along(files)) {
          # Named at construction: the id ("s1c_<enrollment>__<skeleton>") is
          # what a failure among 39k panel builds reports, so it must say
          # exactly which (enrollment, skeleton) pair died, and which stage.
          id <- sprintf("s1c_%s__%s", eid, skel_basenames[j])
          s1c_items[[id]] <- list(
            enrollment_spec = es,
            file_path = files[j],
            spec = spec,
            work_dir = work_dir
          )
          # The panel chunk the worker's return value commits to. `work_dir` is
          # absolute (.s1_work_dir()), which batchit's atomic commit requires.
          s1c_outputs[[id]] <- c(
            panel = .s1c_panel_path(work_dir, eid, skel_basenames[j])
          )
        }
      }
      p_s1c <- progressr::progressor(steps = s1c_steps)
      if (length(s1c_items) > 0L) {
        .batch_run_and_write(
          target = .batch_target("swereg", ".s1c_worker"),
          items = s1c_items,
          outputs = s1c_outputs,
          style = "return",
          n_workers = n_workers,
          dev_path = swereg_dev_path,
          p = p_s1c,
          label = "s1c"
        )
      }
      rm(s1c_items, s1c_outputs)

      # ====================================================================
      # s1d -- per enrollment (single subworker each, run sequentially)
      # ====================================================================
      cat(sprintf(
        "\n[s1d] Combine + impute + IPW + save (per enrollment, single subworker x %d)\n",
        n_enr
      ))
      p_s1d <- progressr::progressor(steps = n_enr)
      for (i in seq_len(n_enr)) {
        eid <- enrollment_ids[i]
        id <- sprintf("s1d_%s", eid)
        s1d_items <- list(list(
          enrollment_spec = all_es[[i]],
          spec = spec,
          work_dir = work_dir,
          skel_basenames = skel_basenames,
          impute_fn = impute_fn,
          stabilize = stabilize
        ))
        names(s1d_items) <- id
        # Declared-output commit, `staged_writer` style. The worker writes
        # each of its two outputs to .batch_where_to_write_output("raw" /
        # "imp") -- staging paths in the final directories -- and batchit
        # renames BOTH into place only once the item has returned. The two
        # writes are separated by imputation, IPW estimation and weight
        # truncation on a multi-GB panel, i.e. minutes; before this, a crash
        # in that window left `file_raw` committed with `file_imp` absent, and
        # nothing downstream could tell.
        #
        # `style = "return"` WOULD BE INCORRECT HERE, not merely slower. DO
        # NOT "simplify" this. TTEEnrollment is R6 wrapping a data.table, and
        # `$s2_ipw()` mutates that data.table BY REFERENCE
        # (R/r6_tteenrollment.R). So a returned `list(raw = trial, imp =
        # trial)` would be two references to the SAME post-mutation object,
        # and `file_raw` would silently contain the imputed, IPW'd panel
        # instead of the raw one. `$clone(deep = TRUE)` does not rescue it
        # either: TTEEnrollment defines no `deep_clone` private method, so R6
        # copies the binding, not the data.table.
        s1d_outputs <- list(c(
          raw = file.path(out_abs, ett_loop1$file_raw[i]),
          imp = file.path(out_abs, ett_loop1$file_imp[i])
        ))
        names(s1d_outputs) <- id
        .batch_run_and_write(
          target = .batch_target("swereg", ".s1d_worker"),
          items = s1d_items,
          outputs = s1d_outputs,
          style = "staged_writer",
          n_workers = 1L,
          dev_path = swereg_dev_path,
          p = p_s1d,
          label = "s1d"
        )
      }

      # All sub-steps complete -- remove the work directory.
      unlink(work_dir, recursive = TRUE, force = TRUE)
      cat(sprintf("\nRemoved work directory: %s\n", work_dir))
      invisible(self)
    },

    #' @description Loop 2: Per-ETT IPCW-PP calculation and analysis file generation.
    #' For each ETT, loads the imputed enrollment file, calls
    #' `$s4_prepare_for_analysis()` (outcome + IPCW-PP + weight combination +
    #' truncation), and saves the analysis-ready file.
    #' @param output_dir Optional directory override containing imp files and
    #'   where analysis files are saved. If `NULL` (default), uses
    #'   `self$dir_tteplan`.
    #' @param estimate_ipcw_pp_separately_by_treatment Logical, estimate IPCW-PP
    #'   separately by treatment group (default: TRUE).
    #' @param estimate_ipcw_pp_with_gam Logical, use GAM for IPCW-PP estimation
    #'   (default: TRUE).
    #' @param n_workers Integer, concurrent subprocesses (default: 1L).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    s2_generate_analysis_files_and_ipcw_pp = function(
      output_dir = NULL,
      estimate_ipcw_pp_separately_by_treatment = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      n_workers = 1L,
      swereg_dev_path = NULL
    ) {
      # Validate FIRST, before any filesystem work.
      n_workers <- .validate_n_workers(n_workers, "s2_generate_analysis_files_and_ipcw_pp()")
      if (is.null(output_dir)) {
        output_dir <- self$dir_tteplan
      }
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
      }

      ett <- self$ett
      n_threads <- .threads_per_worker(n_workers)

      # Declared-output paths must be ABSOLUTE -- see the same block in
      # s1_generate_enrollments_and_ipw(). Create the directory BEFORE
      # normalizing, because normalizePath(mustWork = FALSE) leaves a
      # non-existent relative path relative. `output_dir` itself is untouched.
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      }
      out_abs <- normalizePath(output_dir, mustWork = FALSE)
      if (!grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", out_abs)) {
        stop(
          "s2_generate_analysis_files_and_ipcw_pp(): output_dir did not resolve ",
          "to an absolute path (declared outputs must be absolute): ",
          out_abs,
          call. = FALSE
        )
      }

      sep_by_tx <- estimate_ipcw_pp_separately_by_treatment
      with_gam <- estimate_ipcw_pp_with_gam

      # Each ETT yields two analysis files off the same file_imp: per-protocol
      # (file_analysis, with IPCW) and intention-to-treat (file_analysis_itt,
      # no switch censoring, no IPCW). Old grids without the file_analysis_itt
      # column fall back to deriving the path from file_analysis.
      itt_path <- function(i) {
        if (
          "file_analysis_itt" %in%
            names(ett) &&
            !is.na(ett$file_analysis_itt[i])
        ) {
          ett$file_analysis_itt[i]
        } else {
          sub(
            "_analysis_",
            "_analysis_itt_",
            ett$file_analysis[i],
            fixed = TRUE
          )
        }
      }
      items <- list()
      outputs <- list()
      for (i in seq_len(nrow(ett))) {
        base <- list(
          outcome = ett$outcome_var[i],
          follow_up = ett$follow_up[i],
          file_imp_path = file.path(output_dir, ett$file_imp[i]),
          n_threads = n_threads,
          sep_by_tx = sep_by_tx,
          with_gam = with_gam
        )
        items[[length(items) + 1L]] <- c(base, list(estimand = "pp"))
        outputs[[length(outputs) + 1L]] <- c(
          analysis = file.path(out_abs, ett$file_analysis[i])
        )
        items[[length(items) + 1L]] <- c(base, list(estimand = "itt"))
        outputs[[length(outputs) + 1L]] <- c(
          analysis = file.path(out_abs, itt_path(i))
        )
      }
      # Stable ids: the analysis file each item commits, `s2_`-prefixed so a
      # batchit failure message names the stage as well as the file. Unique by
      # construction (PP and ITT commit different files).
      ids <- paste0(
        "s2_",
        vapply(outputs, function(o) basename(o[["analysis"]]), character(1))
      )
      names(items) <- ids
      names(outputs) <- ids

      cat(sprintf(
        "Loop 2: Building per-ETT analysis files - PP (IPCW) + ITT (%d file(s), %d worker(s), %d threads each)\n",
        length(items),
        n_workers,
        n_threads
      ))

      p <- progressr::progressor(steps = length(items))
      .batch_run_and_write(
        target = .batch_target("swereg", ".s2_worker"),
        items = items,
        outputs = outputs,
        style = "return",
        n_workers = n_workers,
        dev_path = swereg_dev_path,
        p = p
      )
    },

    #' @description Loop 3: Compute all analysis results and store on the plan.
    #'
    #' For each enrollment: loads one analysis file and the raw file, computes
    #' baseline characteristics (raw, unweighted, IPW, IPW truncated).
    #' For each ETT: loads the analysis file, computes rates, IRR, and
    #' heterogeneity test with both truncated and untruncated weights.
    #'
    #' Every ETT also gets the ABSOLUTE scale, and nothing switches it off.
    #' Two estimand and weight combinations carry it: per-protocol on
    #' `analysis_weight_pp_trunc`, stored under `rd_pp_trunc`, and
    #' intention-to-treat on `ipw_trunc`, stored under `rd_itt`. Each stores
    #' one summary row at the end of follow-up, with `rd`, `rd_lo`, `rd_hi`,
    #' `nnt`, `nnt_lo`, `nnt_hi`, `nnt_direction` and `interval_status`. Each
    #' also stores the full band-by-band curve under `rd_curve_pp_trunc` or
    #' `rd_curve_itt`, with `surv_comparator` and `surv_intervention` beside the
    #' risk difference.
    #'
    #' The curve also carries `n_persons_at_risk_comparator` and
    #' `n_persons_at_risk_intervention`. Each is a head count of distinct people
    #' in that arm and band. It is the count a numbers-at-risk row reports. The
    #' figure reads it rather than opening the analysis file again.
    #'
    #' The bootstrap runs at 500 replicates with seed 1. Both are fixed here.
    #' The confidence level is a STUDY property, read from
    #' `spec$study$implementation$conf_level` and defaulting to 0.95. All three
    #' are recorded on every stored row. The export path formats those numbers
    #' and never recomputes them.
    #'
    #' Cost. Each risk difference is its own work item, so it is its own worker
    #' process with its own read of the analysis file. That is two more reads
    #' per ETT, or 1,080 more reads on a 540-ETT grid.
    #'
    #' Results are stored in `self$results_enrollment` and `self$results_ett`.
    #' Every targeted result is recomputed on each call (no skip cache). Use
    #' `plan$save()` to persist.
    #'
    #' @param enrollment_ids Character vector of enrollment IDs to analyze, or
    #'   `NULL` (default) for all.
    #' @param ett_ids Character vector of ETT IDs to analyze, or
    #'   `NULL` (default) for all.
    #' @param output_dir Optional directory override. If `NULL` (default),
    #'   uses `self$dir_tteplan` (falls back to the legacy `self$output_dir`
    #'   for plans created before the CandidatePath migration).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    #' @param n_workers Integer >= 1 (default `1L`). Number of concurrent
    #'   worker subprocesses for both the enrollment loop and the per-ETT
    #'   loop. Each worker reads its own analysis file fresh, so peak RAM
    #'   scales linearly with `n_workers`; on machines with multi-GB
    #'   analysis files, set this conservatively. CPU threads per worker
    #'   are auto-partitioned as `floor(detectCores() / n_workers)`.
    s3_analyze = function(
      enrollment_ids = NULL,
      ett_ids = NULL,
      output_dir = NULL,
      swereg_dev_path = NULL,
      n_workers = default_n_workers("s3")
    ) {
      # This checked >= 1 but never whole-ness, then as.integer()'d anyway -- so
      # s3_analyze(2.5) silently became 2 workers before parallel_pool() ever
      # saw the value.
      n_workers <- .validate_n_workers(n_workers, "s3_analyze()")
      if (is.null(output_dir)) {
        output_dir <- tryCatch(self$dir_tteplan, error = function(e) NULL)
      }
      if (is.null(output_dir)) {
        output_dir <- self$output_dir # legacy fallback
      }
      if (is.null(output_dir)) {
        stop(
          "output_dir is not set. Pass it as an argument, ",
          "configure dir_tteplan_cp, or run $s1_generate_enrollments_and_ipw() first."
        )
      }
      ett <- self$ett
      # The batch runner is thread-agnostic (each target calls setDTthreads()
      # itself), so the per-worker thread count is decided HERE, not injected by
      # the pool. Same value parallel_pool used to overwrite item n_threads
      # with, so runtime threading is unchanged. NB the ETT items below used to
      # say `n_threads = n_cores` and RELY on that overwrite -- carried
      # verbatim to .batch_run, that would have oversubscribed every worker.
      n_threads <- .threads_per_worker(n_workers)

      # Resolve enrollment IDs
      all_enrollment_ids <- unique(ett$enrollment_id)
      if (!is.null(enrollment_ids)) {
        bad <- setdiff(enrollment_ids, all_enrollment_ids)
        if (length(bad) > 0L) {
          stop("Unknown enrollment_ids: ", paste(bad, collapse = ", "))
        }
        all_enrollment_ids <- enrollment_ids
      }
      # When ett_ids is given, auto-narrow enrollments to only those needed
      if (!is.null(ett_ids)) {
        bad_ett <- setdiff(ett_ids, ett$ett_id)
        if (length(bad_ett) > 0L) {
          stop("Unknown ett_ids: ", paste(bad_ett, collapse = ", "))
        }
        ett_enrollment_ids <- unique(
          ett$enrollment_id[ett$ett_id %in% ett_ids]
        )
        all_enrollment_ids <- intersect(all_enrollment_ids, ett_enrollment_ids)
      }

      if (is.null(self$results_enrollment)) {
        self$results_enrollment <- list()
      }
      if (is.null(self$results_ett)) {
        self$results_ett <- list()
      }

      # Recompute everything in the targeted scope on every call: drop any
      # previously stored results for it, so the stores are pure output
      # containers, never a skip cache (Phase 5': the TTE stages hold no
      # staleness opinion; see PROJECT.md).
      if (is.null(enrollment_ids) && is.null(ett_ids)) {
        self$results_enrollment <- list()
        self$results_ett <- list()
      } else {
        for (eid in all_enrollment_ids) {
          self$results_enrollment[[eid]] <- NULL
        }
        # Drop exactly the ETTs that will be recomputed below (== ett_subset):
        # the ETTs under the targeted enrollments, further narrowed by ett_ids
        # if given. Using the raw `ett_ids` here would clear an ETT whose
        # enrollment is outside `all_enrollment_ids` -- dropped but never
        # recomputed, silently losing that result.
        drop_ett_ids <- ett$ett_id[ett$enrollment_id %in% all_enrollment_ids]
        if (!is.null(ett_ids)) {
          drop_ett_ids <- intersect(drop_ett_ids, ett_ids)
        }
        for (eid in drop_ett_ids) {
          self$results_ett[[eid]] <- NULL
        }
      }

      # --- Enrollment loop: baseline characteristics (subprocess-isolated) ---
      # Every targeted enrollment is recomputed (the scope was cleared above).
      enr_todo <- all_enrollment_ids

      # --- Build all work items for both loops ---
      # Enrollment items
      enr_items <- list()
      if (length(enr_todo) > 0L) {
        enr_items <- lapply(enr_todo, function(eid) {
          enr_rows <- ett[ett$enrollment_id == eid]
          analysis_files <- file.path(output_dir, enr_rows$file_analysis)
          sizes <- file.size(analysis_files)
          smallest <- which.min(sizes)
          list(
            analysis_path = analysis_files[smallest],
            raw_path = file.path(output_dir, enr_rows$file_raw[1]),
            enrollment_id = eid,
            n_threads = n_threads,
            arm_labels = .lookup_arm_labels(self$spec, eid)
          )
        })
        # Name the items by enrollment id so .batch_run uses those as stable ids:
        # a worker failure then reports the actual enrollment, not "item 1".
        names(enr_items) <- enr_todo
      }

      # ETT items
      ett_subset <- ett[ett$enrollment_id %in% all_enrollment_ids]
      if (!is.null(ett_ids)) {
        ett_subset <- ett_subset[ett_subset$ett_id %in% ett_ids]
      }
      ett_todo <- ett_subset
      n_ett <- nrow(ett_todo)

      # The study's confidence level, resolved ONCE and carried on every item.
      # It is a study property, not a per-figure one: s3 computes the interval
      # long before any figure exists.
      rd_conf_level <- .s3_conf_level(self$spec)

      all_items <- list()
      item_map <- list()
      if (n_ett > 0L) {
        for (i in seq_len(n_ett)) {
          apath <- file.path(output_dir, ett_todo$file_analysis[i])
          eid <- ett_todo$ett_id[i]
          # subgroup_var = NULL is EXPLICIT: the contract demands every formal,
          # including optional ones -- an optional arg silently absent is the
          # arm_labels bug's shape, and .batch_run rejects it.
          base <- list(
            analysis_path = apath, ett_id = eid, n_threads = n_threads,
            subgroup_var = NULL, conf_level = rd_conf_level
          )
          idx <- length(all_items)
          all_items[[idx + 1L]] <- c(
            base,
            list(
              method = "summary_and_rates",
              weight_col = ""
            )
          )
          item_map[[idx + 1L]] <- list(ett_i = i, slot = "summary_and_rates")

          all_items[[idx + 2L]] <- c(
            base,
            list(
              method = "irr",
              weight_col = "analysis_weight_pp_trunc"
            )
          )
          item_map[[idx + 2L]] <- list(ett_i = i, slot = "irr_pp_trunc")

          all_items[[idx + 3L]] <- c(
            base,
            list(
              method = "irr",
              weight_col = "analysis_weight_pp"
            )
          )
          item_map[[idx + 3L]] <- list(ett_i = i, slot = "irr_pp")

          # Intention-to-treat: read the ITT analysis file and weight on the
          # baseline IPW (ipw_trunc). Old grids without file_analysis_itt fall
          # back to deriving the path from the PP analysis path.
          itt_apath <- if (
            "file_analysis_itt" %in%
              names(ett_todo) &&
              !is.na(ett_todo$file_analysis_itt[i])
          ) {
            file.path(output_dir, ett_todo$file_analysis_itt[i])
          } else {
            sub("_analysis_", "_analysis_itt_", apath, fixed = TRUE)
          }
          all_items[[idx + 4L]] <- list(
            analysis_path = itt_apath,
            ett_id = eid,
            n_threads = n_threads,
            method = "irr",
            weight_col = "ipw_trunc",
            subgroup_var = NULL,
            conf_level = rd_conf_level
          )
          item_map[[idx + 4L]] <- list(ett_i = i, slot = "irr_itt")

          all_items[[idx + 5L]] <- list(
            analysis_path = itt_apath,
            ett_id = eid,
            n_threads = n_threads,
            method = "rates",
            weight_col = "ipw_trunc",
            subgroup_var = NULL,
            conf_level = rd_conf_level
          )
          item_map[[idx + 5L]] <- list(ett_i = i, slot = "rates_itt")

          # The absolute scale, for EVERY ETT and with nothing to switch it
          # off. Two estimand/weight combinations carry it: per-protocol on the
          # truncated weight, and intention-to-treat on the baseline IPW.
          # Per-protocol on the untruncated weight carries rates and the
          # incidence rate ratio only.
          #
          # It used to be computed in the export path, behind a figure option.
          # A production script that did not set the option drew every figure
          # without it, with no error and no warning. A quantity a figure can
          # switch off is a quantity a script can forget to ask for. So this
          # stage computes it. The export path only formats it.
          all_items[[idx + 6L]] <- c(
            base,
            list(
              method = "risk_difference",
              weight_col = "analysis_weight_pp_trunc"
            )
          )
          item_map[[idx + 6L]] <- list(ett_i = i, slot = "rd_pp_trunc")

          all_items[[idx + 7L]] <- list(
            analysis_path = itt_apath,
            ett_id = eid,
            n_threads = n_threads,
            method = "risk_difference",
            weight_col = "ipw_trunc",
            subgroup_var = NULL,
            conf_level = rd_conf_level
          )
          item_map[[idx + 7L]] <- list(ett_i = i, slot = "rd_itt")

          # Effect modification: for each subgroup variable, stratified IRRs
          # (irr_by_subgroup) and the interaction test (effect_modification_test)
          # for BOTH estimands -- PP (analysis_weight_pp_trunc) and ITT
          # (ipw_trunc). Old grids without subgroup_vars contribute nothing.
          sg_vars <- if (
            "subgroup_vars" %in%
              names(ett_todo) &&
              !is.null(ett_todo$subgroup_vars[[i]])
          ) {
            ett_todo$subgroup_vars[[i]]
          } else {
            character(0)
          }
          for (sv in sg_vars) {
            arms <- list(
              list(path = apath, weight = "analysis_weight_pp_trunc"),
              list(path = itt_apath, weight = "ipw_trunc")
            )
            for (arm in arms) {
              k <- length(all_items)
              all_items[[k + 1L]] <- list(
                analysis_path = arm$path,
                ett_id = eid,
                n_threads = n_threads,
                method = "irr_by_subgroup",
                weight_col = arm$weight,
                subgroup_var = sv,
                conf_level = rd_conf_level
              )
              item_map[[k + 1L]] <- list(ett_i = i, slot = "subgroup")
              all_items[[k + 2L]] <- list(
                analysis_path = arm$path,
                ett_id = eid,
                n_threads = n_threads,
                method = "effect_modification_test",
                weight_col = arm$weight,
                subgroup_var = sv,
                conf_level = rd_conf_level
              )
              item_map[[k + 2L]] <- list(ett_i = i, slot = "emtest")
            }
          }
        }
        # Stable ids: one per (ETT, analysis call), so a worker failure names
        # the exact analysis ("e01_f32_104w_45__irr__analysis_weight_pp"), not
        # "item 371". Unique by construction: weight_col separates the PP IRRs
        # from each other and from ITT; subgroup_var separates the stratified
        # calls. .batch_run stops on any collision rather than papering over it.
        names(all_items) <- vapply(
          all_items,
          function(it) {
            paste(
              c(
                it$ett_id,
                it$method,
                if (nzchar(it$weight_col)) it$weight_col,
                it$subgroup_var
              ),
              collapse = "__"
            )
          },
          character(1)
        )
      }

      # Total steps across both loops
      total_steps <- length(enr_items) + length(all_items)
      message("Output dir: ", output_dir)
      n_files <- length(list.files(output_dir, pattern = "\\.qs2$"))
      message(sprintf("  %d .qs2 files found", n_files))
      # The call count is REPORTED, not asserted. It was the literal "5"
      # while the builder emitted five items per ETT. A grid with a subgroup
      # variable takes four more items per variable, so the literal was
      # already wrong there.
      cat(sprintf(
        "Analyzing: %d enrollment(s) + %d ETTs (%d analysis calls, PP + ITT)\n",
        length(enr_items),
        n_ett,
        length(all_items)
      ))

      p <- progressr::progressor(steps = total_steps)

      # --- Enrollment loop ---
      if (length(enr_items) > 0L) {
        # Both s3 loops go through the ONE generic runner. The generic worker
        # do.call()s the target with EVERY named formal, which is what makes
        # the arm_labels class-of-bug (an optional formal silently dropped by a
        # hand-written dispatch script) structurally impossible here.
        enr_results <- .batch_run(
          target = .batch_target("swereg", ".s3_enrollment_worker"),
          items = enr_items,
          n_workers = n_workers,
          dev_path = swereg_dev_path,
          p = p
        )

        for (i in seq_along(enr_todo)) {
          self$results_enrollment[[enr_todo[i]]] <- enr_results[[i]]
        }
        rm(enr_results)
      }

      # --- ETT loop ---
      if (length(all_items) > 0L) {
        all_results <- .batch_run(
          target = .batch_target("swereg", ".s3_ett_worker"),
          items = all_items,
          n_workers = n_workers,
          dev_path = swereg_dev_path,
          p = p
        )

        # Assemble per-ETT results from the flat list
        for (j in seq_along(all_results)) {
          m <- item_map[[j]]
          eid <- ett_todo$ett_id[m$ett_i]
          if (is.null(self$results_ett[[eid]])) {
            self$results_ett[[eid]] <- list(
              enrollment_id = ett_todo$enrollment_id[m$ett_i],
              description = ett_todo$description[m$ett_i],
              computed_at = Sys.time()
            )
          }
          for (k in names(all_results[[j]])) {
            self$results_ett[[eid]][[k]] <- all_results[[j]][[k]]
          }
        }
        rm(all_results)
      }

      invisible(self)
    },

    #' @description Print a diagnostic summary of stored results.
    #'
    #' Shows one row per ETT with enrollment, event count, and whether
    #' IRR/rates computed successfully.
    #'
    #' This method reads `self$results_ett` directly, and it is the one
    #' DIAGNOSTIC exception to the rule that every consumer reads an accessor.
    #' A tool that reports ABSENCE cannot read through an interface that hides
    #' absence. The accessors report a missing slot and a skipped slot the same
    #' way, as absent rows or as `NA`. They expose no skip envelope and no
    #' failure reason. This method prints exactly three states. `"NULL"` names
    #' a slot the plan does not hold. `"SKIP: <reason>"` names a worker that
    #' failed. `"OK"` names a stored result.
    #'
    #' It reports on the CACHE and never on a number. A caller that wants the
    #' numbers calls `$get_estimates()`.
    results_summary = function() {
      if (is.null(self$results_ett) || length(self$results_ett) == 0L) {
        cat("No ETT results stored. Run $s3_analyze() first.\n")
        return(invisible(self))
      }

      rows <- lapply(names(self$results_ett), function(ett_id) {
        r <- self$results_ett[[ett_id]]
        n_events <- if (!is.null(r$summary)) r$summary$n_events else NA
        irr_status <- if (is.null(r$irr_pp_trunc)) {
          "NULL"
        } else if (isTRUE(r$irr_pp_trunc$skipped)) {
          paste0("SKIP: ", r$irr_pp_trunc$reason)
        } else {
          "OK"
        }
        rates_status <- if (is.null(r$rates_pp_trunc)) {
          "NULL"
        } else if (isTRUE(r$rates_pp_trunc$skipped)) {
          "SKIP"
        } else {
          "OK"
        }
        data.table::data.table(
          enrollment = r$enrollment_id,
          ett_id = ett_id,
          description = r$description,
          n_events = n_events,
          irr = irr_status,
          rates = rates_status
        )
      })
      dt <- data.table::rbindlist(rows)
      print(dt, nrows = Inf)

      # Enrollment summary
      if (!is.null(self$results_enrollment)) {
        cat(sprintf(
          "\nEnrollment results: %d/%d computed\n",
          length(self$results_enrollment),
          length(unique(self$ett$enrollment_id))
        ))
      }
      invisible(self)
    },

    #' @description Every stored effect estimate, as one flat table.
    #'
    #' One row per emulated trial, estimand and weighting.
    #'
    #' `estimand` and `weights` are two columns, not one. `estimand` reads
    #' `"pp"` or `"itt"`. `weights` reads `"truncated"` or `"untruncated"` and
    #' names the weighting choice inside per-protocol. Three combinations
    #' occur: per-protocol truncated, per-protocol untruncated, and
    #' intention-to-treat.
    #'
    #' Three rows per emulated trial is an UPPER BOUND, not a promise. A
    #' combination gets a row when the plan holds at least one of its rates,
    #' incidence rate ratio and risk-difference slots. A combination the plan
    #' holds nothing for gets no row. So a complete 540-trial grid returns 1,620
    #' rows, and a partial one returns fewer.
    #'
    #' The method computes nothing. It reads `plan$results_ett`, and it joins
    #' the labels from `plan$ett` and `plan$spec`. A slot the plan does not
    #' carry gives `NA` in that slot's columns. The method MUST NOT fill the
    #' gap from a neighbouring slot.
    #'
    #' `irr_estimable` is READ, not decided. `$s3_analyze()` decides it beside
    #' the ratio and stores it. A result stored before that column existed gives
    #' `NA`, and the method MUST NOT apply the rule to fill the gap.
    #'
    #' Every number is a bare number. `irr_pvalue` is a probability, not
    #' `"<0.001"`. `rd` is a proportion, not a rate per 10,000. The consumer
    #' formats it.
    #'
    #' Five sibling methods return the other stored results in the same shape:
    #' `$get_curves()`, `$get_baselines()`, `$get_attrition()`,
    #' `$get_matching()` and `$get_subgroups()`. Each takes no argument, and
    #' each computes nothing.
    #'
    #' The number needed to treat carries its interval. `nnt` is the point
    #' estimate, and `nnt_lo` and `nnt_hi` are the bounds `$s3_analyze()`
    #' stored. Both bounds are `NA` where `interval_status` reads
    #' `"spans null"`, because the reciprocal of an interval that contains zero
    #' is not an interval. A consumer MUST NOT invert `rd_lo` and `rd_hi`
    #' itself, and MUST NOT print `nnt` alone where the bounds are missing.
    #'
    #' @return A data.table with 41 columns. The identifiers come first, then
    #'   the weighted counts, then the incidence rate ratio, then the risk
    #'   difference and the number needed to treat. `n_boot`, `seed` and
    #'   `conf_level` record what produced the risk-difference interval.
    get_estimates = function() {
      .acc_estimates(self)
    },

    #' @description Every stored survival curve, as one flat table.
    #'
    #' One row per emulated trial, estimand, weighting, arm and band.
    #' `$s3_analyze()` stores one wide curve per estimand, with a survival
    #' column for each arm. This method returns one row per arm instead.
    #'
    #' The table carries the numbers at risk beside survival.
    #' `n_persons_at_risk` is an unweighted count of distinct people, per arm
    #' per band. `$s3_analyze()` stores it and this method melts it. A risk
    #' table reports people, so it cannot be derived from `surv`, which is a
    #' weighted probability.
    #'
    #' A curve stored before that column existed gives `NA`. A consumer that
    #' draws a risk table MUST check for missing values first. It MUST refuse to
    #' draw. A row of missing counts looks like a drawn risk table.
    #'
    #' @return A data.table with columns `ett_id`, `estimand`, `weights`,
    #'   `arm`, `band`, `surv` and `n_persons_at_risk`.
    get_curves = function() {
      .acc_curves(self)
    },

    #' @description Every stored baseline panel, as one flat table.
    #'
    #' One row per enrollment, panel and table row. Three columns identify the
    #' panel. `imputation` reads `"raw"` or `"imputed"`. `weighting` reads
    #' `"none"`, `"ipw"` or `"ipw_trunc"`. `variant` reads `"main"` or
    #' `"supplementary"`. Five combinations occur.
    #'
    #' The `"raw"` panel needs a separate pre-imputation file. The table holds
    #' no `"raw"` rows when the plan holds no such panel. The method MUST NOT
    #' present another panel under that name.
    #'
    #' `overall`, `comparator` and `intervention` are display strings, such as
    #' `"12.3 (4.5)"` or `"120 (8.1%)"`. The producer stores them that way.
    #' `smd_numeric` is the unrounded standardised mean difference.
    #'
    #' `variable` repeats on every row of its block. The stored panel prints
    #' the name once and indents its levels under it, so `variable` is blank
    #' there. A renderer that wants that indent MUST blank the repeat itself.
    #'
    #' @return A data.table. `n_baseline`, `n_baseline_intervention` and
    #'   `n_baseline_comparator` repeat that enrollment's counts on every row.
    get_baselines = function() {
      .acc_baselines(self)
    },

    #' @description The stored eligibility cascade, as one flat table.
    #'
    #' One row per enrollment and stored row, in pipeline order. Counts are
    #' remaining-after-step.
    #'
    #' `$s1_generate_enrollments_and_ipw()` stores one row per trial and
    #' criterion, plus ONE GLOBAL ROW per criterion. The global row carries the
    #' true overall count of distinct people. This method returns EVERY STORED
    #' ROW. `trial_id` is `NA` on a global row and the trial index on a
    #' per-trial row, so the caller filters on that column.
    #'
    #' The method returns the stored rows and nothing else. It does not sum the
    #' per-trial rows. It does not create a global row for a criterion that has
    #' none. A criterion with per-trial rows and no global row therefore yields
    #' per-trial rows and no global row.
    #'
    #' Collapsing to one row per criterion is a RENDERER's decision, and
    #' `.attrition_overall()` makes it. That renderer reads the global rows and
    #' nothing else. It returns NULL when one criterion carries no global row,
    #' and the enrollment then gets no attrition sheet and no CONSORT diagram.
    #' This method makes no such decision. It returns every stored row, and the
    #' renderer needs the per-trial rows to see a criterion that has only
    #' those.
    #'
    #' `step_order` is the position of the criterion in stored order, so every
    #' row of one criterion carries the same value.
    #'
    #' The table holds the ELIGIBILITY CASCADE only. It holds no matching step
    #' and no analysis step, because `$s1_generate_enrollments_and_ipw()` stores
    #' neither as a step. `.build_cohort_flow()` builds those two rows and
    #' derives the per-step change columns. Building a row is a renderer's job,
    #' so this method calls that builder nowhere.
    #'
    #' The table carries no step KIND, because nothing stores one. The first
    #' stored criterion is the cohort start and every later one is an exclusion.
    #' A consumer labels them from `step_order`, and this method decides
    #' nothing.
    #'
    #' @return A data.table with columns `enrollment_id`, `trial_id`,
    #'   `step_order`, `step_name`, `n_persons`, `n_person_trials`,
    #'   `n_arm_intervention` and `n_arm_comparator`.
    get_attrition = function() {
      .acc_attrition(self)
    },

    #' @description The stored matching counts, as one flat table.
    #'
    #' One row per enrollment and trial.
    #' `$s1_generate_enrollments_and_ipw()` stores it that way.
    #' `n_intervention_total` and `n_comparator_total` count every person-trial
    #' that was eligible for an arm. `n_intervention_enrolled` and
    #' `n_comparator_enrolled` count the person-trials the matcher took.
    #'
    #' This is a SIXTH method rather than four more columns on
    #' `$get_attrition()`. The matching table has one row per enrollment and
    #' trial. The attrition table has one row per enrollment, trial and
    #' criterion. Joining them would repeat one matching count on every
    #' criterion row, and report a grain that neither producer stored.
    #'
    #' The method computes nothing. It does not sum across trials, and it
    #' derives no enrolment ratio. `.build_cohort_flow()` sums the enrolled
    #' counts to build its matching step, and that sum is a renderer's.
    #'
    #' An enrollment that stored no matching table gets NO ROW.
    #'
    #' @return A data.table with columns `enrollment_id`, `trial_id`,
    #'   `n_intervention_total`, `n_comparator_total`,
    #'   `n_intervention_enrolled` and `n_comparator_enrolled`.
    get_matching = function() {
      .acc_matching(self)
    },

    #' @description Every stored stratified estimate, as one flat table.
    #'
    #' One row per emulated trial, estimand, weighting, subgroup variable and
    #' subgroup level. `subgroup_level` reads `"all"` on the whole-cohort row,
    #' and the level label on every other row.
    #'
    #' `subgroup_var` is part of the KEY, not a label. One emulated trial MAY
    #' carry several subgroup variables, and each one has its own `"all"` row.
    #'
    #' TWO p-values, and they answer different questions.
    #' \itemize{
    #'   \item `irr_pvalue` is the stratum's own p-value. Is this stratum's rate
    #'     ratio distinguishable from the null?
    #'   \item `em_pvalue` is the interaction test. Do the strata differ from
    #'     each other?
    #' }
    #' A consumer that renders one where the other belongs reports a different
    #' finding. The two never share a name.
    #'
    #' `em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi` come from the
    #' interaction test that `$s3_analyze()` stores. Each is one number for the
    #' whole stratified result, so each repeats on every row of that result. A
    #' renderer that wants them once shows them on the `"all"` row.
    #'
    #' `ratio_of_irrs` is the ratio of the two stratum rate ratios. It is `NA`
    #' unless the subgroup variable has exactly two levels.
    #'
    #' The method reads the UNION of two stored families. `$s3_analyze()`
    #' dispatches the stratified rate ratios and the interaction test as
    #' separate work items, in separate subprocesses, so either can fail alone.
    #' Four states occur.
    #' \itemize{
    #'   \item Both stored. Full rows.
    #'   \item Stratified only. One row per stored level, with all four
    #'     interaction columns `NA`.
    #'   \item Interaction only. ONE row, with `subgroup_level` reading `"all"`
    #'     and the four stratum columns `NA`. No stored table names the levels,
    #'     so the method MUST NOT invent a stratum row.
    #'   \item Neither stored. No rows, even when the specification names the
    #'     variable.
    #' }
    #' A skipped stratified result reads as absent.
    #'
    #' Coverage. Study 002 runs no stratified analysis, so this method is
    #' tested against a fixture. Other studies in the fleet do configure
    #' subgroups, so treat the schema as production.
    #'
    #' @return A data.table with 13 columns: `ett_id`, `estimand`, `weights`,
    #'   `subgroup_var`, `subgroup_level`, `irr`, `irr_lo`, `irr_hi`,
    #'   `irr_pvalue`, `em_pvalue`, `ratio_of_irrs`, `ratio_lo` and `ratio_hi`.
    get_subgroups = function() {
      .acc_subgroups(self)
    },

    #' @description Export the study specification to a standalone Excel file.
    #'
    #' Writes a formatted summary of the spec (design, criteria, confounders,
    #' outcomes, enrollments) with ICD-10/ATC code annotations from the code
    #' registry. No analysis results required.
    #'
    #' @param path Optional output path override. If `NULL` (default), writes
    #'   to `self$spec_xlsx` (that is, `spec_<version>.xlsx` inside
    #'   `self$dir_results`, where `<version>` is `self$spec_version`).
    #' @return `invisible(self)`
    excel_spec_summary = function(path = NULL) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop(
          "Package 'openxlsx' is required. Install with: install.packages('openxlsx')"
        )
      }
      if (is.null(self$spec)) {
        stop("Plan has no spec.")
      }
      if (is.null(path)) {
        path <- self$spec_xlsx
        dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      }
      wb <- openxlsx::createWorkbook()
      .write_spec_summary(wb, self)
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      message("Spec summary saved to: ", path)
      invisible(self)
    },

    #' @description Refresh cosmetic spec fields (enrollment names, treatment
    #' arm labels, outcome names, ETT descriptions) on a cached plan without
    #' re-running the upstream pipeline.
    #'
    #' Structural fields (confounders, exclusion criteria, follow-up windows,
    #' matching parameters, etc.) are *not* applied - they would invalidate
    #' the cached results. The differences are surfaced via a loud warning
    #' and recorded in `self$spec_reload_skipped_diffs`.
    #'
    #' @param spec_path Optional path to a `.yaml` study spec file. If `NULL`
    #'   (default), uses `self$spec_path` (resolved from `dir_spec_cp` +
    #'   `filename_spec(spec_version)`).
    #' @param quiet Logical, suppress the success message (default FALSE).
    #' @return `invisible(self)`.
    reload_spec = function(spec_path = NULL, quiet = FALSE) {
      if (is.null(self$spec)) {
        stop("This plan has no existing spec to reload against.")
      }
      if (is.null(spec_path)) {
        spec_path <- self$spec_path
      }
      new_spec <- tteplan_read_spec(spec_path)
      diffs <- .diff_specs(self$spec, new_spec)
      if (length(diffs$structural) > 0L) {
        warning(
          "Spec has structural changes that were NOT applied (cached results ",
          "are still bound to the old definitions):\n  ",
          paste(diffs$structural, collapse = "\n  "),
          call. = FALSE
        )
        self$spec_reload_skipped_diffs <- diffs$structural
      } else {
        self$spec_reload_skipped_diffs <- NULL
      }
      .apply_cosmetic_spec_updates(self, new_spec)
      self$spec_reloaded_at <- Sys.time()
      if (!quiet) {
        n_cosm <- length(diffs$cosmetic)
        message(
          "Spec reloaded: ",
          n_cosm,
          " cosmetic field(s) updated. Call $export_tables() to regenerate ",
          "the workbook with the new labels."
        )
      }
      invisible(self)
    },

    #' @description Recompute baseline characteristic tables in-process.
    #'
    #' Reads each enrollment's smallest analysis file (and the raw file when
    #' present) from disk and re-runs the new `swereg_table1` engine. Used to
    #' refresh stale results after upgrading swereg, without re-running the
    #' full `$s3_analyze()` pipeline.
    #'
    #' This is a PRODUCER, and the read is s3's. It calls
    #' `.s3_enrollment_worker()`, the same worker `$s3_analyze()` calls, and it
    #' stores what the worker returns. No renderer in the export path opens an
    #' analysis file.
    #'
    #' `$export_tables()` calls this method on its own when a stored panel is
    #' stale. Call it yourself when you want the refresh to be a visible step.
    #' The lazy path costs minutes. Whether it runs at all depends on what a
    #' cached plan happens to hold.
    #'
    #' @param output_dir Optional directory holding the `.qs2` files. Defaults
    #'   to `self$output_dir`.
    #' @param enrollment_ids Optional character vector. If NULL, refreshes
    #'   every enrollment in `self$results_enrollment`.
    #' @return `invisible(self)`.
    recompute_baselines = function(output_dir = NULL, enrollment_ids = NULL) {
      if (is.null(output_dir)) {
        output_dir <- self$output_dir
      }
      if (is.null(output_dir)) {
        stop("output_dir is not set. Pass it as an argument.")
      }
      if (
        is.null(self$results_enrollment) ||
          length(self$results_enrollment) == 0L
      ) {
        stop("No enrollment results to refresh.")
      }
      if (is.null(enrollment_ids)) {
        enrollment_ids <- names(self$results_enrollment)
      }
      ett <- self$ett
      for (eid in enrollment_ids) {
        enr_rows <- ett[ett$enrollment_id == eid]
        if (nrow(enr_rows) == 0L) {
          next
        }
        analysis_files <- file.path(output_dir, enr_rows$file_analysis)
        present <- file.exists(analysis_files)
        if (!any(present)) {
          warning("No analysis files found on disk for enrollment ", eid)
          next
        }
        analysis_files <- analysis_files[present]
        sizes <- file.size(analysis_files)
        smallest <- which.min(sizes)
        analysis_path <- analysis_files[smallest]
        raw_path <- file.path(output_dir, enr_rows$file_raw[1])
        new_result <- .s3_enrollment_worker(
          analysis_path = analysis_path,
          raw_path = raw_path,
          enrollment_id = eid,
          n_threads = data.table::getDTthreads(),
          arm_labels = .lookup_arm_labels(self$spec, eid)
        )
        # Preserve fields like n_baseline that came from the original run if
        # the worker returned NA (it shouldn't, but be defensive).
        prev <- self$results_enrollment[[eid]]
        if (!is.null(prev)) {
          for (k in setdiff(names(prev), names(new_result))) {
            new_result[[k]] <- prev[[k]]
          }
        }
        self$results_enrollment[[eid]] <- new_result
      }
      invisible(self)
    },

    #' @description Export analysis results to an Excel workbook.
    #'
    #' Requires `self$results_enrollment` and `self$results_ett` to be populated
    #' (run `$s3_analyze()` first).
    #'
    #' If the cached baseline tables were produced by an older version of
    #' `swereg` (when Table 1 was a `tableone` object), they are automatically
    #' refreshed in-process via `$recompute_baselines()` using the analysis
    #' files in `output_dir`.
    #'
    #' The workbook carries no forest plot. The `PP results` and `ITT results`
    #' sheets already report every emulated trial with counts, rates, ratios,
    #' risk differences, intervals and numbers needed to treat. A forest image
    #' repeated a subset of those numbers. `$export()` still draws one for a
    #' manuscript.
    #'
    #' @param path File path for the output `.xlsx` file.
    #' @param table1_enrollment Enrollment ID for Table 1 (main baseline table).
    #'   Default: the enrollment with the most baseline observations.
    #' @param protocol_ett_id Optional character(1) ETT id. The
    #'   `Target trial protocol` sheet describes this one emulated trial. An id
    #'   the plan does not hold raises a warning and falls back. When `NULL`
    #'   (default), the sheet describes the first ETT of the Table 1
    #'   enrollment, and otherwise the first ETT in the grid.
    #' @param output_dir Optional directory holding the cached `.qs2` files.
    #'   Used by the lazy `recompute_baselines()` refresh. Defaults to
    #'   `self$output_dir`.
    export_tables = function(
      path = NULL,
      table1_enrollment = NULL,
      protocol_ett_id = NULL,
      output_dir = NULL
    ) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop(
          "Package 'openxlsx' is required. Install with: install.packages('openxlsx')"
        )
      }
      if (
        is.null(self$results_enrollment) ||
          length(self$results_enrollment) == 0L
      ) {
        stop("No enrollment results. Run $s3_analyze() first.")
      }
      if (is.null(self$results_ett) || length(self$results_ett) == 0L) {
        stop("No ETT results. Run $s3_analyze() first.")
      }
      if (is.null(path)) {
        path <- self$tables_xlsx
        dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      }

      # Lazy refresh of stale baseline results (pre-swereg_table1 cache, and
      # pre-smd_numeric cache -- see .baseline_panel_is_stale()).
      stale <- vapply(
        self$results_enrollment,
        .baseline_panel_is_stale,
        logical(1)
      )
      if (any(stale)) {
        message(
          "Refreshing ",
          sum(stale),
          " stale baseline table(s) from disk..."
        )
        self$recompute_baselines(
          output_dir = output_dir,
          enrollment_ids = names(stale)[stale]
        )
      }

      ett <- self$ett
      enrollment_ids <- unique(ett$enrollment_id)

      # Normalise the requested protocol ETT to one id, or to NULL. An id the
      # plan does not hold falls back, exactly as an absent argument does.
      if (length(protocol_ett_id) > 0L) {
        protocol_ett_id <- as.character(protocol_ett_id)[1L]
        if (!protocol_ett_id %in% ett$ett_id) {
          warning(
            "protocol_ett_id is not an ETT id of this plan (ignored): ",
            protocol_ett_id
          )
          protocol_ett_id <- NULL
        }
      } else {
        protocol_ett_id <- NULL
      }

      # Determine table1 enrollment. `$get_baselines()` repeats the baseline
      # size on every row of that enrollment, and it returns a counts-only row
      # for an enrollment that stored no panel, so every analysed enrollment is
      # represented. An enrollment with no stored size counts as 0, which is
      # what the raw read did.
      if (is.null(table1_enrollment)) {
        eids_analysed <- .plan_analysed_enrollment_ids(self)
        base_all <- self$get_baselines()
        n_baselines <- vapply(
          eids_analysed,
          function(eid) {
            n <- .baseline_count(base_all, eid, "n_baseline")
            if (is.na(n)) 0 else n
          },
          numeric(1)
        )
        names(n_baselines) <- eids_analysed
        table1_enrollment <- names(which.max(n_baselines))
      }

      wb <- openxlsx::createWorkbook()
      toc_names <- character()
      toc_desc <- character()

      # --- Provenance sheet ---
      .write_provenance(wb, self)
      toc_names <- c(toc_names, "Provenance")
      toc_desc <- c(toc_desc, "Pipeline metadata and table of contents")

      # --- Study Specification sheet ---
      .write_spec_summary(wb, self)
      toc_names <- c(toc_names, "Study Specification")
      toc_desc <- c(toc_desc, "Study design, variables, ICD-10/ATC codes")

      # --- Target trial protocol sheet ---
      # One sheet documents ONE ETT, so the caller names it through
      # `protocol_ett_id`. Without it, prefer any ETT of the Table 1
      # enrollment, then the first in the grid.
      if (is.null(protocol_ett_id)) {
        t1_rows <- which(ett$enrollment_id == table1_enrollment)
        protocol_ett_id <- if (length(t1_rows) > 0L) {
          ett$ett_id[t1_rows[1]]
        } else {
          ett$ett_id[1]
        }
      }
      .write_protocol_table(
        wb,
        "Target trial protocol",
        self,
        protocol_ett_id
      )
      toc_names <- c(toc_names, "Target trial protocol")
      toc_desc <- c(
        toc_desc,
        paste0(
          "Target trial specification vs emulation (Dickerman Table S1) -- ",
          protocol_ett_id
        )
      )

      # --- Enrollments overview sheet ---
      .write_enrollment_overview(wb, self)
      toc_names <- c(toc_names, "Enrollments")
      toc_desc <- c(
        toc_desc,
        "Enrollment overview (treatment, matching, criteria)"
      )

      # --- ETTs overview sheet ---
      .write_ett_overview(wb, self)
      toc_names <- c(toc_names, "ETTs")
      toc_desc <- c(toc_desc, "ETT overview (outcome, follow-up, events)")

      # --- Table 1: Baseline for chosen enrollment ---
      t1_label <- .enrollment_label(self, table1_enrollment)
      t1_baselines <- self$get_baselines()
      t1_arms <- .baseline_arm_labels(t1_baselines, table1_enrollment)
      t1_panel <- function(weighting, variant) {
        .baseline_panel(
          t1_baselines,
          table1_enrollment,
          "imputed",
          weighting,
          variant,
          t1_arms
        )
      }
      # The Love plot reads the accessor rows themselves. It needs the
      # unrounded `smd_numeric`, which is a programmatic contract rather than a
      # rendered cell, so it never goes through `.baseline_panel()`.
      # `which()` runs OUTSIDE the data.table subset. Inside `t1_baselines[...]`
      # the two arguments would resolve to the COLUMNS of the same name, and
      # the filter would keep every panel.
      t1_rows <- function(want_weighting, want_variant) {
        hit <- which(
          t1_baselines$enrollment_id == table1_enrollment &
            t1_baselines$imputation == "imputed" &
            t1_baselines$weighting == want_weighting &
            t1_baselines$variant == want_variant
        )
        t1_baselines[hit]
      }
      t1_main <- t1_panel("ipw_trunc", "main") %||%
        t1_panel("ipw_trunc", "supplementary")
      if (!is.null(t1_main)) {
        .write_tableone_sheet(
          wb,
          "Table 1",
          t1_main,
          title = paste0(
            "Table 1: Baseline characteristics (IPW-weighted, truncated) -- Enrollment ",
            table1_enrollment,
            " (",
            t1_label,
            ")"
          )
        )
        toc_names <- c(toc_names, "Table 1")
        toc_desc <- c(
          toc_desc,
          paste0(
            "Baseline characteristics (IPW truncated) -- ",
            t1_label
          )
        )
      }

      # Resolve the directory for image sidecars (next to the workbook)
      img_dir <- dirname(path)
      img_basename_root <- tools::file_path_sans_ext(basename(path))

      # --- Love plot sheet (covariate balance for the Table 1 enrollment) ---
      # Series: unweighted vs IPW-truncated. The truncated weights are the
      # analysis weights, so the untruncated panel is not plotted.
      .write_love_plot(
        wb,
        "Love plot",
        t1_unweighted = t1_rows("none", "supplementary"),
        # The SUPPLEMENTARY truncated panel, named by three accessor keys
        # rather than by a slot name. A slot name could partial-match:
        # `table1_ipw_trunc` is a strict prefix of `table1_ipw_trunc_main`, and
        # the Love plot would then draw the main panel as the weighted series.
        # `weighting` and `variant` are separate columns, so no such match
        # exists.
        t1_weighted = t1_rows("ipw_trunc", "supplementary"),
        title = paste0(
          "Love plot: covariate balance before and after weighting",
          " -- Enrollment ",
          table1_enrollment,
          " (",
          t1_label,
          ")"
        ),
        img_dir = img_dir,
        img_basename = paste0(img_basename_root, "_love_plot")
      )
      toc_names <- c(toc_names, "Love plot")
      toc_desc <- c(
        toc_desc,
        paste0(
          "Covariate balance (absolute SMD, unweighted vs IPW truncated) -- ",
          t1_label
        )
      )

      # --- PP results sheet (per-protocol, truncated weights, all ETTs) ---
      .write_results_single(
        wb,
        "PP results",
        self,
        rates_slot = "rates_pp_trunc",
        irr_slot = "irr_pp_trunc",
        rd_slot = "rd_pp_trunc",
        title = "Per-protocol results (truncated weights) - all ETTs"
      )
      toc_names <- c(toc_names, "PP results")
      toc_desc <- c(
        toc_desc,
        "All ETTs - per-protocol rates and IRRs (truncated weights)"
      )

      # --- ITT results sheet (intention-to-treat, all ETTs) ---
      .write_results_single(
        wb,
        "ITT results",
        self,
        rates_slot = "rates_itt",
        irr_slot = "irr_itt",
        rd_slot = "rd_itt",
        title = "Intention-to-treat results - all ETTs"
      )
      toc_names <- c(toc_names, "ITT results")
      toc_desc <- c(
        toc_desc,
        "All ETTs - intention-to-treat rates and IRRs"
      )

      # --- Weight-truncation robustness (supplementary, all ETTs) ---
      # Per-protocol truncated vs untruncated IPW/IPCW weights, side by side.
      # Moved out of the main sequence: the headline sheets are now per
      # estimand; this stays as a robustness check.
      .write_combined_sensitivity(
        wb,
        "Weight truncation (PP)",
        self,
        trunc_rates_slot = "rates_pp_trunc",
        trunc_irr_slot = "irr_pp_trunc",
        untrunc_rates_slot = "rates_pp",
        untrunc_irr_slot = "irr_pp",
        title = paste0(
          "Weight-truncation robustness (per-protocol): truncated (left) vs ",
          "untruncated (right) weights - all ETTs"
        )
      )
      toc_names <- c(toc_names, "Weight truncation (PP)")
      toc_desc <- c(
        toc_desc,
        "Supplementary - PP IRRs, truncated vs untruncated weights"
      )

      # --- Effect modification sheet (only if any subgroups are configured) ---
      has_subgroups <- "subgroup_vars" %in%
        names(self$ett) &&
        any(vapply(
          self$ett$subgroup_vars,
          function(x) length(x) > 0L,
          logical(1)
        ))
      if (has_subgroups) {
        .write_effect_modification(
          wb,
          "Effect modification",
          self,
          title = paste0(
            "Effect modification: stratified IRRs (per-protocol | ",
            "intention-to-treat) and interaction test"
          )
        )
        toc_names <- c(toc_names, "Effect modification")
        toc_desc <- c(
          toc_desc,
          "Stratified IRRs by subgroup (PP and ITT) + interaction test"
        )
      }

      # --- Table S1-SN: Combined baselines per enrollment ---
      for (j in seq_along(enrollment_ids)) {
        eid <- enrollment_ids[j]
        sheet_name <- paste0("Table S", j)
        .write_combined_baseline(wb, sheet_name, self, eid)
        toc_names <- c(toc_names, sheet_name)
        label <- .enrollment_label(self, eid)
        toc_desc <- c(
          toc_desc,
          paste0(
            "Enrollment ",
            eid,
            " (",
            label,
            ") -- combined baselines (Unimputed/Imputed/IPW/IPW trunc)"
          )
        )
      }
      n_s <- length(enrollment_ids)

      # --- CONSORT attrition sheets + sidecar images ---
      # Attrition sheet: tabular form of the per-enrollment CONSORT numbers
      # (criterion x {n_persons, n_person_trials, excluded_*, n_intervention,
      # n_comparator}), so reviewers can cite exact counts instead of reading
      # them off a PNG.
      # CONSORT sidecars: each enrollment still gets a standalone PNG + PDF
      # rendered next to the workbook; Provenance TOC records which were
      # written.
      #
      # ONE condition gates the sheet and its table-of-contents row, and it is
      # the return value of `.write_attrition_sheet()`. A stored attrition
      # table is not enough. The writer also needs a cohort flow, and
      # `.build_cohort_flow()` returns NULL when one criterion carries no
      # global row. A row here that named the sheet on the table alone would
      # advertise a sheet the workbook does not hold.
      consort_files <- character()
      {
        for (eid in enrollment_ids) {
          ec <- .plan_cohort_counts(self, eid)
          if (!is.null(ec$attrition)) {
            attrition_sheet <- paste0("Attrition_", eid)
            label <- .enrollment_label(self, eid)
            if (isTRUE(.write_attrition_sheet(wb, attrition_sheet, self, eid))) {
              toc_names <- c(toc_names, attrition_sheet)
              toc_desc <- c(
                toc_desc,
                paste0(
                  "Enrollment ",
                  eid,
                  " (",
                  label,
                  ") -- CONSORT attrition (numbers behind the diagram)"
                )
              )
            }

            consort_basename <- paste0(img_basename_root, "_consort_", eid)
            paths <- .render_consort_sidecars(
              plan = self,
              ec = ec,
              eid = eid,
              label = label,
              output_dir = img_dir,
              img_basename = consort_basename
            )
            if (!is.null(paths)) {
              consort_files <- c(consort_files, basename(paths$png))
            }
          }
        }
      }
      if (length(consort_files) > 0L) {
        toc_names <- c(toc_names, "CONSORT sidecars (standalone files)")
        toc_desc <- c(
          toc_desc,
          paste0(
            length(consort_files),
            " PNG + matching PDF next to the workbook: ",
            paste(consort_files, collapse = ", ")
          )
        )
      }

      # Write table of contents to Provenance sheet (right side)
      toc <- data.table::data.table(
        Sheet = seq_along(toc_names),
        Name = toc_names,
        Description = toc_desc
      )
      openxlsx::writeData(
        wb,
        "Provenance",
        toc,
        startCol = 4L,
        startRow = 1L,
        headerStyle = openxlsx::createStyle(textDecoration = "bold")
      )
      openxlsx::setColWidths(
        wb,
        "Provenance",
        cols = 4:6,
        widths = c(8, 25, 60)
      )

      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      cat("Saved:", path, "\n")
      invisible(path)
    },

    #' @description Produce an ORDERED set of exhibits (figures and/or tables)
    #' from a manifest and write them to `dir` with two-digit order prefixes, so
    #' the manifest order becomes the exhibit numbering. This is the single
    #' programmatic entry point: a project declares its exhibit set once and
    #' hands it over; other projects reuse the same driver with a different
    #' manifest. Each spec's `type` routes it to a producer:
    #' \describe{
    #'   \item{figures}{`"survival"` (weighted survival curve for one ETT cell,
    #'     one image per estimand), `"forest"` (forest plot over a named
    #'     `exposures` set, one image per estimand), and `"consort"` (CONSORT
    #'     flow diagram for an enrollment).}
    #'   \item{tables}{`"table1"` (baseline characteristics for an enrollment,
    #'     written as CSV).}
    #' }
    #' Full per-type fields are documented on the private `.export_figure()` /
    #' `.export_table()` producers.
    #'
    #' Two `"forest"` and `"survival"` fields carry a decision worth stating
    #' here, because both are silent when they go wrong.
    #'
    #' `"survival"` is drawn on the CUMULATIVE-FAILURE scale, which is one
    #' minus survival. A y-axis window is therefore meaningless until it says
    #' which scale it is measured on, so `ylim` requires a companion
    #' `ylim_scale`, either `"survival"` or `"cumulative_failure"`. A
    #' survival-scale window is translated onto the plotted scale:
    #' `c(0.95, 1)` becomes `c(0, 0.05)` and shows the same band of the figure
    #' it always did. An undeclared window is an error, not a guess. Left
    #' undeclared and applied as given, a survival-scale window clips the whole
    #' cumulative-failure curve out of view and produces a blank panel with no
    #' error and no warning.
    #'
    #' `"forest"` takes `risk_difference = TRUE` to SHOW the signed
    #' cause-specific risk difference per 10,000 people, with its interval.
    #' The option computes nothing. `$s3_analyze()` computes the risk
    #' difference for every ETT and stores it, so this switch only decides
    #' whether the figure carries the two extra columns.
    #'
    #' The `n_boot`, `seed` and `conf_level` fields are inert and warn.
    #' `$s3_analyze()` fixes `n_boot` and `seed`. It reads the confidence level
    #' from `study$implementation$conf_level`, so a study sets its level once
    #' and every result and header carries it. A figure that could restate the
    #' level would print a label the numbers do not have.
    #' @param manifest A non-empty list of exhibit specs. Every spec needs a
    #'   `type`; other fields depend on the type. Optional `label` (filename
    #'   stem) and `title`.
    #' @param dir Output directory. Defaults to `self$dir_results`.
    #' @return Character vector of all written paths (invisibly).
    export = function(manifest, dir = NULL) {
      if (!is.list(manifest) || length(manifest) == 0L) {
        stop("manifest must be a non-empty list of exhibit specs")
      }
      if (is.null(dir)) {
        dir <- self$dir_results
      }
      figure_types <- c("survival", "forest", "consort")
      table_types <- c("table1")
      paths <- character(0)
      for (i in seq_along(manifest)) {
        spec <- manifest[[i]]
        spec$.index <- i
        if (is.null(spec$type)) {
          stop("exhibit spec ", i, " must have a 'type'")
        }
        if (spec$type %in% figure_types) {
          paths <- c(paths, private$.export_figure(spec, dir))
        } else if (spec$type %in% table_types) {
          paths <- c(paths, private$.export_table(spec, dir))
        } else {
          stop(
            "unknown exhibit type '",
            spec$type,
            "' in spec ",
            i,
            ". Figures: survival, forest, consort. Tables: table1."
          )
        }
      }
      cat("Wrote", length(paths), "exhibit file(s) to", dir, "\n")
      invisible(paths)
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
    .schema_version = NULL,

    # Produce one FIGURE exhibit (image) from a spec; dispatched by $export().
    # Types:
    #   "survival": weighted survival curve for one ETT cell (enrollment,
    #     outcome, follow_up, age_group). One image per `estimands` entry --
    #     "pp" reads rd_curve_pp_trunc, "itt" reads rd_curve_itt, both through
    #     $get_curves(). No branch of this method opens an analysis file. The
    #     figure is
    #     drawn on the CUMULATIVE-FAILURE scale, so an optional `ylim` window
    #     must declare its own scale in `ylim_scale` ("survival" or
    #     "cumulative_failure"); a survival-scale window is translated onto the
    #     plotted one, and an undeclared window is an error.
    #   "forest": forest plot over `exposures` (named list label -> ett_id),
    #     one image per `estimands` entry. `group_by` ("exposure"/"outcome")
    #     picks the grouping; `label_format`/`desc_header` tune the text panel;
    #     `role_headers` (named role -> label map, e.g.
    #     c(primary = "Primary outcome", secondary = "Secondary outcomes")) adds
    #     role sub-headers within each exposure block (group_by = "exposure").
    #     `risk_difference = TRUE` SHOWS the signed cause-specific risk
    #     difference per 10,000 with its interval, read from the results
    #     `$s3_analyze()` stored. It computes nothing. `n_boot`, `seed` and
    #     `conf_level` are inert and warn. The header states the level s3 used,
    #     which is `study$implementation$conf_level`, so the column cannot
    #     state a level the numbers do not have.
    .export_figure = function(spec, dir) {
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
      stem <- spec$label %||% spec$type
      base <- if (!is.null(spec$.index)) {
        sprintf("%02d_%s", spec$.index, stem)
      } else {
        stem
      }

      if (identical(spec$type, "survival")) {
        ett_row <- self$ett[
          enrollment_id == spec$enrollment &
            outcome_var == spec$outcome &
            follow_up == spec$follow_up &
            age_group == spec$age_group
        ]
        if (nrow(ett_row) != 1L) {
          stop(
            "survival figure needs exactly 1 matching ETT, found ",
            nrow(ett_row),
            " for enrollment=",
            spec$enrollment,
            " outcome=",
            spec$outcome,
            " follow_up=",
            spec$follow_up,
            " age_group=",
            spec$age_group
          )
        }
        estimands <- spec$estimands %||% "pp"
        # A y-axis window is meaningless without the scale it is measured on.
        # This figure plots CUMULATIVE FAILURE, so a survival-scale window such
        # as c(0.95, 1) would clip the whole curve out of view through
        # coord_cartesian(): a blank panel, with no error and no warning. The
        # scale is therefore declared and translated, never guessed. Neither
        # pure convention is safe on its own, because the mirror mistake -- a
        # failure-scale window silently read as a survival-scale one -- blanks
        # the panel just as quietly.
        ylim_plot <- spec$ylim
        if (!is.null(ylim_plot)) {
          if (
            !is.numeric(ylim_plot) ||
              length(ylim_plot) != 2L ||
              any(!is.finite(ylim_plot)) ||
              ylim_plot[1] >= ylim_plot[2]
          ) {
            stop(
              "survival figure 'ylim' must be two increasing finite numbers, ",
              "low bound first"
            )
          }
          ylim_scale <- spec$ylim_scale
          if (is.null(ylim_scale)) {
            stop(
              "survival figure 'ylim' requires 'ylim_scale', either ",
              "'survival' or 'cumulative_failure'. The figure plots ",
              "cumulative failure, so an undeclared survival-scale window ",
              "such as c(0.95, 1) would blank the panel."
            )
          }
          if (
            !identical(ylim_scale, "survival") &&
              !identical(ylim_scale, "cumulative_failure")
          ) {
            stop(
              "survival figure 'ylim_scale' must be 'survival' or ",
              "'cumulative_failure', got '",
              ylim_scale,
              "'"
            )
          }
          if (identical(ylim_scale, "survival")) {
            # 1 - survival, so the bounds swap roles as well as values.
            ylim_plot <- c(1 - ylim_plot[2], 1 - ylim_plot[1])
          }
        }
        # NO ANALYSIS FILE IS OPENED TO RENDER. This branch read one until
        # 26.8.20, and it was the last RENDER read in the export path.
        # `$s3_analyze()` stores S(t) for both arms, and the head count of
        # people at risk in each arm and band. Both panels of this figure
        # therefore come from `$get_curves()`. s3 computes, s4 formats.
        #
        # One analysis read remains in `$export_tables()` and it is a
        # PRODUCER's. A stale baseline panel sends `$recompute_baselines()` to
        # `.s3_enrollment_worker()`, which is s3's own worker computing and
        # storing a Table 1 panel. That is s3 running late, not s4 computing.
        paths <- character(0)
        id_ett <- as.character(ett_row$ett_id[1])
        curves <- self$get_curves()
        arms <- .tte_arm_labels_resolved(
          .lookup_arm_labels(self$spec, spec$enrollment)
        )
        for (est in estimands) {
          slot <- if (identical(est, "pp")) {
            "rd_curve_pp_trunc"
          } else if (identical(est, "itt")) {
            "rd_curve_itt"
          } else {
            stop("survival estimand must be 'pp' or 'itt', got '", est, "'")
          }
          combo <- .tte_slot_combo(slot)
          cv <- curves[
            ett_id == id_ett &
              estimand == combo[["estimand"]] &
              weights == combo[["weights"]]
          ]
          if (nrow(cv) == 0L) {
            stop(
              "no stored survival curve for ",
              id_ett,
              " (",
              est,
              "). Run $s3_analyze(), which stores '",
              slot,
              "'."
            )
          }
          # The risk table refuses to draw on missing counts. A curve stored
          # before s3 carried them gives `NA`, and a row of missing values
          # looks exactly like a drawn risk table.
          if (anyNA(cv$n_persons_at_risk)) {
            stop(
              "the stored '",
              slot,
              "' curve of ",
              id_ett,
              " carries no numbers at risk. Re-run $s3_analyze(), which ",
              "stores the distinct-person count for each arm and band."
            )
          }
          curve <- data.table::data.table(
            band = as.numeric(cv$band),
            surv = as.numeric(cv$surv),
            n_persons_at_risk = as.numeric(cv$n_persons_at_risk),
            group = data.table::fifelse(
              cv$arm == "intervention",
              arms[["intervention"]],
              arms[["comparator"]]
            )
          )
          data.table::setorderv(curve, c("group", "band"))
          q <- .render_survival_curve(
            curve = curve,
            time_var = "band",
            # Cumulative failure, not survival: a rare outcome is unreadable
            # as a curve pinned near 100%.
            scale = "cumulative_failure",
            # Title is just the outcome (the exposure/contrast is in the
            # legend).
            title = spec$title %||% ett_row$outcome_name,
            ylim = ylim_plot,
            int_lab = arms[["intervention"]],
            cmp_lab = arms[["comparator"]]
          )
          out <- file.path(dir, paste0(base, "_", est, ".png"))
          ggplot2::ggsave(out, q, width = 8, height = 6, dpi = 300)
          paths <- c(paths, out)
        }
        return(paths)
      }

      if (identical(spec$type, "consort")) {
        eid <- spec$enrollment
        if (!eid %in% .plan_counted_enrollment_ids(self)) {
          stop("no enrollment counts for '", eid, "'. Run enrollment first.")
        }
        ec <- .plan_cohort_counts(self, eid)
        .render_consort_sidecars(
          plan = self,
          ec = ec,
          eid = eid,
          label = .enrollment_label(self, eid),
          output_dir = dir,
          img_basename = base
        )
        return(file.path(dir, paste0(base, ".png")))
      }

      if (identical(spec$type, "forest")) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          stop("Package 'openxlsx' is required for forest figures.")
        }
        if (is.null(spec$exposures)) {
          stop(
            "forest figure requires 'exposures' (named list of label -> ett_id)"
          )
        }
        exp_names <- names(spec$exposures)
        if (is.null(exp_names) || anyNA(exp_names) || any(!nzchar(exp_names))) {
          stop(
            "forest 'exposures' must be a fully named list (no blank/NA names)"
          )
        }
        # Flatten to ett ids plus a PARALLEL vector of group labels, one per ett
        # id (.write_forest_irr maps ett_id -> label by position). `group_by`
        # chooses the grouping: "exposure" groups by the exposure contrast with
        # outcomes as rows; "outcome" groups by outcome with exposures as rows.
        keep_ids <- unlist(spec$exposures, use.names = FALSE)
        if (length(keep_ids) == 0L) {
          stop("forest 'exposures' resolved to zero ETT ids")
        }
        missing_ids <- setdiff(keep_ids, self$ett$ett_id)
        if (length(missing_ids) > 0L) {
          stop(
            "forest 'exposures' contains unknown ETT ids: ",
            paste(missing_ids, collapse = ", ")
          )
        }
        group_by <- spec$group_by %||% "exposure"
        if (identical(group_by, "exposure")) {
          keep_groups <- rep(
            names(spec$exposures),
            times = lengths(spec$exposures)
          )
          default_label <- "{outcome_name}"
        } else if (identical(group_by, "outcome")) {
          keep_groups <- self$ett$outcome_name[match(keep_ids, self$ett$ett_id)]
          # Reorder so same-outcome rows are consecutive (in spec outcome order);
          # the renderer only merges consecutive same-label rows, and the ett
          # list arrives exposure-major, which would split each outcome into
          # many single-row groups.
          ord <- order(
            match(keep_groups, unique(self$ett$outcome_name)),
            seq_along(keep_ids)
          )
          keep_ids <- keep_ids[ord]
          keep_groups <- keep_groups[ord]
          default_label <- "{enrollment_name}"
        } else {
          stop(
            "forest group_by must be 'exposure' or 'outcome', got '",
            group_by,
            "'"
          )
        }
        # When the spec assigns outcome roles (primary/secondary) and outcomes
        # are the rows (group_by = "exposure"), surface the role from metadata in
        # the default row label -- the spec `name` stays clean; role rides the
        # `role:` field via {outcome_role}. Overridable with an explicit
        # `spec$label_format`.
        spec_roles <- vapply(
          self$spec$outcomes %||% list(),
          function(o) o$role %||% NA_character_,
          character(1)
        )
        if (
          any(!is.na(spec_roles)) &&
            identical(group_by, "exposure") &&
            is.null(spec$label_format)
        ) {
          default_label <- "{outcome_name} ({outcome_role})"
        }
        # Optional role sub-headers ("Primary outcome" / "Secondary outcomes"):
        # a named map role -> label from the manifest, threaded into the forest
        # as an extra grouping tier. Only meaningful when outcomes are the rows
        # (group_by = "exposure"); pairs naturally with a clean
        # `label_format = "{outcome_name}"` so the role isn't also in the label.
        role_headers_vec <- if (
          identical(group_by, "exposure") && !is.null(spec$role_headers)
        ) {
          unlist(spec$role_headers)
        } else {
          NULL
        }
        estimands <- spec$estimands %||% "pp"
        # `risk_difference` is a DISPLAY switch and computes nothing. s3 stores
        # the risk difference for every ETT, so this option only decides
        # whether the figure carries the two extra columns.
        #
        # It used to gate the computation as well. The quantity was rebuilt
        # here from each featured ETT's analysis panel on disk. A script that
        # left the option unset drew every figure without it. There was no
        # error and no warning.
        show_rd <- isTRUE(spec$risk_difference)
        # The level the HEADER states, read from the same study property s3
        # computed the interval at. One study, one level, one place to set it.
        rd_conf_level <- .s3_conf_level(self$spec)
        # `n_boot`, `seed` and `conf_level` do not reach the estimator from
        # here. s3 fixes the first two and reads the third from
        # `study$implementation$conf_level`. Say so rather than accepting them
        # and ignoring them: a setting that looks live and is not is how the
        # first defect stayed invisible.
        inert <- intersect(c("n_boot", "seed", "conf_level"), names(spec))
        if (length(inert) > 0L) {
          warning(
            "forest figure option(s) ",
            paste(inert, collapse = ", "),
            " do not affect the risk difference. $s3_analyze() computes it ",
            "for every ETT at n_boot = ",
            .S3_RD_N_BOOT,
            ", seed = ",
            .S3_RD_SEED,
            ", conf_level = ",
            rd_conf_level,
            ". Set the level at study$implementation$conf_level, and remove ",
            "these from the manifest."
          )
        }
        paths <- character(0)
        for (est in estimands) {
          # Three RESULT slots and no file name. The forest figure reads
          # `plan$results_ett` only. It opened an analysis file to rebuild the
          # risk difference before, and that read is gone.
          slots <- if (identical(est, "pp")) {
            list(
              r = "rates_pp_trunc",
              i = "irr_pp_trunc",
              rd = "rd_pp_trunc"
            )
          } else if (identical(est, "itt")) {
            list(
              r = "rates_itt",
              i = "irr_itt",
              rd = "rd_itt"
            )
          } else {
            stop("forest estimand must be 'pp' or 'itt', got '", est, "'")
          }
          rd_lookup <- NULL
          if (show_rd) {
            # READ, never recompute. `$get_estimates()` carries the stored risk
            # difference on the same row as the ratio it belongs to. A failed
            # emulated trial stored a skip envelope, which the accessor reports
            # as absent, and it renders an empty cell.
            rd_lookup <- .tte_rd_lookup(self, slots$rd, keep_ids)
          }
          img_base <- paste0(base, "_", est)
          .write_forest_irr(
            openxlsx::createWorkbook(),
            sheet_name = paste0("forest_", est),
            plan = self,
            rates_slot = slots$r,
            irr_slot = slots$i,
            title = spec$title,
            keep_ett_ids = keep_ids,
            group_labels = keep_groups,
            label_format = spec$label_format %||% default_label,
            desc_header = spec$desc_header,
            role_headers = role_headers_vec,
            rd_lookup = rd_lookup,
            # The SAME study property s3 computed the interval at, so the
            # header cannot state a level the numbers do not have.
            # `.write_forest_irr` checks it against each row's own
            # `conf_level` and stops on a disagreement. That check now also
            # catches a specification edited between s3 and the export.
            rd_conf_level = rd_conf_level,
            img_dir = dir,
            img_basename = img_base
          )
          paths <- c(paths, file.path(dir, paste0(img_base, ".png")))
        }
        return(paths)
      }

      stop("unknown figure type '", spec$type, "'")
    },

    # Produce one TABLE exhibit from a spec; dispatched by $export().
    #   "table1": IPW-truncated baseline characteristics for `enrollment`,
    #     written as CSV (from the computed results_enrollment table1).
    .export_table = function(spec, dir) {
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
      stem <- spec$label %||% spec$type
      base <- if (!is.null(spec$.index)) {
        sprintf("%02d_%s", spec$.index, stem)
      } else {
        stem
      }

      if (identical(spec$type, "table1")) {
        eid <- spec$enrollment
        if (!eid %in% .plan_analysed_enrollment_ids(self)) {
          stop("no enrollment results for '", eid, "'. Run analysis first.")
        }
        baselines <- self$get_baselines()
        arms <- .baseline_arm_labels(baselines, eid)
        tbl <- .baseline_panel(
          baselines,
          eid,
          "imputed",
          "ipw_trunc",
          "main",
          arms
        ) %||%
          .baseline_panel(
            baselines,
            eid,
            "imputed",
            "ipw_trunc",
            "supplementary",
            arms
          )
        if (is.null(tbl)) {
          stop("no Table 1 available for enrollment '", eid, "'")
        }
        out <- file.path(dir, paste0(base, "_", eid, ".csv"))
        # `.baseline_panel()` composes display columns only, so `smd_numeric`
        # never reaches the file.
        data.table::fwrite(tbl, out)
        return(out)
      }

      stop("unknown table type '", spec$type, "'")
    }
  )
)


# =============================================================================
# tteplan_load
# =============================================================================

#' Load a TTEPlan from disk with the current class definition
#'
#' R6 objects serialized with [qs2::qs_save()] retain the method bindings from
#' the class version at save time. After a package upgrade that adds new methods
#' or fields, [qs2::qs_read()] returns a stale object. This function reads the
#' file, then copies all public fields into a fresh [TTEPlan] instance so that
#' new methods are available.
#'
#' @param path Path to a `tteplan.qs2` file.
#' @return A [TTEPlan] object with the current class definition.
#'
#' @family tte_plan
#' @export
tteplan_load <- function(path) {
  old <- qs2_read(path)
  if (!inherits(old, "TTEPlan")) {
    stop("File does not contain a TTEPlan object: ", path)
  }
  plan <- TTEPlan$new(
    project_prefix = old$project_prefix,
    skeleton_files = old$skeleton_files,
    global_max_isoyearweek = old$global_max_isoyearweek,
    ett = old$ett
  )
  # Copy all additional public fields (use get() not [[ - R6 [[ doesn't
  # reliably access fields, only $ and environment get() do)
  fields <- c(
    "spec",
    "enrollment_counts",
    "period_width",
    "expected_skeleton_file_count",
    "code_registry",
    "expected_n_ids",
    "created_at",
    "registry_study_created_at",
    "skeleton_created_at",
    "output_dir",
    "results_enrollment",
    "results_ett",
    "spec_reloaded_at",
    "spec_reload_skipped_diffs",
    # New fields added by the CandidatePath migration
    "spec_version",
    "dir_tteplan_cp",
    "dir_spec_cp",
    "dir_results_cp",
    "registrystudy",
    "n_skeleton_files_limit"
  )
  for (f in fields) {
    val <- tryCatch(get(f, envir = old), error = function(e) NULL)
    if (!is.null(val)) plan[[f]] <- val
  }

  # Schema-version guard -- fails loudly on pre-migration plans with a
  # pointer at the renamer + s0_init re-run.
  saved_schema <- tryCatch(
    get(".schema_version", envir = old$.__enclos_env__$private),
    error = function(e) 0L
  )
  if (is.null(saved_schema)) {
    saved_schema <- 0L
  }
  assign(".schema_version", saved_schema, envir = plan$.__enclos_env__$private)
  plan$check_version() # errors if saved_schema is too old

  # Refresh skeleton_files from the embedded registrystudy so file paths are
  # valid on the current host. Falls back to the serialized list for plans
  # without an embedded study (legacy; blocked by check_version() above).
  if (
    !is.null(plan$registrystudy) &&
      inherits(plan$registrystudy, "RegistryStudy")
  ) {
    files <- plan$registrystudy$skeleton_files
    if (!is.null(plan$n_skeleton_files_limit)) {
      files <- utils::head(files, plan$n_skeleton_files_limit)
    }
    plan$skeleton_files <- files
  }

  # Older cached plans did not persist output_dir. Infer it from the plan
  # file's own directory, which is the standard layout (the plan and its
  # companion .qs2 files sit in the same folder).
  if (is.null(plan$output_dir) || !nzchar(plan$output_dir)) {
    inferred_dir <- dirname(path)
    if (dir.exists(inferred_dir)) {
      plan$output_dir <- inferred_dir
    }
  }

  # Backfill enrollment counts from per-enrollment sidecar files
  if (!is.null(plan$output_dir) && dir.exists(plan$output_dir)) {
    if (is.null(plan$enrollment_counts)) {
      plan$enrollment_counts <- list()
    }
    .restore_enrollment_counts(
      plan,
      plan$output_dir,
      unique(plan$ett$enrollment_id)
    )
  }

  plan
}


# =============================================================================
# tteplan_locate_and_load + registrystudy_load
# =============================================================================

#' Locate and load a TTEPlan from candidate directories
#'
#' Walks `candidate_dir_tteplan` to find the first directory that exists on
#' the current host, then loads `tteplan.qs2` from inside it via
#' [tteplan_load()]. The one-line convenience that `s1.R` / `s2.R` / `s3.R` /
#' `s4_export.R` stage scripts call to obtain a plan with all directories
#' already resolved.
#'
#' @param candidate_dir_tteplan Character vector of candidate directories,
#'   in priority order, where `tteplan.qs2` might live.
#' @return A [TTEPlan] with CandidatePath caches cleared and
#'   `skeleton_files` refreshed from the embedded `registrystudy`.
#' @seealso [tteplan_load()], [first_existing_path()]
#' @family tte_plan
#' @export
tteplan_locate_and_load <- function(candidate_dir_tteplan) {
  dir <- first_existing_path(candidate_dir_tteplan, "dir_tteplan")
  tteplan_load(file.path(dir, FILENAME_TTEPLAN))
}

#' Locate and load a RegistryStudy from candidate metadata directories
#'
#' Walks `candidate_dir_meta` to find the first directory that exists on
#' the current host, then reads `registrystudy.qs2` from inside it. Used in
#' `s0_init.R` to pass a pre-loaded `study` object to
#' [tteplan_from_spec_and_registrystudy()].
#'
#' @param candidate_dir_meta Character vector of candidate metadata
#'   directories (where `registrystudy.qs2` lives). Pass the same path you
#'   gave to `RegistryStudy$new(data_meta_dir = ...)` -- typically either
#'   the rawbatch directory (legacy default) or its parent.
#' @return A [RegistryStudy] R6 object.
#' @seealso [first_existing_path()],
#'   [tteplan_from_spec_and_registrystudy()]
#' @family tte_plan
#' @export
registrystudy_load <- function(candidate_dir_meta) {
  dir <- first_existing_path(candidate_dir_meta, "dir_meta")
  qs2_read(file.path(dir, "registrystudy.qs2"))
}


# =============================================================================
# export_tables helpers (internal)
# =============================================================================

#' @noRd
.write_provenance <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "Provenance")
  spec <- plan$spec
  impl <- if (!is.null(spec)) spec$study$implementation else NULL

  rows <- list()
  add <- function(item, value) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      Item = item,
      Value = as.character(value)
    )
  }

  # An absent timestamp prints as an empty cell. `format(NA, "%Y-%m-%d")` reads
  # the format string as the `trim` argument of `format.default()` and stops
  # with `invalid 'trim' argument`, so a plan built without a RegistryStudy
  # could not export at all.
  fmt_time <- function(x) {
    if (is.null(x) || length(x) == 0L || !inherits(x, c("POSIXct", "Date"))) {
      return(NA_character_)
    }
    format(x, "%Y-%m-%d %H:%M:%S")
  }

  add("Exported at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  add("Project", plan$project_prefix)
  if (!is.null(spec)) {
    add("Study title", spec$study$title)
    add("Principal investigator", spec$study$principal_investigator)
    if (!is.null(impl$version)) {
      add("Spec version", impl$version)
    }
    if (!is.null(impl$date)) {
      add("Spec date", impl$date)
    }
    if (!is.null(impl$status)) add("Spec status", impl$status)
  }
  add("", "")
  add("RegistryStudy created", fmt_time(plan$registry_study_created_at))
  add("Skeletons created", fmt_time(plan$skeleton_created_at))
  add("TTEPlan created", fmt_time(plan$created_at))
  add("", "")
  add("Skeleton files", as.character(length(plan$skeleton_files)))
  n_exp <- plan$expected_skeleton_file_count
  if (!is.null(n_exp)) {
    add("Expected skeleton files", as.character(n_exp))
  }
  if (!is.null(plan$expected_n_ids)) {
    add("Individuals", format(plan$expected_n_ids, big.mark = ","))
  }
  add("Admin censoring (isoyearweek)", plan$global_max_isoyearweek)
  add("Period width (weeks)", as.character(plan$period_width))
  add("Enrollments", as.character(length(unique(plan$ett$enrollment_id))))
  add("ETTs", as.character(nrow(plan$ett)))
  add("", "")
  add("R version", R.version.string)
  add("swereg version", as.character(utils::packageVersion("swereg")))
  add("data.table version", as.character(utils::packageVersion("data.table")))

  if (!is.null(plan$spec_reloaded_at)) {
    add("", "")
    add("Spec reloaded at", format(plan$spec_reloaded_at, "%Y-%m-%d %H:%M:%S"))
    if (length(plan$spec_reload_skipped_diffs) > 0L) {
      add(
        "Spec reload - skipped (structural)",
        paste(plan$spec_reload_skipped_diffs, collapse = "; ")
      )
    }
  }

  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(
    wb,
    "Provenance",
    dt,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold"
    )
  )
  openxlsx::setColWidths(wb, "Provenance", cols = 1:2, widths = c(30, 60))
}

#' Build a code lookup environment and variable formatter from a plan's
#' code_registry.
#'
#' @param plan A TTEPlan object with an optional `code_registry` field.
#' @param colorize Logical. If TRUE, wrap variable/code strings in ANSI
#'   color escapes (for terminal). If FALSE, return plain text (for Excel).
#' @return A list with `lookup` (environment or NULL) and `fmt_var` (function).
#' @noRd
.build_code_lookup <- function(plan, colorize = FALSE) {
  code_lookup <- NULL
  st <- plan$code_registry
  if (!is.null(st) && nrow(st) > 0) {
    code_lookup <- new.env(parent = emptyenv())
    for (i in seq_len(nrow(st))) {
      cols <- strsplit(st$generated_columns[i], ", ")[[1]]
      for (col in cols) {
        code_lookup[[col]] <- paste0(st$codes[i], " (", st$label[i], ")")
      }
    }
  }

  # Resolve combined variable names (e.g., "osd_c__can_c")
  .resolve_combined <- function(var) {
    if (is.null(code_lookup)) {
      return(NULL)
    }
    parts <- strsplit(var, "__", fixed = TRUE)[[1]]
    if (length(parts) <= 1L) {
      return(NULL)
    }
    infos <- vapply(
      parts,
      function(p) {
        code_lookup[[p]] %||% p
      },
      character(1)
    )
    paste(infos, collapse = " + ")
  }

  if (colorize) {
    cyan <- function(x) paste0("\033[36m", x, "\033[0m")
    magenta <- function(x) paste0("\033[95m", x, "\033[0m")
    green <- function(x) paste0("\033[92m", x, "\033[0m")
    fmt_one <- function(v) {
      if (is.null(code_lookup)) {
        return(v)
      }
      info <- code_lookup[[v]]
      if (is.null(info)) {
        info <- .resolve_combined(v)
      }
      if (!is.null(info)) {
        paste0(cyan(v), " <- ", magenta(info))
      } else {
        green(v)
      }
    }
    fmt_var <- function(var) {
      paste(vapply(var, fmt_one, character(1)), collapse = " + ")
    }
  } else {
    fmt_one <- function(v) {
      if (is.null(code_lookup)) {
        return(v)
      }
      info <- code_lookup[[v]]
      if (is.null(info)) {
        info <- .resolve_combined(v)
      }
      if (!is.null(info)) paste0(v, " <- ", info) else v
    }
    fmt_var <- function(var) {
      paste(vapply(var, fmt_one, character(1)), collapse = " + ")
    }
  }

  list(lookup = code_lookup, fmt_var = fmt_var)
}

#' @noRd
.write_spec_summary <- function(wb, plan) {
  sht <- "Study Specification"
  openxlsx::addWorksheet(wb, sht)
  spec <- plan$spec
  if (is.null(spec)) {
    openxlsx::writeData(wb, sht, "No spec available.")
    return(invisible(NULL))
  }

  cl <- .build_code_lookup(plan, colorize = FALSE)
  fmt_var <- cl$fmt_var

  # -- styles (matching console ANSI colours) --------------------------------
  # -- code lookup helpers ---------------------------------------------------
  code_lookup <- cl$lookup
  .resolve_combined <- function(var) {
    if (is.null(code_lookup)) {
      return(NULL)
    }
    parts <- strsplit(var, "__", fixed = TRUE)[[1]]
    if (length(parts) <= 1L) {
      return(NULL)
    }
    infos <- vapply(
      parts,
      function(p) {
        code_lookup[[p]] %||% p
      },
      character(1)
    )
    paste(infos, collapse = " + ")
  }
  resolve_one <- function(v) {
    if (is.null(code_lookup)) {
      return(list(var = v, codes = NA_character_))
    }
    info <- code_lookup[[v]]
    if (is.null(info)) {
      combined <- .resolve_combined(v)
      if (!is.null(combined)) {
        return(list(var = v, codes = combined))
      }
      return(list(var = v, codes = NA_character_))
    }
    list(var = v, codes = info)
  }
  # -- styles (matching console ANSI colours) --------------------------------
  st_header <- openxlsx::createStyle(textDecoration = "bold", fontSize = 13)
  st_item <- openxlsx::createStyle(textDecoration = "bold", indent = 1)
  st_sub_item <- openxlsx::createStyle(textDecoration = "bold", indent = 3)
  st_label <- openxlsx::createStyle(indent = 3)
  st_sub_label <- openxlsx::createStyle(indent = 5)
  st_cyan <- openxlsx::createStyle(fontColour = "#008B8B")
  st_magenta <- openxlsx::createStyle(fontColour = "#8B008B")
  st_green <- openxlsx::createStyle(fontColour = "#006400")
  st_yellow <- openxlsx::createStyle(fontColour = "#B8860B")
  st_codes <- openxlsx::createStyle(fontColour = "#8B008B", indent = 5)
  # Inclusion (green) / exclusion (red) col-A styles
  st_incl_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 1,
    fontColour = "#006400"
  )
  st_incl_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 3,
    fontColour = "#006400"
  )
  st_incl_label <- openxlsx::createStyle(indent = 3, fontColour = "#006400")
  st_incl_sub_label <- openxlsx::createStyle(indent = 5, fontColour = "#006400")
  st_excl_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 1,
    fontColour = "#8B0000"
  )
  st_excl_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 3,
    fontColour = "#8B0000"
  )
  st_excl_label <- openxlsx::createStyle(indent = 3, fontColour = "#8B0000")
  st_excl_sub_label <- openxlsx::createStyle(indent = 5, fontColour = "#8B0000")
  # Sub-sub level (one indent deeper): used for named criteria nested under
  # additional_inclusion / additional_exclusion section headers, and their
  # child key-value rows. Without this level the criterion names render at
  # the same indent as their parent header.
  st_sub_sub_item <- openxlsx::createStyle(textDecoration = "bold", indent = 5)
  st_sub_sub_label <- openxlsx::createStyle(indent = 7)
  st_incl_sub_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 5,
    fontColour = "#006400"
  )
  st_incl_sub_sub_label <- openxlsx::createStyle(
    indent = 7,
    fontColour = "#006400"
  )
  st_excl_sub_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 5,
    fontColour = "#8B0000"
  )
  st_excl_sub_sub_label <- openxlsx::createStyle(
    indent = 7,
    fontColour = "#8B0000"
  )

  # -- accumulator (2 columns: a=label, b=value) ----------------------------
  rows <- list()
  r <- 0L

  # tint: NULL (default), "incl" (green), "excl" (red)
  # sub_sub overrides sub. sub_sub = TRUE → indent 7. sub = TRUE → indent 5.
  # Neither → indent 3.
  pick_sa <- function(sub, tint, sub_sub = FALSE) {
    if (sub_sub) {
      if (identical(tint, "incl")) {
        st_incl_sub_sub_label
      } else if (identical(tint, "excl")) {
        st_excl_sub_sub_label
      } else {
        st_sub_sub_label
      }
    } else if (identical(tint, "incl")) {
      if (sub) st_incl_sub_label else st_incl_label
    } else if (identical(tint, "excl")) {
      if (sub) st_excl_sub_label else st_excl_label
    } else {
      if (sub) st_sub_label else st_label
    }
  }

  add_header <- function(text) {
    r <<- r + 1L
    rows[[r]] <<- list(a = text, b = NA_character_, sa = st_header, sb = NULL)
  }
  add_item <- function(text, tint = NULL) {
    sa <- if (identical(tint, "incl")) {
      st_incl_item
    } else if (identical(tint, "excl")) {
      st_excl_item
    } else {
      st_item
    }
    r <<- r + 1L
    rows[[r]] <<- list(a = text, b = NA_character_, sa = sa, sb = NULL)
  }
  add_sub_item <- function(text, tint = NULL) {
    sa <- if (identical(tint, "incl")) {
      st_incl_sub_item
    } else if (identical(tint, "excl")) {
      st_excl_sub_item
    } else {
      st_sub_item
    }
    r <<- r + 1L
    rows[[r]] <<- list(a = text, b = NA_character_, sa = sa, sb = NULL)
  }
  # Bold criterion-name row one indent deeper than add_sub_item. Optionally
  # carries an inline value in column B (used for "Age range: 54 - 60" style
  # rows where the criterion has no further child key-value pairs).
  add_sub_sub_item <- function(text, value = NA_character_, tint = NULL) {
    sa <- if (identical(tint, "incl")) {
      st_incl_sub_sub_item
    } else if (identical(tint, "excl")) {
      st_excl_sub_sub_item
    } else {
      st_sub_sub_item
    }
    r <<- r + 1L
    rows[[r]] <<- list(a = text, b = value, sa = sa, sb = NULL)
  }
  add_blank <- function() {
    r <<- r + 1L
    rows[[r]] <<- list(a = "", b = NA_character_, sa = NULL, sb = NULL)
  }
  add_kv <- function(label, value, sub = FALSE, sub_sub = FALSE, tint = NULL) {
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label,
      b = value,
      sa = pick_sa(sub, tint, sub_sub),
      sb = NULL
    )
  }
  add_yellow <- function(
    label,
    value,
    sub = FALSE,
    sub_sub = FALSE,
    tint = NULL
  ) {
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label,
      b = value,
      sa = pick_sa(sub, tint, sub_sub),
      sb = st_yellow
    )
  }
  add_var <- function(label, var, sub = FALSE, sub_sub = FALSE, tint = NULL) {
    # First row gets the label
    p1 <- resolve_one(var[1])
    has_codes <- !is.na(p1$codes)
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label,
      b = p1$var,
      sa = pick_sa(sub, tint, sub_sub),
      sb = if (has_codes) st_cyan else st_green
    )
    if (has_codes) {
      r <<- r + 1L
      rows[[r]] <<- list(
        a = NA_character_,
        b = paste0("\u21b3 ", p1$codes),
        sa = NULL,
        sb = st_codes
      )
    }
    # Remaining vars on their own rows
    if (length(var) > 1L) {
      for (v in var[-1L]) {
        pv <- resolve_one(v)
        hc <- !is.na(pv$codes)
        r <<- r + 1L
        rows[[r]] <<- list(
          a = NA_character_,
          b = pv$var,
          sa = NULL,
          sb = if (hc) st_cyan else st_green
        )
        if (hc) {
          r <<- r + 1L
          rows[[r]] <<- list(
            a = NA_character_,
            b = paste0("\u21b3 ", pv$codes),
            sa = NULL,
            sb = st_codes
          )
        }
      }
    }
  }
  add_derived_var <- function(
    label,
    derived,
    source_var,
    sub = FALSE,
    sub_sub = FALSE,
    tint = NULL
  ) {
    # First source var with "derived <- var" on the label row
    p1 <- resolve_one(source_var[1])
    has_codes <- !is.na(p1$codes)
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label,
      b = paste0(derived, " <- ", p1$var),
      sa = pick_sa(sub, tint, sub_sub),
      sb = if (has_codes) st_cyan else st_green
    )
    if (has_codes) {
      r <<- r + 1L
      rows[[r]] <<- list(
        a = NA_character_,
        b = paste0("\u21b3 ", p1$codes),
        sa = NULL,
        sb = st_codes
      )
    }
    # Remaining source vars on their own rows
    if (length(source_var) > 1L) {
      for (v in source_var[-1L]) {
        pv <- resolve_one(v)
        hc <- !is.na(pv$codes)
        r <<- r + 1L
        rows[[r]] <<- list(
          a = NA_character_,
          b = pv$var,
          sa = NULL,
          sb = if (hc) st_cyan else st_green
        )
        if (hc) {
          r <<- r + 1L
          rows[[r]] <<- list(
            a = NA_character_,
            b = paste0("\u21b3 ", pv$codes),
            sa = NULL,
            sb = st_codes
          )
        }
      }
    }
  }

  # -- Colour legend --------------------------------------------------------
  add_row <- function(a, b, sa, sb) {
    r <<- r + 1L
    rows[[r]] <<- list(a = a, b = b, sa = sa, sb = sb)
  }
  add_header("Colour legend")
  add_row("Variable name (resolved)", "e.g. osd_f64", NULL, st_cyan)
  add_row(
    "Code annotation",
    paste0("\u21b3 F64 (swereg::add_diagnoses)"),
    NULL,
    st_codes
  )
  add_row(
    "Variable name (unresolved)",
    "e.g. rd_age_continuous",
    NULL,
    st_green
  )
  add_row("Categories / arm values", "e.g. drug_a", NULL, st_yellow)
  add_row("Inclusion criterion", NA_character_, st_incl_item, NULL)
  add_row("Exclusion criterion", NA_character_, st_excl_item, NULL)
  add_blank()

  # -- Study ----------------------------------------------------------------
  add_header("Study")
  add_kv("Title:", spec$study$title)
  add_kv("PI:", spec$study$principal_investigator)
  if (!is.null(spec$study$design)) {
    add_kv("Design:", spec$study$design)
  }
  impl <- spec$study$implementation
  if (!is.null(impl$version)) {
    add_kv("Version:", impl$version)
  }
  if (!is.null(plan$global_max_isoyearweek)) {
    add_kv("Admin censoring:", plan$global_max_isoyearweek)
  }
  add_blank()

  # -- Follow-up ------------------------------------------------------------
  add_header("Follow-up")
  for (fu in spec$follow_up) {
    add_kv(fu$label, paste0(fu$weeks, " weeks"))
  }
  add_blank()

  # -- Inclusion criteria ---------------------------------------------------
  add_header("Inclusion criteria (global)")
  iso <- spec$inclusion_criteria$isoyears
  add_kv("Isoyears:", paste0(iso[1], " - ", iso[2]), tint = "incl")
  add_blank()

  # -- Exclusion criteria ---------------------------------------------------
  add_header("Exclusion criteria (global)")
  for (ec in spec$exclusion_criteria) {
    add_item(ec$name, tint = "excl")
    add_var(
      "Variable:",
      ec$implementation$source_variable_combined %||%
        ec$implementation$source_variable,
      tint = "excl"
    )
    add_kv("Window:", .format_window_human(ec$implementation), tint = "excl")
  }
  add_blank()

  # -- Confounders ----------------------------------------------------------
  add_header("Confounders")
  # Surface standing_methods.calendar_time as the first confounder entry: it
  # IS a confounder, but one that swereg auto-adjusts for via the IPW/IPCW
  # models. Showing it here so readers don't keep asking "what about calendar
  # year?" on every protocol review.
  sm_ct <- spec$standing_methods$calendar_time
  if (!is.null(sm_ct) && identical(sm_ct$handling, "auto-adjusted")) {
    add_item("Calendar time at trial registration")
    add_kv(
      "Handling:",
      sm_ct$note %||%
        "auto-adjusted by swereg (IPW/IPCW models); no explicit covariate needed"
    )
  }
  for (conf in spec$confounders) {
    cimpl <- conf$implementation
    add_item(conf$name)
    if (isTRUE(cimpl$computed)) {
      sv_display <- cimpl$source_variable_combined %||% cimpl$source_variable
      derived <- cimpl$variable %||% sv_display
      add_derived_var("Variable:", derived, sv_display)
      add_kv("Window:", .format_window_human(cimpl))
    } else {
      add_var("Variable:", cimpl$variable)
    }
    if (!is.null(conf$categories)) {
      add_yellow("Categories:", paste(conf$categories, collapse = ", "))
    }
  }
  add_blank()

  # -- Outcomes -------------------------------------------------------------
  add_header("Outcomes")
  for (out in spec$outcomes) {
    add_item(out$name)
    add_var("Variable:", out$implementation$variable)
  }
  add_blank()

  # -- Enrollments ----------------------------------------------------------
  add_header("Enrollments")
  for (enr in spec$enrollments) {
    add_item(paste0(enr$id, ": ", enr$name))

    # Treatment
    add_sub_item("Treatment:")
    tx <- enr$treatment
    add_var("Variable:", tx$implementation$variable, sub = TRUE)
    add_yellow(
      "Intervention:",
      paste0(
        tx$arms$intervention,
        " <- ",
        tx$implementation$intervention_value
      ),
      sub = TRUE
    )
    add_yellow(
      "Comparator:",
      paste0(tx$arms$comparator, " <- ", tx$implementation$comparator_value),
      sub = TRUE
    )
    add_kv(
      "Matching ratio:",
      paste0("1:", tx$implementation$matching_ratio),
      sub = TRUE
    )

    # Additional inclusion
    # Each named criterion (age_range, has_event, ...) is rendered one indent
    # deeper than its parent "Additional inclusion:" header. Child key-value
    # rows (Variable/Window) drop another indent further so the tree reads
    # cleanly.
    if (!is.null(enr$additional_inclusion)) {
      add_sub_item("Additional inclusion:", tint = "incl")
      for (ai in enr$additional_inclusion) {
        if (identical(ai$type, "age_range")) {
          add_sub_sub_item(
            "Age range:",
            paste0(ai$min, " - ", ai$max),
            tint = "incl"
          )
        } else if (identical(ai$type, "has_event")) {
          add_sub_sub_item(ai$name, tint = "incl")
          add_var(
            "Variable:",
            ai$implementation$source_variable_combined %||%
              ai$implementation$source_variable,
            sub_sub = TRUE,
            tint = "incl"
          )
          add_kv(
            "Window:",
            .format_window_human(ai$implementation),
            sub_sub = TRUE,
            tint = "incl"
          )
        } else {
          add_sub_sub_item(ai$name, tint = "incl")
        }
      }
    }

    # Additional exclusion (same indent rule as additional_inclusion)
    if (!is.null(enr$additional_exclusion)) {
      add_sub_item("Additional exclusion:", tint = "excl")
      for (ae in enr$additional_exclusion) {
        add_sub_sub_item(ae$name, tint = "excl")
        add_var(
          "Variable:",
          ae$implementation$source_variable_combined %||%
            ae$implementation$source_variable,
          sub_sub = TRUE,
          tint = "excl"
        )
        add_kv(
          "Window:",
          .format_window_human(ae$implementation),
          sub_sub = TRUE,
          tint = "excl"
        )
      }
    }
  }

  # -- write to sheet -------------------------------------------------------
  col_a <- vapply(rows, function(x) x$a %||% NA_character_, character(1))
  col_b <- vapply(rows, function(x) x$b %||% NA_character_, character(1))
  dt <- data.table::data.table(` ` = col_a, `  ` = col_b)
  openxlsx::writeData(wb, sht, dt, colNames = FALSE)

  for (i in seq_along(rows)) {
    rw <- rows[[i]]
    if (!is.null(rw$sa)) {
      openxlsx::addStyle(wb, sht, rw$sa, rows = i, cols = 1L)
    }
    if (!is.null(rw$sb)) openxlsx::addStyle(wb, sht, rw$sb, rows = i, cols = 2L)
  }
  openxlsx::setColWidths(wb, sht, cols = 1:2, widths = c(35, 70))
}

#' @noRd
#' The emulated trials `$s3_analyze()` has a result entry for.
#'
#' Reads the KEYS of `plan$results_ett` and no value inside it. "Was this trial
#' analysed at all" is a different question from "what does it report". No
#' accessor answers it. An accessor returns rows for what was stored. A trial
#' whose every work item failed stores a skip envelope, and it yields no row.
#'
#' A consumer that must separate "analysed and reported nothing" from "never
#' analysed" calls this. Every consumer that only reports numbers calls an
#' accessor instead.
#'
#' @param plan A TTEPlan.
#' @return A character vector, in stored order.
#' @noRd
.plan_analysed_ett_ids <- function(plan) {
  ids <- names(plan$results_ett)
  if (is.null(ids)) character(0) else as.character(ids)
}


#' The enrollments `$s3_analyze()` has a result entry for.
#'
#' The sibling of [.plan_analysed_ett_ids]. It reads the KEYS of
#' `plan$results_enrollment` and no value inside it. A sheet that says "no
#' results for this enrollment" reports that the stage never ran. That is a
#' different statement from "the stage ran and stored no panel".
#'
#' @param plan A TTEPlan.
#' @return A character vector, in stored order.
#' @noRd
.plan_analysed_enrollment_ids <- function(plan) {
  ids <- names(plan$results_enrollment)
  if (is.null(ids)) character(0) else as.character(ids)
}


#' The enrollments `$s1_generate_enrollments_and_ipw()` has a counts entry for.
#'
#' The third key reader, beside [.plan_analysed_ett_ids] and
#' [.plan_analysed_enrollment_ids]. It reads the KEYS of
#' `plan$enrollment_counts` and no value inside it. "Did the enrollment stage
#' run for this enrollment" is a different question from "what did it count".
#' No accessor answers it. An entry that stored two empty tables yields no
#' accessor row. That is not the same as no entry at all.
#'
#' @param plan A TTEPlan.
#' @return A character vector, in stored order.
#' @noRd
.plan_counted_enrollment_ids <- function(plan) {
  ids <- names(plan$enrollment_counts)
  if (is.null(ids)) character(0) else as.character(ids)
}


#' One enrollment's stored cohort counts, read through the accessors.
#'
#' `.build_cohort_flow()`, `.attrition_overall()` and
#' `.render_consort_sidecars()` all speak the PRODUCER's column names. The two
#' accessors return the same rows under the schema's names, so this renames
#' them back and filters to one enrollment. It selects and renames. It sums
#' nothing, it creates no row, and it fills no gap.
#'
#' @param plan A TTEPlan.
#' @param eid Character(1), the enrollment identifier.
#' @return A list with `attrition` and `matching`. Each is `NULL` when the plan
#'   stores no such table for this enrollment, which is the shape
#'   `.build_cohort_flow()` already tests for.
#' @noRd
.plan_cohort_counts <- function(plan, eid) {
  att <- plan$get_attrition()
  mat <- plan$get_matching()
  a <- att[which(att$enrollment_id == eid)]
  m <- mat[which(mat$enrollment_id == eid)]
  list(
    attrition = if (nrow(a) == 0L) {
      NULL
    } else {
      data.table::data.table(
        trial_id = a$trial_id,
        criterion = a$step_name,
        n_persons = a$n_persons,
        n_person_trials = a$n_person_trials,
        n_intervention = a$n_arm_intervention,
        n_comparator = a$n_arm_comparator
      )
    },
    matching = if (nrow(m) == 0L) {
      NULL
    } else {
      data.table::data.table(
        trial_id = m$trial_id,
        n_intervention_total = m$n_intervention_total,
        n_comparator_total = m$n_comparator_total,
        n_intervention_enrolled = m$n_intervention_enrolled,
        n_comparator_enrolled = m$n_comparator_enrolled
      )
    }
  )
}


.enrollment_label <- function(plan, eid) {
  if (is.null(plan$spec)) {
    return(eid)
  }
  for (enr in plan$spec$enrollments) {
    if (enr$id == eid) {
      if (!is.null(enr$name) && nzchar(enr$name)) return(enr$name)
    }
  }
  eid
}

#' Look up the (comparator, intervention) arm labels for an enrollment id from
#' the study spec. Returns NULL when the spec has no usable arm names.
#' @noRd
.lookup_arm_labels <- function(spec, enrollment_id) {
  if (is.null(spec) || is.null(spec$enrollments)) {
    return(NULL)
  }
  for (enr in spec$enrollments) {
    if (isTRUE(enr$id == enrollment_id)) {
      arms <- enr$treatment$arms
      if (is.null(arms)) {
        return(NULL)
      }
      intervention <- arms$intervention
      comparator <- arms$comparator
      if (is.null(intervention) || is.null(comparator)) {
        return(NULL)
      }
      return(c(
        comparator = as.character(comparator),
        intervention = as.character(intervention)
      ))
    }
  }
  NULL
}

#' (removed) -- main Table 1 is now stored separately by the enrollment
#' worker as `table1_ipw_trunc_main`, so no on-the-fly stripping is needed.

#' Is one enrollment's cached baseline result too old to export?
#'
#' `$export_tables()` calls this over `self$results_enrollment` and re-runs
#' `$recompute_baselines()` for every enrollment it marks stale. Three
#' generations of cache fail here:
#'
#' * **Pre-`swereg_table1`**: the panel is a `tableone` object, so it does not
#'   carry the `swereg_table1` class.
#' * **Pre-`smd_numeric`**: the panel is a `swereg_table1` built before
#'   `.swereg_table1()` emitted the unrounded `smd_numeric` column. The class
#'   test alone declares it current, so the Love plot would receive no numeric
#'   SMDs and no error would be raised.
#' * **Pre-SMD main panel**: the worker built `table1_ipw_trunc_main` with
#'   `include_smd = FALSE`, so the headline Table 1 carries no SMD column.
#'   The worker builds the four supplementary panels with `include_smd = TRUE`.
#'   A predicate that reads only the first present panel therefore calls this
#'   cache current.
#'
#' The check runs on EVERY panel the cached result holds, not on the first one
#' it finds. Each present panel MUST be a `swereg_table1` and MUST carry
#' `smd_numeric`. One failing panel marks the whole result stale.
#'
#' Absence is not failure. A panel the worker never produced is `NULL`, and the
#' check skips it. `table1_raw` is `NULL` when no raw file sits on disk.
#' `table1_ipw_trunc_main` is `NULL` when the enrollment has no `ipw_trunc`
#' column. A result with no panel at all is not stale: there is nothing to
#' refresh.
#'
#' The lookup uses `[[` and not `$`. `table1_ipw` is a strict prefix of
#' `table1_ipw_trunc`, so `$` partial matching would return the truncated panel
#' under the untruncated name.
#'
#' @param r One element of `plan$results_enrollment`, or `NULL`.
#' @return `TRUE` when the cached panels must be recomputed.
#' @noRd
.baseline_panel_is_stale <- function(r) {
  if (is.null(r)) {
    return(FALSE)
  }
  panel_names <- c(
    "table1_ipw_trunc",
    "table1_ipw_trunc_main",
    "table1_unweighted",
    "table1_ipw",
    "table1_raw"
  )
  panels <- lapply(intersect(panel_names, names(r)), function(nm) r[[nm]])
  panels <- Filter(Negate(is.null), panels)
  if (length(panels) == 0L) {
    return(FALSE)
  }
  is_current <- vapply(
    panels,
    function(p) {
      inherits(p, "swereg_table1") && "smd_numeric" %in% names(p)
    },
    logical(1)
  )
  !all(is_current)
}

#' Write a swereg_table1 data.table to a worksheet with bold header styling
#' and a fitted Variable column.
#' @noRd
.write_tableone_sheet <- function(wb, sheet_name, t1_dt, title = NULL) {
  # smd_numeric is a programmatic contract, not a display column.
  t1_dt <- .t1_drop_numeric(t1_dt)
  openxlsx::addWorksheet(wb, sheet_name)
  start_row <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = 1L,
      cols = 1L
    )
    start_row <- 3L
  }
  if (is.null(t1_dt) || nrow(t1_dt) == 0L) {
    openxlsx::writeData(wb, sheet_name, "(no data)", startRow = start_row)
    return(invisible(NULL))
  }
  openxlsx::writeData(
    wb,
    sheet_name,
    t1_dt,
    startRow = start_row,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  ncols <- ncol(t1_dt)
  widths <- c(50, 16, rep(22, max(0, ncols - 2L)))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncols), widths = widths)
}

#' The baseline count of one enrollment, read through `$get_baselines()`.
#'
#' `$get_baselines()` repeats the three enrollment counts on every row of that
#' enrollment's panels, so the first row carries them. An enrollment that
#' stored no panel yields no row and therefore no count.
#'
#' @param baselines A `$get_baselines()` table.
#' @param eid Character(1), the enrollment identifier.
#' @param field Character(1), one of `n_baseline`, `n_baseline_intervention`
#'   and `n_baseline_comparator`.
#' @return Numeric(1), `NA_real_` when the enrollment stored no panel.
#' @noRd
.baseline_count <- function(baselines, eid, field = "n_baseline") {
  if (is.null(baselines) || nrow(baselines) == 0L) {
    return(NA_real_)
  }
  hit <- which(baselines$enrollment_id == eid)
  if (length(hit) == 0L) {
    return(NA_real_)
  }
  as.numeric(baselines[[field]][hit[1L]])
}


#' The two arm labels a rendered baseline panel heads its columns with.
#'
#' Read from the panel that `$s3_analyze()` STORED, through
#' `$get_baselines()`. The panel was built with the arm labels the
#' specification held when the analysis ran. The stored header is therefore the
#' header those numbers belong to.
#'
#' The specification is NOT re-read here. A specification edited between the
#' analysis and the export would otherwise head yesterday's numbers with
#' today's labels. A specification that names no arms would replace a real
#' header with the two values of the treatment variable.
#'
#' @param baselines A `$get_baselines()` table.
#' @param eid Character(1), the enrollment identifier.
#' @return A named character(2), `comparator` and `intervention`. Both are
#'   `NA_character_` when the plan stores no panel for this enrollment.
#' @noRd
.baseline_arm_labels <- function(baselines, eid) {
  out <- c(comparator = NA_character_, intervention = NA_character_)
  if (is.null(baselines) || nrow(baselines) == 0L) {
    return(out)
  }
  hit <- which(
    baselines$enrollment_id == eid & !is.na(baselines$comparator_label)
  )
  if (length(hit) == 0L) {
    return(out)
  }
  c(
    comparator = as.character(baselines$comparator_label[hit[1L]]),
    intervention = as.character(baselines$intervention_label[hit[1L]])
  )
}


#' Rebuild one rendered baseline panel from `$get_baselines()`.
#'
#' `$get_baselines()` returns the stored cells and drops two rendering
#' conventions. This function restores both, which is the consumer's work:
#' \itemize{
#'   \item the variable name prints once per block. The accessor carries it
#'     down every row, so this blanks the repeats.
#'   \item the `SMD` column is a display string. The accessor keeps the
#'     unrounded double, so this formats it with `.t1_fmt_smd()`, the one
#'     formatter the producer used.
#' }
#'
#' The `SMD` column is composed only when the panel carries at least one
#' standardised mean difference. A panel built with `include_smd = FALSE`
#' carries none, and it had no such column.
#'
#' @param baselines A `$get_baselines()` table.
#' @param eid Character(1), the enrollment identifier.
#' @param imputation,weighting,variant The three panel keys.
#' @param arm_labels As returned by [.baseline_arm_labels].
#' @return A data.table with the rendered columns, or `NULL` when the plan
#'   stores no such panel.
#' @noRd
.baseline_panel <- function(
  baselines,
  eid,
  imputation,
  weighting,
  variant,
  arm_labels
) {
  if (is.null(baselines) || nrow(baselines) == 0L) {
    return(NULL)
  }
  hit <- which(
    baselines$enrollment_id == eid &
      baselines$imputation == imputation &
      baselines$weighting == weighting &
      baselines$variant == variant
  )
  if (length(hit) == 0L) {
    return(NULL)
  }
  rows <- baselines[hit]
  variable <- as.character(rows$variable)
  variable[is.na(variable)] <- ""
  n <- length(variable)
  if (n > 1L) {
    repeated <- c(FALSE, variable[-1L] == variable[-n])
    variable[repeated] <- ""
  }
  out <- data.table::data.table(
    Variable = variable,
    Level = as.character(rows$level),
    Overall = as.character(rows$overall)
  )
  data.table::set(
    out,
    j = arm_labels[["comparator"]],
    value = as.character(rows$comparator)
  )
  data.table::set(
    out,
    j = arm_labels[["intervention"]],
    value = as.character(rows$intervention)
  )
  # The stored SHAPE. A panel built with `include_smd = TRUE` carries the
  # column whatever its values, and a panel whose every standardised mean
  # difference is `NA` still heads a blank `SMD` column.
  if (isTRUE(rows$smd_stored[1L])) {
    data.table::set(
      out,
      j = "SMD",
      value = vapply(rows$smd_numeric, .t1_fmt_smd, character(1))
    )
  }
  out[]
}


#' @noRd
.write_enrollment_overview <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "Enrollments")
  enrollment_ids <- unique(plan$ett$enrollment_id)
  baselines <- plan$get_baselines()
  rows <- lapply(enrollment_ids, function(eid) {
    label <- .enrollment_label(plan, eid)
    n_base <- .baseline_count(baselines, eid, "n_baseline")
    # Treatment info from spec
    tx_info <- list(
      variable = NA,
      intervention = NA,
      comparator = NA,
      ratio = NA
    )
    row <- plan$ett[plan$ett$enrollment_id == eid][1]
    if (
      "treatment_impl" %in% names(plan$ett) && !is.null(row$treatment_impl[[1]])
    ) {
      impl <- row$treatment_impl[[1]]
      tx_info$variable <- impl$variable %||% NA
      tx_info$intervention <- impl$intervention_value %||% NA
      tx_info$comparator <- impl$comparator_value %||% NA
    }
    if ("matching_ratio" %in% names(plan$ett)) {
      tx_info$ratio <- row$matching_ratio
    }
    data.table::data.table(
      enrollment_id = eid,
      additional_criteria = label,
      treatment_variable = tx_info$variable,
      intervention_value = tx_info$intervention,
      comparator_value = tx_info$comparator,
      matching_ratio = tx_info$ratio,
      n_baseline = n_base
    )
  })
  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, "Enrollments", dt)
}

#' @noRd
.write_ett_overview <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "ETTs")
  # `n_events` repeats on every estimate row of an emulated trial, so the first
  # row carries it. A trial that stored no estimate at all yields no row and
  # therefore no count.
  est <- plan$get_estimates()
  rows <- lapply(seq_len(nrow(plan$ett)), function(i) {
    r <- plan$ett[i]
    ett_id <- r$ett_id
    hit <- which(est$ett_id == ett_id)
    data.table::data.table(
      ett_id = ett_id,
      enrollment_id = r$enrollment_id,
      outcome_var = r$outcome_var,
      outcome_name = r$outcome_name,
      follow_up = r$follow_up,
      description = r$description,
      n_events = if (length(hit) > 0L) est$n_events[hit[1L]] else NA
    )
  })
  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, "ETTs", dt)
}

#' The description of each emulated trial, read from `plan$ett`.
#'
#' `plan$ett` is an INPUT and it holds one row per emulated trial, so every
#' identifier has a description whatever the analysis stored.
#'
#' The stored result carries a `description` field too. Reading THAT over the
#' whole result list stopped an export. One trial's copy could be absent, or
#' could be more than one string. A single stale entry then blocked the trials
#' the caller had asked for. `$reload_spec()` no longer refreshes the stored
#' copy, so the field is now more likely to be absent.
#'
#' @param plan A TTEPlan.
#' @param ett_ids Character vector of identifiers, in the wanted order.
#' @return A named character vector as long as `ett_ids`. An identifier the
#'   grid does not carry falls back to the identifier itself.
#' @noRd
.ett_descriptions <- function(plan, ett_ids) {
  ett_ids <- as.character(ett_ids)
  out <- stats::setNames(ett_ids, ett_ids)
  ett <- plan$ett
  if (
    is.null(ett) ||
      nrow(ett) == 0L ||
      !all(c("ett_id", "description") %in% names(ett))
  ) {
    return(out)
  }
  hit <- match(ett_ids, as.character(ett$ett_id))
  desc <- as.character(ett$description)[hit]
  ok <- !is.na(hit) & !is.na(desc)
  out[ok] <- desc[ok]
  out
}


#' @noRd
.prepare_combine_data <- function(plan, slot, keep_ett_ids = NULL) {
  results <- plan$results_ett
  if (!is.null(keep_ett_ids)) {
    results <- results[names(results) %in% keep_ett_ids]
  }
  results_list <- lapply(results, function(r) {
    val <- r[[slot]]
    if (is.null(val) || isTRUE(val$skipped)) {
      return(NULL)
    }
    list(x = val)
  })
  results_list <- Filter(Negate(is.null), results_list)
  if (length(results_list) == 0L) {
    return(NULL)
  }

  combine_input <- lapply(results_list, `[[`, "x")
  names(combine_input) <- names(results_list)

  wrapped <- lapply(names(combine_input), function(n) {
    lst <- list()
    lst[[slot]] <- combine_input[[n]]
    lst
  })
  names(wrapped) <- names(combine_input)

  ett_desc <- .ett_descriptions(plan, names(wrapped))

  if (!is.null(keep_ett_ids)) {
    # Reorder to follow the user-specified ETT order
    keep <- intersect(keep_ett_ids, names(wrapped))
    wrapped <- wrapped[keep]
    ett_desc <- ett_desc[keep]
  }

  list(wrapped = wrapped, ett_desc = ett_desc)
}

#' Build a "Treatment definitions" data.table for the unique enrollments
#' touched by a set of ETT ids. Returns NULL when no enrollment metadata
#' is available.
#' @noRd
.build_treatment_legend <- function(plan, ett_ids = NULL) {
  ett <- plan$ett
  if (!is.null(ett_ids)) {
    ett <- ett[ett$ett_id %in% ett_ids]
  }
  if (nrow(ett) == 0L) {
    return(NULL)
  }
  enrollment_ids <- unique(ett$enrollment_id)
  rows <- lapply(enrollment_ids, function(eid) {
    enr <- NULL
    if (!is.null(plan$spec) && !is.null(plan$spec$enrollments)) {
      for (e in plan$spec$enrollments) {
        if (isTRUE(e$id == eid)) {
          enr <- e
          break
        }
      }
    }
    arms <- if (!is.null(enr)) enr$treatment$arms else NULL
    data.table::data.table(
      enrollment_id = eid,
      name = if (!is.null(enr$name)) enr$name else .enrollment_label(plan, eid),
      intervention = arms$intervention %||% NA_character_,
      comparator = arms$comparator %||% NA_character_,
      description = enr$treatment$description %||% NA_character_
    )
  })
  data.table::rbindlist(rows)
}

#' Decide whether to relabel the generic Intervention/Comparator column suffixes
#' to spec-derived arm labels. Only does so when every featured ETT shares the
#' same (intervention, comparator) labels.
#' @noRd
.unique_arm_labels <- function(legend) {
  if (is.null(legend) || nrow(legend) == 0L) {
    return(NULL)
  }
  int <- unique(stats::na.omit(legend$intervention))
  cmp <- unique(stats::na.omit(legend$comparator))
  if (length(int) != 1L || length(cmp) != 1L) {
    return(NULL)
  }
  c(intervention = int, comparator = cmp)
}

#' Rename `*_Intervention` / `*_Comparator` column suffixes on a combined
#' rates data.table to use spec-derived arm labels. No-op when labels can't
#' be resolved.
#' @noRd
.rename_treatment_columns <- function(dt, legend) {
  arms <- .unique_arm_labels(legend)
  if (is.null(arms)) {
    return(dt)
  }
  nm <- names(dt)
  nm <- gsub("_Intervention$", paste0("_", arms[["intervention"]]), nm)
  nm <- gsub("_Comparator$", paste0("_", arms[["comparator"]]), nm)
  data.table::setnames(dt, nm)
  dt
}

#' Write a treatment-definitions block to a worksheet at the given row, then
#' return the next free row.
#' @noRd
.write_treatment_legend <- function(wb, sheet_name, legend, start_row) {
  if (is.null(legend) || nrow(legend) == 0L) {
    return(start_row)
  }
  openxlsx::writeData(
    wb,
    sheet_name,
    "Treatment definitions",
    startRow = start_row,
    startCol = 1L
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold"),
    rows = start_row,
    cols = 1L
  )
  start_row <- start_row + 1L
  openxlsx::writeData(
    wb,
    sheet_name,
    legend,
    startRow = start_row,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  start_row + nrow(legend) + 2L
}

#' @noRd
.write_combined_rates <- function(
  wb,
  sheet_name,
  plan,
  slot,
  title = NULL,
  keep_ett_ids = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  prep <- .prepare_combine_data(plan, slot, keep_ett_ids = keep_ett_ids)
  if (is.null(prep)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid rates results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_rates_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  dt <- .rename_treatment_columns(dt, legend)
  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
}

#' Merge rates and IRR results for the same set of ETTs into one sheet.
#'
#' Uses [tteenrollment_combined_combine()] under the hood, then applies
#' `.rename_treatment_columns()` so the `_Intervention`/`_Comparator` suffixes
#' pick up spec-derived arm labels when the featured ETTs share one enrollment.
#' @noRd
.write_combined_rates_irr <- function(
  wb,
  sheet_name,
  plan,
  rates_slot,
  irr_slot,
  title = NULL,
  keep_ett_ids = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  # Keep only ETTs that have BOTH rates and IRR results. This avoids a
  # size-mismatch recycling warning in the merge step.
  results <- plan$results_ett
  if (!is.null(keep_ett_ids)) {
    results <- results[names(results) %in% keep_ett_ids]
  }
  keep_ids <- Filter(
    function(eid) {
      r <- results[[eid]]
      if (is.null(r)) {
        return(FALSE)
      }
      rv <- r[[rates_slot]]
      iv <- r[[irr_slot]]
      !is.null(rv) && !isTRUE(rv$skipped) && !is.null(iv) && !isTRUE(iv$skipped)
    },
    names(results)
  )
  if (length(keep_ids) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid combined results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }
  results <- results[keep_ids]
  if (!is.null(keep_ett_ids)) {
    # Preserve user-specified order
    keep_ordered <- intersect(keep_ett_ids, names(results))
    results <- results[keep_ordered]
  }

  ett_desc <- .ett_descriptions(plan, names(results))

  dt <- tryCatch(
    tteenrollment_combined_combine(
      results,
      rates_slot,
      irr_slot,
      ett_desc
    ),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  dt <- .rename_treatment_columns(dt, legend)
  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
}

#' Pull a one-row measurement block (events/PY/rate per arm + IRR + CI +
#' p-value) for a single emulated trial and a single estimand and weighting
#' combination.
#'
#' Reads `$get_estimates()`, never a result slot. Column names use generic
#' suffixes (`events_intervention`, `rate_cmp`, etc.) since the arm identities
#' are carried in the separate id columns of the sensitivity sheet.
#'
#' Returns `NULL` when the combination has nothing to report. Three states give
#' that answer, and `$get_estimates()` reports all three as absent rows or as
#' `NA`:
#' \itemize{
#'   \item the combination stored neither rates nor a ratio, so it has no row;
#'   \item the rates are unusable, which is every per-arm field `NA`. A stored
#'     rates table with no arm column, or with the wrong number of arm rows,
#'     reads this way;
#'   \item the ratio is unusable, which is every ratio field `NA`.
#' }
#'
#' @param est A `$get_estimates()` table.
#' @param ett_id Character(1).
#' @param slot Character(1), any slot name of the wanted combination.
#' @return A named list of eleven fields, or `NULL`.
#' @noRd
.sensitivity_row_measurements <- function(est, ett_id, slot) {
  combo <- .tte_slot_combo(slot)
  hit <- which(
    est$ett_id == ett_id &
      est$estimand == combo[["estimand"]] &
      est$weights == combo[["weights"]]
  )
  if (length(hit) == 0L) {
    return(NULL)
  }
  row <- est[hit[1L]]
  # The stored SHAPE, not the stored values. A combination whose rates table
  # holds `NA` numbers still reports its identifiers and its ratio, with blank
  # rate cells. A combination that has no usable rates table reports nothing.
  if (!isTRUE(row$rates_stored)) {
    return(NULL)
  }
  # The stored SHAPE, not the stored values. A combination whose ratio failed
  # still reports its arm counts, and a combination that has no ratio slot
  # reports nothing.
  if (!isTRUE(row$irr_stored) || !isTRUE(row$irr_interval_stored)) {
    return(NULL)
  }

  list(
    events_intervention = row$events_int,
    py_intervention = row$py_int,
    rate_intervention = row$rate_int,
    events_cmp = row$events_cmp,
    py_cmp = row$py_cmp,
    rate_cmp = row$rate_cmp,
    irr = row$irr,
    lo = row$irr_lo,
    hi = row$irr_hi,
    pvalue = row$irr_pvalue,
    irr_estimable = row$irr_estimable
  )
}


#' Excel number formats for the 9 fixed measurement columns. `NA` marks a
#' column that stays a human-formatted display string (IRR, 95% CI) -- those
#' are inherently composite, like Table 1's "n (%)". Every other column is
#' written as a bare number and formatted in Excel so it sorts and sums and
#' never trips the "number stored as text" warning.
#' @noRd
.MEASUREMENT_NUMFMT <- c(
  "Events (int)" = "0.0",
  "PY (int)" = "#,##0",
  "Rate/100k (int)" = "0.0",
  "Events (cmp)" = "0.0",
  "PY (cmp)" = "#,##0",
  "Rate/100k (cmp)" = "0.0",
  "IRR" = NA,
  "95% CI" = NA,
  "p-value" = "[<0.001]\"<0.001\";0.000"
)


#' Apply the measurement-column number formats to one side-by-side block whose
#' first measurement column sits at `block_start`, over body rows `data_rows`.
#' Numeric columns get their Excel numFmt; the IRR/CI display strings are left
#' alone. Styles are stacked so existing fills (e.g. block shading) survive.
#' @noRd
.apply_measurement_numfmt <- function(wb, sheet_name, block_start, data_rows) {
  if (length(data_rows) == 0L) {
    return(invisible(NULL))
  }
  fmts <- .MEASUREMENT_NUMFMT
  for (j in seq_along(fmts)) {
    f <- fmts[[j]]
    if (is.na(f)) {
      next
    }
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(numFmt = f),
      rows = data_rows,
      cols = block_start + j - 1L,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  invisible(NULL)
}


#' Is an incidence rate ratio estimable?
#'
#' The ONE place the package answers that question. `$s3_analyze()` calls it and
#' stores the answer as the `irr_estimable` column, beside the ratio it belongs
#' to. `.sensitivity_row_fmt()` calls it to decide whether to print the ratio.
#' Two copies of this test could drift apart, and a results sheet and a figure
#' would then disagree about the same ratio.
#'
#' An arm with no event gives a ratio of exactly 0, which is FINITE. An
#' `is.finite()` guard alone lets it print as `"0.00"` beside a zero-width
#' interval `"0.00 to 0.00"`. That reads as a point estimate of no risk, known
#' perfectly. It is neither: the ratio is inestimable.
#'
#' Every display reads the STORED answer through
#' `.tte_irr_estimable_stored()`. This function is the producer's rule and the
#' fallback for a result stored before the column existed.
#'
#' @param irr Numeric, the stored ratio. `NA` and `NaN` are not estimable.
#' @return A logical vector as long as `irr`.
#' @noRd
.tte_irr_estimable <- function(irr) {
  irr <- suppressWarnings(as.numeric(irr))
  is.finite(irr) & irr >= 0.01
}


#' The estimability decision for ONE stored incidence rate ratio.
#'
#' Reads the stored `irr_estimable` column. `$s3_analyze()` decides it once,
#' beside the ratio, and `$get_estimates()` carries it. A formatter that
#' re-tested the threshold would be a second decision site, and two displays of
#' one ratio could then disagree.
#'
#' A result stored before that column existed passes `NA`. The rule is then
#' applied here, by the ONE function that holds it. That is the consumer
#' deriving what the producer did not store, and three live projects hold such
#' results. Rendering nothing for them would blank a ratio that used to print.
#'
#' @param irr Numeric(1), the stored ratio.
#' @param irr_estimable Logical(1), the stored decision, or `NA`.
#' @return Logical(1).
#' @noRd
.tte_irr_estimable_stored <- function(irr, irr_estimable) {
  if (length(irr_estimable) == 1L && !is.na(irr_estimable)) {
    return(isTRUE(as.logical(irr_estimable)))
  }
  isTRUE(.tte_irr_estimable(irr))
}


#' Attach the estimability decision to one stored incidence rate ratio.
#'
#' The DECISION is data, and `$s3_analyze()` stores it. A reader of
#' `plan$results_ett` then sees whether the ratio may be printed, without
#' repeating the rule. This mirrors `nnt_direction` on the risk-difference row.
#'
#' A value that is not a table with an `IRR` column passes through unchanged.
#' That covers the skip envelope a failed worker returns.
#'
#' @param value One `$irr()` return value, or a skip envelope.
#' @return The same object, with an `irr_estimable` column when it carries one.
#' @noRd
.s3_mark_irr_estimable <- function(value) {
  if (!data.table::is.data.table(value) || !"IRR" %in% names(value)) {
    return(value)
  }
  data.table::set(value, j = "irr_estimable", value = .tte_irr_estimable(value$IRR))
  value
}


#' Format a single measurement block for one row of a results / sensitivity
#' sheet. Returns a named list of **typed** cells keyed by internal
#' disambiguating column names (`col_key_prefix` prepended to the 9 fixed
#' column names): events / PY / rate / p-value are bare numerics (formatted in
#' Excel via [.apply_measurement_numfmt]); IRR and 95% CI stay display strings.
#' Display headers are written separately by the sheet writer, so the prefix
#' never appears in the worksheet.
#' @noRd
.sensitivity_row_fmt <- function(m, col_key_prefix) {
  display_names <- names(.MEASUREMENT_NUMFMT)
  if (is.null(m)) {
    cells <- list(
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_character_,
      NA_character_,
      NA_real_
    )
  } else {
    # The STORED decision. `$s3_analyze()` makes it once, beside the ratio.
    # See `.tte_irr_estimable()` for why a ratio of exactly 0 is inestimable
    # rather than zero.
    irr_estimable <- .tte_irr_estimable_stored(m$irr, m$irr_estimable)
    ci <- if (irr_estimable && is.finite(m$lo) && is.finite(m$hi) &&
              m$lo > 0 && m$hi > 0) {
      sprintf("%.2f to %.2f", m$lo, m$hi)
    } else {
      NA_character_
    }
    cells <- list(
      as.numeric(m$events_intervention),
      as.numeric(m$py_intervention),
      as.numeric(m$rate_intervention),
      as.numeric(m$events_cmp),
      as.numeric(m$py_cmp),
      as.numeric(m$rate_cmp),
      if (irr_estimable) sprintf("%.2f", m$irr) else NA_character_,
      ci,
      as.numeric(m$pvalue)
    )
  }
  setNames(cells, paste0(col_key_prefix, display_names))
}


#' Write the "Full results" sheet: one row per ETT, with 5
#' identifier columns (Enrollment | Intervention | Comparator | Outcome |
#' Follow-up) and two side-by-side measurement blocks.
#'
#' Order: **truncated weights on the left, untruncated weights on the
#' right**. The untruncated block is shaded light grey to emphasise the
#' side-by-side comparison. Column headers within each block are just
#' `Events (int)`, `PY (int)`, etc. (no `[truncated]`/`[untruncated]`
#' suffix) -- the merged group header row carries the distinction.
#'
#' @noRd
.write_combined_sensitivity <- function(
  wb,
  sheet_name,
  plan,
  trunc_rates_slot,
  trunc_irr_slot,
  untrunc_rates_slot,
  untrunc_irr_slot,
  title = NULL,
  left_label = "Truncated weights",
  right_label = "Untruncated weights"
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  # Each side names ONE estimand and weighting combination, and the two slot
  # arguments of a side MUST agree about which. `$get_estimates()` keys the
  # result on the combination, so a mismatched pair would silently report the
  # rates of one weighting beside the ratio of another.
  for (pair in list(
    c(trunc_rates_slot, trunc_irr_slot),
    c(untrunc_rates_slot, untrunc_irr_slot)
  )) {
    if (!identical(.tte_slot_combo(pair[1]), .tte_slot_combo(pair[2]))) {
      stop(
        "'",
        pair[1],
        "' and '",
        pair[2],
        "' name different estimand and weighting combinations"
      )
    }
  }

  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No ETTs to report.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  display_names <- c(
    "Events (int)",
    "PY (int)",
    "Rate/100k (int)",
    "Events (cmp)",
    "PY (cmp)",
    "Rate/100k (cmp)",
    "IRR",
    "95% CI",
    "p-value"
  )

  # Build one row per ETT. Truncated columns come first, then untruncated.
  est <- plan$get_estimates()
  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    untrunc_m <- .sensitivity_row_measurements(est, eid, untrunc_irr_slot)
    trunc_m <- .sensitivity_row_measurements(est, eid, trunc_irr_slot)
    if (is.null(trunc_m) && is.null(untrunc_m)) {
      next
    }

    enr_id <- ett$enrollment_id[i]
    enr_name <- .enrollment_label(plan, enr_id)
    arms <- .lookup_arm_labels(plan$spec, enr_id)
    intervention_name <- if (!is.null(arms)) {
      arms[["intervention"]]
    } else {
      "Intervention"
    }
    comparator_name <- if (!is.null(arms)) {
      arms[["comparator"]]
    } else {
      "Comparator"
    }

    id_cols <- list(
      Enrollment = enr_name,
      Intervention = intervention_name,
      Comparator = comparator_name,
      Outcome = ett$outcome_name[i],
      `Follow-up (weeks)` = as.integer(ett$follow_up[i])
    )
    left_cols <- .sensitivity_row_fmt(trunc_m, "t_")
    right_cols <- .sensitivity_row_fmt(untrunc_m, "u_")
    rows[[length(rows) + 1L]] <- c(id_cols, left_cols, right_cols)
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid sensitivity results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)

  # Layout constants
  n_id <- 5L
  n_block <- length(display_names)
  trunc_cols_start <- n_id + 1L
  trunc_cols_end <- n_id + n_block
  untrunc_cols_start <- trunc_cols_end + 1L
  untrunc_cols_end <- trunc_cols_end + n_block

  group_header_row <- row_ptr
  col_header_row <- row_ptr + 1L
  data_start_row <- row_ptr + 2L

  # --- Styles ---
  group_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    fontSize = 12,
    fgFill = "#D9D9D9",
    border = "TopBottom"
  )
  group_header_untrunc_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    fontSize = 12,
    fgFill = "#BFBFBF",
    border = "TopBottom"
  )
  id_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )
  col_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )
  col_header_untrunc_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#DDDDDD",
    border = "bottom"
  )
  body_untrunc_style <- openxlsx::createStyle(fgFill = "#F2F2F2")

  # --- Group header row ---
  openxlsx::mergeCells(
    wb,
    sheet_name,
    cols = untrunc_cols_start:untrunc_cols_end,
    rows = group_header_row
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    right_label,
    startCol = untrunc_cols_start,
    startRow = group_header_row
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = group_header_untrunc_style,
    rows = group_header_row,
    cols = untrunc_cols_start
  )

  openxlsx::mergeCells(
    wb,
    sheet_name,
    cols = trunc_cols_start:trunc_cols_end,
    rows = group_header_row
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    left_label,
    startCol = trunc_cols_start,
    startRow = group_header_row
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = group_header_style,
    rows = group_header_row,
    cols = trunc_cols_start
  )

  # --- Column header row (id cols + display names for both blocks) ---
  id_names <- c(
    "Enrollment",
    "Intervention",
    "Comparator",
    "Outcome",
    "Follow-up (weeks)"
  )
  header_row <- c(id_names, display_names, display_names)
  for (k in seq_along(header_row)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      header_row[k],
      startCol = k,
      startRow = col_header_row
    )
  }
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = id_header_style,
    rows = col_header_row,
    cols = seq_len(n_id),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = col_header_untrunc_style,
    rows = col_header_row,
    cols = untrunc_cols_start:untrunc_cols_end,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = col_header_style,
    rows = col_header_row,
    cols = trunc_cols_start:trunc_cols_end,
    gridExpand = TRUE
  )

  # --- Body: write the data without its own header row ---
  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = data_start_row,
    colNames = FALSE
  )

  data_end_row <- data_start_row + nrow(dt) - 1L
  if (nrow(dt) > 0L) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = body_untrunc_style,
      rows = data_start_row:data_end_row,
      cols = untrunc_cols_start:untrunc_cols_end,
      gridExpand = TRUE,
      stack = TRUE
    )
    body_rows <- data_start_row:data_end_row
    .apply_measurement_numfmt(wb, sheet_name, trunc_cols_start, body_rows)
    .apply_measurement_numfmt(wb, sheet_name, untrunc_cols_start, body_rows)
  }

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(untrunc_cols_end),
    widths = c(
      30,
      20,
      20,
      30,
      12,
      rep(14, n_block),
      rep(14, n_block)
    )
  )
  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = data_start_row,
    firstActiveCol = n_id + 1L
  )
}


#' Excel number formats for the three NUMERIC risk-difference columns that the
#' single-estimand results sheets carry after the measurement block. The
#' interval is a fourth column and stays a display string, like `95% CI`.
#'
#' The risk-difference format prints an explicit `+` on a positive value. The
#' sign is the clinical direction, so it is not decoration. `+4.88` and `-4.88`
#' are opposite results, and a reader must not have to look for a minus.
#' @noRd
.RD_SHEET_NUMFMT <- c(
  "Persons with event (int)" = "#,##0",
  "Persons with event (cmp)" = "#,##0",
  "Risk difference per 10,000" = "+0.00;-0.00;0.00"
)


#' Build the `rd_lookup` a forest figure draws its risk-difference columns
#' from, out of `$get_estimates()`.
#'
#' `.forest_rd_map()` keys the lookup on `ett_id` and reads six required
#' columns plus the two decision columns. `$get_estimates()` carries all eight
#' under the accessor's own names, so this renames rather than computes.
#'
#' An emulated trial gets a row when the plan stored a risk difference for that
#' estimand and weighting. Every risk-difference field `NA` means the plan
#' stored none, and the trial then gets no row and renders an empty cell.
#'
#' @param plan A TTEPlan.
#' @param rd_slot Character(1), the risk-difference slot naming the wanted
#'   combination.
#' @param keep_ett_ids Character vector of the identifiers the figure draws.
#' @return A data.table, or `NULL` when nothing was stored.
#' @noRd
.tte_rd_lookup <- function(plan, rd_slot, keep_ett_ids) {
  est <- .tte_estimates_for_slot(plan, rd_slot)
  if (nrow(est) == 0L) {
    return(NULL)
  }
  # The stored SHAPE. A risk-difference row whose values are `NA` still gets a
  # lookup entry, as it did when this read the slot directly.
  hit <- which(est$rd_stored & est$ett_id %in% keep_ett_ids)
  if (length(hit) == 0L) {
    return(NULL)
  }
  data.table::data.table(
    ett_id = as.character(est$ett_id[hit]),
    rd = est$rd[hit],
    rd_lo = est$rd_lo[hit],
    rd_hi = est$rd_hi[hit],
    nnt = est$nnt[hit],
    nnt_direction = est$nnt_direction[hit],
    n_persons_with_event_intervention = est$persons_event_int[hit],
    n_persons_with_event_comparator = est$persons_event_cmp[hit],
    conf_level = est$conf_level[hit]
  )
}


#' Build the four risk-difference cells for one row of a results sheet.
#'
#' The two counts are distinct PEOPLE who had the outcome, unweighted. They are
#' NOT the `Events (int)` / `Events (cmp)` columns in the measurement block.
#' Those are weighted sums over event ROWS, and they count one woman twice when
#' she carries the event in two of her sequential trials. The headers say which
#' is which.
#'
#' The risk difference keeps its sign and is scaled to 10,000 people, matching
#' the forest figure.
#'
#' @param rd_row A one-row table carrying the `$get_estimates()` risk-difference
#'   columns (`persons_event_int`, `persons_event_cmp`, `rd`, `rd_lo` and
#'   `rd_hi`), or NULL.
#' @return An unnamed list of four cells: two counts, the risk difference, and
#'   its interval as a display string.
#' @noRd
.rd_sheet_cells <- function(rd_row) {
  if (is.null(rd_row) || nrow(rd_row) == 0L) {
    return(list(NA_real_, NA_real_, NA_real_, NA_character_))
  }
  per <- 10000
  pick <- function(nm) as.numeric(rd_row[[nm]])[1]
  rd <- pick("rd")
  lo <- pick("rd_lo")
  hi <- pick("rd_hi")
  list(
    pick("persons_event_int"),
    pick("persons_event_cmp"),
    if (is.finite(rd)) rd * per else NA_real_,
    if (is.finite(lo) && is.finite(hi)) {
      sprintf("%+.2f to %+.2f", lo * per, hi * per)
    } else {
      NA_character_
    }
  )
}


#' Resolve the confidence level the risk-difference interval header states.
#'
#' One header covers the whole column, so every interval under it must have
#' been computed at one level. This keeps the contract [.forest_rd_conf_level()]
#' sets for the figure: refuse rather than print a level the numbers do not
#' have.
#'
#' @param levels Numeric vector of per-row confidence levels, `NA` allowed.
#' @return A character(1) percentage with no percent sign. Falls back to `"95"`
#'   when no row recorded a level.
#' @noRd
.rd_sheet_conf_pct <- function(levels) {
  seen <- unique(as.numeric(levels))
  seen <- seen[!is.na(seen)]
  if (length(seen) == 0L) {
    return(.ff_conf_pct(0.95))
  }
  if (length(seen) > 1L) {
    stop(
      "the risk differences on this sheet mix confidence levels (",
      paste(seen, collapse = ", "),
      "); one column cannot carry two."
    )
  }
  .ff_conf_pct(seen)
}


#' Write a single-estimand results sheet: one row per ETT with the 5 identifier
#' columns and one measurement block (events / PY / rate per arm + IRR + 95% CI
#' + p-value). Numbers are real (Excel numFmt via [.apply_measurement_numfmt]);
#' IRR and 95% CI are display strings. Used for "PP results" and "ITT results".
#'
#' `rd_slot` names the per-ETT list element holding the risk-difference row
#' (`"rd_pp_trunc"` or `"rd_itt"`, written by `$s3_analyze()` for every ETT).
#' When at least one ETT carries one, four more columns follow the measurement
#' block.
#' Those are the per-arm distinct-person event counts, the signed risk
#' difference per 10,000 people, and its interval. When no ETT carries one, the
#' four columns are left out rather than heading a block of empty cells.
#' @noRd
.write_results_single <- function(
  wb,
  sheet_name,
  plan,
  rates_slot,
  irr_slot,
  rd_slot = NULL,
  title = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No ETTs to report.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  if (!identical(.tte_slot_combo(rates_slot), .tte_slot_combo(irr_slot))) {
    stop(
      "'",
      rates_slot,
      "' and '",
      irr_slot,
      "' name different estimand and weighting combinations"
    )
  }
  # The risk difference belongs to the SAME combination as the rates and the
  # ratio, so `$get_estimates()` already carries it on the same row.
  if (
    !is.null(rd_slot) &&
      !identical(.tte_slot_combo(rd_slot), .tte_slot_combo(irr_slot))
  ) {
    stop(
      "'",
      rd_slot,
      "' and '",
      irr_slot,
      "' name different estimand and weighting combinations"
    )
  }

  est <- plan$get_estimates()
  combo <- .tte_slot_combo(irr_slot)
  display_names <- names(.MEASUREMENT_NUMFMT)
  rd_names <- names(.RD_SHEET_NUMFMT)
  rd_cells <- list()
  rd_levels <- numeric(0)
  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    m <- .sensitivity_row_measurements(est, eid, irr_slot)
    if (is.null(m)) {
      next
    }
    hit <- which(
      est$ett_id == eid &
        est$estimand == combo[["estimand"]] &
        est$weights == combo[["weights"]]
    )
    rd_row <- if (
      is.null(rd_slot) ||
        length(hit) == 0L ||
        !isTRUE(est$rd_stored[hit[1L]])
    ) {
      NULL
    } else {
      est[hit[1L]]
    }
    rd_cells[[length(rd_cells) + 1L]] <- .rd_sheet_cells(rd_row)
    if (!is.null(rd_row)) {
      rd_levels <- c(rd_levels, as.numeric(rd_row[["conf_level"]])[1])
    }
    enr_id <- ett$enrollment_id[i]
    arms <- .lookup_arm_labels(plan$spec, enr_id)
    intervention_name <- if (!is.null(arms)) {
      arms[["intervention"]]
    } else {
      "Intervention"
    }
    comparator_name <- if (!is.null(arms)) {
      arms[["comparator"]]
    } else {
      "Comparator"
    }
    id_cols <- list(
      Enrollment = .enrollment_label(plan, enr_id),
      Intervention = intervention_name,
      Comparator = comparator_name,
      Outcome = ett$outcome_name[i],
      `Follow-up (weeks)` = as.integer(ett$follow_up[i])
    )
    rows[[length(rows) + 1L]] <- c(id_cols, .sensitivity_row_fmt(m, ""))
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(wb, sheet_name, "No valid results.", startRow = row_ptr)
    return(invisible(NULL))
  }

  # The four risk-difference columns are composed only when something populated
  # them. A header over a block of empty cells claims a quantity that was never
  # computed, and computing it costs minutes per ETT, so most exports have none.
  has_rd <- any(vapply(
    rd_cells,
    function(cells) is.finite(cells[[3]]),
    logical(1)
  ))
  if (has_rd) {
    rd_headers <- c(
      rd_names,
      paste0("Risk difference ", .rd_sheet_conf_pct(rd_levels), "% CI")
    )
    for (k in seq_along(rows)) {
      rows[[k]] <- c(rows[[k]], setNames(rd_cells[[k]], rd_headers))
    }
  } else {
    # A caller that named an `rd_slot` asked for the risk difference and got
    # nothing. $s3_analyze() writes that slot for every ETT, so a cold cache
    # now means s3 has not run against this plan, or every ETT failed. Say so.
    # Dropping four columns in silence is how a stale results file stays
    # invisible: the sheet still looks complete.
    if (!is.null(rd_slot) && length(rd_cells) > 0L) {
      message(
        "No cached risk difference for '", rd_slot, "', so this sheet omits ",
        "the risk-difference columns. Run $s3_analyze() before ",
        "$export_tables()."
      )
    }
    rd_headers <- character(0)
  }

  dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  n_id <- 5L
  col_header_row <- row_ptr
  data_start_row <- row_ptr + 1L

  id_names <- c(
    "Enrollment",
    "Intervention",
    "Comparator",
    "Outcome",
    "Follow-up (weeks)"
  )
  header_row <- c(id_names, display_names, rd_headers)
  for (k in seq_along(header_row)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      header_row[k],
      startCol = k,
      startRow = col_header_row
    )
  }
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    ),
    rows = col_header_row,
    cols = seq_along(header_row),
    gridExpand = TRUE
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = data_start_row,
    colNames = FALSE
  )
  data_end_row <- data_start_row + nrow(dt) - 1L
  .apply_measurement_numfmt(
    wb,
    sheet_name,
    n_id + 1L,
    data_start_row:data_end_row
  )
  if (has_rd) {
    rd_start <- n_id + length(display_names) + 1L
    for (j in seq_along(.RD_SHEET_NUMFMT)) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        style = openxlsx::createStyle(numFmt = .RD_SHEET_NUMFMT[[j]]),
        rows = data_start_row:data_end_row,
        cols = rd_start + j - 1L,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_along(header_row),
    widths = c(
      30,
      20,
      20,
      30,
      12,
      rep(14, length(display_names)),
      rep(24, length(rd_headers))
    )
  )
  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = data_start_row,
    firstActiveCol = n_id + 1L
  )
  invisible(NULL)
}


#' Write the "ITT vs PP forest" sheet: a numeric head-to-head table (real
#' `ITT IRR` / `PP IRR` columns + CIs + p-values) on top, and the two-colour
#' overlay forest plot (blue intention-to-treat, red per-protocol) embedded
#' below. Plot colours live only in the figure; the table cells are plain
#' numbers.
#' @noRd
.write_itt_vs_pp_forest <- function(
  wb,
  sheet_name,
  plan,
  keep_ett_ids = NULL,
  group_labels = NULL,
  title = NULL,
  label_format = NULL,
  desc_header = NULL,
  role_headers = NULL,
  img_dir,
  img_basename
) {
  outcome_name <- group_label <- follow_up <- NULL # nolint
  irr_pp <- lo_pp <- hi_pp <- pvalue_pp <- NULL # nolint
  irr_itt <- lo_itt <- hi_itt <- pvalue_itt <- NULL # nolint

  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  df <- .build_itt_vs_pp_df(plan, keep_ett_ids, group_labels)
  if (is.null(df) || nrow(df) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid IRR results to plot.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  tab <- df[, .(
    Comparison = group_label,
    Outcome = outcome_name,
    `Follow-up (weeks)` = follow_up,
    `ITT IRR` = irr_itt,
    `ITT 95% CI` = mapply(.ff_ci_only, lo_itt, hi_itt),
    `ITT p` = pvalue_itt,
    `PP IRR` = irr_pp,
    `PP 95% CI` = mapply(.ff_ci_only, lo_pp, hi_pp),
    `PP p` = pvalue_pp
  )]
  openxlsx::writeData(
    wb,
    sheet_name,
    tab,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  tab_rows <- (row_ptr + 1L):(row_ptr + nrow(tab))
  st_irr <- openxlsx::createStyle(numFmt = "0.00")
  st_p <- openxlsx::createStyle(numFmt = "[<0.001]\"<0.001\";0.000")
  for (cc in c(4L, 7L)) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      st_irr,
      rows = tab_rows,
      cols = cc,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  for (cc in c(6L, 9L)) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      st_p,
      rows = tab_rows,
      cols = cc,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = 1:9,
    widths = c(34, 30, 14, 10, 16, 10, 10, 16, 10)
  )

  plot_row <- row_ptr + nrow(tab) + 2L
  rendered <- tryCatch(
    .render_itt_vs_pp_overlay(
      df,
      title = NULL,
      label_format = label_format,
      desc_header = desc_header,
      role_headers = role_headers
    ),
    error = function(e) {
      warning("ITT vs PP overlay rendering failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(rendered)) {
    return(invisible(NULL))
  }
  paths <- .save_plot_sidecars(
    rendered$plot,
    rendered$width,
    rendered$height,
    img_dir,
    img_basename
  )
  openxlsx::insertImage(
    wb,
    sheet_name,
    paths$png,
    startRow = plot_row,
    startCol = 1L,
    width = rendered$width,
    height = rendered$height,
    units = "in",
    dpi = 300
  )
  invisible(paths)
}


#' Write an "Effect modification" sheet: per ETT x subgroup, the stratum IRRs
#' (per-protocol and intention-to-treat side by side) and the interaction-test
#' p-value / ratio of stratum IRRs.
#'
#' Reads `$get_subgroups()`, which returns the union of the two stored slot
#' families and reports a skipped result as absent.
#'
#' The sheet iterates the SPECIFICATION, `plan$ett$subgroup_vars`, and
#' `$get_subgroups()` iterates what was stored. A variable the specification
#' names and no worker stored therefore gets no accessor row, and this function
#' emits the one all-`NA` row it always did. That row is the consumer's, and
#' the accessor invents nothing.
#' @noRd
.write_effect_modification <- function(wb, sheet_name, plan, title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No ETTs to report.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  sg <- plan$get_subgroups()
  analysed <- .plan_analysed_ett_ids(plan)

  # `which()` runs OUTSIDE the data.table subset, so `want_estimand` is the
  # argument. Inside `sg[...]` it would resolve to the COLUMN of that name and
  # the filter would keep every estimand.
  slot_rows <- function(eid, sv, want_estimand) {
    if (nrow(sg) == 0L) {
      return(sg)
    }
    hit <- which(
      sg$ett_id == eid &
        sg$subgroup_var == sv &
        sg$estimand == want_estimand
    )
    sg[hit]
  }
  # `strata_stored` is the stored SHAPE: the plan holds a stratified table for
  # this subgroup variable and estimand. A row without it is the accessor's
  # INTERACTION-ONLY row, which stands for a stored interaction test with no
  # stored stratified table, and it names no stratum.
  #
  # The test is on the shape and never on the numbers. A stored stratum whose
  # rate ratio is inestimable keeps its level, so a per-protocol level that
  # could not be computed never removes the intention-to-treat result beside
  # it.
  strata_levels <- function(rows) {
    if (nrow(rows) == 0L) {
      return(character(0))
    }
    as.character(rows$subgroup_level)[which(rows$strata_stored)]
  }
  irr_cell <- function(rows, lvl) {
    hit <- which(as.character(rows$subgroup_level) == lvl)
    if (nrow(rows) == 0L || length(hit) == 0L) {
      return(list(irr = NA_real_, ci = NA_character_))
    }
    rr <- rows[hit[1L]]
    list(
      irr = rr$irr,
      ci = if (is.na(rr$irr)) {
        NA_character_
      } else {
        sprintf("(%.2f, %.2f)", rr$irr_lo, rr$irr_hi)
      }
    )
  }
  em_val <- function(rows, field) {
    if (nrow(rows) == 0L) {
      return(NA_real_)
    }
    as.numeric(rows[[field]][1L])
  }

  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    if (!eid %in% analysed) {
      next
    }
    sg_vars <- if (
      "subgroup_vars" %in% names(ett) && !is.null(ett$subgroup_vars[[i]])
    ) {
      ett$subgroup_vars[[i]]
    } else {
      character(0)
    }
    for (sv in sg_vars) {
      pp <- slot_rows(eid, sv, "pp")
      itt <- slot_rows(eid, sv, "itt")
      pp_levels <- strata_levels(pp)
      itt_levels <- strata_levels(itt)
      levels <- if (length(pp_levels) > 0L) {
        pp_levels
      } else if (length(itt_levels) > 0L) {
        itt_levels
      } else {
        "all"
      }
      for (lvl in levels) {
        pc <- irr_cell(pp, lvl)
        ic <- irr_cell(itt, lvl)
        is_all <- identical(lvl, "all")
        rows[[length(rows) + 1L]] <- data.frame(
          Enrollment = eid,
          Outcome = ett$outcome_name[i],
          Subgroup = sv,
          Level = as.character(lvl),
          `PP IRR` = pc$irr,
          `PP 95% CI` = pc$ci,
          `ITT IRR` = ic$irr,
          `ITT 95% CI` = ic$ci,
          `EM p (PP)` = if (is_all) em_val(pp, "em_pvalue") else NA_real_,
          `EM ratio (PP)` = if (is_all) {
            em_val(pp, "ratio_of_irrs")
          } else {
            NA_real_
          },
          `EM p (ITT)` = if (is_all) em_val(itt, "em_pvalue") else NA_real_,
          `EM ratio (ITT)` = if (is_all) {
            em_val(itt, "ratio_of_irrs")
          } else {
            NA_real_
          },
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No subgroups configured (add a top-level `subgroups:` block to the spec).",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  df <- do.call(rbind, rows)
  openxlsx::writeData(
    wb,
    sheet_name,
    df,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(df)),
    widths = "auto"
  )
  invisible(NULL)
}


#' @noRd
.write_combined_irr <- function(
  wb,
  sheet_name,
  plan,
  slot,
  title = NULL,
  keep_ett_ids = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  prep <- .prepare_combine_data(plan, slot, keep_ett_ids = keep_ett_ids)
  if (is.null(prep)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid IRR results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_irr_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
}

# .write_consort() / .write_consort_text() / .write_consort_flowchart() live
# in R/consort.R. The dispatcher tries the flowchart path and falls back to
# the text table when DiagrammeR/DiagrammeRsvg/rsvg are unavailable or
# rendering errors out.

#' @noRd
.write_combined_baseline <- function(wb, sheet_name, plan, eid) {
  openxlsx::addWorksheet(wb, sheet_name)
  label <- .enrollment_label(plan, eid)
  title <- paste0(
    "Enrollment ",
    eid,
    " (",
    label,
    ") -- Baseline characteristics"
  )
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )

  # Summary sentence: unique persons + sequential-TTE person-trial counts
  # pulled from the attrition table + the post-matching baseline row count.
  # Surfacing both numbers protects against the common reviewer confusion
  # where a 22M-person-week figure is mistaken for 22M participants.
  summary_line <- .format_enrollment_summary(plan, eid)
  header_row <- 2L
  data_row <- 3L
  if (!is.null(summary_line)) {
    openxlsx::writeData(wb, sheet_name, summary_line, startRow = 2L)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(fontSize = 10, textDecoration = "italic"),
      rows = 2L,
      cols = 1L
    )
    header_row <- 4L
    data_row <- 5L
  }

  if (!eid %in% .plan_analysed_enrollment_ids(plan)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No results for this enrollment.",
      startRow = header_row + 1L
    )
    return(invisible(NULL))
  }

  baselines <- plan$get_baselines()
  arm_labels <- .baseline_arm_labels(baselines, eid)
  panel <- function(imputation, weighting) {
    .baseline_panel(
      baselines,
      eid,
      imputation,
      weighting,
      "supplementary",
      arm_labels
    )
  }
  panels <- list(
    `Unimputed and unweighted` = panel("raw", "none"),
    `Imputed and unweighted` = panel("imputed", "none"),
    `Imputed and IPW` = panel("imputed", "ipw"),
    `Imputed and IPW truncated` = panel("imputed", "ipw_trunc")
  )

  panels <- Filter(Negate(is.null), panels)
  # smd_numeric is a programmatic contract, not a display column. Strip it
  # before ncol() decides the merged header width for each panel.
  panels <- lapply(panels, .t1_drop_numeric)
  if (length(panels) == 0L) {
    return(invisible(NULL))
  }

  start_col <- 1L

  bold_centre <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center"
  )
  table_header <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )

  for (name in names(panels)) {
    df <- panels[[name]]
    ncols <- ncol(df)
    if (ncols > 1) {
      openxlsx::mergeCells(
        wb,
        sheet_name,
        cols = start_col:(start_col + ncols - 1L),
        rows = header_row
      )
    }
    openxlsx::writeData(
      wb,
      sheet_name,
      name,
      startCol = start_col,
      startRow = header_row
    )
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = bold_centre,
      rows = header_row,
      cols = start_col
    )
    openxlsx::writeData(
      wb,
      sheet_name,
      df,
      startCol = start_col,
      startRow = data_row,
      headerStyle = table_header
    )
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = start_col,
      widths = 50
    )
    if (ncols > 1) {
      openxlsx::setColWidths(
        wb,
        sheet_name,
        cols = (start_col + 1L):(start_col + ncols - 1L),
        widths = 18
      )
    }
    start_col <- start_col + ncols + 1L
  }
}


#' Render a one-line enrollment summary sentence for the top of a results
#' sheet. Pulls unique-person and person-trial counts from `$get_attrition()`
#' (final criterion row) and the post-matching baseline count from
#' `$get_baselines()`. Returns NULL when the required fields are absent.
#' @noRd
.format_enrollment_summary <- function(plan, eid) {
  ec <- .plan_cohort_counts(plan, eid)
  if (is.null(ec$attrition) || nrow(ec$attrition) == 0L) {
    return(NULL)
  }
  overall <- .attrition_overall(ec$attrition)
  if (is.null(overall) || nrow(overall) == 0L) {
    return(NULL)
  }
  last <- overall[nrow(overall)]
  n_baseline <- .baseline_count(plan$get_baselines(), eid, "n_baseline")
  fmt <- function(x) format(x, big.mark = ",")
  parts <- c(
    sprintf(
      "Cohort: %s unique persons contributed %s sequential trial enrollments (intervention: %s / comparator: %s person-trials).",
      fmt(last$n_persons),
      fmt(last$n_person_trials),
      fmt(last$n_intervention),
      fmt(last$n_comparator)
    )
  )
  # True post-matching count comes from the matching table (enrolled
  # intervention + comparator person-trials), NOT from n_baseline.
  if (!is.null(ec$matching)) {
    m <- ec$matching
    n_int <- sum(m$n_intervention_enrolled, na.rm = TRUE)
    n_cmp <- sum(m$n_comparator_enrolled, na.rm = TRUE)
    if ((n_int + n_cmp) > 0L) {
      parts <- c(
        parts,
        sprintf(
          "After matching: %s person-trials entered baseline (intervention: %s / comparator: %s).",
          fmt(n_int + n_cmp),
          fmt(n_int),
          fmt(n_cmp)
        )
      )
    }
  }
  # n_baseline is the per-protocol analysis dataset (matched person-trials
  # minus those censored in the first period for protocol deviation or loss
  # to follow-up), NOT the post-matching count.
  # `.baseline_count()` reports an absent count as `NA`, so the guard tests for
  # a true comparison rather than for a non-NULL value.
  if (isTRUE(n_baseline > 0)) {
    parts <- c(
      parts,
      sprintf(
        "Analysis dataset (per-protocol): %s person-trials, after first-period censoring (protocol deviation or loss to follow-up; accounted for by IPCW).",
        fmt(n_baseline)
      )
    )
  }
  paste(parts, collapse = " ")
}


#' Write the CONSORT attrition numbers for one enrollment to a sheet.
#' Carries `criterion`, `n_persons`, `n_person_trials`, `n_intervention`,
#' and `n_comparator`, aggregated across trial_ids. Companion to the
#' CONSORT PNG/PDF sidecars: readers can cite exact numbers without
#' measuring pixels. The counts come from `$get_attrition()` and
#' `$get_matching()`.
#'
#' @return `TRUE` when the sheet was added to `wb`, and `FALSE` when it was
#'   not. The caller MUST read this before it names the sheet in the table of
#'   contents. Two states write nothing: an absent attrition table, and an
#'   attrition table that `.build_cohort_flow()` refuses.
#' @noRd
.write_attrition_sheet <- function(wb, sheet_name, plan, eid) {
  ec <- .plan_cohort_counts(plan, eid)
  if (is.null(ec$attrition) || nrow(ec$attrition) == 0L) {
    return(invisible(FALSE))
  }
  # Same single source of truth as the CONSORT diagram, so the sheet and the
  # picture cannot disagree. Includes the matching (selection) and per-
  # protocol analysis (censoring) steps, each tagged by `kind`/`change_kind`
  # so the matching/analysis reductions are NOT mislabelled as exclusions.
  baselines <- plan$get_baselines()
  analysis_n <- .baseline_count(baselines, eid, "n_baseline")
  flow <- .build_cohort_flow(
    ec,
    # `.build_cohort_flow()` treats an absent size as `NULL`, and
    # `.baseline_count()` reports it as `NA`.
    analysis_n = if (is.na(analysis_n)) NULL else analysis_n,
    analysis_n_intervention = .baseline_count(
      baselines,
      eid,
      "n_baseline_intervention"
    ),
    analysis_n_comparator = .baseline_count(
      baselines,
      eid,
      "n_baseline_comparator"
    )
  )
  if (is.null(flow) || nrow(flow) == 0L) {
    return(invisible(FALSE))
  }

  openxlsx::addWorksheet(wb, sheet_name)
  label <- .enrollment_label(plan, eid)
  title <- paste0(
    "Enrollment ",
    eid,
    " (",
    label,
    ") -- cohort derivation (CONSORT)"
  )
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )

  out <- data.table::copy(flow)
  data.table::setcolorder(
    out,
    c(
      "step",
      "kind",
      "n_persons",
      "n_person_trials",
      "change_persons",
      "change_person_trials",
      "change_kind",
      "n_intervention",
      "n_comparator"
    )
  )

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    out,
    startRow = 3L,
    headerStyle = header_style
  )
  # Counts are already real numbers (writeData on a numeric data.table); add a
  # thousands-separator display format so they read cleanly. Columns 1/2/7 are
  # text (step / kind / change_kind).
  if (nrow(out) > 0L) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(numFmt = "#,##0"),
      rows = 4L:(3L + nrow(out)),
      cols = c(3L, 4L, 5L, 6L, 8L, 9L),
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  openxlsx::setColWidths(wb, sheet_name, cols = 1L, widths = 45)
  openxlsx::setColWidths(wb, sheet_name, cols = 2L:9L, widths = 18)
  invisible(TRUE)
}


# =============================================================================
# Package-level workers for Loop 1 and Loop 2 (not exported)
# =============================================================================

# --- Enrollment counts persistence helpers -----------------------------------

#' Build path for a per-enrollment counts file.
#' @noRd
.enrollment_counts_path <- function(output_dir, prefix, eid) {
  file.path(output_dir, paste0(prefix, "_enrollment_counts_", eid, ".qs2"))
}

# --- s1 work directory + path constructors -----------------------------------
#
# Loop 1 splits into four sub-steps (s1a..s1d). Each sub-step runs in a
# subprocess (parallel for skeleton-level work, single for enrollment-level
# work) and communicates with the next sub-step via files in a per-project
# work directory:
#
#   {data_meta_dir}/s1_work/{project_prefix}/
#
# This directory is transient dataflow, not a cache: it is cleared at the
# start of every $s1_generate_enrollments_and_ipw() call and removed again on
# success (Phase 5': s1 has no resume, so nothing here is ever read across
# runs).
#
# File-name conventions:
#
#   s1a_cache_enr{eid}_{skel_basename}            ← projected skeleton cache
#   s1a_pre_enr{eid}_{skel_basename}              ← (tuples, attrition) chunk
#   s1b_enrolled_ids_enr{eid}.qs2                 ← post-match enrolled IDs
#   s1c_panel_enr{eid}_{skel_basename}            ← per-(enr, skel) panel chunk
#
# The work_dir is removed on successful completion of $s1_generate_*().

#' Resolve and (optionally) create the s1 work directory for a plan.
#'
#' `{data_meta_dir}/s1_work/{project_prefix}/` -- transient dataflow between
#' the s1 sub-steps, cleared at the start of each run and removed on success.
#' @param plan A TTEPlan.
#' @param ensure_exists Create the directory if missing (default TRUE).
#' @noRd
.s1_work_dir <- function(plan, ensure_exists = TRUE) {
  if (is.null(plan$registrystudy)) {
    stop(
      "TTEPlan has no embedded RegistryStudy. ",
      "The s1 work directory is derived from study$data_meta_dir."
    )
  }
  meta_dir <- plan$registrystudy$data_meta_dir
  if (is.null(meta_dir) || !nzchar(meta_dir)) {
    stop("Could not resolve study$data_meta_dir for the s1 work directory.")
  }
  # ABSOLUTE, always: files under this work dir become batchit declared
  # `outputs`, and batchit rejects a relative declared-output path. Safe to
  # normalize `meta_dir` itself because it is guaranteed to EXIST --
  # first_existing_path() (R/path_resolution.R) returns an existing candidate,
  # creates the first one whose parent exists, or errors. That matters:
  # normalizePath(mustWork = FALSE) returns an absolute path only for a path
  # that exists, and silently returns a non-existent relative path UNCHANGED.
  dir <- file.path(
    normalizePath(meta_dir, mustWork = FALSE),
    "s1_work",
    plan$project_prefix
  )
  if (ensure_exists && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

#' @noRd
.s1a_cache_path <- function(work_dir, eid, skel_basename) {
  file.path(work_dir, sprintf("s1a_cache_enr%s_%s", eid, skel_basename))
}
#' @noRd
.s1a_pre_path <- function(work_dir, eid, skel_basename) {
  file.path(work_dir, sprintf("s1a_pre_enr%s_%s", eid, skel_basename))
}

# --- s1a declared-output NAMES (the batchit `outputs` keys) -----------------
#
# s1a runs `style = "staged_writer"`: the PARENT declares every file an item
# will write, and the WORKER asks for each destination by NAME via
# .batch_where_to_write_output(). The two ends must agree on the name set
# exactly -- an unknown name is a hard batchit error inside the child, not a
# silent fallback. These three helpers are the single source of that name set,
# so parent and worker cannot drift apart: the parent calls
# .s1a_outputs_for_skeleton() and the worker calls .s1a_cache_name() /
# .s1a_pre_name() with the same enrollment id.
#
# The names are batchit keys; the VALUES are the on-disk paths that s1b, s1c
# and s1d later read back through .s1a_cache_path() / .s1a_pre_path(). Keeping
# both in one function is what makes "declared here" and "read there" provably
# the same string.

#' @noRd
.s1a_cache_name <- function(eid) paste0("cache_", eid)

#' @noRd
.s1a_pre_name <- function(eid) paste0("pre_", eid)

#' Every declared output of one s1a item: `2 x length(eids)` named paths.
#'
#' Grouped by enrollment (`cache_<eid>`, `pre_<eid>`, next enrollment, ...).
#' @noRd
.s1a_outputs_for_skeleton <- function(work_dir, eids, skel_basename) {
  out <- unlist(lapply(eids, function(eid) {
    x <- c(
      .s1a_cache_path(work_dir, eid, skel_basename),
      .s1a_pre_path(work_dir, eid, skel_basename)
    )
    names(x) <- c(.s1a_cache_name(eid), .s1a_pre_name(eid))
    x
  }))
  if (is.null(out)) character(0) else out
}

#' @noRd
.s1b_enrolled_ids_path <- function(work_dir, eid) {
  file.path(work_dir, sprintf("s1b_enrolled_ids_enr%s.qs2", eid))
}
#' @noRd
.s1c_panel_path <- function(work_dir, eid, skel_basename) {
  file.path(work_dir, sprintf("s1c_panel_enr%s_%s", eid, skel_basename))
}

#' Restore enrollment counts from per-enrollment sidecar files on disk.
#' Only fills entries not already present on the plan.
#' @noRd
.restore_enrollment_counts <- function(plan, output_dir, enrollment_ids) {
  for (eid in enrollment_ids) {
    if (!is.null(plan$enrollment_counts[[eid]])) {
      next
    }
    counts_path <- .enrollment_counts_path(output_dir, plan$project_prefix, eid)
    if (file.exists(counts_path)) {
      plan$enrollment_counts[[eid]] <- qs2_read(counts_path)
    }
  }
}

# --- Shared preparation helpers (used by s1a and s1b workers) ----------------

#' Read skeleton, apply exclusions, optionally derive confounders, set treatment.
#' Used by `.s1b_worker()` (full, with confounders); the scout path
#' (`.s1a_worker_multi()`) reads the canonical once and calls
#' `.s1_prepare_loaded()` directly.
#' @noRd
.s1_prepare_skeleton <- function(
  enrollment_spec,
  file_path,
  spec,
  derive_confounders = TRUE
) {
  data.table::setDTthreads(enrollment_spec$n_threads)
  skeleton <- .s1_load_skeleton(file_path, enrollment_spec$n_threads)
  .s1_prepare_loaded(
    skeleton,
    enrollment_spec,
    spec,
    derive_confounders = derive_confounders
  )
}

# --- internal: read + key + alloccol a canonical skeleton ------------------
#
# Split out of .s1_prepare_skeleton so .s1a_worker_multi() can read the
# canonical ONCE and reuse it across multiple enrollment_specs (Lever 2:
# reduces canonical reads from 19x per skeleton to 1x).
.s1_load_skeleton <- function(file_path, n_threads) {
  id <- isoyearweek <- NULL
  obj <- qs2_read(file_path, nthreads = 1L)
  # Under the Skeleton R6 migration, skeleton_*.qs2 files hold a
  # Skeleton R6 object wrapping the data.table. Legacy bare-data.table
  # files are still supported for backwards compat.
  skeleton <- if (inherits(obj, "Skeleton")) obj$data else obj
  rm(obj)
  # qs2 round-tripping drops data.table over-allocation; restore it so
  # subsequent `:=` mutations don't reallocate at a new address.
  skeleton <- data.table::setalloccol(
    skeleton,
    n = getOption("datatable.alloccol", 4096L)
  )
  # Skeleton is already sorted by (id, isoyearweek) from create_skeleton();
  # qs2 preserves row order so setkey is an O(n) verification, not a full sort.
  data.table::setkey(skeleton, id, isoyearweek)
  skeleton
}

# --- internal: apply exclusions + treatment to a pre-loaded skeleton -------
#
# Mutates skeleton in place. Caller is responsible for having called
# .s1_load_skeleton() (which sets the key + over-alloc). When called from
# .s1a_worker_multi() against a copy of the canonical, the previous
# enrollment's eligible_* columns don't leak in because the caller passes a
# fresh data.table::copy().
.s1_prepare_loaded <- function(
  skeleton,
  enrollment_spec,
  spec,
  derive_confounders = TRUE
) {
  baseline_intervention <- rd_intervention <- eligible_valid_treatment <- NULL
  # Combine exclusion grouped specs + computed-confounder grouped specs into
  # a SINGLE `dt[, c(...) := list(...), by = id]` call.
  built_excl <- .tte_build_exclusion_specs(skeleton, spec, enrollment_spec)
  conf_specs <- if (derive_confounders) {
    .tte_build_confounder_specs(skeleton, spec)
  } else {
    list()
  }
  skeleton <- .tte_apply_eligibility_batch(
    skeleton,
    c(built_excl$grouped_specs, conf_specs),
    id_col = "id"
  )
  skeleton <- skeleton_eligible_combine(skeleton, built_excl$eligible_cols)
  data.table::setattr(skeleton, "eligible_cols", built_excl$eligible_cols)
  x_tx <- enrollment_spec$treatment_impl
  skeleton[,
    c(
      "rd_intervention",
      "baseline_intervention",
      "eligible_valid_treatment"
    ) := {
      rd <- data.table::fcase(
        get(x_tx$variable) == x_tx$intervention_value , TRUE  ,
        get(x_tx$variable) == x_tx$comparator_value   , FALSE ,
        default = NA
      )
      list(rd, rd, !is.na(rd))
    }
  ]

  eligible_cols <- attr(skeleton, "eligible_cols")
  data.table::setattr(
    skeleton,
    "eligible_cols",
    c("eligible_valid_treatment", eligible_cols)
  )
  skeleton_eligible_combine(skeleton, attr(skeleton, "eligible_cols"))

  skeleton
}


#' Get all eligible (person_id, trial_id, intervention, recruit_week_index)
#' tuples from a skeleton.
#' Used by `.s1a_finalize_on_skeleton()` for scouting and available for direct
#' use. Caller should pre-sort by (pid, trial_id, isoyearweek) for efficiency.
#'
#' `recruit_week_index` names the week that recruited each person into each
#' band. It travels the whole scout chain: these tuples reach `.s1b_worker()`,
#' the comparator draw keeps it, and it lands in `enrolled_ids` on disk. The
#' s1c enrollment then reads it back on `entry_dt`.
#' @noRd
.s1_eligible_tuples <- function(skeleton, design) {
  if (!"trial_id" %in% names(skeleton)) {
    .assign_trial_ids(skeleton, design$period_width)
  }
  # `.band_baseline_treatment()` is the single source of truth for the
  # (person, band) -> baseline treatment mapping, and `enroll()` Phase C
  # calls the same function. It drops the weeks that are not eligible or
  # not in an arm, then uses any() and not first() over the weeks that are
  # left: treatment can start at any week within a trial period, not just
  # the first. first() silently drops ~75% of intervention people whose
  # treatment initiation falls mid-period. The no_prior_intervention exclusion
  # criterion handles the new-user restriction (one-time initiation)
  # separately.
  #
  # No setorderv() before the group-by: the scout path has already
  # sorted the skeleton by (pid, trial_id, isoyearweek), logical-vector
  # subsetting preserves order, and any() is order-independent regardless.
  # Dropping the re-sort avoids a 17M-row radix sort per scout worker.
  .band_baseline_treatment(
    data = skeleton,
    person_id_col = design$person_id_var,
    treatment_col = "rd_intervention",
    eligible_col = design$eligible_var,
    out_col = "intervention"
  )
}


# --- Attrition helper -------------------------------------------------------

#' Compute cumulative attrition counts per eligibility criterion.
#'
#' Returns a long-format data.table with rows per (trial_id, criterion)
#' AND a global row (`trial_id = NA`) per criterion. The global row
#' carries true overall `uniqueN(person_id)` — summing the per-trial
#' `n_persons` across trial_ids over-counts because one person who
#' enters N trials contributes N times to that sum. Downstream CONSORT
#' consumers must prefer the NA-trial_id rows for person headcounts.
#' Per-trial rows are retained for diagnostic slicing.
#'
#' Each row includes a "before_exclusions" entry plus one per cumulative
#' eligibility level, with intervention/comparator breakdowns (always in
#' person-trial units) for TARGET Item 8 reporting.
#'
#' @param skeleton data.table with trial_id and eligible_* columns assigned.
#' @param eligible_cols Character vector of eligible_* column names in
#'   application order.
#' @param pid Character, person ID column name.
#' @param treatment_var Character, name of the treatment column (default
#'   `"rd_intervention"`).
#' @return data.table with columns: trial_id, criterion, n_persons,
#'   n_person_trials, n_intervention, n_comparator. Rows with `trial_id = NA`
#'   carry true overall uniqueN of persons.
#' @noRd
.s1_compute_attrition <- function(
  skeleton,
  eligible_cols,
  pid,
  treatment_var = "rd_intervention"
) {
  .tte_pid <- .tte_tx <- .tte_tx_any <- trial_id <- . <- criterion <- NULL
  if (is.null(eligible_cols) || length(eligible_cols) == 0L) {
    stop("eligible_cols must be a non-empty character vector")
  }

  # Subset to needed columns for efficiency
  .cols <- c(pid, "trial_id", eligible_cols, treatment_var)
  sk <- skeleton[, .cols, with = FALSE]

  # Alias pid and treatment columns to fixed names for j-expressions
  data.table::setnames(sk, c(pid, treatment_var), c(".tte_pid", ".tte_tx"))

  # Classify each (person, trial) as any()-exposed so that a row in `pt0`
  # corresponds to one person-trial with a single boolean treatment flag.
  # Treatment uses any(): a person-trial is "intervention" if ANY week within
  # the trial period has .tte_tx == TRUE. This matches .s1_eligible_tuples().
  pt0 <- sk[,
    .(
      .tte_tx_any = any(.tte_tx == TRUE, na.rm = TRUE)
    ),
    by = c(".tte_pid", "trial_id")
  ]
  # Per-trial summary: drop rows where trial_id is NA (person-weeks that
  # fall outside any trial period). Without this filter, those rows
  # collapse into a `(trial_id = NA, criterion)` group whose `n_persons`
  # later gets summed together with the genuine `before_global` row in
  # the per-batch aggregation step (line ~1641, `by = .(trial_id,
  # criterion)`), inflating the reported global cohort by ~2x in CONSORT.
  before_row <- pt0[
    !is.na(trial_id),
    .(
      n_persons = data.table::uniqueN(.tte_pid),
      n_person_trials = .N,
      n_intervention = sum(.tte_tx_any, na.rm = TRUE),
      n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
    ),
    by = trial_id
  ]
  before_row[, criterion := "before_exclusions"]
  # Global (across-trials) row: true uniqueN of persons, not a sum of
  # per-trial uniqueNs. CONSORT reporting reads this row; without it, the
  # person column of the attrition table double-counts everyone who
  # enters more than one sequential trial.
  before_global <- pt0[, .(
    trial_id = NA_integer_,
    n_persons = data.table::uniqueN(.tte_pid),
    n_person_trials = .N,
    n_intervention = sum(.tte_tx_any, na.rm = TRUE),
    n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
  )]
  before_global[, criterion := "before_exclusions"]

  # For each cumulative criterion level, filter the full skeleton to rows where
  # ALL criteria 1..i pass, then classify treatment per person-trial using
  # any() --a person-trial is "intervention" if ANY eligible week within the
  # trial period has .tte_tx == TRUE. This matches .s1_eligible_tuples().
  rows <- vector("list", length(eligible_cols))
  global_rows <- vector("list", length(eligible_cols))
  cumulative_mask <- rep(TRUE, nrow(sk))

  for (i in seq_along(eligible_cols)) {
    # `sk[[col]]` is already logical; the explicit `== TRUE` is a no-op
    # except for cycling NA values, which `&` propagates either way.
    cumulative_mask <- cumulative_mask & sk[[eligible_cols[i]]]
    # Fused `[i, j, by=]` skips the intermediate `filtered` data.table
    # (a ~220 MB allocation on a 17 M-row panel) and lets data.table
    # do the filter + group-by in a single internal pass.
    pt_i <- sk[
      cumulative_mask,
      .(.tte_tx_any = any(.tte_tx == TRUE, na.rm = TRUE)),
      by = c(".tte_pid", "trial_id")
    ]
    # Same filter as `before_row` above: drop the spurious `trial_id = NA`
    # group so it doesn't collide with `global_rows[[i]]` during the
    # per-batch aggregation summing.
    rows[[i]] <- pt_i[
      !is.na(trial_id),
      .(
        n_persons = data.table::uniqueN(.tte_pid),
        n_person_trials = .N,
        n_intervention = sum(.tte_tx_any, na.rm = TRUE),
        n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
      ),
      by = trial_id
    ][, criterion := eligible_cols[i]]
    # Global (trial_id = NA) companion row: true uniqueN of persons
    # across all trials after this cumulative criterion.
    global_rows[[i]] <- pt_i[,
      .(
        trial_id = NA_integer_,
        n_persons = data.table::uniqueN(.tte_pid),
        n_person_trials = .N,
        n_intervention = sum(.tte_tx_any, na.rm = TRUE),
        n_comparator = sum(!.tte_tx_any, na.rm = TRUE)
      )
    ][, criterion := eligible_cols[i]]
  }

  # sk is a local copy (column subset), no need to restore names

  data.table::rbindlist(
    c(list(before_row, before_global), rows, global_rows),
    use.names = TRUE
  )
}


# --- internal: finalize one enrollment's scout on a prepared skeleton ------
#
# Called by .s1a_worker_multi() (one canonical read shared across all
# enrollments). Its single-enrollment predecessor .s1a_worker() -- one
# canonical read PER enrollment -- was deleted in Phase 3: no production
# call site had selected it since the multi-enrollment scout landed. The
# caller is responsible for handing in a skeleton that has already had
# exclusions + treatment applied (.s1_prepare_loaded()).
.s1a_finalize_on_skeleton <- function(
  skeleton,
  enrollment_spec,
  spec,
  cache_path
) {
  enrollment_person_trial_id <- trial_id <- NULL
  pid <- enrollment_spec$design$person_id_var

  .assign_trial_ids(skeleton, enrollment_spec$design$period_width)
  data.table::setorderv(skeleton, c(pid, "trial_id", "isoyearweek"))

  eligible_cols <- attr(skeleton, "eligible_cols")
  attrition <- .s1_compute_attrition(skeleton, eligible_cols, pid)

  tuples <- .s1_eligible_tuples(skeleton, enrollment_spec$design)

  # Landmark qualification. `.s1b_worker()` draws comparators from the pooled
  # tuples and never sees a person-week again, so the drop MUST happen here,
  # while the weekly source data is still in hand. Every tuple that reaches
  # `enrolled_ids <- all_tuples[...]` is therefore already observed, eligible
  # and event-free at its landmark, and the draw refills the ratio from
  # qualified comparators alone.
  #
  # The four cascade rows stack onto the exclusion cascade: same columns, same
  # units, so `.s1b_worker()` sums them across skeletons unchanged and CONSORT
  # reads one continuous table.
  qualified <- .tte_qualify_bands(
    bands = tuples,
    data = skeleton,
    design = enrollment_spec$design,
    person_id_col = pid,
    arm_col = "intervention"
  )
  tuples <- qualified$bands
  if (!is.null(qualified$attrition)) {
    attrition <- data.table::rbindlist(
      list(attrition, qualified$attrition),
      use.names = TRUE
    )
  }

  tuples[,
    enrollment_person_trial_id := stringi::stri_c(
      enrollment_spec$enrollment_id,
      ".",
      get(pid),
      ".",
      trial_id
    )
  ]

  # Cache prepared skeleton for s1b reuse, projected to only the columns
  # s1b actually consumes (Lever 1 -- ~10x smaller cache, ~10x faster s1b
  # cache read).
  if (!is.null(cache_path)) {
    cache_cols <- .tte_s1_cache_columns(skeleton, enrollment_spec, spec)
    qs2_write_atomic(
      skeleton[, ..cache_cols],
      cache_path,
      nthreads = 1L
    )
  }

  list(tuples = tuples, attrition = attrition)
}

# --- internal: union of canonical columns needed across ALL enrollments ----
#
# Walks every enrollment_spec + the global spec to collect every column
# that s1a + s1b for any enrollment will read. Used by .s1a_worker_multi
# to project the canonical immediately after qs2 deserialisation, so the
# working skeleton is ~50-100 cols instead of ~1025 (the bulk being
# registry diagnosis/medication flags that no enrollment touches).
.tte_canonical_needed_cols <- function(spec, enrollment_specs, all_cols) {
  needed <- c("id", "isoyearweek", "isoyear")
  add_source <- function(impl) {
    if (!is.null(impl$source_variable)) {
      needed <<- c(needed, impl$source_variable)
    }
    if (
      !is.null(impl$source_variable_combined) &&
        impl$source_variable_combined %in% all_cols
    ) {
      needed <<- c(needed, impl$source_variable_combined)
    }
    if (!is.null(impl$variable)) {
      needed <<- c(needed, impl$variable)
    }
  }
  for (es in enrollment_specs) {
    if (!is.null(es$treatment_impl$variable)) {
      needed <- c(needed, es$treatment_impl$variable)
    }
    # The observation column, when the design names one. This projection runs
    # BEFORE .tte_s1_cache_columns(), so a column missing here never reaches
    # the cache allow-list at all.
    needed <- c(needed, .tte_observed_column(es$design$observed_var))
  }
  for (enr in spec$enrollments) {
    for (ae in enr$additional_inclusion %||% list()) {
      add_source(ae$implementation)
    }
    for (ec in enr$additional_exclusion %||% list()) {
      add_source(ec$implementation)
    }
  }
  for (ec in spec$exclusion_criteria %||% list()) {
    add_source(ec$implementation)
  }
  for (conf in spec$confounders %||% list()) {
    add_source(conf$implementation)
  }
  for (out in spec$outcomes %||% list()) {
    add_source(out$implementation)
  }
  intersect(unique(needed), all_cols)
}

# --- internal: multi-enrollment scout worker (sub-step s1a) -----------------
#
# Reads the canonical skeleton ONCE, projects it to the union of columns
# any enrollment needs (dropping the ~95% of registry-flag columns no
# enrollment touches), then applies each enrollment's exclusions +
# treatment + scout in place against that small projection. Between
# enrollments we drop any columns that prepare_loaded() / finalize() added
# to reveal the projected canonical. No data.table::copy() needed.
#
# Per-(enrollment, skeleton) outputs are streamed to disk inside the loop --
# 2 x length(enrollment_specs) files per item:
#
#   cache_{eid}   -> s1a_cache_enr{eid}_{basename}  (projected skeleton cache,
#                    written one frame down by .s1a_finalize_on_skeleton)
#   pre_{eid}     -> s1a_pre_enr{eid}_{basename}    (tuples + attrition)
#
# Dispatched via .batch_run_and_write(style = "staged_writer"): the PARENT
# declares all 2N paths (.s1a_outputs_for_skeleton()) and this worker BUILDS
# NO PATH of its own -- it resolves each destination by NAME through
# .batch_where_to_write_output(), which answers only inside a staged_writer
# item and errors on a name the parent did not declare. That turns a
# parent/worker name drift into a loud child failure instead of a cache file
# written where s1c will never look for it (which s1c would otherwise absorb
# by silently recomputing; see .s1c_worker_impl()'s `require_cache`).
#
# Consequence: `.s1a_worker_multi()` is NOT callable outside a staged_writer
# dispatch. It takes no `work_dir` -- there is nowhere for it to decide to
# write. The atomic commit also means a crashed item leaves none of its 2N
# files behind, where the old streamed writes left a partial set.
#
# The worker returns nothing through the result envelope, so the master never
# holds 19 (tuples, attrition) chunks in RAM after the pool completes.
.s1a_worker_multi <- function(file_path, enrollment_specs, spec) {
  n_threads <- enrollment_specs[[1L]]$n_threads %||% 1L
  data.table::setDTthreads(n_threads)
  skel_basename <- basename(file_path)

  canonical <- .s1_load_skeleton(file_path, n_threads)
  # Drop unneeded columns in place via `:= NULL` instead of copying the
  # needed subset out to a new data.table. With ~970 columns to drop out
  # of ~1025, the in-place drop is essentially free (each `:= NULL` just
  # removes the column reference); a `[, ..needed]` projection would
  # allocate a fresh data.table and copy every kept column's values.
  needed <- .tte_canonical_needed_cols(
    spec,
    enrollment_specs,
    names(canonical)
  )
  drop_cols <- setdiff(names(canonical), needed)
  if (length(drop_cols) > 0L) {
    canonical[, (drop_cols) := NULL]
  }
  data.table::setkey(canonical, id, isoyearweek)
  pristine_cols <- copy(names(canonical))

  for (k in seq_along(enrollment_specs)) {
    es <- enrollment_specs[[k]]
    eid <- es$enrollment_id
    canonical <- .s1_prepare_loaded(
      canonical,
      es,
      spec,
      derive_confounders = FALSE
    )
    one <- .s1a_finalize_on_skeleton(
      canonical,
      es,
      spec,
      cache_path = .batch_where_to_write_output(.s1a_cache_name(eid))
    )
    qs2_write_atomic(
      one,
      .batch_where_to_write_output(.s1a_pre_name(eid)),
      nthreads = 1L
    )
    rm(one)
    added_cols <- setdiff(names(canonical), pristine_cols)
    if (length(added_cols) > 0L) {
      canonical[, (added_cols) := NULL]
    }
    data.table::setattr(canonical, "eligible_cols", NULL)
  }
  invisible(NULL)
}

# --- internal: enumerate columns s1b will actually read from the cache -----
#
# The cache must contain:
#   - id, isoyearweek, trial_id          (keying + grouping)
#   - rd_intervention, baseline_intervention (treatment, computed in s1a)
#   - design$confounder_vars             (Phase B `first()` aggregation)
#   - design$treatment_var               (Phase B treatment override)
#   - design$outcome_vars                (Phase B `max()` aggregation)
#   - the observation column, when the design names one. This allow-list is
#     named, so a column absent from it is dropped before s1b and s1c ever
#     see it. Drop the observation column here and every later landmark step
#     reads an unobserved person as an ineligible one. Nothing errors and
#     nothing warns.
#   - all eligible_* columns             (matching + attrition)
#   - source variables for any `computed = TRUE` confounder, because
#     tteplan_apply_derived_confounders() runs against the cached
#     skeleton in s1b and reads those raw sources (the OR'd
#     `*_combined` column is materialised at apply time).
.tte_s1_cache_columns <- function(skeleton, enrollment_spec, spec) {
  design <- enrollment_spec$design
  needed <- c(
    "id",
    "isoyearweek",
    "trial_id",
    "rd_intervention",
    "baseline_intervention",
    design$confounder_vars,
    design$treatment_var,
    design$outcome_vars,
    .tte_observed_column(design$observed_var),
    attr(skeleton, "eligible_cols")
  )
  for (conf in spec$confounders %||% list()) {
    impl <- conf$implementation
    if (isTRUE(impl$computed)) {
      needed <- c(needed, impl$source_variable)
    }
  }
  unique(intersect(needed, names(skeleton)))
}


# --- s1c: Panel build worker (formerly .s1b_worker) ------------------------

#' Per-(enrollment, skeleton) panel build worker for sub-step s1c.
#'
#' Reads the s1a cache, restricts to enrolled persons (from s1b), derives
#' confounders, and expands to the trial-week panel via [TTEEnrollment$new()].
#' Dispatched via .batch_run_and_write(style = "return") in a fresh R session:
#' the worker RETURNS the panel and writes nothing itself, and batchit commits
#' the returned `panel` element to the declared output path atomically.
#'
#' CONTRACT CHANGE (s1a staged_writer phase): the production path now passes
#' `require_cache = TRUE`, so a missing s1a cache is a LOUD ERROR here instead
#' of a silent recompute. The recompute fallback produces a different column
#' set and runs roughly 10x slower, and it used to hide a parent/worker path
#' drift completely -- no error, no warning, changed production output. A
#' standalone partial s1c run, or an external `swereg:::.s1c_worker()` caller
#' that has not run s1a first, will now fail where it previously recomputed.
#' Call `.s1c_worker_impl()` directly with `require_cache = FALSE` (the
#' default) if you want the old recomputing behaviour.
#'
#' @param enrollment_spec Enrollment spec list.
#' @param file_path Path to a skeleton `.qs2` file. Read only on the
#'   `require_cache = FALSE` recompute fallback, which this worker never
#'   takes; kept so `.s1c_worker_impl()` has it for dev callers.
#' @param spec Parsed study spec.
#' @param work_dir Per-project s1 work directory ([.s1_work_dir()]). An INPUT
#'   here, not an output-path source: the worker reads the s1a cache and the
#'   s1b enrolled-ids file from it.
#' @return `list(panel = <TTEEnrollment>)`. Writes nothing.
#' @noRd
.s1c_worker <- function(enrollment_spec, file_path, spec, work_dir) {
  eid <- enrollment_spec$enrollment_id
  skel_basename <- basename(file_path)
  cache_path <- .s1a_cache_path(work_dir, eid, skel_basename)
  enrolled_ids_path <- .s1b_enrolled_ids_path(work_dir, eid)

  enrolled_ids <- qs2_read(enrolled_ids_path, nthreads = 1L)
  enrollment <- .s1c_worker_impl(
    enrollment_spec,
    file_path,
    spec,
    enrolled_ids,
    cache_path,
    require_cache = TRUE
  )
  list(panel = enrollment)
}

# Core panel-build logic, kept separate from .s1c_worker() so dev/verify
# scripts and tests can drive it directly with in-memory enrolled_ids
# instead of having to materialise a work_dir.
#
# `require_cache` is the guard against a SILENT production-output change. s1a
# declares its cache path in the parent and writes it by name in the worker;
# .s1c_worker() recomputes that same path here to read it back. If those two
# ever drift by one character the old code just took the `else` branch --
# ~10x slower, a different column set, no error and no warning. With
# require_cache = TRUE the branch decision is made ONCE (`use_cache` below,
# so there is no TOCTOU gap between the check and the read) and a missing
# cache stops the item.
#
# It stays FALSE by default: dev/verify scripts and direct callers that never
# ran s1a legitimately want the recompute.
#' @noRd
.s1c_worker_impl <- function(
  enrollment_spec,
  file_path,
  spec,
  enrolled_ids,
  cache_path = NULL,
  require_cache = FALSE
) {
  id <- isoyearweek <- NULL
  # Subset to enrolled persons before expensive confounder computation
  pid <- enrollment_spec$design$person_id_var
  enrolled_persons <- unique(enrolled_ids[[pid]])

  # Decide ONCE. Reusing `use_cache` for the branch below is what closes the
  # TOCTOU gap a bare `stop()` in the wrapper would leave open.
  use_cache <- !is.null(cache_path) && file.exists(cache_path)
  if (isTRUE(require_cache) && !use_cache) {
    stop(
      ".s1c_worker_impl(): the s1a skeleton cache is required but absent: ",
      if (is.null(cache_path)) "<NULL>" else cache_path,
      "\nThis means s1a never committed the cache this (enrollment, skeleton) ",
      "pair needs, or the path s1c derives no longer matches the one s1a ",
      "declared. Recomputing instead would succeed silently with a DIFFERENT ",
      "column set at ~10x the cost, so it is refused on the production path.",
      call. = FALSE
    )
  }

  if (use_cache) {
    # Reuse cached skeleton from s1a (already has exclusions + treatment applied)
    data.table::setDTthreads(enrollment_spec$n_threads)
    skeleton <- qs2_read(cache_path, nthreads = 1L)
    # qs2 drops data.table over-allocation slots; restore them so
    # subsequent `:=` mutations don't reallocate at a new address.
    # (Same rationale as RegistryStudy$load_skeleton(); see that method
    # for the full explanation.)
    skeleton <- data.table::setalloccol(
      skeleton,
      n = getOption("datatable.alloccol", 4096L)
    )
    data.table::setkey(skeleton, id, isoyearweek)
    # Binary-search join on the existing (id, isoyearweek) key beats
    # `%in%` for selecting enrolled persons from a 17 M-row panel; same
    # fix as in `private$enroll`. Saves ~2 s + a hash allocation per
    # stage-1b worker call.
    skeleton <- skeleton[
      .(unique(enrolled_persons)),
      on = pid,
      nomatch = NULL
    ]
    # Mark that we've already filtered to enrolled persons so
    # private$enroll() in Phase B doesn't redo the same filter (which
    # otherwise allocates another 2.85 GB copy of the panel as an
    # identity transformation).
    data.table::setattr(skeleton, ".tte_filtered_to_enrolled", TRUE)
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  } else {
    skeleton <- .s1_prepare_skeleton(
      enrollment_spec,
      file_path,
      spec,
      derive_confounders = FALSE
    )
    skeleton <- skeleton[
      .(unique(enrolled_persons)),
      on = pid,
      nomatch = NULL
    ]
    data.table::setattr(skeleton, ".tte_filtered_to_enrolled", TRUE)
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  }
  enrollment <- TTEEnrollment$new(
    data = skeleton,
    design = enrollment_spec$design,
    enrolled_ids = enrolled_ids,
    seed = enrollment_spec$seed,
    extra_cols = "isoyearweek",
    own_data = TRUE
  )
  rm(skeleton)

  # Prefix enrollment_person_trial_id with enrollment_id
  id_var <- enrollment$design$id_var
  if (nrow(enrollment$data) > 0L && id_var %in% names(enrollment$data)) {
    enrollment$data[,
      (id_var) := stringi::stri_c(
        enrollment_spec$enrollment_id,
        ".",
        get(id_var)
      )
    ]
  }
  enrollment
}


# --- s1b: Match worker (single subprocess per enrollment) ------------------

#' Match sub-step: pool per-skeleton scout outputs for one enrollment, then
#' sample comparators at the matching ratio.
#'
#' Reads the 2,194-ish `s1a_pre_*` chunks for this enrollment, rbindlists
#' tuples + attrition, samples comparators per `trial_id`, and RETURNS the two
#' declared outputs:
#'   - `enrolled_ids` (post-match enrolled IDs for s1c)
#'   - `counts`       (matching + attrition sidecar the master reads back)
#'
#' Runs in a fresh R session via .batch_run_and_write() with `n_workers = 1L`
#' and `style = "return"`: batchit commits both objects to their declared
#' paths. The worker itself writes nothing, and the master never holds the
#' rbinded tuples in RAM.
#'
#' @param enrollment_spec Enrollment spec list (includes seed, matching_ratio,
#'   design$person_id_var, enrollment_id).
#' @param spec Parsed study spec (not currently used; reserved for future
#'   per-spec matching rules).
#' @param work_dir Per-project s1 work directory.
#' @param skel_basenames Character vector of skeleton basenames (used to
#'   construct `s1a_pre_*` paths).
#' @return `list(enrolled_ids = , counts = )`, matching the declared output
#'   names at the call site.
#' @noRd
.s1b_worker <- function(
  enrollment_spec,
  spec,
  work_dir,
  skel_basenames
) {
  intervention <- trial_id <- criterion <- n_persons <- n_person_trials <-
    n_intervention <- n_comparator <- NULL

  eid <- enrollment_spec$enrollment_id
  data.table::setDTthreads(enrollment_spec$n_threads)

  pre_paths <- vapply(
    skel_basenames,
    function(bn) {
      .s1a_pre_path(work_dir, eid, bn)
    },
    character(1)
  )
  missing_pre <- !file.exists(pre_paths)
  if (any(missing_pre)) {
    stop(sprintf(
      "s1b: %d/%d pre files missing for enrollment '%s'. First missing: %s",
      sum(missing_pre),
      length(pre_paths),
      eid,
      pre_paths[which(missing_pre)[1L]]
    ))
  }

  tuples_chunks <- vector("list", length(pre_paths))
  attr_chunks <- vector("list", length(pre_paths))
  for (j in seq_along(pre_paths)) {
    pre <- qs2_read(pre_paths[j], nthreads = 1L)
    tuples_chunks[[j]] <- pre$tuples
    attr_chunks[[j]] <- pre$attrition
    rm(pre)
  }
  all_tuples <- data.table::rbindlist(tuples_chunks, use.names = TRUE)
  all_attrition <- data.table::rbindlist(attr_chunks, use.names = TRUE)
  rm(tuples_chunks, attr_chunks)

  set.seed(enrollment_spec$seed)
  x_ratio <- enrollment_spec$matching_ratio

  enrolled_ids <- all_tuples[,
    {
      int_rows <- .SD[intervention == TRUE]
      cmp_rows <- .SD[intervention == FALSE]
      n_to_sample <- min(
        round(x_ratio * nrow(int_rows)),
        nrow(cmp_rows)
      )
      sampled <- if (n_to_sample > 0) {
        cmp_rows[sample(.N, n_to_sample)]
      } else {
        cmp_rows[0]
      }
      data.table::rbindlist(list(int_rows, sampled))
    },
    by = trial_id
  ]

  global_counts <- all_tuples[,
    .(
      n_intervention_total = sum(intervention == TRUE),
      n_comparator_total = sum(intervention == FALSE)
    ),
    by = trial_id
  ]
  enrolled_counts <- enrolled_ids[,
    .(
      n_intervention_enrolled = sum(intervention == TRUE),
      n_comparator_enrolled = sum(intervention == FALSE)
    ),
    by = trial_id
  ]
  matching_counts <- merge(
    global_counts,
    enrolled_counts,
    by = "trial_id",
    all.x = TRUE
  )

  attrition_summary <- all_attrition[,
    .(
      n_persons = sum(n_persons),
      n_person_trials = sum(n_person_trials),
      n_intervention = sum(n_intervention),
      n_comparator = sum(n_comparator)
    ),
    by = .(trial_id, criterion)
  ]

  counts <- list(attrition = attrition_summary, matching = matching_counts)

  list(enrolled_ids = enrolled_ids, counts = counts)
}


# --- s1d: Post worker (single subprocess per enrollment) -------------------

#' Post sub-step: pool per-skeleton panel chunks for one enrollment, impute,
#' compute IPW, truncate, and save the final `file_raw` + `file_imp`.
#'
#' Runs in a fresh R session via `.batch_run_and_write()` with
#' `style = "staged_writer"` and `n_workers = 1L`. The master never holds the
#' rbinded panel in RAM, so multi-GB enrollments don't push the parent process
#' over the OOM line.
#'
#' It is handed NO output paths. Both destinations are resolved with
#' `.batch_where_to_write_output("raw" / "imp")`, which only answers inside an
#' active `staged_writer` run -- so this worker cannot be called directly
#' in-process. That indirection is what makes the pair ALL-OR-NONE: the two
#' writes land on attempt-scoped staging files, and batchit renames both into
#' place only after the item returns. Minutes of imputation + IPW + weight
#' truncation sit between them, and a failure anywhere in that window now
#' leaves both final paths untouched -- absent if they were absent,
#' byte-identical to their previous contents if they existed.
#'
#' `qs2_write_atomic()` is kept for both writes: its `.tmp` litter matches
#' batchit's attempt-scoped failure sweep.
#'
#' @param enrollment_spec Enrollment spec list.
#' @param spec Parsed study spec (not currently used; reserved).
#' @param work_dir Per-project s1 work directory.
#' @param skel_basenames Character vector of skeleton basenames.
#' @param impute_fn Imputation callback or NULL. It receives the
#'   `.tte_entry__` snapshot names, and not the plain confounder names.
#' @param stabilize Logical, stabilize IPW.
#' @return Invisible NULL.
#' @noRd
.s1d_worker <- function(
  enrollment_spec,
  spec,
  work_dir,
  skel_basenames,
  impute_fn = NULL,
  stabilize = TRUE
) {
  eid <- enrollment_spec$enrollment_id
  data.table::setDTthreads(enrollment_spec$n_threads)

  panel_paths <- vapply(
    skel_basenames,
    function(bn) {
      .s1c_panel_path(work_dir, eid, bn)
    },
    character(1)
  )
  missing_panels <- !file.exists(panel_paths)
  if (any(missing_panels)) {
    stop(sprintf(
      "s1d: %d/%d panel files missing for enrollment '%s'. First missing: %s",
      sum(missing_panels),
      length(panel_paths),
      eid,
      panel_paths[which(missing_panels)[1L]]
    ))
  }

  panels <- vector("list", length(panel_paths))
  for (j in seq_along(panel_paths)) {
    panels[[j]] <- qs2_read(panel_paths[j], nthreads = 1L)
  }
  trial <- tteenrollment_rbind(panels)
  rm(panels)

  qs2_write_atomic(trial, .batch_where_to_write_output("raw"), nthreads = 1L)

  if (!is.null(impute_fn)) {
    # Imputation is name-list driven, so it MUST be handed the entry-window
    # snapshot names. `$s2_ipw()` fits on those columns, and handing it the
    # plain confounder names would leave every one of them unimputed. It also
    # MUST NOT overwrite the follow-up value under the plain name.
    trial <- impute_fn(
      trial,
      .tte_entry_col(enrollment_spec$design$confounder_vars)
    )
  }
  trial$s2_ipw(stabilize = stabilize)
  trial$s3_truncate_weights(weight_cols = "ipw")

  qs2_write_atomic(trial, .batch_where_to_write_output("imp"), nthreads = 1L)
  invisible(NULL)
}


# --- s2_worker: Loop 2 IPCW-PP worker ----------------------------------------

#' Worker function for Loop 2: per-ETT IPCW-PP + save (internal)
#'
#' Loads an imputed enrollment file, runs `$s4_prepare_for_analysis()`, and
#' RETURNS the analysis-ready object. It writes nothing and knows no output
#' path: dispatched via `.batch_run_and_write(style = "return")`, batchit
#' serializes the returned `analysis` element to the declared output path and
#' commits it atomically.
#'
#' @param outcome Character, outcome variable name.
#' @param follow_up Integer, follow-up duration in weeks.
#' @param file_imp_path Path to the imputed enrollment .qs2 file.
#' @param n_threads Integer, number of data.table threads.
#' @param sep_by_tx Logical, estimate IPCW separately by treatment.
#' @param with_gam Logical, use GAM for IPCW estimation.
#' @param estimand Character, `"pp"` (default) or `"itt"`. ITT skips IPCW.
#' @return `list(analysis = <analysis-ready enrollment object>)`, matching the
#'   single declared output name `analysis`.
#' @noRd
.s2_worker <- function(
  outcome,
  follow_up,
  file_imp_path,
  n_threads,
  sep_by_tx,
  with_gam,
  estimand = "pp"
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(file_imp_path, nthreads = 1L)
  enrollment$s4_prepare_for_analysis(
    outcome = outcome,
    follow_up = follow_up,
    estimand = estimand,
    estimate_ipcw_pp_separately_by_treatment = sep_by_tx,
    estimate_ipcw_pp_with_gam = with_gam
  )
  list(analysis = enrollment)
}


# --- s3_enrollment_worker: Loop 3a enrollment-level baseline worker -----------

#' Compute a single Table 1 panel from the baseline slice of a loaded
#' enrollment object. Bypasses the R6 method on the cached instance so it
#' works against pre-upgrade saved objects.
#' @noRd
.s3_enrollment_table1 <- function(
  enrollment,
  ipw_col = NULL,
  arm_labels = NULL,
  include_smd = TRUE,
  show_missing = TRUE
) {
  design <- enrollment$design
  baseline <- enrollment$data[get(design$tstart_var) == 0]
  if (!is.null(ipw_col) && !ipw_col %in% names(baseline)) {
    return(NULL)
  }
  # The same entry-window read as `$table1()`. The two routes MUST agree.
  baseline <- .tte_entry_view(
    baseline,
    design$confounder_vars,
    keep_cols = c(design$treatment_var, ipw_col)
  )
  .swereg_table1(
    data = baseline,
    vars = design$confounder_vars,
    strata = design$treatment_var,
    weights = ipw_col,
    include_smd = include_smd,
    show_missing = show_missing,
    arm_labels = arm_labels
  )
}

#' Worker function for Loop 3a: per-enrollment baseline analysis in a subprocess.
#'
#' Loads an analysis file and raw file, computes table1 variants, and returns
#' the results. Dispatched via the generic batch runner (.batch_run()) in a
#' fresh R session for memory isolation.
#'
#' @param analysis_path Path to an analysis .qs2 file for this enrollment.
#' @param raw_path Path to the raw .qs2 file for this enrollment.
#' @param enrollment_id Character, enrollment identifier.
#' @param n_threads Integer, number of data.table threads.
#' @param arm_labels Optional named character vector with `comparator` and
#'   `intervention` keys, passed through to `$table1()`.
#' @return A named list with enrollment-level results.
#' @noRd
.s3_enrollment_worker <- function(
  analysis_path,
  raw_path,
  enrollment_id,
  n_threads,
  arm_labels = NULL
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = 1L)

  # Supplemental variant: Missing row forced for every variable, SMD column
  # included. Percentages over total N.
  supp_args <- list(
    arm_labels = arm_labels,
    include_smd = TRUE,
    show_missing = "always"
  )
  # Main variant: no Missing rows, SMD column included (used by the headline
  # "Table 1" sheet and by the "table1" CSV exhibit). Percentages over the
  # non-missing denominator so levels still sum to 100.
  main_args <- list(
    arm_labels = arm_labels,
    include_smd = TRUE,
    show_missing = "none"
  )

  safe <- function(fn_args, label) {
    tryCatch(
      do.call(.s3_enrollment_table1, fn_args),
      error = function(e) {
        warning(
          "table1 ",
          label,
          " failed for ",
          enrollment_id,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
  }

  table1_unweighted <- safe(
    c(list(enrollment = enrollment), supp_args),
    "unweighted"
  )
  table1_ipw_trunc <- safe(
    c(list(enrollment = enrollment, ipw_col = "ipw_trunc"), supp_args),
    "ipw_trunc"
  )
  table1_ipw <- safe(
    c(list(enrollment = enrollment, ipw_col = "ipw"), supp_args),
    "ipw"
  )
  table1_ipw_trunc_main <- safe(
    c(list(enrollment = enrollment, ipw_col = "ipw_trunc"), main_args),
    "ipw_trunc_main"
  )
  baseline_rows <- enrollment$data[
    get(enrollment$design$tstart_var) == 0
  ]
  n_baseline <- nrow(baseline_rows)
  # Per-arm analysis-set counts for the CONSORT "Analysis dataset" box.
  # Treatment is logical (intervention == TRUE), matching s2_ipw's
  # convention. If the split does not reconcile to the total (e.g. a
  # non-logical treatment var), fall back to NA so the box omits the split
  # rather than showing wrong arm counts.
  tv <- enrollment$design$treatment_var
  n_baseline_intervention <- sum(baseline_rows[[tv]] == TRUE, na.rm = TRUE)
  n_baseline_comparator <- sum(baseline_rows[[tv]] == FALSE, na.rm = TRUE)
  if ((n_baseline_intervention + n_baseline_comparator) != n_baseline) {
    n_baseline_intervention <- NA_integer_
    n_baseline_comparator <- NA_integer_
  }
  rm(enrollment, baseline_rows)
  gc()

  table1_raw <- NULL
  if (file.exists(raw_path)) {
    enrollment_raw <- swereg::qs2_read(raw_path, nthreads = 1L)
    table1_raw <- tryCatch(
      do.call(
        .s3_enrollment_table1,
        c(list(enrollment = enrollment_raw), supp_args)
      ),
      error = function(e) {
        warning(
          "table1 raw failed for ",
          enrollment_id,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
    rm(enrollment_raw)
    gc()
  }

  list(
    table1_raw = table1_raw,
    table1_unweighted = table1_unweighted,
    table1_ipw_trunc = table1_ipw_trunc,
    table1_ipw = table1_ipw,
    table1_ipw_trunc_main = table1_ipw_trunc_main,
    n_baseline = n_baseline,
    n_baseline_intervention = n_baseline_intervention,
    n_baseline_comparator = n_baseline_comparator,
    arm_labels = arm_labels,
    computed_at = Sys.time()
  )
}


# --- s3_ett_worker: Loop 3b per-ETT / per-analysis worker --------------------

#' Bootstrap replicates for every risk difference s3 computes.
#'
#' A fixed property of the stage, not an argument of a figure. A figure that
#' could lower it could lower the precision of a published interval. A figure
#' that could raise it could disagree with the results sheet beside it.
#' @noRd
.S3_RD_N_BOOT <- 500L

#' Random seed for every risk difference s3 computes.
#'
#' Fixed for the same reason as `.S3_RD_N_BOOT`, and recorded on every stored
#' result so a reader can reproduce the interval from the plan alone.
#' @noRd
.S3_RD_SEED <- 1L

#' Confidence level used when the study specification names none.
#'
#' The DEFAULT, not a constant. Unlike `.S3_RD_N_BOOT` and `.S3_RD_SEED`, the
#' confidence level is a scientific choice, so the study owns it. See
#' `.s3_conf_level()`.
#' @noRd
.S3_RD_CONF_LEVEL_DEFAULT <- 0.95


#' Read the study's confidence level for the risk-difference interval.
#'
#' The level is a STUDY property, read from
#' `spec$study$implementation$conf_level`. A study that wants 90 percent
#' intervals writes 90 percent once, in the specification, and every stored
#' result and every printed header then carries it.
#'
#' It is not a per-exhibit property. s3 computes the interval long before any
#' figure exists, so one study has one level. A figure that could restate the
#' level would print a label the numbers do not have.
#'
#' It is not a constant either. Fixing it at 0.95 would take a real capability
#' away from a study, and would take it away quietly.
#'
#' @param spec A parsed study specification, or `NULL`.
#' @return Numeric(1) strictly between 0 and 1. Returns
#'   `.S3_RD_CONF_LEVEL_DEFAULT` when the specification names no level.
#' @noRd
.s3_conf_level <- function(spec) {
  v <- spec$study$implementation$conf_level
  if (is.null(v)) {
    return(.S3_RD_CONF_LEVEL_DEFAULT)
  }
  v <- suppressWarnings(as.numeric(v))
  if (length(v) != 1L || is.na(v) || v <= 0 || v >= 1) {
    stop(
      "study$implementation$conf_level must be a single number strictly ",
      "between 0 and 1. It sets the risk-difference interval and the header ",
      "that states it."
    )
  }
  v
}


#' Split one risk-difference curve into the two results s3 stores.
#'
#' The row and the curve answer different questions, so they get different
#' slots. One shape cannot serve both. A results sheet reads the FIRST row of
#' whatever it is handed. Storing the 39-band curve where a one-row summary
#' belongs would report the first band under the header for the last one.
#'
#' The row is the end of follow-up. `.forest_rd_row()` takes the last band, and
#' this function adds the three fields that make the row self-describing:
#' `interval_status`, `n_boot` and `seed`. A reader of `plan$results_ett` can
#' then see why a bound is missing, and what produced the bound that is there,
#' without opening the curve.
#'
#' The curve is every band, with `surv_comparator` and `surv_intervention`
#' beside the risk difference. The risk difference is built from those two
#' columns. The old code threw them away, then read the analysis panel again
#' to recover them.
#'
#' It also carries `n_persons_at_risk_comparator` and
#' `n_persons_at_risk_intervention`, the head count of distinct people in each
#' arm and band. That count is what a numbers-at-risk row reports. It was the
#' last quantity a RENDERER had to open an analysis file for.
#'
#' The replicate matrix is DROPPED. `.tte_rd_curve()` attaches the whole
#' `n_boot` by `n_band` bootstrap matrix as the `rd_boot` attribute. Measured
#' on a 39-band curve at 500 replicates it is 156,216 bytes. Kept, it would add
#' 169 MB to a 540-ETT plan across two estimands. The stored percentiles
#' already summarise it.
#'
#' What stays is small. The row and the curve serialise to 2,335 bytes
#' together, which is 2.5 MB across that same plan.
#'
#' @param slot Character(1), the row slot name (`"rd_pp_trunc"` or
#'   `"rd_itt"`). The curve slot is the same name with `rd_curve_` in place of
#'   `rd_`.
#' @param curve The `$risk_difference()` return value, or the skip envelope
#'   `safe_call()` produces when it failed.
#' @param ett_id Character(1), the ETT the curve belongs to.
#' @param time_var Character(1), the band column name (`design$tstop_var`).
#' @return A named list of two elements, one per slot.
#' @noRd
.s3_rd_result <- function(slot, curve, ett_id, time_var) {
  curve_slot <- sub("^rd_", "rd_curve_", slot)
  usable <- data.table::is.data.table(curve) && nrow(curve) > 0L
  if (!usable) {
    # `curve` is the skip envelope here. It goes into BOTH slots. A slot left
    # absent reads as "this ETT was never asked", and that is the confusion
    # this whole phase exists to remove.
    return(stats::setNames(list(curve, curve), c(slot, curve_slot)))
  }
  i <- which.max(curve[[time_var]])
  row <- .forest_rd_row(ett_id, curve, time_var)
  data.table::set(
    row,
    j = "interval_status",
    value = as.character(curve$interval_status[i])
  )
  data.table::set(row, j = "n_boot", value = .S3_RD_N_BOOT)
  data.table::set(row, j = "seed", value = .S3_RD_SEED)
  data.table::setattr(curve, "rd_boot", NULL)
  data.table::setattr(curve, "seed", .S3_RD_SEED)
  stats::setNames(list(row, curve), c(slot, curve_slot))
}


#' Worker function for Loop 3b: runs ONE analysis on ONE ETT file.
#'
#' Loads an analysis file and calls a single method (rates or irr).
#' Dispatched via the generic batch runner (.batch_run()); each heavy call
#' gets its own subprocess so the OS reclaims all memory.
#'
#' @param analysis_path Path to the analysis .qs2 file.
#' @param method Character: "summary_and_rates", "rates", "irr",
#'   "risk_difference", "irr_by_subgroup", or "effect_modification_test".
#' @param weight_col Character, weight column name ("" for unweighted).
#' @param ett_id Character, ETT identifier (for logging).
#' @param n_threads Integer, number of data.table threads.
#' @param subgroup_var Optional column name for the stratified methods
#'   (`irr_by_subgroup`, `effect_modification_test`); `NULL` otherwise.
#' @param conf_level Numeric, the risk-difference interval level the study
#'   specification names. `.s3_conf_level()` resolves it in the parent, and
#'   every item carries it because batchit demands every formal on every item.
#'   Only `method = "risk_difference"` reads it.
#' @return The method result (data.table, list, etc.).
#' @noRd
.s3_ett_worker <- function(
  analysis_path,
  method,
  weight_col,
  ett_id,
  n_threads,
  subgroup_var = NULL,
  conf_level = .S3_RD_CONF_LEVEL_DEFAULT
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = 1L)

  safe_call <- function(expr_fn, label) {
    tryCatch(
      expr_fn(),
      error = function(e) {
        warning(label, " failed for ", ett_id, ": ", conditionMessage(e))
        list(skipped = TRUE, reason = conditionMessage(e))
      }
    )
  }

  # Always return a named list so the caller can merge with:
  #   for (k in names(res)) self$results_ett[[eid]][[k]] <- res[[k]]
  if (method == "summary_and_rates") {
    list(
      summary = enrollment$summary(),
      rates_pp_trunc = safe_call(
        \() enrollment$rates(weight_col = "analysis_weight_pp_trunc"),
        "rates_pp_trunc"
      ),
      rates_pp = safe_call(
        \() enrollment$rates(weight_col = "analysis_weight_pp"),
        "rates_pp"
      )
    )
  } else if (method == "irr") {
    # ITT weights on ipw_trunc (its only valid weight); name that slot irr_itt.
    # PP weights on analysis_weight_pp[_trunc] -> irr_pp[_trunc].
    slot <- if (identical(weight_col, "ipw_trunc")) {
      "irr_itt"
    } else {
      paste0("irr_", sub("^analysis_weight_", "", weight_col))
    }
    # The estimability decision is stored beside the ratio, exactly as
    # `nnt_direction` is stored beside the risk difference. A reader of
    # `plan$results_ett` reads the decision and applies no rule of its own.
    setNames(
      list(.s3_mark_irr_estimable(
        safe_call(\() enrollment$irr(weight_col = weight_col), slot)
      )),
      slot
    )
  } else if (method == "rates") {
    # ITT rates (weight ipw_trunc) -> rates_itt, for the ITT forest plot.
    slot <- if (identical(weight_col, "ipw_trunc")) {
      "rates_itt"
    } else {
      paste0("rates_", sub("^analysis_weight_", "", weight_col))
    }
    setNames(
      list(safe_call(\() enrollment$rates(weight_col = weight_col), slot)),
      slot
    )
  } else if (method == "risk_difference") {
    # The absolute scale. ITT weights on ipw_trunc -> rd_itt; PP weights on
    # analysis_weight_pp_trunc -> rd_pp_trunc. Nothing gates this branch: the
    # item builder emits it for every ETT, and the export path only formats
    # what it stores.
    slot <- if (identical(weight_col, "ipw_trunc")) {
      "rd_itt"
    } else {
      paste0("rd_", sub("^analysis_weight_", "", weight_col))
    }
    curve <- safe_call(
      \() {
        # Re-wrapped under the CURRENT class. A serialized R6 object keeps
        # the method bindings it was saved with. So an analysis file from an
        # earlier release carries no $risk_difference() at all. `own_data`
        # skips the defensive copy, which here is the whole panel.
        enr <- TTEEnrollment$new(
          enrollment$data,
          enrollment$design,
          data_level = "trial",
          own_data = TRUE
        )
        enr$risk_difference(
          weight_col = weight_col,
          n_boot = .S3_RD_N_BOOT,
          seed = .S3_RD_SEED,
          conf_level = conf_level
        )
      },
      slot
    )
    .s3_rd_result(slot, curve, ett_id, enrollment$design$tstop_var)
  } else if (method == "irr_by_subgroup") {
    # Stratified IRRs within subgroup_var; slot e.g. subgroup_rd_sex_pp / _itt.
    suffix <- if (identical(weight_col, "ipw_trunc")) "itt" else "pp"
    slot <- paste0("subgroup_", subgroup_var, "_", suffix)
    setNames(
      list(safe_call(
        \() enrollment$irr_by_subgroup(weight_col, subgroup_var),
        slot
      )),
      slot
    )
  } else if (method == "effect_modification_test") {
    # Interaction Wald test; slot e.g. emtest_rd_sex_pp / _itt.
    suffix <- if (identical(weight_col, "ipw_trunc")) "itt" else "pp"
    slot <- paste0("emtest_", subgroup_var, "_", suffix)
    setNames(
      list(safe_call(
        \() enrollment$effect_modification_test(weight_col, subgroup_var),
        slot
      )),
      slot
    )
  } else {
    stop("Unknown method: ", method)
  }
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
  if (is.null(x$ett) || nrow(x$ett) == 0) {
    return(0L)
  }
  data.table::uniqueN(x$ett$enrollment_id)
}


# =============================================================================
# Spec functions (called by TTEPlan methods and Loop 1 workers)
# =============================================================================

# =============================================================================
# tteplan_read_spec
# =============================================================================

#' Read and validate a YAML study specification
#'
#' Parses a YAML study specification file, converts human-readable windows
#' (e.g., "lifetime_before_baseline", "3 years") to weeks, and validates that all required
#' fields are present.
#'
#' @param spec_path Path to the YAML specification file.
#' @return A nested list representing the parsed specification, with
#'   `window_weeks` fields added to exclusion criteria and confounders.
#'
#' @details
#' Window conversion rules:
#' \itemize{
#'   \item `"lifetime_before_baseline"` -> `Inf`
#'   \item `"N year"` or `"N years"` -> `N * 52`
#' }
#'
#' Validation checks:
#' \itemize{
#'   \item Required sections: study, enrollments, outcomes, follow_up
#'   \item `study$implementation$project_prefix` must exist
#'   \item Each exclusion criterion must have `implementation$source_variable`
#'   \item Each outcome must have `implementation$variable`
#'   \item Each enrollment must have `id` and `treatment$implementation$variable`
#'   \item Computed confounders must have `implementation$source_variable`
#' }
#'
#' Warns about open questions with `status: "open"`.
#'
#' @section The observation contract:
#'
#' An enrollment states how the data records that a person was under
#' observation in a week. It is a flat key on the enrollment, `observed_var`,
#' and it takes exactly one of two forms.
#'
#' \preformatted{
#' observed_var: {column: rd_observed}      # a real logical person-week column
#' observed_var: {sentinel: row_presence}   # the skeleton is trimmed
#' }
#'
#' The `row_presence` sentinel asserts that the caller already deleted every
#' unobserved person-week. A row then exists if and only if the person was
#' observed that week. Use it when the skeleton already deletes every
#' person-week the person was not under observation. The production skeleton
#' is one example. It deletes every person-week up to and including first
#' immigration, every person-week on or after emigration, and every
#' person-week after death. It keeps the death week itself. A real `observed`
#' column there would hold `TRUE` on every retained row. It could not
#' represent an absent week. Row presence as a silent proxy stays forbidden.
#' The sentinel is what makes the assumption explicit and testable.
#'
#' Two flat sibling keys carry the arm tolerances:
#' `intervention_tolerance_weeks` and `comparator_tolerance_weeks`. Each MUST
#' be a whole number of at least 0. Each defaults to 0.
#'
#' Every enrollment MUST declare `observed_var`. There is no exemption for an
#' older spec. A spec that cannot say who was under observation carries the
#' immortal-time defect silently. It looks exactly like a spec that can.
#' To migrate a spec, copy it to a new version and add the key to every
#' enrollment. Never edit a released spec version: that version is the record
#' of what produced a run.
#'
#' The function rejects a declaration that gives both `column` and `sentinel`,
#' a declaration that gives neither, and a sentinel name swereg does not know.
#' It cannot check that a named column exists and is logical, because it reads
#' no data. [tteplan_validate_spec()] runs that check against the skeleton.
#'
#' @family tte_spec
#' @export
tteplan_read_spec <- function(spec_path) {
  if (!file.exists(spec_path)) {
    stop("Spec file not found: ", spec_path)
  }

  # Read the spec as raw bytes and decode as UTF-8 explicitly, independent of
  # the session locale. yaml::read_yaml() -> readLines() under a non-UTF-8
  # locale (e.g. LC_CTYPE=C in headless/cron runs) silently truncates the YAML
  # at the first non-ASCII byte: the v008 spec lost statin arms 16-18 (a
  # 15/18-enrollment grid) at an em-dash in a comment, with only a readLines
  # warning. readBin bypasses readLines; validUTF8() then fails loudly on a
  # genuinely non-UTF-8 file instead of silently mis-decoding it.
  fsize <- file.info(spec_path)$size
  if (is.na(fsize)) {
    stop("Cannot determine the size of the spec file: ", spec_path)
  }
  spec_bytes <- readBin(spec_path, "raw", n = fsize)
  if (
    length(spec_bytes) >= 3L &&
      identical(spec_bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  ) {
    spec_bytes <- spec_bytes[-(1:3)] # strip a UTF-8 BOM (some Windows editors add one)
  }
  spec_txt <- rawToChar(spec_bytes)
  if (!validUTF8(spec_txt)) {
    stop("Spec file is not valid UTF-8 (re-save it as UTF-8): ", spec_path)
  }
  Encoding(spec_txt) <- "UTF-8"
  spec <- yaml::yaml.load(spec_txt)

  # Validate required sections
  required_sections <- c(
    "study",
    "enrollments",
    "outcomes",
    "follow_up"
  )
  missing <- setdiff(required_sections, names(spec))
  if (length(missing) > 0) {
    stop(
      "Missing required sections in spec: ",
      paste(missing, collapse = ", ")
    )
  }

  # Validate study has implementation$project_prefix
  if (is.null(spec$study$implementation$project_prefix)) {
    stop("study$implementation$project_prefix is required")
  }

  # Validate and convert exclusion_criteria
  if (!is.null(spec$exclusion_criteria)) {
    for (i in seq_along(spec$exclusion_criteria)) {
      ec <- spec$exclusion_criteria[[i]]

      if (is.null(ec$implementation$source_variable)) {
        stop(
          "exclusion_criteria[",
          i,
          "] '",
          ec$name,
          "' is missing implementation$source_variable"
        )
      }

      # Normalize source_variable (may be a YAML list for multi-source)
      spec$exclusion_criteria[[i]]$implementation <-
        .normalize_source_variable(spec$exclusion_criteria[[i]]$implementation)

      if (
        identical(
          ec$implementation$window,
          "lifetime_before_and_after_baseline"
        )
      ) {
        # Person-level: no window_weeks conversion needed
      } else {
        if (is.null(ec$implementation$window)) {
          stop(
            "exclusion_criteria[",
            i,
            "] '",
            ec$name,
            "' is missing implementation$window"
          )
        }
        spec$exclusion_criteria[[i]]$implementation$window_weeks <-
          .convert_window(ec$implementation$window)
      }
    }
  }

  # Validate and normalize outcomes
  for (i in seq_along(spec$outcomes)) {
    if (is.null(spec$outcomes[[i]]$implementation$variable)) {
      stop(
        "outcomes[",
        i,
        "] '",
        spec$outcomes[[i]]$name,
        "' is missing implementation$variable"
      )
    }
    # Normalize variable (may be a YAML list for multi-source outcomes)
    v <- spec$outcomes[[i]]$implementation$variable
    if (is.list(v)) {
      v <- unlist(v)
    }
    spec$outcomes[[i]]$implementation$variable <- as.character(v)
    spec$outcomes[[i]]$implementation$variable_combined <-
      paste(spec$outcomes[[i]]$implementation$variable, collapse = "__")
  }

  # Validate enrollments
  for (i in seq_along(spec$enrollments)) {
    enr <- spec$enrollments[[i]]
    if (is.null(enr$id)) {
      stop("enrollments[", i, "] is missing 'id'")
    }

    # The observation contract. Every enrollment MUST state how observation is
    # encoded. There is no exemption for an older spec. A spec that cannot say
    # who was under observation carries the immortal-time defect silently. It
    # looks exactly like one that can.
    if (is.null(enr$observed_var)) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing 'observed_var'. Every enrollment MUST state how ",
        "observation is encoded: `observed_var: {column: <name>}` for a real ",
        "logical column, or `observed_var: {sentinel: row_presence}` for a ",
        "trimmed skeleton. Copy the spec to a new version and add the key ",
        "to every enrollment. Never edit a released spec version."
      )
    }
    spec$enrollments[[i]]$observed_var <- .tte_observed_var(
      enr$observed_var,
      paste0("enrollments[", i, "]$observed_var")
    )
    spec$enrollments[[i]]$intervention_tolerance_weeks <- .tte_tolerance_weeks(
      enr$intervention_tolerance_weeks,
      paste0("enrollments[", i, "]$intervention_tolerance_weeks")
    )
    spec$enrollments[[i]]$comparator_tolerance_weeks <- .tte_tolerance_weeks(
      enr$comparator_tolerance_weeks,
      paste0("enrollments[", i, "]$comparator_tolerance_weeks")
    )

    if (is.null(enr$treatment$implementation$variable)) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing treatment$implementation$variable"
      )
    }
    if (is.null(enr$treatment$implementation$matching_ratio)) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing treatment$implementation$matching_ratio"
      )
    }

    # Validate and convert additional_exclusion entries
    if (!is.null(enr$additional_exclusion)) {
      for (j in seq_along(enr$additional_exclusion)) {
        ae <- enr$additional_exclusion[[j]]
        if (is.null(ae$implementation$source_variable)) {
          stop(
            "enrollments[",
            i,
            "] '",
            enr$name %||% enr$id,
            "' additional_exclusion[",
            j,
            "] '",
            ae$name,
            "' is missing implementation$source_variable"
          )
        }

        # Normalize source_variable (may be a YAML list for multi-source)
        spec$enrollments[[i]]$additional_exclusion[[
          j
        ]]$implementation <-
          .normalize_source_variable(ae$implementation)

        if (
          identical(
            ae$implementation$window,
            "lifetime_before_and_after_baseline"
          )
        ) {
          # Person-level: no window_weeks conversion needed
        } else {
          if (is.null(ae$implementation$window)) {
            stop(
              "enrollments[",
              i,
              "] '",
              enr$name %||% enr$id,
              "' additional_exclusion[",
              j,
              "] '",
              ae$name,
              "' is missing implementation$window"
            )
          }
          spec$enrollments[[i]]$additional_exclusion[[
            j
          ]]$implementation$window_weeks <-
            .convert_window(ae$implementation$window)
        }
      }
    }

    # Normalize has_event additional_inclusion entries
    if (!is.null(enr$additional_inclusion)) {
      for (j in seq_along(enr$additional_inclusion)) {
        ai <- enr$additional_inclusion[[j]]
        if (identical(ai$type, "has_event")) {
          if (is.null(ai$implementation$source_variable)) {
            stop(
              "enrollments[",
              i,
              "] '",
              enr$name %||% enr$id,
              "' additional_inclusion[",
              j,
              "] '",
              ai$name,
              "' is missing implementation$source_variable"
            )
          }
          spec$enrollments[[i]]$additional_inclusion[[
            j
          ]]$implementation <-
            .normalize_source_variable(ai$implementation)
          spec$enrollments[[i]]$additional_inclusion[[
            j
          ]]$implementation$window_weeks <-
            .convert_window(
              ai$implementation$window %||% "lifetime_before_baseline"
            )
        }
      }
    }
  }

  # New-user / washout guard: enrollment classifies a person-band as
  # "intervention" via any(rd_intervention) with no built-in initiation rule,
  # so without an exclusion tied to the treatment variable, prevalent users
  # enrol as intervention at every eligible band and discontinuers flip to
  # comparator -- a prevalent-user design, almost never the intended estimand.
  # Warn rather than stop: discontinuation/switching studies legitimately
  # enrol prevalent users.
  for (enr in spec$enrollments) {
    tx_var <- enr$treatment$implementation$variable
    if (is.null(tx_var)) {
      next
    }
    excls <- c(
      spec$exclusion_criteria %||% list(),
      enr$additional_exclusion %||% list()
    )
    has_newuser <- any(vapply(
      excls,
      function(ec) {
        impl <- ec$implementation %||% list()
        identical(impl$type, "no_prior_intervention") ||
          tx_var %in% (impl$source_variable %||% character())
      },
      logical(1)
    ))
    if (
      !has_newuser &&
        isTRUE(getOption("swereg.warn_prevalent_user", TRUE))
    ) {
      warning(
        "enrollment '",
        enr$id %||% enr$name,
        "' has no new-user/washout exclusion on its treatment variable ('",
        tx_var,
        "'): prevalent users will enrol as intervention at every eligible ",
        "trial period (prevalent-user design). If an incident-user design ",
        "is intended, add an exclusion on the treatment variable -- either ",
        "a finite washout window (e.g. window: 104 weeks, as in Danaei ",
        "2013) or window: 'lifetime_before_baseline' for a never-user ",
        "design (implementation type 'no_prior_intervention').",
        call. = FALSE
      )
    }
  }

  # Convert confounder windows and validate computed confounders
  if (!is.null(spec$confounders)) {
    for (i in seq_along(spec$confounders)) {
      conf <- spec$confounders[[i]]
      if (!is.null(conf$implementation$window)) {
        spec$confounders[[i]]$implementation$window_weeks <-
          .convert_window(conf$implementation$window)
      }
      if (isTRUE(conf$implementation$computed)) {
        if (is.null(conf$implementation$source_variable)) {
          stop(
            "confounders[",
            i,
            "] '",
            conf$name,
            "' is computed but missing implementation$source_variable"
          )
        }
        if (is.null(conf$implementation$window)) {
          stop(
            "confounders[",
            i,
            "] '",
            conf$name,
            "' is computed but missing implementation$window"
          )
        }

        # Normalize source_variable (may be a YAML list for multi-source)
        spec$confounders[[i]]$implementation <-
          .normalize_source_variable(spec$confounders[[i]]$implementation)

        # Auto-derive variable name from source_variable_combined + window
        spec$confounders[[i]]$implementation$variable <- paste0(
          "rd_no_",
          spec$confounders[[i]]$implementation$source_variable_combined,
          "_",
          .window_label(spec$confounders[[i]]$implementation$window_weeks)
        )
      }
    }
  }

  # Normalize subgroups (optional): categorical effect modifiers, each with an
  # implementation$variable that must also be a confounder (checked in
  # tteplan_validate_spec against the skeleton + confounder list).
  if (!is.null(spec$subgroups)) {
    for (i in seq_along(spec$subgroups)) {
      if (is.null(spec$subgroups[[i]]$implementation$variable)) {
        stop(
          "subgroups[",
          i,
          "] (",
          spec$subgroups[[i]]$name %||% "unnamed",
          ") is missing implementation$variable"
        )
      }
      spec$subgroups[[i]]$implementation$variable <-
        as.character(spec$subgroups[[i]]$implementation$variable)
    }
  }

  # Warn about open questions
  if (!is.null(spec$open_questions)) {
    open <- Filter(
      function(q) is.null(q$status) || q$status == "open",
      spec$open_questions
    )
    if (length(open) > 0) {
      warning(
        length(open),
        " open question(s) in spec:\n",
        paste0(
          "  - ",
          vapply(open, function(q) q$question, character(1)),
          collapse = "\n"
        )
      )
    }
  }

  spec
}


#' Convert a window value to weeks
#'
#' @param window Numeric weeks, or character: "lifetime_before_baseline",
#'   "N year", "N years".
#' @return Numeric: `Inf` for lifetime_before_baseline, integer weeks otherwise.
#' @noRd
.convert_window <- function(window) {
  if (identical(window, "lifetime_before_baseline")) {
    return(Inf)
  }
  if (is.numeric(window)) {
    return(as.integer(window))
  }

  # Legacy string support
  m <- regmatches(window, regexec("^(\\d+)\\s+years?$", window))[[1]]
  if (length(m) == 2) {
    return(as.integer(m[2]) * 52L)
  }

  stop(
    "Cannot parse window: '",
    window,
    "'. Expected 'lifetime_before_baseline', numeric weeks, or 'N year(s)'."
  )
}


# =============================================================================
# tteplan_apply_exclusions
# =============================================================================

# --- internal: batched per-group eligibility evaluator -----------------------
#
# Collects an arbitrary set of eligibility specs and emits them all in ONE
# `dt[, c(...) := list(...), by = id]` call. Each spec is one of:
#
#   list(col_name, type = "lifetime",         source_var)
#   list(col_name, type = "windowed",         source_var, window_weeks,
#        negate_final = FALSE)
#   list(col_name, type = "windowed_no_obs",  source_var, value, window_weeks)
#
# The per-criterion helpers (skeleton_eligible_*) each do their own by=id
# pass; with 12 exclusions in 003 that's 12 separate radix-walks + 12 column
# allocations. Batching shares the group identification across all specs.
# Each spec contributes one element to the `list(...)` j-expression below,
# so data.table compiles the j once and evaluates it per group.
.tte_apply_eligibility_batch <- function(skeleton, specs, id_col = "id") {
  if (length(specs) == 0L) {
    return(skeleton)
  }

  col_names <- vapply(specs, `[[`, character(1), "col_name")

  # Build the j-expression as a list() call of N typed sub-expressions.
  # Using bquote() so the function is named at compile time (no .C-style
  # dispatch through `get()` per group, and no R-level for-loop inside j).
  per_spec_call <- lapply(specs, function(sp) {
    src_sym <- as.name(sp$source_var)
    switch(
      sp$type,
      "lifetime" = bquote(!any(.(src_sym), na.rm = TRUE)),
      "windowed" = {
        inner <- bquote(swereg::any_events_prior_to(
          .(src_sym),
          window_excluding_wk0 = .(as.integer(sp$window_weeks))
        ))
        if (isTRUE(sp$negate_final)) inner else bquote(!.(inner))
      },
      "windowed_no_obs" = bquote(
        !swereg::any_events_prior_to(
          .(src_sym) == .(sp$value),
          window_excluding_wk0 = .(as.integer(sp$window_weeks))
        )
      ),
      stop("Unknown eligibility spec type: ", sp$type)
    )
  })
  j_expr <- as.call(c(quote(list), per_spec_call))

  skeleton[,
    (col_names) := eval(j_expr),
    by = id_col
  ]
  skeleton
}

# --- internal: collect exclusion grouped specs ------------------------------
#
# Mirrors tteplan_apply_exclusions but returns
#   list(eligible_cols = ..., grouped_specs = ...)
# instead of running the by=id batch. The skeleton is still mutated in place
# (combined outcome columns, vectorized isoyears + age_range eligibles,
# .ensure_combined_column for list-valued source_variable). Callers run the
# batch themselves -- standalone via tteplan_apply_exclusions(), or fused
# with .tte_build_confounder_specs() in .s1_prepare_skeleton() for a single
# combined batch across exclusions + computed confounders.
.tte_build_exclusion_specs <- function(skeleton, spec, enrollment_spec) {
  enrollment_id <- enrollment_spec$enrollment_id

  enrollment_def <- NULL
  for (enr in spec$enrollments) {
    if (enr$id == enrollment_id) {
      enrollment_def <- enr
      break
    }
  }
  if (is.null(enrollment_def)) {
    stop("Enrollment ID '", enrollment_id, "' not found in spec$enrollments")
  }

  # 0. Create combined outcome columns (multi-source outcomes)
  for (outcome in spec$outcomes) {
    v <- outcome$implementation$variable
    if (length(v) > 1L) {
      combined <- outcome$implementation$variable_combined
      if (!combined %in% names(skeleton)) {
        skeleton[, (combined) := Reduce(`|`, .SD), .SDcols = v]
      }
    }
  }

  # 1. Calendar years (vectorized, no by=id grouping)
  years <- seq(
    spec$inclusion_criteria$isoyears[1],
    spec$inclusion_criteria$isoyears[2]
  )
  skeleton_eligible_isoyears(skeleton, years)
  eligible_cols <- "eligible_isoyears"

  grouped_specs <- list()

  # 2. Enrollment-specific additional inclusion (age_range is vectorized;
  #    has_event goes into the grouped batch)
  if (!is.null(enrollment_def$additional_inclusion)) {
    for (ae in enrollment_def$additional_inclusion) {
      if (identical(ae$type, "age_range")) {
        skeleton_eligible_age_range(
          skeleton,
          age_var = ae$implementation$variable,
          min_age = ae$min,
          max_age = ae$max
        )
        eligible_cols <- c(eligible_cols, "eligible_age")
      } else if (identical(ae$type, "has_event")) {
        impl <- ae$implementation
        sv <- impl$source_variable_combined
        .ensure_combined_column(skeleton, impl)
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_has_",
          sv,
          "_",
          .window_label(window)
        )
        # negate_final = TRUE: emit `any_events_prior_to(...)` directly
        # (i.e. has-event semantics) without the temp-col round-trip.
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "windowed",
          source_var = sv,
          window_weeks = if (is.infinite(window)) {
            99999L
          } else {
            as.integer(window)
          },
          negate_final = TRUE
        )
        eligible_cols <- c(eligible_cols, col_name)
      }
    }
  }

  # 3. Global exclusion criteria (all grouped)
  for (ec in spec$exclusion_criteria) {
    impl <- ec$implementation
    sv <- impl$source_variable_combined
    .ensure_combined_column(skeleton, impl)

    if (identical(impl$window, "lifetime_before_and_after_baseline")) {
      col_name <- paste0(
        "eligible_no_",
        sv,
        "_lifetime_before_and_after_baseline"
      )
      grouped_specs[[length(grouped_specs) + 1L]] <- list(
        col_name = col_name,
        type = "lifetime",
        source_var = sv
      )
    } else if (identical(impl$type, "no_prior_intervention")) {
      window <- impl$window_weeks
      col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
      grouped_specs[[length(grouped_specs) + 1L]] <- list(
        col_name = col_name,
        type = "windowed_no_obs",
        source_var = sv,
        value = impl$intervention_value,
        window_weeks = if (is.infinite(window)) 99999L else as.integer(window)
      )
    } else {
      window <- impl$window_weeks
      col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
      grouped_specs[[length(grouped_specs) + 1L]] <- list(
        col_name = col_name,
        type = "windowed",
        source_var = sv,
        window_weeks = if (is.infinite(window)) 99999L else as.integer(window),
        negate_final = FALSE
      )
    }
    eligible_cols <- c(eligible_cols, col_name)
  }

  # 4. Enrollment-specific additional exclusion criteria (all grouped)
  if (!is.null(enrollment_def$additional_exclusion)) {
    for (ec in enrollment_def$additional_exclusion) {
      impl <- ec$implementation
      sv <- impl$source_variable_combined
      .ensure_combined_column(skeleton, impl)

      if (identical(impl$window, "lifetime_before_and_after_baseline")) {
        col_name <- paste0(
          "eligible_no_",
          sv,
          "_lifetime_before_and_after_baseline"
        )
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "lifetime",
          source_var = sv
        )
      } else if (identical(impl$type, "no_prior_intervention")) {
        window <- impl$window_weeks
        col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "windowed_no_obs",
          source_var = sv,
          value = impl$intervention_value,
          window_weeks = if (is.infinite(window)) 99999L else as.integer(window)
        )
      } else {
        window <- impl$window_weeks
        col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "windowed",
          source_var = sv,
          window_weeks = if (is.infinite(window)) {
            99999L
          } else {
            as.integer(window)
          },
          negate_final = FALSE
        )
      }
      eligible_cols <- c(eligible_cols, col_name)
    }
  }

  list(eligible_cols = eligible_cols, grouped_specs = grouped_specs)
}

#' Apply exclusion criteria from a study spec to a skeleton
#'
#' Applies calendar year eligibility, enrollment-specific additional inclusion
#' (e.g., age range), global exclusion criteria, and enrollment-specific
#' additional exclusion criteria from the parsed study specification. Calls
#' [skeleton_eligible_combine()] at the end to AND all criteria into a single
#' `eligible` column.
#'
#' @param skeleton A data.table skeleton (person-week panel).
#' @param spec Parsed study specification from [tteplan_read_spec()].
#' @param enrollment_spec Enrollment spec from the plan (must contain
#'   `enrollment_id`), as returned by `plan[[i]]`.
#' @return The skeleton (modified by reference), with eligibility columns
#'   added and a combined `eligible` column.
#'
#' @family tte_spec
#' @export
tteplan_apply_exclusions <- function(skeleton, spec, enrollment_spec) {
  built <- .tte_build_exclusion_specs(skeleton, spec, enrollment_spec)
  skeleton <- .tte_apply_eligibility_batch(
    skeleton,
    built$grouped_specs,
    id_col = "id"
  )
  skeleton <- skeleton_eligible_combine(skeleton, built$eligible_cols)
  data.table::setattr(skeleton, "eligible_cols", built$eligible_cols)
  skeleton
}


#' Format a window value as a label for column names
#'
#' @param window_weeks Numeric: weeks or Inf.
#' @return Character: "everbefore" for Inf, "{weeks}wk" otherwise.
#' @noRd
.window_label <- function(window_weeks) {
  if (is.infinite(window_weeks)) "everbefore" else paste0(window_weeks, "wk")
}


#' Normalize source_variable to a character vector and derive a combined name
#'
#' YAML lists become R lists; this ensures we always work with character vectors.
#' If multiple variables, `source_variable_combined` is the `__`-joined name.
#' If single, `source_variable_combined` equals `source_variable`.
#'
#' @param impl The implementation list from a spec entry.
#' @return The implementation list with `source_variable` as character vector
#'   and `source_variable_combined` as a single string.
#' @noRd
.normalize_source_variable <- function(impl) {
  sv <- impl$source_variable
  if (is.list(sv)) {
    sv <- unlist(sv)
  }
  impl$source_variable <- as.character(sv)
  impl$source_variable_combined <- paste(impl$source_variable, collapse = "__")
  impl
}


#' Ensure a combined source variable column exists on the skeleton
#'
#' If `source_variable` has multiple elements, creates (or overwrites) the
#' combined column as the row-wise OR. If single, does nothing.
#'
#' @param skeleton A data.table.
#' @param impl Implementation list (after `.normalize_source_variable()`).
#' @return The skeleton (modified by reference).
#' @noRd
.ensure_combined_column <- function(skeleton, impl) {
  sv <- impl$source_variable
  if (length(sv) > 1L) {
    combined <- impl$source_variable_combined
    skeleton[, (combined) := Reduce(`|`, .SD), .SDcols = sv]
  }
  invisible(skeleton)
}


# =============================================================================
# tteplan_apply_derived_confounders
# =============================================================================

# --- internal: collect computed-confounder grouped specs --------------------
#
# Mutates skeleton in place (calls .ensure_combined_column for any list-valued
# source_variable), then returns the list of grouped specs ready for
# .tte_apply_eligibility_batch(). Used by both tteplan_apply_derived_confounders
# (standalone) and .s1_prepare_skeleton (combined batch with exclusions).
.tte_build_confounder_specs <- function(skeleton, spec) {
  if (is.null(spec$confounders)) {
    return(list())
  }
  grouped_specs <- list()
  for (conf in spec$confounders) {
    impl <- conf$implementation
    if (!isTRUE(impl$computed)) {
      next
    }
    .ensure_combined_column(skeleton, impl)
    window <- impl$window_weeks
    grouped_specs[[length(grouped_specs) + 1L]] <- list(
      col_name = impl$variable,
      type = "windowed",
      source_var = impl$source_variable_combined,
      window_weeks = if (is.infinite(window)) 99999L else as.integer(window),
      negate_final = FALSE
    )
  }
  grouped_specs
}

#' Compute derived confounder columns from a study spec
#'
#' For confounders with `implementation$computed: true`, computes rolling
#' window indicators using [skeleton_eligible_no_events_in_window_excluding_wk0()].
#' Requires `implementation$source_variable` and `implementation$window` to be set.
#'
#' @param skeleton A data.table skeleton (person-week panel).
#' @param spec Parsed study specification from [tteplan_read_spec()].
#' @return The skeleton (modified by reference), with derived confounder
#'   columns added.
#'
#' @family tte_spec
#' @export
tteplan_apply_derived_confounders <- function(skeleton, spec) {
  if (is.null(spec$confounders)) {
    return(skeleton)
  }
  grouped_specs <- .tte_build_confounder_specs(skeleton, spec)
  .tte_apply_eligibility_batch(skeleton, grouped_specs, id_col = "id")
}


# =============================================================================
# tteplan_validate_spec
# =============================================================================

#' Validate spec variables against skeleton data
#'
#' Checks that all `implementation$variable` references in the spec actually
#' exist as columns in the skeleton data.table. For categorical confounders,
#' also checks that the declared categories match the data. Collects all
#' issues before reporting.
#'
#' It also checks the observation column of every enrollment that names one:
#' the column MUST exist in the skeleton, and it MUST be logical.
#' [tteplan_read_spec()] cannot run that check, because it reads no data. An
#' enrollment that declares the `row_presence` sentinel names no column, so
#' there is nothing to check.
#'
#' @param spec Parsed study specification from [tteplan_read_spec()].
#' @param skeleton A data.table skeleton (person-week panel) to validate
#'   against.
#' @return `invisible(TRUE)` on success; emits a warning with a numbered
#'   issue list if any checks fail.
#'
#' @family tte_spec
#' @export
tteplan_validate_spec <- function(spec, skeleton) {
  if (!data.table::is.data.table(skeleton)) {
    stop("skeleton must be a data.table, got ", class(skeleton)[1])
  }

  errors <- character(0)
  warnings <- character(0)
  n_checked <- 0L
  skel_cols <- names(skeleton)

  # --- Exclusion criteria ---
  for (i in seq_along(spec$exclusion_criteria)) {
    ec <- spec$exclusion_criteria[[i]]
    vars <- ec$implementation$source_variable
    n_checked <- n_checked + 1L
    missing <- vars[!vars %in% skel_cols]
    if (length(missing) > 0) {
      errors <- c(
        errors,
        paste0(
          "exclusion_criteria '",
          ec$name,
          "': source_variable '",
          paste(missing, collapse = "', '"),
          "' not found in skeleton"
        )
      )
    }
  }

  # --- Outcomes ---
  for (i in seq_along(spec$outcomes)) {
    out <- spec$outcomes[[i]]
    vars <- out$implementation$variable
    n_checked <- n_checked + 1L
    missing <- vars[!vars %in% skel_cols]
    if (length(missing) > 0) {
      errors <- c(
        errors,
        paste0(
          "outcomes '",
          out$name,
          "': variable '",
          paste(missing, collapse = "', '"),
          "' not found in skeleton"
        )
      )
    }
  }

  # --- Confounders ---
  for (i in seq_along(spec$confounders)) {
    conf <- spec$confounders[[i]]
    impl <- conf$implementation

    if (isTRUE(impl$computed)) {
      # Computed: check source_variable exists, skip variable (created later)
      n_checked <- n_checked + 1L
      missing <- impl$source_variable[!impl$source_variable %in% skel_cols]
      if (length(missing) > 0) {
        errors <- c(
          errors,
          paste0(
            "confounders '",
            conf$name,
            "': source_variable '",
            paste(missing, collapse = "', '"),
            "' not found in skeleton"
          )
        )
      }
    } else {
      # Non-computed: check variable exists
      n_checked <- n_checked + 1L
      if (!impl$variable %in% skel_cols) {
        errors <- c(
          errors,
          paste0(
            "confounders '",
            conf$name,
            "': variable '",
            impl$variable,
            "' not found in skeleton"
          )
        )
      } else if (!is.null(conf$categories)) {
        # Category check (soft: categories may be absent in small batches)
        data_values <- unique(stats::na.omit(skeleton[[impl$variable]]))
        spec_values <- unlist(conf$categories)
        in_data_not_spec <- setdiff(data_values, spec_values)
        in_spec_not_data <- setdiff(spec_values, data_values)
        if (length(in_data_not_spec) > 0) {
          errors <- c(
            errors,
            paste0(
              "confounders '",
              conf$name,
              "': values in data but not spec: ",
              paste(in_data_not_spec, collapse = ", ")
            )
          )
        }
        if (length(in_spec_not_data) > 0) {
          warnings <- c(
            warnings,
            paste0(
              "confounders '",
              conf$name,
              "': values in spec but not data (may be absent in this batch): ",
              paste(in_spec_not_data, collapse = ", ")
            )
          )
        }
      }
    }
  }

  # --- Enrollments ---
  for (i in seq_along(spec$enrollments)) {
    enr <- spec$enrollments[[i]]
    tx_impl <- enr$treatment$implementation

    # Treatment variable
    n_checked <- n_checked + 1L
    if (!tx_impl$variable %in% skel_cols) {
      errors <- c(
        errors,
        paste0(
          "enrollments '",
          enr$name %||% enr$id,
          "': treatment variable '",
          tx_impl$variable,
          "' not found in skeleton"
        )
      )
    } else {
      # Check intervention_value and comparator_value are present in data
      data_values <- unique(skeleton[[tx_impl$variable]])
      if (!tx_impl$intervention_value %in% data_values) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': intervention_value '",
            tx_impl$intervention_value,
            "' not found in column '",
            tx_impl$variable,
            "'"
          )
        )
      }
      if (!tx_impl$comparator_value %in% data_values) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': comparator_value '",
            tx_impl$comparator_value,
            "' not found in column '",
            tx_impl$variable,
            "'"
          )
        )
      }
    }

    # Observation column. The parser cannot run this check, because it reads
    # no data. A sentinel names no column, so there is nothing to check.
    obs_col <- .tte_observed_column(enr$observed_var)
    if (!is.null(obs_col)) {
      n_checked <- n_checked + 1L
      if (!obs_col %in% skel_cols) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': observed_var column '",
            obs_col,
            "' not found in skeleton"
          )
        )
      } else if (!is.logical(skeleton[[obs_col]])) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': observed_var column '",
            obs_col,
            "' must be logical, and it is ",
            class(skeleton[[obs_col]])[1]
          )
        )
      }
    }

    # Additional inclusion variables
    if (!is.null(enr$additional_inclusion)) {
      for (ae in enr$additional_inclusion) {
        if (identical(ae$type, "has_event")) {
          vars <- ae$implementation$source_variable
          n_checked <- n_checked + 1L
          missing <- vars[!vars %in% skel_cols]
          if (length(missing) > 0) {
            errors <- c(
              errors,
              paste0(
                "enrollments '",
                enr$name %||% enr$id,
                "': additional_inclusion source_variable '",
                paste(missing, collapse = "', '"),
                "' not found in skeleton"
              )
            )
          }
        } else if (!is.null(ae$implementation$variable)) {
          n_checked <- n_checked + 1L
          if (!ae$implementation$variable %in% skel_cols) {
            errors <- c(
              errors,
              paste0(
                "enrollments '",
                enr$name %||% enr$id,
                "': additional_inclusion variable '",
                ae$implementation$variable,
                "' not found in skeleton"
              )
            )
          }
        }
      }
    }

    # Additional exclusion variables
    if (!is.null(enr$additional_exclusion)) {
      for (ae in enr$additional_exclusion) {
        var <- ae$implementation$source_variable
        n_checked <- n_checked + 1L
        if (!var %in% skel_cols) {
          errors <- c(
            errors,
            paste0(
              "enrollments '",
              enr$name %||% enr$id,
              "': additional_exclusion source_variable '",
              var,
              "' not found in skeleton"
            )
          )
        }
      }
    }
  }

  # --- Subgroups: each must exist in the skeleton AND be a confounder. The
  # within-stratum validity of the marginal weights requires the subgroup to
  # be in the PS / IPCW models (i.e. among the confounders).
  if (!is.null(spec$subgroups)) {
    confounder_vars <- vapply(
      spec$confounders %||% list(),
      function(cf) cf$implementation$variable %||% NA_character_,
      character(1)
    )
    for (i in seq_along(spec$subgroups)) {
      sv <- spec$subgroups[[i]]$implementation$variable
      if (!sv %in% skel_cols) {
        errors <- c(
          errors,
          paste0(
            "subgroups[",
            i,
            "] variable '",
            sv,
            "' not found in skeleton columns"
          )
        )
      }
      if (!sv %in% confounder_vars) {
        errors <- c(
          errors,
          paste0(
            "subgroups[",
            i,
            "] variable '",
            sv,
            "' must also be a confounder (effect-modifier weights are only ",
            "valid within strata when the subgroup is in the PS/IPCW models)"
          )
        )
      }
    }
  }

  # --- Report results ---
  # Warnings are soft issues (e.g. category absent in this batch)
  if (length(warnings) > 0) {
    warning(
      "Spec validation: ",
      length(warnings),
      " warning(s):\n",
      paste0("  ", seq_along(warnings), ". ", warnings, collapse = "\n"),
      call. = FALSE
    )
  }

  # Errors are hard failures (missing variables that will break the pipeline)
  if (length(errors) > 0) {
    stop(
      "Spec validation failed: ",
      length(errors),
      " error(s):\n",
      paste0("  ", seq_along(errors), ". ", errors, collapse = "\n"),
      call. = FALSE
    )
  }

  message(
    "Spec validation passed: ",
    n_checked,
    " entries checked against ",
    length(skel_cols),
    " columns"
  )
  invisible(TRUE)
}


# =============================================================================
# tteplan_from_spec_and_registrystudy
# =============================================================================

#' Create a TTEPlan from a study specification
#'
#' Builds a [TTEPlan] with a full ETT grid (enrollments x outcomes x
#' follow-up) from the parsed study specification and a pre-loaded
#' [RegistryStudy]. Also stores each enrollment's treatment implementation
#' details in the ETT data.table so they are available via
#' `plan[[i]]$treatment_impl`.
#'
#' It carries each enrollment's observation encoding and both arm tolerances
#' from the spec into the ETT data.table. `plan[[i]]$design` reads them back
#' onto a [TTEDesign]. See the observation contract section of
#' [tteplan_read_spec()].
#'
#' Directory-resolution fields (`dir_tteplan_cp`, `dir_spec_cp`,
#' `dir_results_cp`) are stored on the plan as [CandidatePath] instances.
#' Stage scripts (`s1.R`, `s2.R`, `s3.R`, `s4_export.R`) can then re-load
#' the plan on any host with [tteplan_locate_and_load()] and call
#' `plan$save()`, `plan$s1_generate_enrollments_and_ipw()`, etc. without
#' re-specifying any paths.
#'
#' @param study A [RegistryStudy] R6 object, typically loaded via
#'   [registrystudy_load()]. Owns the rawbatch and skeleton path candidates.
#' @param candidate_dir_spec Character vector of candidate directories that
#'   contain the spec YAML `spec_vXXX.yaml`. The first existing directory is
#'   used to locate the spec.
#' @param candidate_dir_tteplan Character vector of candidate directories
#'   where `tteplan.qs2` lives (or will be created by `plan$save()`).
#' @param candidate_dir_results Character vector of candidate directories for
#'   the results BASE directory (without the version suffix -- the plan
#'   appends `spec_version` internally).
#' @param spec_version Optional character scalar like `"v003"` selecting the
#'   spec YAML. When `NULL`, read from `spec$study$implementation$version`.
#' @param project_id Optional character scalar for display/logging. When
#'   `NULL`, read from `spec$study$implementation$project_prefix`.
#' @param n_skeleton_files Optional integer: if not NULL, only the first
#'   `n_skeleton_files` files are used (for faster dev iterations). Stored
#'   on the plan as `n_skeleton_files_limit` so [tteplan_load()] can
#'   re-apply it after a host transfer.
#' @param global_max_isoyearweek Administrative censoring boundary
#'   (isoyearweek string, e.g., "2023-52"). If `NULL` (default), auto-detected
#'   from `max(isoyearweek)` in the first skeleton file. Also runs
#'   [tteplan_validate_spec()] on that skeleton.
#' @param period_width Integer, band width in weeks for enrollment and
#'   time aggregation (default: 4L). Stored on the plan and passed through
#'   to TTEDesign.
#' @return A [TTEPlan] object with the full ETT grid, embedded
#'   `registrystudy`, and CandidatePath fields populated.
#'
#' @family tte_spec
#' @seealso [registrystudy_load()], [tteplan_locate_and_load()]
#' @export
tteplan_from_spec_and_registrystudy <- function(
  study,
  candidate_dir_spec,
  candidate_dir_tteplan,
  candidate_dir_results,
  spec_version = NULL,
  project_id = NULL,
  n_skeleton_files = NULL,
  global_max_isoyearweek = NULL,
  period_width = 4L
) {
  isoyearweek <- treatment_impl <- matching_ratio <- seed <- NULL

  if (is.null(study) || is.null(study$skeleton_files)) {
    stop(
      "`study` must provide a `$skeleton_files` accessor (use registrystudy_load() to load a RegistryStudy)."
    )
  }

  # Wrap candidate-dir vectors in CandidatePath instances.
  dir_spec_cp <- CandidatePath$new(candidate_dir_spec, "dir_spec")
  dir_tteplan_cp <- CandidatePath$new(candidate_dir_tteplan, "dir_tteplan")
  dir_results_cp <- CandidatePath$new(candidate_dir_results, "dir_results")

  # Read the spec YAML from the resolved spec directory. If spec_version
  # wasn't supplied, we don't yet know which file to read -- require it.
  if (is.null(spec_version)) {
    stop(
      "`spec_version` must be supplied (e.g. \"v003\") so the spec YAML filename can be built."
    )
  }
  spec_dir <- dir_spec_cp$resolve()
  spec_path <- file.path(spec_dir, filename_spec(spec_version))
  if (!file.exists(spec_path)) {
    stop("Spec YAML not found: ", spec_path)
  }
  spec <- tteplan_read_spec(spec_path)
  yaml_version <- spec$study$implementation$version
  if (!identical(yaml_version, spec_version)) {
    stop(
      "spec_version mismatch: argument was '",
      spec_version,
      "' but the YAML at ",
      spec_path,
      " has implementation.version = '",
      yaml_version %||% "NULL",
      "'"
    )
  }

  if (is.null(project_id)) {
    project_id <- spec$study$implementation$project_prefix
  }

  # Resolve skeleton_files from RegistryStudy
  skeleton_files <- study$skeleton_files

  # Apply n_skeleton_files limit
  if (!is.null(n_skeleton_files)) {
    skeleton_files <- utils::head(skeleton_files, n_skeleton_files)
  }
  skeleton_created_at <- NULL
  # Extract the batch number from the first skeleton file so we can go
  # through the study's load_skeleton() API (which unwraps Skeleton R6,
  # falls back to legacy bare-dt files, and restores over-allocation).
  .first_batch_number <- function(path) {
    m <- regmatches(
      basename(path),
      regexec("skeleton_(\\d+)\\.qs2$", basename(path))
    )[[1]]
    if (length(m) < 2L) {
      return(NA_integer_)
    }
    as.integer(m[[2]])
  }
  .load_first_skeleton_dt <- function() {
    batch_num <- .first_batch_number(skeleton_files[1])
    if (is.na(batch_num)) {
      # Unusual filename -- fall back to a raw qs2_read + unwrap
      obj <- qs2_read(skeleton_files[1])
      if (inherits(obj, "Skeleton")) {
        return(list(data = obj$data, created_at = obj$created_at))
      }
      return(list(data = obj, created_at = attr(obj, "created_at")))
    }
    sk <- study$load_skeleton(batch_num)
    if (is.null(sk)) {
      stop("Skeleton file not found: ", skeleton_files[1], call. = FALSE)
    }
    list(data = sk$data, created_at = sk$created_at)
  }

  if (is.null(global_max_isoyearweek)) {
    first <- .load_first_skeleton_dt()
    skeleton <- first$data
    tteplan_validate_spec(spec, skeleton)
    global_max_isoyearweek <- skeleton[, max(isoyearweek, na.rm = TRUE)]
    message("Admin censoring cutoff from skeleton: ", global_max_isoyearweek)
    skeleton_created_at <- first$created_at
    rm(skeleton, first)
  } else if (file.exists(skeleton_files[1])) {
    first <- .load_first_skeleton_dt()
    skeleton_created_at <- first$created_at
    rm(first)
  }

  # Extract confounder variable names
  confounder_vars <- vapply(
    spec$confounders,
    function(c) c$implementation$variable,
    character(1)
  )

  # Extract subgroup (effect-modifier) variable names (optional)
  subgroup_vars <- if (!is.null(spec$subgroups)) {
    vapply(
      spec$subgroups,
      function(s) s$implementation$variable,
      character(1)
    )
  } else {
    NULL
  }

  plan <- TTEPlan$new(
    project_prefix = project_id,
    skeleton_files = skeleton_files,
    global_max_isoyearweek = global_max_isoyearweek
  )
  plan$period_width <- as.integer(period_width)
  plan$spec <- spec
  plan$spec_version <- spec_version

  # CandidatePath fields + embedded study
  plan$dir_spec_cp <- dir_spec_cp
  plan$dir_tteplan_cp <- dir_tteplan_cp
  plan$dir_results_cp <- dir_results_cp
  plan$registrystudy <- study
  plan$n_skeleton_files_limit <- if (is.null(n_skeleton_files)) {
    NULL
  } else {
    as.integer(n_skeleton_files)
  }

  if (!is.null(study$expected_skeleton_file_count)) {
    plan$expected_skeleton_file_count <- study$expected_skeleton_file_count
  }
  if (is.function(study$summary_table)) {
    plan$code_registry <- study$summary_table()
  }
  if (!is.null(study$n_ids)) {
    plan$expected_n_ids <- study$n_ids
  }

  # Provenance timestamps
  plan$registry_study_created_at <- study$created_at
  plan$skeleton_created_at <- skeleton_created_at

  for (enrollment in spec$enrollments) {
    # Extract age range from additional_inclusion
    age_min <- NULL
    age_max <- NULL
    age_group <- NULL
    if (!is.null(enrollment$additional_inclusion)) {
      for (ae in enrollment$additional_inclusion) {
        if (identical(ae$type, "age_range")) {
          age_min <- ae$min
          age_max <- ae$max
          age_group <- paste0(ae$min, "_", ae$max)
        }
      }
    }
    if (is.null(age_min) || is.null(age_max)) {
      stop(
        "Enrollment '",
        enrollment$id,
        "' has no age_range in additional_inclusion"
      )
    }

    for (outcome in spec$outcomes) {
      for (fu in spec$follow_up) {
        plan$add_one_ett(
          enrollment_id = enrollment$id,
          outcome_var = outcome$implementation$variable_combined,
          outcome_name = outcome$name,
          follow_up = fu$weeks,
          confounder_vars = confounder_vars,
          subgroup_vars = subgroup_vars,
          time_treatment_var = "rd_intervention",
          eligible_var = "eligible",
          observed_var = enrollment$observed_var,
          intervention_tolerance_weeks = enrollment$intervention_tolerance_weeks,
          comparator_tolerance_weeks = enrollment$comparator_tolerance_weeks,
          argset = list(
            age_group = age_group,
            age_min = age_min,
            age_max = age_max,
            outcome_description = outcome$description %||% NA_character_,
            outcome_role = outcome$role %||% NA_character_
          )
        )
      }
    }

    # Store treatment implementation in the ETT for this enrollment
    impl <- enrollment$treatment$implementation
    rows <- plan$ett$enrollment_id == enrollment$id
    plan$ett[rows, treatment_impl := list(list(impl))]
    plan$ett[rows, matching_ratio := impl$matching_ratio]
    plan$ett[rows, seed := impl$seed]
  }

  plan
}


#' Format a window spec as human-readable text
#'
#' @param impl Implementation list with `window` field.
#' @return Character string.
#' @noRd
.format_window_human <- function(impl) {
  w <- impl$window
  if (is.null(w)) {
    return("(not specified)")
  }
  if (identical(w, "lifetime_before_and_after_baseline")) {
    return("lifetime before and after baseline")
  }
  if (identical(w, "lifetime_before_baseline")) {
    return("lifetime before baseline")
  }
  if (is.numeric(w)) {
    years <- w / 52
    if (years == as.integer(years) && years >= 1) {
      return(paste0(
        as.integer(years),
        if (years == 1) " year" else " years",
        " before baseline"
      ))
    }
    return(paste0(w, " weeks before baseline"))
  }
  as.character(w)
}
