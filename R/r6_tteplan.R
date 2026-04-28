# =============================================================================
# TTEPlan R6 class and spec functions
# =============================================================================
# This file contains the planning-side R6 class and all standalone functions
# called by its methods:
#
#   1. TTEPlan R6 class
#   2. .s1_prepare_skeleton(), .s1_eligible_tuples() (shared helpers)
#   3. .s1a_worker(), .s1b_worker() (Loop 1 workers)
#   4. .s2_worker() (Loop 2 IPCW-PP worker)
#   5. S3 methods: [[.TTEPlan, length.TTEPlan
#   6. Spec functions: tteplan_read_spec, tteplan_apply_exclusions,
#      tteplan_apply_derived_confounders, tteplan_validate_spec,
#      tteplan_from_spec_and_registrystudy
# =============================================================================

.TTE_PLAN_SCHEMA_VERSION <- 2L

# On-disk filename constants. The directory is the scope; the filename is
# the role. See "stub-free filenames" in the refactor plan.
FILENAME_TTEPLAN     <- "tteplan.qs2"
FILENAME_SPEC_XLSX   <- "spec.xlsx"
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
#'   project_prefix = "project002",
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
          class(self)[1], " on disk has schema version ", saved,
          " but this swereg requires version ", current, ".\n",
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
          fmt_var(ec$implementation$source_variable_combined %||% ec$implementation$source_variable),
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
                fmt_var(ai$implementation$source_variable_combined %||% ai$implementation$source_variable),
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
              fmt_var(ae$implementation$source_variable_combined %||% ae$implementation$source_variable),
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
                ec$implementation$source_variable_combined %||% ec$implementation$source_variable,
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
            paste0(o$name, " (variable: ", o$implementation$variable, ")")
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
          "Eligibility (6a): Eligibility was assessed at the start of each sequential trial. ",
          "Individuals entered the pool of eligible person-trials if they met the inclusion criteria (calendar year range, age) and had not met any exclusion criterion ",
          "(e.g., no prior intervention within the specified washout window, no prior outcome event within the lookback window or over the lifetime, as defined in the specification). ",
          "Exclusion criteria were evaluated cumulatively, and the number of persons and person-trials remaining after each criterion was recorded for the participant flow diagram. ",
          # 7b: Treatment strategies
          "Treatment strategies (6b): Treatment status was determined from registry data at the start of each enrollment period, using the variable and values specified in the study configuration. ",
          "The enrollment period width (period_width) determines the granularity of sequential trial entry; narrower periods reduce residual immortal time bias ",
          "at the cost of fewer eligible individuals per trial (Caniglia et al., 2023). ",
          "The period width also serves as an implicit grace period: treatment status is assessed once per period, ",
          "so that initiation occurring anywhere within the period is attributed to its start. ",
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

      # Item 8: auto-populate from enrollment_counts$attrition if available
      item8_text <- NULL
      if (!is.null(self$enrollment_counts)) {
        item8_parts <- character()
        for (enr_id in names(self$enrollment_counts)) {
          ec <- self$enrollment_counts[[enr_id]]
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
            overall[, criterion := factor(criterion, levels = unique(criterion))]
            data.table::setorder(overall, criterion)

            # Compute column widths for right-justified alignment
            all_totals <- overall$n_person_trials
            all_intervention <- overall$n_intervention
            all_comparator <- overall$n_comparator
            deltas_total <- c(0, -diff(all_totals))
            deltas_intervention <- c(0, -diff(all_intervention))
            deltas_comparator <- c(0, -diff(all_comparator))

            fmt_num <- function(x, w) formatC(format(x, big.mark = ","), width = w)
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
                item8_parts <- c(item8_parts,
                  "  Before exclusions:",
                  sprintf("    \u21b3 %s person-trials",
                    cyan(fmt_num(tot, w_total)))
                )
              } else {
                d_tot <- all_totals[j - 1] - tot
                d_intervention <- all_intervention[j - 1] - n_int
                d_comparator <- all_comparator[j - 1] - n_cmp
                item8_parts <- c(item8_parts,
                  sprintf("  Applying %s:", bold(as.character(overall$criterion[j]))),
                  sprintf("    \u21b3 Excluding %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                    red(fmt_num(d_tot, w_total)),
                    red(fmt_num(d_intervention, w_intervention)),
                    red(fmt_num(d_comparator, w_comparator))),
                  sprintf("    \u21b3 Remaining %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                    cyan(fmt_num(tot, w_total)),
                    cyan(fmt_num(n_int, w_intervention)),
                    cyan(fmt_num(n_cmp, w_comparator)))
                )
              }
            }
          }
          if (!is.null(ec$matching)) {
            m <- ec$matching
            n_int <- sum(m$n_intervention_enrolled, na.rm = TRUE)
            n_cmp <- sum(m$n_comparator_enrolled, na.rm = TRUE)
            n_match_total <- n_int + n_cmp
            item8_parts <- c(item8_parts,
              "  Post-matching:",
              sprintf("    \u21b3 %s person-trials (%s intervention person-trials, %s comparator person-trials)",
                cyan(fmt_num(n_match_total, w_total)),
                cyan(fmt_num(n_int, w_intervention)),
                cyan(fmt_num(n_cmp, w_comparator)))
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
    #' @param time_treatment_var Character or NULL, time-varying treatment column.
    #' @param eligible_var Character or NULL, eligibility column.
    #' @param argset Named list with age_group, age_min, age_max (and optional
    #'   person_id_var, outcome_description).
    add_one_ett = function(
      enrollment_id,
      outcome_var,
      outcome_name,
      follow_up,
      confounder_vars,
      time_treatment_var,
      eligible_var,
      argset = list()
    ) {
      outcome_description <- argset$outcome_description %||% NA_character_
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
        }
      }

      ett_num <- if (is.null(self$ett)) 1L else nrow(self$ett) + 1L
      ett_id <- paste0("ETT", sprintf("%03d", ett_num))
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
        description = description,
        file_raw = file_raw,
        file_imp = file_imp,
        file_analysis = file_analysis,
        confounder_vars = list(confounder_vars),
        person_id_var = person_id_var,
        treatment_var = treatment_var,
        time_treatment_var = tv_intervention,
        eligible_var = elig
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
      qs2::qs_save(self, dest, nthreads = parallel::detectCores())
      invisible(dest)
    },

    #' @description Extract enrollment spec for the i-th enrollment_id group.
    #'
    #' @param i Integer index (1-based).
    #' @return A list with:
    #'   \describe{
    #'     \item{design}{A [TTEDesign] object with column mappings}
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

      result <- list(
        design = TTEDesign$new(
          person_id_var = x_person_id,
          treatment_var = first$treatment_var,
          time_treatment_var = x_time_treatment,
          eligible_var = x_eligible,
          outcome_vars = rows$outcome_var,
          confounder_vars = first$confounder_vars[[1]],
          follow_up_time = as.integer(max(rows$follow_up)),
          admin_censor_isoyearweek = self$global_max_isoyearweek,
          period_width = self$period_width
        ),
        enrollment_id = eid,
        age_range = c(first$age_min, first$age_max),
        n_threads = parallel::detectCores()
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
    #' @param impute_fn Imputation callback or NULL (default: [tteenrollment_impute_confounders]).
    #' @param stabilize Logical, stabilize IPW (default: TRUE).
    #' @param n_workers Integer, concurrent subprocesses (default: 3L).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    #' @param resume Logical. If `TRUE`, skip enrollments whose `_imp_` file
    #'   already exists in `output_dir` (default: FALSE).
    s1_generate_enrollments_and_ipw = function(
      output_dir = NULL,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = 3L,
      swereg_dev_path = NULL,
      resume = FALSE
    ) {
      if (is.null(output_dir)) {
        output_dir <- self$dir_tteplan
      }
      # --- Loop 1: Create trial panels from skeleton files ---
      #
      # Two-pass pipeline:
      #
      #   Pass 1a (scout, parallel):
      #     skeleton files -> exclusions -> treatment -> eligible tuples
      #     Returns: (person_id, trial_id, intervention) per file
      #     Caches prepared skeleton to tempdir for s1b reuse
      #
      #   Match (main process):
      #     Combine all tuples -> per trial_id, keep all intervention,
      #     sample ratio * n_intervention comparator -> enrolled_ids
      #
      #   Pass 1b (full enrollment, parallel):
      #     Load cached skeleton -> derive confounders
      #     -> enroll with pre-matched enrolled_ids (skip matching)
      #     -> TTEEnrollment (panel-expanded)
      #     Falls back to full re-read if cache miss
      #
      #   Post-processing (main):
      #     tteenrollment_rbind -> save raw -> impute -> IPW -> truncate -> save imp
      #
      # Why callr subprocesses?
      #   data.table uses OpenMP threads internally. Forking (via parallel::mclapply)
      #   after OpenMP initialization causes segfaults. callr::r_bg() launches fresh
      #   R sessions, avoiding the fork+OpenMP conflict entirely.

      if (is.null(self$ett) || nrow(self$ett) == 0) {
        stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
      }

      if (is.null(self$spec)) {
        stop(
          "plan has no spec. ",
          "Create the plan with tteplan_from_spec_and_registrystudy()."
        )
      }
      self$output_dir <- output_dir
      spec <- self$spec

      ett <- self$ett
      files <- self$skeleton_files
      n_cores <- parallel::detectCores()
      n_threads <- max(1L, floor(n_cores / n_workers))

      # Build a Loop 1 summary: one row per enrollment_id with the max
      # follow-up and output file names
      ett_loop1 <- ett[,
        .(
          max_follow_up = max(follow_up),
          age_grp = age_group[1],
          file_raw = file_raw[1],
          file_imp = file_imp[1]
        ),
        by = enrollment_id
      ]

      total_steps <- nrow(ett_loop1) * length(files) * 2L

      cat(sprintf(
        "Creating enrollment files: %d enrollment(s) x %d skeleton files\n",
        nrow(ett_loop1),
        length(files)
      ))

      p <- progressr::progressor(steps = total_steps)

      if (is.null(self$enrollment_counts)) {
        self$enrollment_counts <- list()
      }

      # Restore enrollment counts from per-enrollment sidecar files
      .restore_enrollment_counts(
        self, output_dir, unique(ett_loop1$enrollment_id)
      )

      # Determine which enrollments to skip when resuming
      resume_skip_up_to <- 0L
      if (resume) {
        imp_paths <- file.path(output_dir, ett_loop1$file_imp)
        imp_exists <- file.exists(imp_paths)
        if (any(imp_exists)) {
          imp_mtimes <- file.mtime(imp_paths[imp_exists])
          newest_mtime <- max(imp_mtimes)
          age_hours <- as.numeric(
            difftime(Sys.time(), newest_mtime, units = "hours")
          )
          if (age_hours <= 24) {
            # Find the last enrollment (by index) whose imp file is the newest
            # Enrollments are sequential, so everything up to this index is done
            for (k in rev(which(imp_exists))) {
              if (file.mtime(imp_paths[k]) == newest_mtime) {
                resume_skip_up_to <- k
                break
              }
            }
            cat(sprintf(
              "  [resume] Newest imp file is %.1fh old (enrollment %d/%d). Skipping 1-%d.\n",
              age_hours, resume_skip_up_to, nrow(ett_loop1), resume_skip_up_to
            ))
          } else {
            cat(sprintf(
              "  [resume] Newest imp file is %.0fh old -- too stale, redoing all.\n",
              age_hours
            ))
          }
        }
      }

      for (i in seq_len(nrow(ett_loop1))) {
        x_file_raw <- ett_loop1$file_raw[i]
        x_file_imp <- ett_loop1$file_imp[i]

        if (i <= resume_skip_up_to) {
          cat(sprintf(
            "  [resume] Skipping enrollment %d/%d (%s)\n",
            i, nrow(ett_loop1), ett_loop1$enrollment_id[i]
          ))
          if (!is.null(p)) {
            for (s in seq_len(2L * length(files))) p(message = "skipped")
          }
          next
        }

        enrollment_spec <- self$enrollment_spec(i)
        enrollment_spec$n_threads <- n_threads

        # ---- Cache paths for s1a -> s1b reuse ----
        cache_paths <- vapply(files, function(f) {
          file.path(tempdir(), paste0(
            "s1_enr", enrollment_spec$enrollment_id, "_", basename(f)
          ))
        }, character(1))
        on.exit(unlink(cache_paths, force = TRUE), add = TRUE)

        # ---- Pass 1a: Scout (parallel) ----
        scout_items <- lapply(seq_along(files), \(j) {
          list(
            enrollment_spec = enrollment_spec,
            file_path = files[j],
            spec = spec,
            cache_path = cache_paths[j]
          )
        })
        scout_results <- parallel_pool(
          items = scout_items,
          worker_script = "worker_s1a.R",
          n_workers = n_workers,
          swereg_dev_path = swereg_dev_path,
          p = p
        )

        # ---- Centralized matching (main process) ----
        data.table::setDTthreads(n_cores)
        all_tuples <- data.table::rbindlist(
          lapply(scout_results, `[[`, "tuples"),
          use.names = TRUE
        )
        all_attrition <- data.table::rbindlist(
          lapply(scout_results, `[[`, "attrition"),
          use.names = TRUE
        )
        rm(scout_results)

        set.seed(enrollment_spec$seed)
        x_ratio <- enrollment_spec$matching_ratio
        pid <- enrollment_spec$design$person_id_var

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

        # Store counts for TARGET reporting
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

        # Aggregate attrition across batches (skeleton batches are
        # person-disjoint, so summing per-batch uniqueN is correct)
        attrition_summary <- all_attrition[,
          .(
            n_persons = sum(n_persons),
            n_person_trials = sum(n_person_trials),
            n_intervention = sum(n_intervention),
            n_comparator = sum(n_comparator)
          ),
          by = .(trial_id, criterion)
        ]

        self$enrollment_counts[[enrollment_spec$enrollment_id]] <- list(
          attrition = attrition_summary,
          matching = matching_counts
        )
        qs2::qs_save(
          self$enrollment_counts[[enrollment_spec$enrollment_id]],
          .enrollment_counts_path(
            output_dir, self$project_prefix, enrollment_spec$enrollment_id
          )
        )
        rm(
          all_tuples,
          all_attrition,
          global_counts,
          enrolled_counts,
          matching_counts,
          attrition_summary
        )

        # ---- Pass 1b: Full enrollment with pre-matched IDs (parallel) ----
        # Save enrolled_ids ONCE to a shared tempfile (all workers read it)
        enrolled_ids_path <- tempfile(
          pattern = "enrolled_ids_", fileext = ".qs2"
        )
        qs2::qs_save(enrolled_ids, enrolled_ids_path, nthreads = n_cores)
        on.exit(unlink(enrolled_ids_path, force = TRUE), add = TRUE)
        rm(enrolled_ids)

        enroll_items <- lapply(seq_along(files), \(j) {
          list(
            enrollment_spec = enrollment_spec,
            file_path = files[j],
            spec = spec,
            enrolled_ids_path = enrolled_ids_path,
            cache_path = cache_paths[j]
          )
        })
        results <- parallel_pool(
          items = enroll_items,
          worker_script = "worker_s1b.R",
          n_workers = n_workers,
          swereg_dev_path = swereg_dev_path,
          p = p
        )

        # Combine per-file enrollments into one
        data.table::setDTthreads(n_cores)
        trial <- tteenrollment_rbind(results)

        # Save raw enrollment (before imputation/weighting)
        qs2::qs_save(
          trial,
          file.path(output_dir, x_file_raw),
          nthreads = n_cores
        )

        # Impute missing confounders (sampling from observed distribution)
        if (!is.null(impute_fn)) {
          trial <- impute_fn(trial, enrollment_spec$design$confounder_vars)
        }

        # Compute IPW and truncate extreme weights
        trial$s2_ipw(stabilize = stabilize)
        trial$s3_truncate_weights(weight_cols = "ipw")

        # Save imputed + weighted enrollment
        qs2::qs_save(
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
    #' @param resume Logical. If `TRUE`, skip ETTs whose analysis file already
    #'   exists in `output_dir` (default: FALSE).
    s2_generate_analysis_files_and_ipcw_pp = function(
      output_dir = NULL,
      estimate_ipcw_pp_separately_by_treatment = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      n_workers = 1L,
      swereg_dev_path = NULL,
      resume = FALSE
    ) {
      if (is.null(output_dir)) {
        output_dir <- self$dir_tteplan
      }
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
      }

      ett <- self$ett
      n_cores <- parallel::detectCores()
      n_threads <- max(1L, floor(n_cores / n_workers))

      sep_by_tx <- estimate_ipcw_pp_separately_by_treatment
      with_gam <- estimate_ipcw_pp_with_gam
      items <- lapply(seq_len(nrow(ett)), function(i) {
        list(
          outcome = ett$outcome_var[i],
          follow_up = ett$follow_up[i],
          file_imp_path = file.path(output_dir, ett$file_imp[i]),
          file_analysis_path = file.path(output_dir, ett$file_analysis[i]),
          n_threads = n_threads,
          sep_by_tx = sep_by_tx,
          with_gam = with_gam
        )
      })

      if (resume) {
        analysis_exists <- vapply(items, function(it) {
          file.exists(it$file_analysis_path)
        }, logical(1))
        if (any(analysis_exists)) {
          mtimes <- vapply(
            items[analysis_exists],
            function(it) file.mtime(it$file_analysis_path),
            numeric(1)
          )
          newest <- max(mtimes)
          age_hours <- as.numeric(
            difftime(Sys.time(), as.POSIXct(newest, origin = "1970-01-01"),
                     units = "hours")
          )
          if (age_hours <= 24) {
            keep <- !analysis_exists
            n_skipped <- sum(!keep)
            cat(sprintf(
              "  [resume] Skipping %d/%d ETTs -- analysis files <24h old\n",
              n_skipped, length(items)
            ))
            items <- items[keep]
          } else {
            cat(sprintf(
              "  [resume] Newest analysis file is %.0fh old -- too stale, redoing all.\n",
              age_hours
            ))
          }
        }
      }

      if (length(items) == 0L) {
        cat("  All ETTs already complete.\n")
        return(invisible(self))
      }

      cat(sprintf(
        "Loop 2: Calculating per-ETT weights - IPCW-PP (%d ETT(s), %d worker(s), %d threads each)\n",
        length(items),
        n_workers,
        n_threads
      ))

      p <- progressr::progressor(steps = length(items))
      parallel_pool(
        items = items,
        worker_script = "worker_s2.R",
        n_workers = n_workers,
        swereg_dev_path = swereg_dev_path,
        p = p,
        collect = FALSE
      )
    },

    #' @description Loop 3: Compute all analysis results and store on the plan.
    #'
    #' For each enrollment: loads one analysis file and the raw file, computes
    #' baseline characteristics (raw, unweighted, IPW, IPW truncated).
    #' For each ETT: loads the analysis file, computes rates, IRR, and
    #' heterogeneity test with both truncated and untruncated weights.
    #'
    #' Results are stored in `self$results_enrollment` and `self$results_ett`.
    #' Existing results are skipped (resume-safe). Use `plan$save()` to persist.
    #'
    #' @param enrollment_ids Character vector of enrollment IDs to analyze, or
    #'   `NULL` (default) for all.
    #' @param ett_ids Character vector of ETT IDs to analyze, or
    #'   `NULL` (default) for all.
    #' @param output_dir Optional directory override. If `NULL` (default),
    #'   uses `self$dir_tteplan` (falls back to the legacy `self$output_dir`
    #'   for plans created before the CandidatePath migration).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    #' @param force Logical (default `FALSE`). When `TRUE`, drops cached
    #'   results in the targeted scope before recomputing. Scope follows
    #'   `enrollment_ids` and `ett_ids`: if both are `NULL`, all cached
    #'   results are cleared; otherwise only the matching entries are
    #'   dropped (untargeted enrollments / ETTs stay cached). Useful when
    #'   prior results were produced under a broken environment (e.g.
    #'   missing `survey` package -> 135 silent `skipped = TRUE` entries)
    #'   and you want to recompute without manually mutating
    #'   `self$results_ett` / `self$results_enrollment`.
    #' @param n_workers Integer >= 1 (default `1L`). Number of concurrent
    #'   worker subprocesses for both the enrollment loop and the per-ETT
    #'   loop. Each worker reads its own analysis file fresh, so peak RAM
    #'   scales linearly with `n_workers`; on machines with multi-GB
    #'   analysis files, set this conservatively. CPU threads per worker
    #'   are auto-partitioned as `floor(detectCores() / n_workers)`.
    s3_analyze = function(enrollment_ids = NULL, ett_ids = NULL,
                          output_dir = NULL, swereg_dev_path = NULL,
                          force = FALSE, n_workers = 1L) {
      if (!is.numeric(n_workers) || length(n_workers) != 1L ||
          is.na(n_workers) || n_workers < 1L) {
        stop("n_workers must be a single integer >= 1")
      }
      n_workers <- as.integer(n_workers)
      if (is.null(output_dir)) {
        output_dir <- tryCatch(self$dir_tteplan, error = function(e) NULL)
      }
      if (is.null(output_dir)) {
        output_dir <- self$output_dir  # legacy fallback
      }
      if (is.null(output_dir)) {
        stop(
          "output_dir is not set. Pass it as an argument, ",
          "configure dir_tteplan_cp, or run $s1_generate_enrollments_and_ipw() first."
        )
      }
      ett <- self$ett
      n_cores <- parallel::detectCores()

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

      if (is.null(self$results_enrollment)) self$results_enrollment <- list()
      if (is.null(self$results_ett)) self$results_ett <- list()

      # --- force = TRUE: invalidate cached results in the targeted scope ---
      if (isTRUE(force)) {
        if (is.null(enrollment_ids) && is.null(ett_ids)) {
          self$results_enrollment <- list()
          self$results_ett <- list()
        } else {
          for (eid in all_enrollment_ids) {
            self$results_enrollment[[eid]] <- NULL
          }
          drop_ett_ids <- if (!is.null(ett_ids)) {
            ett_ids
          } else {
            ett$ett_id[ett$enrollment_id %in% all_enrollment_ids]
          }
          for (eid in drop_ett_ids) {
            self$results_ett[[eid]] <- NULL
          }
        }
      }

      # --- Enrollment loop: baseline characteristics (subprocess-isolated) ---
      # Filter out already-cached enrollments
      enr_todo <- character()
      for (eid in all_enrollment_ids) {
        if (is.null(self$results_enrollment[[eid]])) {
          enr_todo <- c(enr_todo, eid)
        }
      }
      n_cached_enr <- length(all_enrollment_ids) - length(enr_todo)

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
            n_threads = n_cores,
            arm_labels = .lookup_arm_labels(self$spec, eid)
          )
        })
      }

      # ETT items
      ett_subset <- ett[ett$enrollment_id %in% all_enrollment_ids]
      if (!is.null(ett_ids)) {
        ett_subset <- ett_subset[ett_subset$ett_id %in% ett_ids]
      }
      keep <- vapply(ett_subset$ett_id, function(eid) {
        is.null(self$results_ett[[eid]])
      }, logical(1))
      n_cached <- sum(!keep)
      ett_todo <- ett_subset[keep]
      n_ett <- nrow(ett_todo)

      all_items <- list()
      item_map <- list()
      if (n_ett > 0L) {
        for (i in seq_len(n_ett)) {
          apath <- file.path(output_dir, ett_todo$file_analysis[i])
          eid <- ett_todo$ett_id[i]
          base <- list(analysis_path = apath, ett_id = eid,
                       n_threads = n_cores)
          idx <- length(all_items)
          all_items[[idx + 1L]] <- c(base, list(
            method = "summary_and_rates", weight_col = ""))
          item_map[[idx + 1L]] <- list(ett_i = i, slot = "summary_and_rates")

          all_items[[idx + 2L]] <- c(base, list(
            method = "irr", weight_col = "analysis_weight_pp_trunc"))
          item_map[[idx + 2L]] <- list(ett_i = i, slot = "irr_pp_trunc")

          all_items[[idx + 3L]] <- c(base, list(
            method = "irr", weight_col = "analysis_weight_pp"))
          item_map[[idx + 3L]] <- list(ett_i = i, slot = "irr_pp")
        }
      }

      # Total steps across both loops
      total_steps <- length(enr_items) + length(all_items)
      message("Output dir: ", output_dir)
      n_files <- length(list.files(output_dir, pattern = "\\.qs2$"))
      message(sprintf("  %d .qs2 files found", n_files))
      cat(sprintf(
        "Analyzing: %d enrollment(s) + %d ETTs x 3 analysis calls%s\n",
        length(enr_items), n_ett,
        if (n_cached_enr + n_cached > 0L) {
          sprintf(" (%d cached)", n_cached_enr + n_cached)
        } else {
          ""
        }
      ))

      p <- progressr::progressor(steps = total_steps)

      # --- Enrollment loop ---
      if (length(enr_items) > 0L) {
        enr_results <- parallel_pool(
          items = enr_items,
          worker_script = "worker_s3_enrollment.R",
          n_workers = n_workers,
          swereg_dev_path = swereg_dev_path,
          p = p
        )

        for (i in seq_along(enr_todo)) {
          self$results_enrollment[[enr_todo[i]]] <- enr_results[[i]]
        }
        rm(enr_results)
      }

      # --- ETT loop ---
      if (length(all_items) > 0L) {
        all_results <- parallel_pool(
          items = all_items,
          worker_script = "worker_s3.R",
          n_workers = n_workers,
          swereg_dev_path = swereg_dev_path,
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

    #' @description Export the study specification to a standalone Excel file.
    #'
    #' Writes a formatted summary of the spec (design, criteria, confounders,
    #' outcomes, enrollments) with ICD-10/ATC code annotations from the code
    #' registry. No analysis results required.
    #'
    #' @param path Optional output path override. If `NULL` (default), writes
    #'   to `self$spec_xlsx` (that is, `spec.xlsx` inside `self$dir_results`).
    #' @return `invisible(self)`
    excel_spec_summary = function(path = NULL) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required. Install with: install.packages('openxlsx')")
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
          "Spec reloaded: ", n_cosm,
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
    #' @param output_dir Optional directory holding the `.qs2` files. Defaults
    #'   to `self$output_dir`.
    #' @param enrollment_ids Optional character vector. If NULL, refreshes
    #'   every enrollment in `self$results_enrollment`.
    #' @return `invisible(self)`.
    recompute_baselines = function(output_dir = NULL, enrollment_ids = NULL) {
      if (is.null(output_dir)) output_dir <- self$output_dir
      if (is.null(output_dir)) {
        stop("output_dir is not set. Pass it as an argument.")
      }
      if (is.null(self$results_enrollment) ||
          length(self$results_enrollment) == 0L) {
        stop("No enrollment results to refresh.")
      }
      if (is.null(enrollment_ids)) {
        enrollment_ids <- names(self$results_enrollment)
      }
      ett <- self$ett
      for (eid in enrollment_ids) {
        enr_rows <- ett[ett$enrollment_id == eid]
        if (nrow(enr_rows) == 0L) next
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
    #' @param path File path for the output `.xlsx` file.
    #' @param table1_enrollment Enrollment ID for Table 1 (main baseline table).
    #'   Default: the enrollment with the most baseline observations.
    #' @param featured_etts Optional, either a flat character vector of ETT
    #'   ids to feature in the Forest plot, or a **named list** of such
    #'   vectors. When supplied as a named list, each name becomes a bold
    #'   group header in the Forest plot (e.g. one group per treatment
    #'   contrast). Order follows the list (and the vectors inside). When
    #'   `NULL` (default), all ETTs are shown in the Forest plot with no
    #'   grouping.
    #' @param output_dir Optional directory holding the cached `.qs2` files.
    #'   Used by the lazy `recompute_baselines()` refresh. Defaults to
    #'   `self$output_dir`.
    #' @param forest_label_format Optional character(1) format string for
    #'   the Forest plot row description. Supports `{placeholder}` tokens:
    #'   `{outcome_name}`, `{outcome_description}`, `{enrollment_name}`,
    #'   `{enrollment_id}`, `{intervention_name}`, `{comparator_name}`,
    #'   `{follow_up}`, `{ett_id}`. When `NULL` (default), uses
    #'   `"{outcome_name} ({follow_up}w)"` for grouped featured ETTs and
    #'   `"{enrollment_name} - {outcome_name} ({follow_up}w)"` otherwise.
    #' @param forest_desc_header Optional character(1) header label for
    #'   the description column of the Forest plot left text panel.
    #'   Defaults to `"ETT"`.
    export_tables = function(path = NULL, table1_enrollment = NULL,
                             featured_etts = NULL, output_dir = NULL,
                             forest_label_format = NULL,
                             forest_desc_header = NULL) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required. Install with: install.packages('openxlsx')")
      }
      if (is.null(self$results_enrollment) || length(self$results_enrollment) == 0L) {
        stop("No enrollment results. Run $s3_analyze() first.")
      }
      if (is.null(self$results_ett) || length(self$results_ett) == 0L) {
        stop("No ETT results. Run $s3_analyze() first.")
      }
      if (is.null(path)) {
        path <- self$tables_xlsx
        dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      }

      # Lazy refresh of stale baseline results (pre-swereg_table1 cache)
      stale <- vapply(self$results_enrollment, function(r) {
        if (is.null(r)) return(FALSE)
        any_panel <- r$table1_ipw_trunc %||% r$table1_unweighted %||%
          r$table1_ipw %||% r$table1_raw
        !is.null(any_panel) && !inherits(any_panel, "swereg_table1")
      }, logical(1))
      if (any(stale)) {
        message(
          "Refreshing ", sum(stale), " stale baseline table(s) from disk..."
        )
        self$recompute_baselines(
          output_dir = output_dir,
          enrollment_ids = names(stale)[stale]
        )
      }

      ett <- self$ett
      enrollment_ids <- unique(ett$enrollment_id)

      # Normalise featured_etts to a flat character vector + a parallel
      # vector of group labels (same length as featured_flat; NA when no
      # grouping). Accepts either a character vector or a named list of
      # character vectors.
      featured_flat <- NULL
      featured_groups <- NULL
      if (!is.null(featured_etts)) {
        if (is.list(featured_etts)) {
          if (is.null(names(featured_etts)) ||
              any(!nzchar(names(featured_etts)))) {
            stop(
              "featured_etts is a list but has missing or blank group names"
            )
          }
          featured_flat <- unlist(featured_etts, use.names = FALSE)
          featured_groups <- rep(
            names(featured_etts),
            times = lengths(featured_etts)
          )
        } else {
          featured_flat <- as.character(featured_etts)
          featured_groups <- rep(NA_character_, length(featured_flat))
        }
        bad <- setdiff(featured_flat, ett$ett_id)
        if (length(bad) > 0L) {
          warning(
            "featured_etts contains unknown ETT ids (ignored): ",
            paste(bad, collapse = ", ")
          )
          keep_mask <- featured_flat %in% ett$ett_id
          featured_flat <- featured_flat[keep_mask]
          featured_groups <- featured_groups[keep_mask]
        }
        if (length(featured_flat) == 0L) {
          featured_flat <- NULL
          featured_groups <- NULL
        }
      }

      # Determine table1 enrollment
      if (is.null(table1_enrollment)) {
        n_baselines <- vapply(self$results_enrollment, function(r) {
          r$n_baseline %||% 0L
        }, numeric(1))
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

      # --- Enrollments overview sheet ---
      .write_enrollment_overview(wb, self)
      toc_names <- c(toc_names, "Enrollments")
      toc_desc <- c(toc_desc, "Enrollment overview (treatment, matching, criteria)")

      # --- ETTs overview sheet ---
      .write_ett_overview(wb, self)
      toc_names <- c(toc_names, "ETTs")
      toc_desc <- c(toc_desc, "ETT overview (outcome, follow-up, events)")

      # --- Table 1: Baseline for chosen enrollment ---
      t1_label <- .enrollment_label(self, table1_enrollment)
      t1_data <- self$results_enrollment[[table1_enrollment]]
      t1_main <- t1_data$table1_ipw_trunc_main %||% t1_data$table1_ipw_trunc
      if (!is.null(t1_main)) {
        .write_tableone_sheet(
          wb, "Table 1", t1_main,
          title = paste0(
            "Table 1: Baseline characteristics (IPW-weighted, truncated) -- Enrollment ",
            table1_enrollment, " (", t1_label, ")"
          )
        )
        toc_names <- c(toc_names, "Table 1")
        toc_desc <- c(toc_desc, paste0(
          "Baseline characteristics (IPW truncated) -- ", t1_label))
      }

      featured_label <- if (!is.null(featured_flat)) {
        " (featured ETTs)"
      } else {
        ""
      }

      # Resolve the directory for image sidecars (next to the workbook)
      img_dir <- dirname(path)
      img_basename_root <- tools::file_path_sans_ext(basename(path))

      # --- Forest plot sheet (main visualisation of featured ETTs) ---
      forest_basename <- paste0(img_basename_root, "_forest_plot")
      .write_forest_irr(
        wb, "Forest plot", self,
        rates_slot = "rates_pp_trunc",
        irr_slot = "irr_pp_trunc",
        title = paste0(
          "Forest plot: Events, person-years, rates, and IRRs",
          " (per-protocol, truncated weights)", featured_label
        ),
        keep_ett_ids = featured_flat,
        group_labels = featured_groups,
        label_format = forest_label_format,
        desc_header = forest_desc_header,
        img_dir = img_dir,
        img_basename = forest_basename
      )
      toc_names <- c(toc_names, "Forest plot")
      toc_desc <- c(toc_desc, paste0(
        "Forest plot (events, person-years, rates, IRRs)", featured_label))

      # --- Table S1-SN: Combined baselines per enrollment ---
      for (j in seq_along(enrollment_ids)) {
        eid <- enrollment_ids[j]
        sheet_name <- paste0("Table S", j)
        .write_combined_baseline(wb, sheet_name, self, eid)
        toc_names <- c(toc_names, sheet_name)
        label <- .enrollment_label(self, eid)
        toc_desc <- c(toc_desc, paste0(
          "Enrollment ", eid, " (", label,
          ") -- combined baselines (Unimputed/Imputed/IPW/IPW trunc)"))
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
      consort_files <- character()
      if (!is.null(self$enrollment_counts)) {
        for (eid in enrollment_ids) {
          ec <- self$enrollment_counts[[eid]]
          if (!is.null(ec$attrition)) {
            attrition_sheet <- paste0("Attrition_", eid)
            .write_attrition_sheet(wb, attrition_sheet, self, eid)
            toc_names <- c(toc_names, attrition_sheet)
            label <- .enrollment_label(self, eid)
            toc_desc <- c(toc_desc, paste0(
              "Enrollment ", eid, " (", label,
              ") -- CONSORT attrition (numbers behind the diagram)"))

            consort_basename <- paste0(img_basename_root, "_consort_", eid)
            paths <- .render_consort_sidecars(
              plan = self, ec = ec, eid = eid, label = label,
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
        toc_desc <- c(toc_desc, paste0(
          length(consort_files),
          " PNG + matching PDF next to the workbook: ",
          paste(consort_files, collapse = ", ")
        ))
      }

      # --- Supplementary sensitivity sheet: truncated vs untruncated ---
      n_s <- length(enrollment_ids)
      s_idx <- n_s + 1L
      s_sens_name <- paste0("Table S", s_idx)
      .write_combined_sensitivity(
        wb, s_sens_name, self,
        trunc_rates_slot = "rates_pp_trunc",
        trunc_irr_slot   = "irr_pp_trunc",
        untrunc_rates_slot = "rates_pp",
        untrunc_irr_slot   = "irr_pp",
        title = paste0(
          s_sens_name,
          ": Merged results - truncated (left) vs untruncated (right) weights"
        )
      )
      toc_names <- c(toc_names, s_sens_name)
      toc_desc <- c(toc_desc,
                    "Merged results (truncated left / untruncated right)")

      # Write table of contents to Provenance sheet (right side)
      toc <- data.table::data.table(
        Sheet = seq_along(toc_names),
        Name = toc_names,
        Description = toc_desc
      )
      openxlsx::writeData(wb, "Provenance", toc, startCol = 4L, startRow = 1L,
        headerStyle = openxlsx::createStyle(textDecoration = "bold"))
      openxlsx::setColWidths(wb, "Provenance", cols = 4:6, widths = c(8, 25, 60))

      openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
      cat("Saved:", path, "\n")
      invisible(path)
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
        stop("TTEPlan has no dir_tteplan_cp -- was it created with the new tteplan_from_spec_and_registrystudy() signature?")
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

    #' @field spec_xlsx (read-only) Full path to `spec.xlsx` inside
    #'   `self$dir_results`.
    spec_xlsx = function() {
      file.path(self$dir_results, FILENAME_SPEC_XLSX)
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
    "spec", "enrollment_counts", "period_width",
    "expected_skeleton_file_count", "code_registry", "expected_n_ids",
    "created_at", "registry_study_created_at", "skeleton_created_at",
    "output_dir", "results_enrollment", "results_ett",
    "spec_reloaded_at", "spec_reload_skipped_diffs",
    # New fields added by the CandidatePath migration
    "spec_version", "dir_tteplan_cp", "dir_spec_cp", "dir_results_cp",
    "registrystudy", "n_skeleton_files_limit"
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
  if (is.null(saved_schema)) saved_schema <- 0L
  assign(".schema_version", saved_schema, envir = plan$.__enclos_env__$private)
  plan$check_version()  # errors if saved_schema is too old

  # Refresh skeleton_files from the embedded registrystudy so file paths are
  # valid on the current host. Falls back to the serialized list for plans
  # without an embedded study (legacy; blocked by check_version() above).
  if (!is.null(plan$registrystudy) && inherits(plan$registrystudy, "RegistryStudy")) {
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
    if (is.null(plan$enrollment_counts)) plan$enrollment_counts <- list()
    .restore_enrollment_counts(
      plan, plan$output_dir, unique(plan$ett$enrollment_id)
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

#' Locate and load a RegistryStudy from candidate rawbatch directories
#'
#' Walks `candidate_dir_rawbatch` to find the first directory that exists on
#' the current host, then reads `registrystudy.qs2` from inside it. Used in
#' `s0_init.R` to pass a pre-loaded `study` object to
#' [tteplan_from_spec_and_registrystudy()].
#'
#' @param candidate_dir_rawbatch Character vector of candidate rawbatch
#'   directories.
#' @return A [RegistryStudy] R6 object.
#' @seealso [first_existing_path()],
#'   [tteplan_from_spec_and_registrystudy()]
#' @family tte_plan
#' @export
registrystudy_load <- function(candidate_dir_rawbatch) {
  dir <- first_existing_path(candidate_dir_rawbatch, "dir_rawbatch")
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
      Item = item, Value = as.character(value)
    )
  }

  add("Exported at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  add("Project", plan$project_prefix)
  if (!is.null(spec)) {
    add("Study title", spec$study$title)
    add("Principal investigator", spec$study$principal_investigator)
    if (!is.null(impl$version)) add("Spec version", impl$version)
    if (!is.null(impl$date)) add("Spec date", impl$date)
    if (!is.null(impl$status)) add("Spec status", impl$status)
  }
  add("", "")
  add("RegistryStudy created", format(
    plan$registry_study_created_at %||% NA, "%Y-%m-%d %H:%M:%S"
  ))
  add("Skeletons created", format(
    plan$skeleton_created_at %||% NA, "%Y-%m-%d %H:%M:%S"
  ))
  add("TTEPlan created", format(plan$created_at %||% NA, "%Y-%m-%d %H:%M:%S"))
  add("", "")
  add("Skeleton files", as.character(length(plan$skeleton_files)))
  n_exp <- plan$expected_skeleton_file_count
  if (!is.null(n_exp)) add("Expected skeleton files", as.character(n_exp))
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
    add("Spec reloaded at", format(plan$spec_reloaded_at,
                                   "%Y-%m-%d %H:%M:%S"))
    if (length(plan$spec_reload_skipped_diffs) > 0L) {
      add("Spec reload - skipped (structural)",
          paste(plan$spec_reload_skipped_diffs, collapse = "; "))
    }
  }

  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, "Provenance", dt, headerStyle = openxlsx::createStyle(
    textDecoration = "bold"
  ))
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
    if (is.null(code_lookup)) return(NULL)
    parts <- strsplit(var, "__", fixed = TRUE)[[1]]
    if (length(parts) <= 1L) return(NULL)
    infos <- vapply(parts, function(p) {
      code_lookup[[p]] %||% p
    }, character(1))
    paste(infos, collapse = " + ")
  }

  if (colorize) {
    cyan <- function(x) paste0("\033[36m", x, "\033[0m")
    magenta <- function(x) paste0("\033[95m", x, "\033[0m")
    green <- function(x) paste0("\033[92m", x, "\033[0m")
    fmt_one <- function(v) {
      if (is.null(code_lookup)) return(v)
      info <- code_lookup[[v]]
      if (is.null(info)) info <- .resolve_combined(v)
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
      if (is.null(code_lookup)) return(v)
      info <- code_lookup[[v]]
      if (is.null(info)) info <- .resolve_combined(v)
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
    if (is.null(code_lookup)) return(NULL)
    parts <- strsplit(var, "__", fixed = TRUE)[[1]]
    if (length(parts) <= 1L) return(NULL)
    infos <- vapply(parts, function(p) {
      code_lookup[[p]] %||% p
    }, character(1))
    paste(infos, collapse = " + ")
  }
  resolve_one <- function(v) {
    if (is.null(code_lookup)) return(list(var = v, codes = NA_character_))
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
    textDecoration = "bold", indent = 1, fontColour = "#006400"
  )
  st_incl_sub_item <- openxlsx::createStyle(
    textDecoration = "bold", indent = 3, fontColour = "#006400"
  )
  st_incl_label <- openxlsx::createStyle(indent = 3, fontColour = "#006400")
  st_incl_sub_label <- openxlsx::createStyle(indent = 5, fontColour = "#006400")
  st_excl_item <- openxlsx::createStyle(
    textDecoration = "bold", indent = 1, fontColour = "#8B0000"
  )
  st_excl_sub_item <- openxlsx::createStyle(
    textDecoration = "bold", indent = 3, fontColour = "#8B0000"
  )
  st_excl_label <- openxlsx::createStyle(indent = 3, fontColour = "#8B0000")
  st_excl_sub_label <- openxlsx::createStyle(indent = 5, fontColour = "#8B0000")

  # -- accumulator (2 columns: a=label, b=value) ----------------------------
  rows <- list()
  r <- 0L

  # tint: NULL (default), "incl" (green), "excl" (red)
  pick_sa <- function(sub, tint) {
    if (identical(tint, "incl")) {
      if (sub) st_incl_sub_label else st_incl_label
    } else if (identical(tint, "excl")) {
      if (sub) st_excl_sub_label else st_excl_label
    } else {
      if (sub) st_sub_label else st_label
    }
  }

  add_header <- function(text) {
    r <<- r + 1L
    rows[[r]] <<- list(a = text, b = NA_character_,
                       sa = st_header, sb = NULL)
  }
  add_item <- function(text, tint = NULL) {
    sa <- if (identical(tint, "incl")) st_incl_item
          else if (identical(tint, "excl")) st_excl_item
          else st_item
    r <<- r + 1L
    rows[[r]] <<- list(a = text, b = NA_character_, sa = sa, sb = NULL)
  }
  add_sub_item <- function(text, tint = NULL) {
    sa <- if (identical(tint, "incl")) st_incl_sub_item
          else if (identical(tint, "excl")) st_excl_sub_item
          else st_sub_item
    r <<- r + 1L
    rows[[r]] <<- list(a = text, b = NA_character_, sa = sa, sb = NULL)
  }
  add_blank <- function() {
    r <<- r + 1L
    rows[[r]] <<- list(a = "", b = NA_character_, sa = NULL, sb = NULL)
  }
  add_kv <- function(label, value, sub = FALSE, tint = NULL) {
    r <<- r + 1L
    rows[[r]] <<- list(a = label, b = value,
                       sa = pick_sa(sub, tint), sb = NULL)
  }
  add_yellow <- function(label, value, sub = FALSE, tint = NULL) {
    r <<- r + 1L
    rows[[r]] <<- list(a = label, b = value,
                       sa = pick_sa(sub, tint), sb = st_yellow)
  }
  add_var <- function(label, var, sub = FALSE, tint = NULL) {
    # First row gets the label
    p1 <- resolve_one(var[1])
    has_codes <- !is.na(p1$codes)
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label, b = p1$var,
      sa = pick_sa(sub, tint),
      sb = if (has_codes) st_cyan else st_green
    )
    if (has_codes) {
      r <<- r + 1L
      rows[[r]] <<- list(
        a = NA_character_,
        b = paste0("\u21b3 ", p1$codes),
        sa = NULL, sb = st_codes
      )
    }
    # Remaining vars on their own rows
    if (length(var) > 1L) {
      for (v in var[-1L]) {
        pv <- resolve_one(v)
        hc <- !is.na(pv$codes)
        r <<- r + 1L
        rows[[r]] <<- list(
          a = NA_character_, b = pv$var,
          sa = NULL,
          sb = if (hc) st_cyan else st_green
        )
        if (hc) {
          r <<- r + 1L
          rows[[r]] <<- list(
            a = NA_character_,
            b = paste0("\u21b3 ", pv$codes),
            sa = NULL, sb = st_codes
          )
        }
      }
    }
  }
  add_derived_var <- function(label, derived, source_var, sub = FALSE,
                              tint = NULL) {
    # First source var with "derived <- var" on the label row
    p1 <- resolve_one(source_var[1])
    has_codes <- !is.na(p1$codes)
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label, b = paste0(derived, " <- ", p1$var),
      sa = pick_sa(sub, tint),
      sb = if (has_codes) st_cyan else st_green
    )
    if (has_codes) {
      r <<- r + 1L
      rows[[r]] <<- list(
        a = NA_character_,
        b = paste0("\u21b3 ", p1$codes),
        sa = NULL, sb = st_codes
      )
    }
    # Remaining source vars on their own rows
    if (length(source_var) > 1L) {
      for (v in source_var[-1L]) {
        pv <- resolve_one(v)
        hc <- !is.na(pv$codes)
        r <<- r + 1L
        rows[[r]] <<- list(
          a = NA_character_, b = pv$var,
          sa = NULL,
          sb = if (hc) st_cyan else st_green
        )
        if (hc) {
          r <<- r + 1L
          rows[[r]] <<- list(
            a = NA_character_,
            b = paste0("\u21b3 ", pv$codes),
            sa = NULL, sb = st_codes
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
  add_row("Code annotation", paste0("\u21b3 F64 (swereg::add_diagnoses)"),
          NULL, st_codes)
  add_row("Variable name (unresolved)", "e.g. rd_age_continuous",
          NULL, st_green)
  add_row("Categories / arm values", "e.g. systemic_mht",
          NULL, st_yellow)
  add_row("Inclusion criterion", NA_character_, st_incl_item, NULL)
  add_row("Exclusion criterion", NA_character_, st_excl_item, NULL)
  add_blank()

  # -- Study ----------------------------------------------------------------
  add_header("Study")
  add_kv("Title:", spec$study$title)
  add_kv("PI:", spec$study$principal_investigator)
  if (!is.null(spec$study$design)) add_kv("Design:", spec$study$design)
  impl <- spec$study$implementation
  if (!is.null(impl$version)) add_kv("Version:", impl$version)
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
    add_var("Variable:",
            ec$implementation$source_variable_combined %||%
              ec$implementation$source_variable, tint = "excl")
    add_kv("Window:", .format_window_human(ec$implementation), tint = "excl")
  }
  add_blank()

  # -- Confounders ----------------------------------------------------------
  add_header("Confounders")
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
    add_yellow("Intervention:",
               paste0(tx$arms$intervention, " <- ",
                      tx$implementation$intervention_value), sub = TRUE)
    add_yellow("Comparator:",
               paste0(tx$arms$comparator, " <- ",
                      tx$implementation$comparator_value), sub = TRUE)
    add_kv("Matching ratio:", paste0("1:", tx$implementation$matching_ratio),
           sub = TRUE)

    # Additional inclusion
    if (!is.null(enr$additional_inclusion)) {
      add_sub_item("Additional inclusion:", tint = "incl")
      for (ai in enr$additional_inclusion) {
        if (identical(ai$type, "age_range")) {
          add_kv("Age range:", paste0(ai$min, " - ", ai$max),
                 sub = TRUE, tint = "incl")
        } else if (identical(ai$type, "has_event")) {
          add_sub_item(ai$name, tint = "incl")
          add_var("Variable:",
                  ai$implementation$source_variable_combined %||%
                    ai$implementation$source_variable,
                  sub = TRUE, tint = "incl")
          add_kv("Window:",
                 .format_window_human(ai$implementation),
                 sub = TRUE, tint = "incl")
        } else {
          add_sub_item(ai$name, tint = "incl")
        }
      }
    }

    # Additional exclusion
    if (!is.null(enr$additional_exclusion)) {
      add_sub_item("Additional exclusion:", tint = "excl")
      for (ae in enr$additional_exclusion) {
        add_sub_item(ae$name, tint = "excl")
        add_var("Variable:",
                ae$implementation$source_variable_combined %||%
                  ae$implementation$source_variable,
                sub = TRUE, tint = "excl")
        add_kv("Window:",
               .format_window_human(ae$implementation),
               sub = TRUE, tint = "excl")
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
    if (!is.null(rw$sa)) openxlsx::addStyle(wb, sht, rw$sa, rows = i, cols = 1L)
    if (!is.null(rw$sb)) openxlsx::addStyle(wb, sht, rw$sb, rows = i, cols = 2L)
  }
  openxlsx::setColWidths(wb, sht, cols = 1:2, widths = c(35, 70))
}

#' @noRd
.enrollment_label <- function(plan, eid) {
  if (is.null(plan$spec)) return(eid)
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
  if (is.null(spec) || is.null(spec$enrollments)) return(NULL)
  for (enr in spec$enrollments) {
    if (isTRUE(enr$id == enrollment_id)) {
      arms <- enr$treatment$arms
      if (is.null(arms)) return(NULL)
      intervention <- arms$intervention
      comparator <- arms$comparator
      if (is.null(intervention) || is.null(comparator)) return(NULL)
      return(c(comparator   = as.character(comparator),
               intervention = as.character(intervention)))
    }
  }
  NULL
}

#' (removed) -- main Table 1 is now stored separately by the enrollment
#' worker as `table1_ipw_trunc_main`, so no on-the-fly stripping is needed.

#' Write a swereg_table1 data.table to a worksheet with bold header styling
#' and a fitted Variable column.
#' @noRd
.write_tableone_sheet <- function(wb, sheet_name, t1_dt, title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  start_row <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = 1L, cols = 1L
    )
    start_row <- 3L
  }
  if (is.null(t1_dt) || nrow(t1_dt) == 0L) {
    openxlsx::writeData(wb, sheet_name, "(no data)", startRow = start_row)
    return(invisible(NULL))
  }
  openxlsx::writeData(
    wb, sheet_name, t1_dt, startRow = start_row,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
    )
  )
  ncols <- ncol(t1_dt)
  widths <- c(50, 16, rep(22, max(0, ncols - 2L)))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncols), widths = widths)
}

#' @noRd
.write_enrollment_overview <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "Enrollments")
  enrollment_ids <- unique(plan$ett$enrollment_id)
  rows <- lapply(enrollment_ids, function(eid) {
    label <- .enrollment_label(plan, eid)
    n_base <- if (!is.null(plan$results_enrollment[[eid]])) {
      plan$results_enrollment[[eid]]$n_baseline
    } else {
      NA_integer_
    }
    # Treatment info from spec
    tx_info <- list(variable = NA, intervention = NA, comparator = NA, ratio = NA)
    row <- plan$ett[plan$ett$enrollment_id == eid][1]
    if ("treatment_impl" %in% names(plan$ett) && !is.null(row$treatment_impl[[1]])) {
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
  rows <- lapply(seq_len(nrow(plan$ett)), function(i) {
    r <- plan$ett[i]
    ett_id <- r$ett_id
    res <- plan$results_ett[[ett_id]]
    data.table::data.table(
      ett_id = ett_id,
      enrollment_id = r$enrollment_id,
      outcome_var = r$outcome_var,
      outcome_name = r$outcome_name,
      follow_up = r$follow_up,
      description = r$description,
      n_events = if (!is.null(res$summary)) res$summary$n_events else NA
    )
  })
  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, "ETTs", dt)
}

#' @noRd
.prepare_combine_data <- function(plan, slot, keep_ett_ids = NULL) {
  results <- plan$results_ett
  if (!is.null(keep_ett_ids)) {
    results <- results[names(results) %in% keep_ett_ids]
  }
  results_list <- lapply(results, function(r) {
    val <- r[[slot]]
    if (is.null(val) || isTRUE(val$skipped)) return(NULL)
    list(x = val)
  })
  results_list <- Filter(Negate(is.null), results_list)
  if (length(results_list) == 0L) return(NULL)

  combine_input <- lapply(results_list, `[[`, "x")
  names(combine_input) <- names(results_list)

  wrapped <- lapply(names(combine_input), function(n) {
    lst <- list()
    lst[[slot]] <- combine_input[[n]]
    lst
  })
  names(wrapped) <- names(combine_input)

  all_desc <- setNames(
    vapply(plan$results_ett, `[[`, character(1), "description"),
    names(plan$results_ett)
  )
  ett_desc <- all_desc[names(wrapped)]

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
  if (nrow(ett) == 0L) return(NULL)
  enrollment_ids <- unique(ett$enrollment_id)
  rows <- lapply(enrollment_ids, function(eid) {
    enr <- NULL
    if (!is.null(plan$spec) && !is.null(plan$spec$enrollments)) {
      for (e in plan$spec$enrollments) {
        if (isTRUE(e$id == eid)) { enr <- e; break }
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
  if (is.null(legend) || nrow(legend) == 0L) return(NULL)
  int <- unique(stats::na.omit(legend$intervention))
  cmp <- unique(stats::na.omit(legend$comparator))
  if (length(int) != 1L || length(cmp) != 1L) return(NULL)
  c(intervention = int, comparator = cmp)
}

#' Rename `*_Intervention` / `*_Comparator` column suffixes on a combined
#' rates data.table to use spec-derived arm labels. No-op when labels can't
#' be resolved.
#' @noRd
.rename_treatment_columns <- function(dt, legend) {
  arms <- .unique_arm_labels(legend)
  if (is.null(arms)) return(dt)
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
  if (is.null(legend) || nrow(legend) == 0L) return(start_row)
  openxlsx::writeData(
    wb, sheet_name, "Treatment definitions",
    startRow = start_row, startCol = 1L
  )
  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold"),
    rows = start_row, cols = 1L
  )
  start_row <- start_row + 1L
  openxlsx::writeData(
    wb, sheet_name, legend, startRow = start_row,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
    )
  )
  start_row + nrow(legend) + 2L
}

#' @noRd
.write_combined_rates <- function(wb, sheet_name, plan, slot, title = NULL,
                                  keep_ett_ids = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr, cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  prep <- .prepare_combine_data(plan, slot, keep_ett_ids = keep_ett_ids)
  if (is.null(prep)) {
    openxlsx::writeData(wb, sheet_name, "No valid rates results.",
                        startRow = row_ptr)
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_rates_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  dt <- .rename_treatment_columns(dt, legend)
  openxlsx::writeData(
    wb, sheet_name, dt, startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
    )
  )
}

#' Merge rates and IRR results for the same set of ETTs into one sheet.
#'
#' Uses [tteenrollment_combined_combine()] under the hood, then applies
#' `.rename_treatment_columns()` so the `_Intervention`/`_Comparator` suffixes
#' pick up spec-derived arm labels when the featured ETTs share one enrollment.
#' @noRd
.write_combined_rates_irr <- function(wb, sheet_name, plan,
                                      rates_slot, irr_slot,
                                      title = NULL, keep_ett_ids = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr, cols = 1L
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
  keep_ids <- Filter(function(eid) {
    r <- results[[eid]]
    if (is.null(r)) return(FALSE)
    rv <- r[[rates_slot]]
    iv <- r[[irr_slot]]
    !is.null(rv) && !isTRUE(rv$skipped) &&
      !is.null(iv) && !isTRUE(iv$skipped)
  }, names(results))
  if (length(keep_ids) == 0L) {
    openxlsx::writeData(
      wb, sheet_name, "No valid combined results.",
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

  ett_desc <- setNames(
    vapply(results, `[[`, character(1), "description"),
    names(results)
  )

  dt <- tryCatch(
    tteenrollment_combined_combine(
      results, rates_slot, irr_slot, ett_desc
    ),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  dt <- .rename_treatment_columns(dt, legend)
  openxlsx::writeData(
    wb, sheet_name, dt, startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
    )
  )
}

#' Pull a one-row measurement block (events/PY/rate per arm + IRR + CI +
#' p-value) for a single ETT and a single (rates_slot, irr_slot) pair.
#' Returns NULL when any component is missing. Column names use generic
#' suffixes (`events_intervention`, `rate_cmp`, etc.) since the arm identities are
#' carried in the separate id columns of the sensitivity sheet.
#'
#' @noRd
.sensitivity_row_measurements <- function(r, rates_slot, irr_slot) {
  rv <- r[[rates_slot]]
  iv <- r[[irr_slot]]
  if (is.null(rv) || isTRUE(rv$skipped)) return(NULL)
  if (is.null(iv) || isTRUE(iv$skipped)) return(NULL)
  if (!all(c("events_weighted", "py_weighted", "rate_per_100000py") %in%
           names(rv))) return(NULL)
  if (!all(c("IRR", "IRR_lower", "IRR_upper") %in% names(iv))) return(NULL)

  treatment_var <- attr(rv, "treatment_var")
  if (is.null(treatment_var) || !treatment_var %in% names(rv)) return(NULL)
  row_intervention <- rv[get(treatment_var) == TRUE]
  row_cmp <- rv[get(treatment_var) == FALSE]
  if (nrow(row_intervention) != 1L || nrow(row_cmp) != 1L) return(NULL)

  list(
    events_intervention = row_intervention$events_weighted,
    py_intervention     = row_intervention$py_weighted,
    rate_intervention   = row_intervention$rate_per_100000py,
    events_cmp = row_cmp$events_weighted,
    py_cmp     = row_cmp$py_weighted,
    rate_cmp   = row_cmp$rate_per_100000py,
    irr        = iv$IRR,
    lo         = iv$IRR_lower,
    hi         = iv$IRR_upper,
    pvalue     = iv$IRR_pvalue
  )
}


#' Format a single measurement block for one row of the sensitivity sheet.
#' Returns a named list of character cells keyed by internal disambiguating
#' column names (`col_key_prefix` prepended to the 9 fixed column names).
#' Display headers are written separately by the sheet writer, so the
#' prefix never appears in the worksheet.
#' @noRd
.sensitivity_row_fmt <- function(m, col_key_prefix) {
  display_names <- c(
    "Events (int)", "PY (int)", "Rate/100k (int)",
    "Events (cmp)", "PY (cmp)", "Rate/100k (cmp)",
    "IRR", "95% CI", "p-value"
  )
  if (is.null(m)) {
    cells <- rep("-", length(display_names))
  } else {
    ci <- if (is.finite(m$lo) && is.finite(m$hi)) {
      sprintf("%.2f to %.2f", m$lo, m$hi)
    } else {
      "-"
    }
    cells <- c(
      formatC(m$events_intervention, format = "f", digits = 1, big.mark = ","),
      formatC(m$py_intervention,     format = "d",             big.mark = ","),
      formatC(m$rate_intervention,   format = "f", digits = 1, big.mark = ","),
      formatC(m$events_cmp, format = "f", digits = 1, big.mark = ","),
      formatC(m$py_cmp,     format = "d",             big.mark = ","),
      formatC(m$rate_cmp,   format = "f", digits = 1, big.mark = ","),
      sprintf("%.2f", m$irr),
      ci,
      format.pval(m$pvalue, digits = 3)
    )
  }
  setNames(as.list(cells), paste0(col_key_prefix, display_names))
}


#' Write the supplementary sensitivity sheet: one row per ETT, with 5
#' identifier columns (Enrollment | Intervention | Comparator | Outcome |
#' Follow-up) and two side-by-side measurement blocks.
#'
#' Order: **untruncated weights on the left, truncated weights on the
#' right**. The untruncated block is shaded light grey to emphasise the
#' side-by-side comparison. Column headers within each block are just
#' `Events (int)`, `PY (int)`, etc. (no `[truncated]`/`[untruncated]`
#' suffix) -- the merged group header row carries the distinction.
#'
#' @noRd
.write_combined_sensitivity <- function(wb, sheet_name, plan,
                                        trunc_rates_slot,
                                        trunc_irr_slot,
                                        untrunc_rates_slot,
                                        untrunc_irr_slot,
                                        title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr, cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    openxlsx::writeData(wb, sheet_name, "No ETTs to report.", startRow = row_ptr)
    return(invisible(NULL))
  }

  display_names <- c(
    "Events (int)", "PY (int)", "Rate/100k (int)",
    "Events (cmp)", "PY (cmp)", "Rate/100k (cmp)",
    "IRR", "95% CI", "p-value"
  )

  # Build one row per ETT. Untruncated columns come first, then truncated.
  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    r <- plan$results_ett[[eid]]
    if (is.null(r)) next
    untrunc_m <- .sensitivity_row_measurements(
      r, untrunc_rates_slot, untrunc_irr_slot
    )
    trunc_m <- .sensitivity_row_measurements(
      r, trunc_rates_slot, trunc_irr_slot
    )
    if (is.null(trunc_m) && is.null(untrunc_m)) next

    enr_id <- ett$enrollment_id[i]
    enr_name <- .enrollment_label(plan, enr_id)
    arms <- .lookup_arm_labels(plan$spec, enr_id)
    intervention_name <- if (!is.null(arms)) arms[["intervention"]] else "Intervention"
    comparator_name <- if (!is.null(arms)) arms[["comparator"]] else "Comparator"

    id_cols <- list(
      Enrollment   = enr_name,
      Intervention = intervention_name,
      Comparator   = comparator_name,
      Outcome      = ett$outcome_name[i],
      `Follow-up (weeks)` = as.integer(ett$follow_up[i])
    )
    left_cols  <- .sensitivity_row_fmt(untrunc_m, "u_")
    right_cols <- .sensitivity_row_fmt(trunc_m,   "t_")
    rows[[length(rows) + 1L]] <- c(id_cols, left_cols, right_cols)
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(wb, sheet_name, "No valid sensitivity results.",
                        startRow = row_ptr)
    return(invisible(NULL))
  }

  dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)

  # Layout constants
  n_id <- 5L
  n_block <- length(display_names)
  untrunc_cols_start <- n_id + 1L
  untrunc_cols_end   <- n_id + n_block
  trunc_cols_start   <- untrunc_cols_end + 1L
  trunc_cols_end     <- untrunc_cols_end + n_block

  group_header_row <- row_ptr
  col_header_row   <- row_ptr + 1L
  data_start_row   <- row_ptr + 2L

  # --- Styles ---
  group_header_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center", fontSize = 12,
    fgFill = "#D9D9D9", border = "TopBottom"
  )
  group_header_untrunc_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center", fontSize = 12,
    fgFill = "#BFBFBF", border = "TopBottom"
  )
  id_header_style <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
  )
  col_header_style <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
  )
  col_header_untrunc_style <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#DDDDDD", border = "bottom"
  )
  body_untrunc_style <- openxlsx::createStyle(fgFill = "#F2F2F2")

  # --- Group header row ---
  openxlsx::mergeCells(
    wb, sheet_name,
    cols = untrunc_cols_start:untrunc_cols_end, rows = group_header_row
  )
  openxlsx::writeData(
    wb, sheet_name, "Untruncated weights",
    startCol = untrunc_cols_start, startRow = group_header_row
  )
  openxlsx::addStyle(
    wb, sheet_name, style = group_header_untrunc_style,
    rows = group_header_row, cols = untrunc_cols_start
  )

  openxlsx::mergeCells(
    wb, sheet_name,
    cols = trunc_cols_start:trunc_cols_end, rows = group_header_row
  )
  openxlsx::writeData(
    wb, sheet_name, "Truncated weights",
    startCol = trunc_cols_start, startRow = group_header_row
  )
  openxlsx::addStyle(
    wb, sheet_name, style = group_header_style,
    rows = group_header_row, cols = trunc_cols_start
  )

  # --- Column header row (id cols + display names for both blocks) ---
  id_names <- c("Enrollment", "Intervention", "Comparator", "Outcome",
                "Follow-up (weeks)")
  header_row <- c(id_names, display_names, display_names)
  for (k in seq_along(header_row)) {
    openxlsx::writeData(
      wb, sheet_name, header_row[k],
      startCol = k, startRow = col_header_row
    )
  }
  openxlsx::addStyle(
    wb, sheet_name, style = id_header_style,
    rows = col_header_row, cols = seq_len(n_id), gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, sheet_name, style = col_header_untrunc_style,
    rows = col_header_row,
    cols = untrunc_cols_start:untrunc_cols_end, gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, sheet_name, style = col_header_style,
    rows = col_header_row,
    cols = trunc_cols_start:trunc_cols_end, gridExpand = TRUE
  )

  # --- Body: write the data without its own header row ---
  openxlsx::writeData(
    wb, sheet_name, dt,
    startRow = data_start_row,
    colNames = FALSE
  )

  data_end_row <- data_start_row + nrow(dt) - 1L
  if (nrow(dt) > 0L) {
    openxlsx::addStyle(
      wb, sheet_name, style = body_untrunc_style,
      rows = data_start_row:data_end_row,
      cols = untrunc_cols_start:untrunc_cols_end,
      gridExpand = TRUE, stack = TRUE
    )
  }

  openxlsx::setColWidths(
    wb, sheet_name,
    cols = seq_len(trunc_cols_end),
    widths = c(
      30, 20, 20, 30, 12,
      rep(14, n_block),
      rep(14, n_block)
    )
  )
  openxlsx::freezePane(wb, sheet_name,
                      firstActiveRow = data_start_row,
                      firstActiveCol = n_id + 1L)
}


#' @noRd
.write_combined_irr <- function(wb, sheet_name, plan, slot, title = NULL,
                                keep_ett_ids = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr, cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  prep <- .prepare_combine_data(plan, slot, keep_ett_ids = keep_ett_ids)
  if (is.null(prep)) {
    openxlsx::writeData(wb, sheet_name, "No valid IRR results.",
                        startRow = row_ptr)
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_irr_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  openxlsx::writeData(
    wb, sheet_name, dt, startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
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
  title <- paste0("Enrollment ", eid, " (", label, ") -- Baseline characteristics")
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L, cols = 1L
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
      wb, sheet_name,
      style = openxlsx::createStyle(fontSize = 10, textDecoration = "italic"),
      rows = 2L, cols = 1L
    )
    header_row <- 4L
    data_row <- 5L
  }

  r <- plan$results_enrollment[[eid]]
  if (is.null(r)) {
    openxlsx::writeData(
      wb, sheet_name, "No results for this enrollment.",
      startRow = header_row + 1L
    )
    return(invisible(NULL))
  }

  panels <- list(
    `Unimputed and unweighted`  = r$table1_raw,
    `Imputed and unweighted`    = r$table1_unweighted,
    `Imputed and IPW`           = r$table1_ipw,
    `Imputed and IPW truncated` = r$table1_ipw_trunc
  )

  panels <- Filter(Negate(is.null), panels)
  if (length(panels) == 0L) return(invisible(NULL))

  start_col <- 1L

  bold_centre <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center"
  )
  table_header <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
  )

  for (name in names(panels)) {
    df <- panels[[name]]
    ncols <- ncol(df)
    if (ncols > 1) {
      openxlsx::mergeCells(
        wb, sheet_name,
        cols = start_col:(start_col + ncols - 1L),
        rows = header_row
      )
    }
    openxlsx::writeData(
      wb, sheet_name, name,
      startCol = start_col, startRow = header_row
    )
    openxlsx::addStyle(
      wb, sheet_name, style = bold_centre,
      rows = header_row, cols = start_col
    )
    openxlsx::writeData(
      wb, sheet_name, df,
      startCol = start_col, startRow = data_row,
      headerStyle = table_header
    )
    openxlsx::setColWidths(
      wb, sheet_name,
      cols = start_col,
      widths = 50
    )
    if (ncols > 1) {
      openxlsx::setColWidths(
        wb, sheet_name,
        cols = (start_col + 1L):(start_col + ncols - 1L),
        widths = 18
      )
    }
    start_col <- start_col + ncols + 1L
  }
}


#' Render a one-line enrollment summary sentence for the top of a results
#' sheet. Pulls unique-person and person-trial counts from
#' `plan$enrollment_counts[[eid]]$attrition` (final criterion row) and the
#' post-matching baseline count from `plan$results_enrollment[[eid]]$n_baseline`.
#' Returns NULL when the required fields are absent.
#' @noRd
.format_enrollment_summary <- function(plan, eid) {
  ec <- plan$enrollment_counts[[eid]]
  if (is.null(ec) || is.null(ec$attrition) || nrow(ec$attrition) == 0L) {
    return(NULL)
  }
  overall <- .attrition_overall(ec$attrition)
  if (is.null(overall) || nrow(overall) == 0L) return(NULL)
  last <- overall[nrow(overall)]
  r <- plan$results_enrollment[[eid]]
  n_baseline <- if (!is.null(r)) r$n_baseline else NULL
  fmt <- function(x) format(x, big.mark = ",")
  parts <- c(
    sprintf(
      "Cohort: %s unique persons contributed %s sequential trial enrollments (intervention: %s / comparator: %s person-trials).",
      fmt(last$n_persons), fmt(last$n_person_trials),
      fmt(last$n_intervention), fmt(last$n_comparator)
    )
  )
  if (!is.null(n_baseline) && is.numeric(n_baseline) && n_baseline > 0L) {
    parts <- c(parts, sprintf(
      "After matching: %s person-trials entered baseline.", fmt(n_baseline)
    ))
  }
  paste(parts, collapse = " ")
}


#' Write the raw CONSORT attrition numbers for one enrollment to a sheet.
#' Carries `criterion`, `n_persons`, `n_person_trials`, `n_intervention`,
#' and `n_comparator`, aggregated across trial_ids. Companion to the
#' CONSORT PNG/PDF sidecars: readers can cite exact numbers without
#' measuring pixels.
#' @noRd
.write_attrition_sheet <- function(wb, sheet_name, plan, eid) {
  n_persons <- n_person_trials <- NULL
  ec <- plan$enrollment_counts[[eid]]
  if (is.null(ec) || is.null(ec$attrition) || nrow(ec$attrition) == 0L) {
    return(invisible(NULL))
  }
  openxlsx::addWorksheet(wb, sheet_name)
  label <- .enrollment_label(plan, eid)
  title <- paste0(
    "Enrollment ", eid, " (", label, ") -- CONSORT attrition counts"
  )
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
  openxlsx::addStyle(
    wb, sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L, cols = 1L
  )

  overall <- .attrition_overall(ec$attrition)
  if (is.null(overall) || nrow(overall) == 0L) return(invisible(NULL))

  # Per-criterion excluded deltas (counts lost at each step).
  if (nrow(overall) > 1L) {
    overall[, excluded_persons := c(NA_integer_, -diff(n_persons))]
    overall[, excluded_person_trials := c(NA_integer_, -diff(n_person_trials))]
  } else {
    overall[, excluded_persons := NA_integer_]
    overall[, excluded_person_trials := NA_integer_]
  }

  # Reorder columns so remaining/excluded pairs sit adjacent.
  data.table::setcolorder(overall, c(
    "criterion",
    "n_persons", "n_person_trials",
    "excluded_persons", "excluded_person_trials",
    "n_intervention", "n_comparator"
  ))

  header_style <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#EFEFEF", border = "bottom"
  )
  openxlsx::writeData(
    wb, sheet_name, overall,
    startRow = 3L, headerStyle = header_style
  )
  openxlsx::setColWidths(
    wb, sheet_name, cols = 1L, widths = 45
  )
  openxlsx::setColWidths(
    wb, sheet_name, cols = 2L:7L, widths = 18
  )
  invisible(NULL)
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

#' Restore enrollment counts from per-enrollment sidecar files on disk.
#' Only fills entries not already present on the plan.
#' @noRd
.restore_enrollment_counts <- function(plan, output_dir, enrollment_ids) {
  for (eid in enrollment_ids) {
    if (!is.null(plan$enrollment_counts[[eid]])) next
    counts_path <- .enrollment_counts_path(output_dir, plan$project_prefix, eid)
    if (file.exists(counts_path)) {
      plan$enrollment_counts[[eid]] <- qs2_read(counts_path)
    }
  }
}

# --- Shared preparation helpers (used by s1a and s1b workers) ----------------

#' Read skeleton, apply exclusions, optionally derive confounders, set treatment.
#' Shared by `.s1a_worker()` (scout, no confounders) and `.s1b_worker()` (full).
#' @noRd
.s1_prepare_skeleton <- function(
  enrollment_spec,
  file_path,
  spec,
  derive_confounders = TRUE
) {
  baseline_intervention <- rd_intervention <- eligible_valid_treatment <- isoyearweek <- id <- NULL
  data.table::setDTthreads(enrollment_spec$n_threads)
  obj <- qs2_read(file_path, nthreads = enrollment_spec$n_threads)
  # Under the Skeleton R6 migration, skeleton_*.qs2 files hold a
  # Skeleton R6 object wrapping the data.table. Legacy bare-data.table
  # files are still supported for backwards compat.
  skeleton <- if (inherits(obj, "Skeleton")) obj$data else obj
  rm(obj)
  # qs2 round-tripping drops data.table over-allocation; restore it
  # so subsequent `:=` mutations (in apply_exclusions, apply_derived_confounders,
  # etc.) don't reallocate at a new address and strand our reference.
  # See RegistryStudy$load_skeleton() for the full rationale.
  skeleton <- data.table::setalloccol(
    skeleton, n = getOption("datatable.alloccol", 4096L)
  )
  # Skeleton is already sorted by (id, isoyearweek) from create_skeleton();
  # qs2 preserves row order so setkey is an O(n) verification, not a full sort.
  # Enables binary-search grouping for the repeated by=list(id) calls in
  # exclusion and confounder functions.
  data.table::setkey(skeleton, id, isoyearweek)
  skeleton <- tteplan_apply_exclusions(skeleton, spec, enrollment_spec)
  if (derive_confounders) {
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  }
  x_tx <- enrollment_spec$treatment_impl
  skeleton[,
    rd_intervention := data.table::fcase(
      get(x_tx$variable) == x_tx$intervention_value    , TRUE  ,
      get(x_tx$variable) == x_tx$comparator_value , FALSE ,
      default = NA
    )
  ]
  skeleton[, baseline_intervention := rd_intervention]

  # Mark rows with valid (non-NA) treatment as the first exclusion criterion
  skeleton[, eligible_valid_treatment := !is.na(rd_intervention)]

  # Prepend to eligible_cols so it appears first in attrition reporting
  eligible_cols <- attr(skeleton, "eligible_cols")
  data.table::setattr(skeleton, "eligible_cols", c("eligible_valid_treatment", eligible_cols))

  # Re-combine eligible column to include valid_treatment
  skeleton_eligible_combine(skeleton, attr(skeleton, "eligible_cols"))

  skeleton
}


#' Get all eligible (person_id, trial_id, intervention) tuples from a skeleton.
#' Used by `.s1a_worker()` for scouting and available for direct use.
#' Caller should pre-sort by (pid, trial_id, isoyearweek) for efficiency.
#' @noRd
.s1_eligible_tuples <- function(skeleton, design) {
  . <- rd_intervention <- NULL
  if (!"trial_id" %in% names(skeleton)) {
    .assign_trial_ids(skeleton, design$period_width)
  }
  eligible_rows <- if (!is.null(design$eligible_var)) {
    skeleton[get(design$eligible_var) == TRUE]
  } else {
    skeleton
  }
  pid <- design$person_id_var
  data.table::setorderv(eligible_rows, c(pid, "trial_id", "isoyearweek"))
  # any() not first(): treatment can start at any week within a trial period,

  # not just the first. first() silently drops ~75% of intervention people
  # whose MHT initiation falls mid-period. The no_prior_intervention exclusion
  # criterion handles the new-user restriction (one-time initiation) separately.
  eligible_rows[,
    .(intervention = any(rd_intervention, na.rm = TRUE)),
    by = c(pid, "trial_id")
  ]
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
.s1_compute_attrition <- function(skeleton, eligible_cols, pid,
                                  treatment_var = "rd_intervention") {
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
  pt0 <- sk[, .(
    .tte_tx_any = any(.tte_tx == TRUE, na.rm = TRUE)
  ), by = c(".tte_pid", "trial_id")]
  # Per-trial summary: drop rows where trial_id is NA (person-weeks that
  # fall outside any trial period). Without this filter, those rows
  # collapse into a `(trial_id = NA, criterion)` group whose `n_persons`
  # later gets summed together with the genuine `before_global` row in
  # the per-batch aggregation step (line ~1641, `by = .(trial_id,
  # criterion)`), inflating the reported global cohort by ~2x in CONSORT.
  before_row <- pt0[!is.na(trial_id), .(
    n_persons = data.table::uniqueN(.tte_pid),
    n_person_trials = .N,
    n_intervention = sum(.tte_tx_any == TRUE),
    n_comparator = sum(.tte_tx_any == FALSE)
  ), by = trial_id]
  before_row[, criterion := "before_exclusions"]
  # Global (across-trials) row: true uniqueN of persons, not a sum of
  # per-trial uniqueNs. CONSORT reporting reads this row; without it, the
  # person column of the attrition table double-counts everyone who
  # enters more than one sequential trial.
  before_global <- pt0[, .(
    trial_id = NA_integer_,
    n_persons = data.table::uniqueN(.tte_pid),
    n_person_trials = .N,
    n_intervention = sum(.tte_tx_any == TRUE),
    n_comparator = sum(.tte_tx_any == FALSE)
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
    cumulative_mask <- cumulative_mask & (sk[[eligible_cols[i]]] == TRUE)
    filtered <- sk[cumulative_mask]
    pt_i <- filtered[, .(
      .tte_tx_any = any(.tte_tx == TRUE, na.rm = TRUE)
    ), by = c(".tte_pid", "trial_id")]
    # Same filter as `before_row` above: drop the spurious `trial_id = NA`
    # group so it doesn't collide with `global_rows[[i]]` during the
    # per-batch aggregation summing.
    rows[[i]] <- pt_i[!is.na(trial_id),
      .(
        n_persons = data.table::uniqueN(.tte_pid),
        n_person_trials = .N,
        n_intervention = sum(.tte_tx_any == TRUE),
        n_comparator = sum(.tte_tx_any == FALSE)
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
        n_intervention = sum(.tte_tx_any == TRUE),
        n_comparator = sum(.tte_tx_any == FALSE)
      )
    ][, criterion := eligible_cols[i]]
  }

  # sk is a local copy (column subset), no need to restore names

  data.table::rbindlist(
    c(list(before_row, before_global), rows, global_rows),
    use.names = TRUE
  )
}


# --- s1a: Scout worker (lightweight, no confounders) ------------------------

#' Scout worker for pass 1a: returns eligible tuples and attrition counts.
#'
#' Reads skeleton, applies exclusions (no confounders), sets treatment, then
#' computes per-criterion attrition and extracts eligible tuples.
#'
#' @param enrollment_spec Enrollment spec list.
#' @param file_path Path to a skeleton `.qs2` file.
#' @param spec Parsed study spec.
#' @param cache_path Optional file path to save the prepared skeleton for s1b
#'   reuse. If non-NULL, the skeleton (with exclusions + treatment applied) is
#'   written to this path via `qs2::qs_save()`.
#' @return A list with two elements:
#'   \describe{
#'     \item{tuples}{data.table with columns: person_id_var, trial_id, intervention,
#'       enrollment_person_trial_id.}
#'     \item{attrition}{data.table with columns: trial_id, criterion, n_persons,
#'       n_person_trials, n_intervention, n_comparator.}
#'   }
#' @noRd
.s1a_worker <- function(enrollment_spec, file_path, spec, cache_path = NULL) {
  enrollment_person_trial_id <- trial_id <- NULL
  skeleton <- .s1_prepare_skeleton(
    enrollment_spec,
    file_path,
    spec,
    derive_confounders = FALSE
  )

  pid <- enrollment_spec$design$person_id_var

  # Assign trial_ids and sort once (used by both attrition and tuple extraction)
  .assign_trial_ids(skeleton, enrollment_spec$design$period_width)
  data.table::setorderv(skeleton, c(pid, "trial_id", "isoyearweek"))

  # Compute attrition from eligible_cols attribute set by tteplan_apply_exclusions
  eligible_cols <- attr(skeleton, "eligible_cols")
  attrition <- .s1_compute_attrition(skeleton, eligible_cols, pid)

  # Extract eligible tuples (skips .assign_trial_ids since trial_id exists;
  # skeleton already sorted)
  tuples <- .s1_eligible_tuples(skeleton, enrollment_spec$design)
  tuples[,
    enrollment_person_trial_id := paste0(
      enrollment_spec$enrollment_id,
      ".",
      get(pid),
      ".",
      trial_id
    )
  ]

  # Cache prepared skeleton for s1b reuse (avoids re-reading + re-applying exclusions)
  if (!is.null(cache_path)) {
    qs2::qs_save(skeleton, cache_path, nthreads = enrollment_spec$n_threads)
  }

  rm(skeleton)
  list(tuples = tuples, attrition = attrition)
}


# --- s1b: Full enrollment worker (with pre-matched IDs) --------------------

#' Full enrollment worker for pass 1b: uses pre-matched enrolled_ids.
#'
#' Reads skeleton, applies exclusions + confounders, sets treatment, then
#' enrolls using the pre-decided `enrolled_ids` (skipping the matching phase).
#'
#' @param enrollment_spec Enrollment spec list.
#' @param file_path Path to a skeleton `.qs2` file.
#' @param spec Parsed study spec.
#' @param enrolled_ids data.table with pre-matched enrollment decisions.
#' @param cache_path Optional file path to a cached skeleton from s1a. If the
#'   file exists, it is loaded instead of re-reading the original skeleton and
#'   re-applying exclusions. Only derived confounders are applied on top.
#' @return A [TTEEnrollment] object at "trial" level (panel-expanded).
#' @noRd
.s1b_worker <- function(enrollment_spec, file_path, spec, enrolled_ids,
                        cache_path = NULL) {
  id <- isoyearweek <- NULL
  # Subset to enrolled persons before expensive confounder computation
  pid <- enrollment_spec$design$person_id_var
  enrolled_persons <- unique(enrolled_ids[[pid]])

  if (!is.null(cache_path) && file.exists(cache_path)) {
    # Reuse cached skeleton from s1a (already has exclusions + treatment applied)
    data.table::setDTthreads(enrollment_spec$n_threads)
    skeleton <- qs2_read(cache_path, nthreads = enrollment_spec$n_threads)
    # qs2 drops data.table over-allocation slots; restore them so
    # subsequent `:=` mutations don't reallocate at a new address.
    # (Same rationale as RegistryStudy$load_skeleton(); see that method
    # for the full explanation.)
    skeleton <- data.table::setalloccol(
      skeleton, n = getOption("datatable.alloccol", 4096L)
    )
    data.table::setkey(skeleton, id, isoyearweek)
    skeleton <- skeleton[get(pid) %in% enrolled_persons]
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  } else {
    skeleton <- .s1_prepare_skeleton(
      enrollment_spec,
      file_path,
      spec,
      derive_confounders = FALSE
    )
    skeleton <- skeleton[get(pid) %in% enrolled_persons]
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
      (id_var) := paste0(enrollment_spec$enrollment_id, ".", get(id_var))
    ]
  }
  enrollment
}


# --- s2_worker: Loop 2 IPCW-PP worker ----------------------------------------

#' Worker function for Loop 2: per-ETT IPCW-PP + save (internal)
#'
#' Loads an imputed enrollment file, runs `$s4_prepare_for_analysis()`, and saves
#' the analysis-ready file. Called via [parallel_pool()] in a fresh R session.
#'
#' @param outcome Character, outcome variable name.
#' @param follow_up Integer, follow-up duration in weeks.
#' @param file_imp_path Path to the imputed enrollment .qs2 file.
#' @param file_analysis_path Path to save the analysis-ready file.
#' @param n_threads Integer, number of data.table threads.
#' @param sep_by_tx Logical, estimate IPCW separately by treatment.
#' @param with_gam Logical, use GAM for IPCW estimation.
#' @return TRUE on success.
#' @noRd
.s2_worker <- function(
  outcome,
  follow_up,
  file_imp_path,
  file_analysis_path,
  n_threads,
  sep_by_tx,
  with_gam
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(file_imp_path, nthreads = n_threads)
  enrollment$s4_prepare_for_analysis(
    outcome = outcome,
    follow_up = follow_up,
    estimate_ipcw_pp_separately_by_treatment = sep_by_tx,
    estimate_ipcw_pp_with_gam = with_gam
  )
  qs2::qs_save(enrollment, file_analysis_path, nthreads = n_threads)
  TRUE
}


# --- s3_enrollment_worker: Loop 3a enrollment-level baseline worker -----------

#' Compute a single Table 1 panel from the baseline slice of a loaded
#' enrollment object. Bypasses the R6 method on the cached instance so it
#' works against pre-upgrade saved objects.
#' @noRd
.s3_enrollment_table1 <- function(enrollment, ipw_col = NULL,
                                  arm_labels = NULL,
                                  include_smd = TRUE,
                                  show_missing = TRUE) {
  design <- enrollment$design
  baseline <- enrollment$data[get(design$tstart_var) == 0]
  if (!is.null(ipw_col) && !ipw_col %in% names(baseline)) {
    return(NULL)
  }
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
#' the results. Runs in a fresh R session via [parallel_pool()] for memory
#' isolation.
#'
#' @param analysis_path Path to an analysis .qs2 file for this enrollment.
#' @param raw_path Path to the raw .qs2 file for this enrollment.
#' @param enrollment_id Character, enrollment identifier.
#' @param n_threads Integer, number of data.table threads.
#' @param arm_labels Optional named character vector with `comparator` and
#'   `intervention` keys, passed through to `$table1()`.
#' @return A named list with enrollment-level results.
#' @noRd
.s3_enrollment_worker <- function(analysis_path, raw_path, enrollment_id,
                                  n_threads, arm_labels = NULL) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = n_threads)

  # Supplemental variant: Missing row forced for every variable, SMD column
  # included. Percentages over total N.
  supp_args <- list(arm_labels = arm_labels,
                    include_smd = TRUE, show_missing = "always")
  # Main variant: no Missing rows, no SMD column (used by the headline
  # "Table 1" sheet). Percentages over the non-missing denominator so levels
  # still sum to 100.
  main_args <- list(arm_labels = arm_labels,
                    include_smd = FALSE, show_missing = "none")

  safe <- function(fn_args, label) {
    tryCatch(
      do.call(.s3_enrollment_table1, fn_args),
      error = function(e) {
        warning("table1 ", label, " failed for ", enrollment_id, ": ",
                conditionMessage(e))
        NULL
      }
    )
  }

  table1_unweighted <- safe(c(list(enrollment = enrollment), supp_args),
                            "unweighted")
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
  n_baseline <- nrow(enrollment$data[
    get(enrollment$design$tstart_var) == 0
  ])
  rm(enrollment)
  gc()

  table1_raw <- NULL
  if (file.exists(raw_path)) {
    enrollment_raw <- swereg::qs2_read(raw_path, nthreads = n_threads)
    table1_raw <- tryCatch(
      do.call(
        .s3_enrollment_table1,
        c(list(enrollment = enrollment_raw), supp_args)
      ),
      error = function(e) {
        warning("table1 raw failed for ", enrollment_id, ": ",
                conditionMessage(e))
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
    arm_labels = arm_labels,
    computed_at = Sys.time()
  )
}


# --- s3_ett_worker: Loop 3b per-ETT / per-analysis worker --------------------

#' Worker function for Loop 3b: runs ONE analysis on ONE ETT file.
#'
#' Loads an analysis file and calls a single method (rates or irr).
#' Each heavy call gets its own subprocess so the OS reclaims all memory.
#'
#' @param analysis_path Path to the analysis .qs2 file.
#' @param method Character: "summary_and_rates" or "irr".
#' @param weight_col Character, weight column name.
#' @param ett_id Character, ETT identifier (for logging).
#' @param n_threads Integer, number of data.table threads.
#' @return The method result (data.table, list, etc.).
#' @noRd
.s3_ett_worker <- function(analysis_path, method, weight_col, ett_id,
                           n_threads) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = n_threads)

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
    slot <- paste0("irr_", sub("^analysis_weight_", "", weight_col))
    setNames(
      list(safe_call(\() enrollment$irr(weight_col = weight_col), slot)),
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
#' @family tte_spec
#' @export
tteplan_read_spec <- function(spec_path) {
  if (!file.exists(spec_path)) {
    stop("Spec file not found: ", spec_path)
  }

  spec <- yaml::read_yaml(spec_path)

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
    if (is.list(v)) v <- unlist(v)
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
              "enrollments[", i, "] '", enr$name %||% enr$id,
              "' additional_inclusion[", j, "] '", ai$name,
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
            .convert_window(ai$implementation$window %||% "lifetime_before_baseline")
        }
      }
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
  enrollment_id <- enrollment_spec$enrollment_id

  # Find enrollment definition in the spec
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

  # 1. Calendar years
  years <- seq(
    spec$inclusion_criteria$isoyears[1],
    spec$inclusion_criteria$isoyears[2]
  )
  skeleton <- skeleton_eligible_isoyears(skeleton, years)
  eligible_cols <- "eligible_isoyears"

  # 2. Enrollment-specific additional inclusion (before global exclusions)
  if (!is.null(enrollment_def$additional_inclusion)) {
    for (ae in enrollment_def$additional_inclusion) {
      if (identical(ae$type, "age_range")) {
        skeleton <- skeleton_eligible_age_range(
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
        # Create exclusion column (TRUE = no events), then negate for inclusion
        temp_col <- paste0(
          ".temp_eligible_no_", sv, "_", .window_label(window)
        )
        col_name <- paste0(
          "eligible_has_", sv, "_", .window_label(window)
        )
        skeleton <- skeleton_eligible_no_events_in_window_excluding_wk0(
          skeleton,
          event_var = sv,
          window = window,
          col_name = temp_col
        )
        skeleton[, (col_name) := !get(temp_col)]
        skeleton[, (temp_col) := NULL]
        eligible_cols <- c(eligible_cols, col_name)
      }
    }
  }

  # 3. Global exclusion criteria
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
      skeleton <- skeleton_eligible_no_events_lifetime_before_and_after_baseline(
        skeleton,
        event_var = sv,
        col_name = col_name
      )
    } else if (identical(impl$type, "no_prior_intervention")) {
      window <- impl$window_weeks
      col_name <- paste0(
        "eligible_no_",
        sv,
        "_",
        .window_label(window)
      )
      skeleton <- skeleton_eligible_no_observation_in_window_excluding_wk0(
        skeleton,
        var = sv,
        value = impl$intervention_value,
        window = window,
        col_name = col_name
      )
    } else {
      window <- impl$window_weeks
      col_name <- paste0(
        "eligible_no_",
        sv,
        "_",
        .window_label(window)
      )
      skeleton <- skeleton_eligible_no_events_in_window_excluding_wk0(
        skeleton,
        event_var = sv,
        window = window,
        col_name = col_name
      )
    }
    eligible_cols <- c(eligible_cols, col_name)
  }

  # 4. Enrollment-specific additional exclusion criteria
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
        skeleton <- skeleton_eligible_no_events_lifetime_before_and_after_baseline(
          skeleton,
          event_var = sv,
          col_name = col_name
        )
      } else if (identical(impl$type, "no_prior_intervention")) {
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_no_",
          sv,
          "_",
          .window_label(window)
        )
        skeleton <- skeleton_eligible_no_observation_in_window_excluding_wk0(
          skeleton,
          var = sv,
          value = impl$intervention_value,
          window = window,
          col_name = col_name
        )
      } else {
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_no_",
          sv,
          "_",
          .window_label(window)
        )
        skeleton <- skeleton_eligible_no_events_in_window_excluding_wk0(
          skeleton,
          event_var = sv,
          window = window,
          col_name = col_name
        )
      }
      eligible_cols <- c(eligible_cols, col_name)
    }
  }

  # 5. Combine all criteria
  skeleton <- skeleton_eligible_combine(skeleton, eligible_cols)

  data.table::setattr(skeleton, "eligible_cols", eligible_cols)
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
  if (is.list(sv)) sv <- unlist(sv)
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

  for (conf in spec$confounders) {
    impl <- conf$implementation
    if (isTRUE(impl$computed)) {
      .ensure_combined_column(skeleton, impl)
      skeleton <- skeleton_eligible_no_events_in_window_excluding_wk0(
        skeleton,
        event_var = impl$source_variable_combined,
        window = impl$window_weeks,
        col_name = impl$variable
      )
    }
  }

  skeleton
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
    stop("`study` must provide a `$skeleton_files` accessor (use registrystudy_load() to load a RegistryStudy).")
  }

  # Wrap candidate-dir vectors in CandidatePath instances.
  dir_spec_cp    <- CandidatePath$new(candidate_dir_spec,    "dir_spec")
  dir_tteplan_cp <- CandidatePath$new(candidate_dir_tteplan, "dir_tteplan")
  dir_results_cp <- CandidatePath$new(candidate_dir_results, "dir_results")

  # Read the spec YAML from the resolved spec directory. If spec_version
  # wasn't supplied, we don't yet know which file to read -- require it.
  if (is.null(spec_version)) {
    stop("`spec_version` must be supplied (e.g. \"v003\") so the spec YAML filename can be built.")
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
      "spec_version mismatch: argument was '", spec_version,
      "' but the YAML at ", spec_path,
      " has implementation.version = '", yaml_version %||% "NULL", "'"
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
    m <- regmatches(basename(path), regexec("skeleton_(\\d+)\\.qs2$", basename(path)))[[1]]
    if (length(m) < 2L) return(NA_integer_)
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

  plan <- TTEPlan$new(
    project_prefix = project_id,
    skeleton_files = skeleton_files,
    global_max_isoyearweek = global_max_isoyearweek
  )
  plan$period_width <- as.integer(period_width)
  plan$spec <- spec
  plan$spec_version <- spec_version

  # CandidatePath fields + embedded study
  plan$dir_spec_cp    <- dir_spec_cp
  plan$dir_tteplan_cp <- dir_tteplan_cp
  plan$dir_results_cp <- dir_results_cp
  plan$registrystudy  <- study
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
          time_treatment_var = "rd_intervention",
          eligible_var = "eligible",
          argset = list(
            age_group = age_group,
            age_min = age_min,
            age_max = age_max,
            outcome_description = outcome$description %||% NA_character_
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
