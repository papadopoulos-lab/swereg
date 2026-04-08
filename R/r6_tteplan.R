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

.TTE_PLAN_SCHEMA_VERSION <- 1L

#' TTEPlan class for trial generation planning
#'
#' Bundles the ETT grid, skeleton file paths, and design column names into a
#' single object using a builder pattern. Create an empty plan with
#' [TTEPlan$new()], then add ETTs one at a time with `$add_one_ett()`.
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
    #'       n_persons, n_person_trials, n_exposed, n_unexposed) showing
    #'       cumulative attrition at each eligibility step. Includes a
    #'       \code{"before_exclusions"} row with pre-filtering counts.}
    #'     \item{matching}{data.table (trial_id, n_exposed_total,
    #'       n_unexposed_total, n_exposed_enrolled, n_unexposed_enrolled).}
    #'   }
    enrollment_counts = NULL,
    #' @field output_dir Character. Directory where enrollment/analysis files are stored.
    output_dir = NULL,
    #' @field results_enrollment Named list of per-enrollment analysis results (keyed by enrollment_id).
    results_enrollment = NULL,
    #' @field results_ett Named list of per-ETT analysis results (keyed by ett_id).
    results_ett = NULL,

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
            "exposure_var"
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

    #' @description Check if this object's schema version matches the current class version.
    #' Warns if the object was saved with an older schema version.
    #' @return `invisible(TRUE)` if versions match, `invisible(FALSE)` otherwise.
    check_version = function() {
      current <- .TTE_PLAN_SCHEMA_VERSION
      saved <- private$.schema_version %||% 0L
      if (saved < current) {
        warning(
          "This ",
          class(self)[1],
          " was saved with schema version ",
          saved,
          " but current version is ",
          current,
          ". Re-create this object.",
          call. = FALSE
        )
      }
      invisible(saved == current)
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
          "  Category levels / exposure values\n",
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
          fmt_var(ec$implementation$source_variable),
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
              cimpl$source_variable,
              "_",
              .window_label(cimpl$window_weeks)
            )
          cat(
            "    Variable:   ",
            derived,
            "<-",
            fmt_var(cimpl$source_variable),
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

        # Additional inclusion
        if (!is.null(enr$additional_inclusion)) {
          cat("    Additional inclusion:\n")
          for (ai in enr$additional_inclusion) {
            if (identical(ai$type, "age_range")) {
              cat(sprintf("      %-18s%d-%d\n", "Age range:", ai$min, ai$max))
            } else {
              cat("      -", ai$name, "\n")
            }
          }
        }

        # Exposure sub-block
        exp <- enr$exposure
        cat("    Exposure:\n")
        cat(sprintf(
          "      %-18s%s\n",
          "Variable:",
          fmt_var(exp$implementation$variable)
        ))
        cat(sprintf(
          "      %-18s%s <- %s\n",
          "Exposed:",
          exp$arms$exposed,
          yellow(exp$implementation$exposed_value)
        ))
        cat(sprintf(
          "      %-18s%s <- %s\n",
          "Comparator:",
          exp$arms$comparator,
          yellow(exp$implementation$comparator_value)
        ))
        cat(sprintf(
          "      %-18s1:%d\n",
          "Matching ratio:",
          exp$implementation$matching_ratio
        ))

        # Additional exclusion
        if (!is.null(enr$additional_exclusion)) {
          cat("    Additional exclusion:\n")
          for (ae in enr$additional_exclusion) {
            cat("      -", ae$name, "\n")
            cat(
              "        Variable:    ",
              fmt_var(ae$implementation$source_variable),
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
          exp <- enr$exposure
          parts <- c(
            parts,
            paste0(
              "Enrollment '",
              enr$id,
              "': ",
              exp$arms$exposed,
              " vs ",
              exp$arms$comparator,
              " (variable: ",
              exp$implementation$variable,
              ", ratio: 1:",
              exp$implementation$matching_ratio,
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
        ratio <- enr$exposure$implementation$matching_ratio
        assign_parts <- c(
          assign_parts,
          sprintf(
            "In enrollment %s, each exposed individual was matched to %d unexposed individual%s from the same sequential trial.",
            enr$id,
            ratio,
            if (ratio > 1) "s" else ""
          )
        )
      }
      assign_text <- paste0(
        paste(assign_parts, collapse = " "),
        " Matching was stratified by sequential trial to preserve the temporal structure of the emulation. ",
        "Within each trial, all exposed individuals were retained and unexposed individuals were sampled at the specified ratio from the full study population. ",
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
                impl$source_variable,
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
          "conditional on baseline covariates, and fitted separately for the exposed and unexposed arms. ",
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
          "(e.g., no prior exposure within the specified washout window, no prior outcome event within the lookback window or over the lifetime, as defined in the specification). ",
          "Exclusion criteria were evaluated cumulatively, and the number of persons and person-trials remaining after each criterion was recorded for the participant flow diagram. ",
          # 7b: Treatment strategies
          "Treatment strategies (6b): Treatment status was determined from registry data at the start of each enrollment period, using the variable and values specified in the study configuration. ",
          "The enrollment period width (period_width) determines the granularity of sequential trial entry; narrower periods reduce residual immortal time bias ",
          "at the cost of fewer eligible individuals per trial (Caniglia et al., 2023). ",
          "The period width also serves as an implicit grace period: treatment status is assessed once per period, ",
          "so that initiation occurring anywhere within the period is attributed to its start. ",
          # 7c: Assignment
          "Assignment (6c): Treatment assignment was emulated through stratified matching of unexposed to exposed individuals within each sequential trial, ",
          "rather than including all eligible non-initiators with inverse probability weighting alone (Danaei et al., 2013). ",
          "This approach was chosen for computational tractability with large registry datasets. ",
          "Residual confounding within the matched set was addressed by inverse probability weighting using baseline covariates. ",
          # 7d: Follow-up
          "Follow-up (6d): Follow-up began at the start of the enrollment period in which an individual met eligibility and exposure criteria ",
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
                n_exposed = sum(n_exposed),
                n_unexposed = sum(n_unexposed)
              ),
              by = criterion
            ]
            # Preserve criterion order from attrition (before_exclusions first)
            overall[, criterion := factor(criterion, levels = unique(criterion))]
            data.table::setorder(overall, criterion)

            # Compute column widths for right-justified alignment
            all_totals <- overall$n_person_trials
            all_exposed <- overall$n_exposed
            all_unexposed <- overall$n_unexposed
            deltas_total <- c(0, -diff(all_totals))
            deltas_exp <- c(0, -diff(all_exposed))
            deltas_unexp <- c(0, -diff(all_unexposed))

            fmt_num <- function(x, w) formatC(format(x, big.mark = ","), width = w)
            col_width <- function(vals, deltas) {
              max(nchar(format(c(vals, abs(deltas)), big.mark = ",")))
            }
            w_total <- col_width(all_totals, deltas_total)
            w_exp <- col_width(all_exposed, deltas_exp)
            w_unexp <- col_width(all_unexposed, deltas_unexp)

            item8_parts <- c(
              item8_parts,
              paste0("Enrollment '", enr_id, "' participant flow:")
            )

            for (j in seq_len(nrow(overall))) {
              tot <- all_totals[j]
              exp <- all_exposed[j]
              unexp <- all_unexposed[j]

              if (overall$criterion[j] == "before_exclusions") {
                item8_parts <- c(item8_parts,
                  "  Before exclusions:",
                  sprintf("    \u21b3 %s person-trials",
                    cyan(fmt_num(tot, w_total)))
                )
              } else {
                d_tot <- all_totals[j - 1] - tot
                d_exp <- all_exposed[j - 1] - exp
                d_unexp <- all_unexposed[j - 1] - unexp
                item8_parts <- c(item8_parts,
                  sprintf("  Applying %s:", bold(as.character(overall$criterion[j]))),
                  sprintf("    \u21b3 Excluding %s person-trials (%s exposed person-trials, %s comparator person-trials)",
                    red(fmt_num(d_tot, w_total)),
                    red(fmt_num(d_exp, w_exp)),
                    red(fmt_num(d_unexp, w_unexp))),
                  sprintf("    \u21b3 Remaining %s person-trials (%s exposed person-trials, %s comparator person-trials)",
                    cyan(fmt_num(tot, w_total)),
                    cyan(fmt_num(exp, w_exp)),
                    cyan(fmt_num(unexp, w_unexp)))
                )
              }
            }
          }
          if (!is.null(ec$matching)) {
            m <- ec$matching
            n_exp <- sum(m$n_exposed_enrolled, na.rm = TRUE)
            n_unexp <- sum(m$n_unexposed_enrolled, na.rm = TRUE)
            n_match_total <- n_exp + n_unexp
            item8_parts <- c(item8_parts,
              "  Post-matching:",
              sprintf("    \u21b3 %s person-trials (%s exposed person-trials, %s comparator person-trials)",
                cyan(fmt_num(n_match_total, w_total)),
                cyan(fmt_num(n_exp, w_exp)),
                cyan(fmt_num(n_unexp, w_unexp)))
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
      person_id_var <- if (!is.null(argset$person_id_var)) {
        argset$person_id_var
      } else {
        "id"
      }
      exposure_var <- "baseline_exposed"

      tv_exp <- if (is.null(time_exposure_var)) {
        NA_character_
      } else {
        time_exposure_var
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
          if (first$exposure_var != exposure_var) {
            stop("exposure_var mismatch within enrollment_id ", enrollment_id)
          }
          first_tv <- first$time_exposure_var
          if (
            !identical(is.na(first_tv), is.na(tv_exp)) ||
              (!is.na(first_tv) && first_tv != tv_exp)
          ) {
            stop(
              "time_exposure_var mismatch within enrollment_id ",
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
        self$ett <- data.table::rbindlist(
          list(self$ett, new_row),
          use.names = TRUE,
          fill = TRUE
        )
      }
      invisible(self)
    },

    #' @description Save the plan to disk.
    #' File is named \code{{project_prefix}_plan.qs2} inside `dir`.
    #' Saves the R6 object directly; reload with [qs2_read()].
    #' @param dir Directory to save into.
    #' @return Invisibly returns the file path.
    save = function(dir) {
      path <- file.path(dir, paste0(self$project_prefix, "_plan.qs2"))
      qs2::qs_save(self, path, nthreads = parallel::detectCores())
      invisible(path)
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
    #'     \item{exposure_impl}{List with variable, exposed_value, comparator_value
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
      x_time_exp <- first$time_exposure_var
      if (is.na(x_time_exp)) {
        x_time_exp <- NULL
      }
      x_eligible <- first$eligible_var
      if (is.na(x_eligible)) {
        x_eligible <- NULL
      }

      result <- list(
        design = TTEDesign$new(
          person_id_var = x_person_id,
          exposure_var = first$exposure_var,
          time_exposure_var = x_time_exp,
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
    #'
    #' Uses a two-pass pipeline to fix cross-batch matching ratio imbalance.
    #' Requires `self$spec` to be set (e.g., via
    #' [tteplan_from_spec_and_registrystudy()]).
    #'
    #' \enumerate{
    #'   \item **Pass 1a (scout)**: Lightweight parallel pass that reads each
    #'     skeleton file, applies exclusions and exposure, and returns eligible
    #'     `(person_id, trial_id, exposed)` tuples. No confounders or enrollment.
    #'   \item **Centralized matching**: Combines all tuples from all batches,
    #'     then per `trial_id` keeps all exposed and samples
    #'     `ratio * n_exposed` unexposed globally. Stores counts on
    #'     `self$enrollment_counts` for TARGET Item 8 reporting.
    #'   \item **Pass 1b (full enrollment)**: Parallel pass that re-reads each
    #'     skeleton file with full processing (exclusions + confounders +
    #'     exposure), then enrolls using pre-matched IDs (skipping per-batch
    #'     matching). Produces panel-expanded TTEEnrollment objects.
    #' }
    #'
    #' @param output_dir Directory for output files.
    #' @param impute_fn Imputation callback or NULL (default: [tteenrollment_impute_confounders]).
    #' @param stabilize Logical, stabilize IPW (default: TRUE).
    #' @param n_workers Integer, concurrent subprocesses (default: 3L).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    #' @param resume Logical. If `TRUE`, skip enrollments whose `_imp_` file
    #'   already exists in `output_dir` (default: FALSE).
    s1_generate_enrollments_and_ipw = function(
      output_dir,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = 3L,
      swereg_dev_path = NULL,
      resume = FALSE
    ) {
      # --- Loop 1: Create trial panels from skeleton files ---
      #
      # Two-pass pipeline:
      #
      #   Pass 1a (scout, parallel):
      #     skeleton files -> exclusions -> exposure -> eligible tuples
      #     Returns: (person_id, trial_id, exposed) per file
      #     Caches prepared skeleton to tempdir for s1b reuse
      #
      #   Match (main process):
      #     Combine all tuples -> per trial_id, keep all exposed,
      #     sample ratio * n_exposed unexposed -> enrolled_ids
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
            exp_rows <- .SD[exposed == TRUE]
            unexp_rows <- .SD[exposed == FALSE]
            n_to_sample <- min(
              round(x_ratio * nrow(exp_rows)),
              nrow(unexp_rows)
            )
            sampled <- if (n_to_sample > 0) {
              unexp_rows[sample(.N, n_to_sample)]
            } else {
              unexp_rows[0]
            }
            data.table::rbindlist(list(exp_rows, sampled))
          },
          by = trial_id
        ]

        # Store counts for TARGET reporting
        global_counts <- all_tuples[,
          .(
            n_exposed_total = sum(exposed == TRUE),
            n_unexposed_total = sum(exposed == FALSE)
          ),
          by = trial_id
        ]
        enrolled_counts <- enrolled_ids[,
          .(
            n_exposed_enrolled = sum(exposed == TRUE),
            n_unexposed_enrolled = sum(exposed == FALSE)
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
            n_exposed = sum(n_exposed),
            n_unexposed = sum(n_unexposed)
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
    #' @param output_dir Directory containing imp files and where analysis files
    #'   are saved.
    #' @param estimate_ipcw_pp_separately_by_exposure Logical, estimate IPCW-PP
    #'   separately by exposure group (default: TRUE).
    #' @param estimate_ipcw_pp_with_gam Logical, use GAM for IPCW-PP estimation
    #'   (default: TRUE).
    #' @param n_workers Integer, concurrent subprocesses (default: 1L).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    #' @param resume Logical. If `TRUE`, skip ETTs whose analysis file already
    #'   exists in `output_dir` (default: FALSE).
    s2_generate_analysis_files_and_ipcw_pp = function(
      output_dir,
      estimate_ipcw_pp_separately_by_exposure = TRUE,
      estimate_ipcw_pp_with_gam = TRUE,
      n_workers = 1L,
      swereg_dev_path = NULL,
      resume = FALSE
    ) {
      if (is.null(self$ett) || nrow(self$ett) == 0) {
        stop("plan has no ETTs. Use $add_one_ett() to add ETTs first.")
      }

      ett <- self$ett
      n_cores <- parallel::detectCores()
      n_threads <- max(1L, floor(n_cores / n_workers))

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
    #' @param output_dir Directory containing analysis/raw files. Defaults to
    #'   `self$output_dir` (set by `$s1_generate_enrollments_and_ipw()`).
    #' @param swereg_dev_path Path to local swereg dev copy, or NULL.
    s3_analyze = function(enrollment_ids = NULL, ett_ids = NULL,
                          output_dir = NULL, swereg_dev_path = NULL) {
      if (is.null(output_dir)) {
        output_dir <- self$output_dir
      }
      if (is.null(output_dir)) {
        stop(
          "output_dir is not set. Pass it as an argument, ",
          "or run $s1_generate_enrollments_and_ipw() first."
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
            n_threads = n_cores
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
          n_workers = 1L,
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
          n_workers = 1L,
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

    #' @description Export analysis results to an Excel workbook.
    #'
    #' Requires `self$results_enrollment` and `self$results_ett` to be populated
    #' (run `$s3_analyze()` first).
    #'
    #' @param path File path for the output `.xlsx` file.
    #' @param table1_enrollment Enrollment ID for Table 1 (main baseline table).
    #'   Default: the enrollment with the most baseline observations.
    export_tables = function(path, table1_enrollment = NULL) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required. Install with: install.packages('openxlsx')")
      }
      if (is.null(self$results_enrollment) || length(self$results_enrollment) == 0L) {
        stop("No enrollment results. Run $s3_analyze() first.")
      }
      if (is.null(self$results_ett) || length(self$results_ett) == 0L) {
        stop("No ETT results. Run $s3_analyze() first.")
      }

      ett <- self$ett
      enrollment_ids <- unique(ett$enrollment_id)

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
      toc_desc <- c(toc_desc, "Enrollment overview (exposure, matching, criteria)")

      # --- ETTs overview sheet ---
      .write_ett_overview(wb, self)
      toc_names <- c(toc_names, "ETTs")
      toc_desc <- c(toc_desc, "ETT overview (outcome, follow-up, events)")

      # --- Table 1: Baseline for chosen enrollment ---
      t1_label <- .enrollment_label(self, table1_enrollment)
      t1_data <- self$results_enrollment[[table1_enrollment]]
      if (!is.null(t1_data$table1_ipw_trunc)) {
        .write_tableone_sheet(
          wb, "Table 1", t1_data$table1_ipw_trunc,
          title = paste0(
            "Table 1: Baseline characteristics (IPW-weighted, truncated) -- Enrollment ",
            table1_enrollment, " (", t1_label, ")"
          )
        )
        toc_names <- c(toc_names, "Table 1")
        toc_desc <- c(toc_desc, paste0(
          "Baseline characteristics (IPW truncated) -- ", t1_label))
      }

      # --- Table 2: Rates PP truncated ---
      .write_combined_rates(wb, "Table 2", self, "rates_pp_trunc",
        title = "Table 2: Events, person-years, and incidence rates (per-protocol, truncated weights)")
      toc_names <- c(toc_names, "Table 2")
      toc_desc <- c(toc_desc, "Events, person-years, incidence rates (truncated weights)")

      # --- Table 3: IRR PP truncated ---
      .write_combined_irr(wb, "Table 3", self, "irr_pp_trunc",
        title = "Table 3: Incidence rate ratios (per-protocol, truncated weights)")
      toc_names <- c(toc_names, "Table 3")
      toc_desc <- c(toc_desc, "Incidence rate ratios (truncated weights)")

      # --- Table S1-SN: Combined baselines per enrollment ---
      for (j in seq_along(enrollment_ids)) {
        eid <- enrollment_ids[j]
        sheet_name <- paste0("Table S", j)
        .write_combined_baseline(wb, sheet_name, self, eid)
        toc_names <- c(toc_names, sheet_name)
        label <- .enrollment_label(self, eid)
        toc_desc <- c(toc_desc, paste0(
          "Enrollment ", eid, " (", label,
          ") -- combined baselines (Raw/Unweighted/IPW/IPW Trunc)"))
      }
      n_s <- length(enrollment_ids)

      # --- CONSORT sheets ---
      if (!is.null(self$enrollment_counts)) {
        for (j in seq_along(enrollment_ids)) {
          eid <- enrollment_ids[j]
          ec <- self$enrollment_counts[[eid]]
          if (!is.null(ec$attrition)) {
            label <- .enrollment_label(self, eid)
            sheet_name <- paste0("CONSORT ", eid)
            .write_consort(wb, sheet_name, ec, eid, label)
            toc_names <- c(toc_names, sheet_name)
            toc_desc <- c(toc_desc, paste0(
              "Enrollment ", eid, " (", label, ") -- participant flow"))
          }
        }
      }

      # --- Table S{last-1}: Rates PP untruncated ---
      s_rates_name <- paste0("Table S", n_s + 1L)
      .write_combined_rates(
        wb, s_rates_name, self, "rates_pp",
        title = paste0(s_rates_name,
          ": Events, person-years, and incidence rates (per-protocol, untruncated weights -- sensitivity analysis)")
      )
      toc_names <- c(toc_names, s_rates_name)
      toc_desc <- c(toc_desc, "Events, person-years, incidence rates (untruncated -- sensitivity)")

      # --- Table S{last}: IRR PP untruncated ---
      s_irr_name <- paste0("Table S", n_s + 2L)
      .write_combined_irr(
        wb, s_irr_name, self, "irr_pp",
        title = paste0(s_irr_name,
          ": Incidence rate ratios (per-protocol, untruncated weights -- sensitivity analysis)")
      )
      toc_names <- c(toc_names, s_irr_name)
      toc_desc <- c(toc_desc, "Incidence rate ratios (untruncated -- sensitivity)")

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
#' @param path Path to a `_plan.qs2` file.
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
  # Copy all additional public fields (use get() not [[ — R6 [[ doesn't
  # reliably access fields, only $ and environment get() do)
  fields <- c(
    "spec", "enrollment_counts", "period_width",
    "expected_skeleton_file_count", "code_registry", "expected_n_ids",
    "created_at", "registry_study_created_at", "skeleton_created_at",
    "output_dir", "results_enrollment", "results_ett"
  )
  for (f in fields) {
    val <- tryCatch(get(f, envir = old), error = function(e) NULL)
    if (!is.null(val)) plan[[f]] <- val
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
        code_lookup[[col]] <- paste0(st$codes[i], " (", st$type[i], ")")
      }
    }
  }

  if (colorize) {
    cyan <- function(x) paste0("\033[36m", x, "\033[0m")
    magenta <- function(x) paste0("\033[95m", x, "\033[0m")
    green <- function(x) paste0("\033[92m", x, "\033[0m")
    fmt_var <- function(var) {
      if (is.null(code_lookup)) return(var)
      info <- code_lookup[[var]]
      if (!is.null(info)) {
        paste0(cyan(var), " <- ", magenta(info))
      } else {
        green(var)
      }
    }
  } else {
    fmt_var <- function(var) {
      if (is.null(code_lookup)) return(var)
      info <- code_lookup[[var]]
      if (!is.null(info)) paste0(var, " <- ", info) else var
    }
  }

  list(lookup = code_lookup, fmt_var = fmt_var)
}

#' @noRd
.write_spec_summary <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "Study Specification")
  spec <- plan$spec
  if (is.null(spec)) {
    openxlsx::writeData(wb, "Study Specification", "No spec available.")
    return(invisible(NULL))
  }

  cl <- .build_code_lookup(plan, colorize = FALSE)
  fmt_var <- cl$fmt_var

  rows <- list()
  add <- function(section, item, value) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      Section = section, Item = item, Value = value
    )
  }

  # Study
  add("Study", "Title", spec$study$title)
  add("Study", "PI", spec$study$principal_investigator)
  if (!is.null(spec$study$design)) add("Study", "Design", spec$study$design)
  impl <- spec$study$implementation
  if (!is.null(impl$version)) add("Study", "Version", impl$version)
  if (!is.null(plan$global_max_isoyearweek)) {
    add("Study", "Admin censoring", plan$global_max_isoyearweek)
  }

  # Inclusion
  iso <- spec$inclusion_criteria$isoyears
  add("Inclusion", "Isoyears", paste0(iso[1], "-", iso[2]))

  # Follow-up
  for (fu in spec$follow_up) {
    add("Follow-up", fu$label, paste0(fu$weeks, " weeks"))
  }

  # Exclusion criteria
  for (ec in spec$exclusion_criteria) {
    add("Exclusion", ec$name, paste0(
      fmt_var(ec$implementation$source_variable),
      " | ", .format_window_human(ec$implementation)
    ))
  }

  # Confounders
  for (conf in spec$confounders) {
    cimpl <- conf$implementation
    var_str <- if (isTRUE(cimpl$computed)) {
      paste0(
        cimpl$variable %||% cimpl$source_variable,
        " <- ", fmt_var(cimpl$source_variable),
        " | ", .format_window_human(cimpl)
      )
    } else {
      fmt_var(cimpl$variable)
    }
    cats <- if (!is.null(conf$categories)) {
      paste0(" [", paste(conf$categories, collapse = ", "), "]")
    } else {
      ""
    }
    add("Confounder", conf$name, paste0(var_str, cats))
  }

  # Outcomes
  for (out in spec$outcomes) {
    add("Outcome", out$name, fmt_var(out$implementation$variable))
  }

  # Enrollments
  for (enr in spec$enrollments) {
    enr_label <- paste0(enr$id, ": ", enr$name)
    exp <- enr$exposure
    add("Enrollment", enr_label, paste0(
      "Exposed: ", fmt_var(exp$implementation$variable),
      " = ", exp$implementation$exposed_value,
      " | Comparator: ", exp$implementation$comparator_value,
      " | Ratio 1:", exp$implementation$matching_ratio
    ))
    if (!is.null(enr$additional_inclusion)) {
      for (ai in enr$additional_inclusion) {
        if (identical(ai$type, "age_range")) {
          add("Enrollment", paste0("  ", enr$id, " inclusion"),
              paste0("Age ", ai$min, "-", ai$max))
        }
      }
    }
    if (!is.null(enr$additional_exclusion)) {
      for (ae in enr$additional_exclusion) {
        add("Enrollment", paste0("  ", enr$id, " exclusion"),
            paste0(ae$name, ": ", fmt_var(ae$implementation$source_variable),
                   " | ", .format_window_human(ae$implementation)))
      }
    }
  }

  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, "Study Specification", dt)

  # Bold section headers
  bold_style <- openxlsx::createStyle(textDecoration = "bold")
  sections <- unique(dt$Section)
  for (sec in sections) {
    first_row <- which(dt$Section == sec)[1] + 1L  # +1 for header row
    openxlsx::addStyle(wb, "Study Specification", bold_style,
                       rows = first_row, cols = 1L)
  }
  openxlsx::setColWidths(wb, "Study Specification", cols = 1:3,
                         widths = c(15, 30, 80))
}

#' @noRd
.tableone_to_df <- function(x) {
  # Explicitly resolve print.TableOne from tableone namespace —
  # S3 dispatch may not find it when tableone is only in Imports
  print_fn <- getFromNamespace("print.TableOne", "tableone")
  mat <- print_fn(x, printToggle = FALSE, showAllLevels = TRUE, smd = TRUE)
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  df <- cbind(Variable = rownames(df), df)
  rownames(df) <- NULL
  df
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

#' @noRd
.write_tableone_sheet <- function(wb, sheet_name, tableone_obj, title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  start_row <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
    start_row <- 3L
  }
  df <- .tableone_to_df(tableone_obj)
  openxlsx::writeData(wb, sheet_name, df, startRow = start_row)
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
    # Exposure info from spec
    exp_info <- list(variable = NA, exposed = NA, comparator = NA, ratio = NA)
    row <- plan$ett[plan$ett$enrollment_id == eid][1]
    if ("exposure_impl" %in% names(plan$ett) && !is.null(row$exposure_impl[[1]])) {
      impl <- row$exposure_impl[[1]]
      exp_info$variable <- impl$variable %||% NA
      exp_info$exposed <- impl$exposed_value %||% NA
      exp_info$comparator <- impl$comparator_value %||% NA
    }
    if ("matching_ratio" %in% names(plan$ett)) {
      exp_info$ratio <- row$matching_ratio
    }
    data.table::data.table(
      enrollment_id = eid,
      additional_criteria = label,
      exposure_variable = exp_info$variable,
      exposed_value = exp_info$exposed,
      comparator_value = exp_info$comparator,
      matching_ratio = exp_info$ratio,
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
.prepare_combine_data <- function(plan, slot) {
  results_list <- lapply(plan$results_ett, function(r) {
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

  ett_desc <- setNames(
    vapply(plan$results_ett, `[[`, character(1), "description"),
    names(plan$results_ett)
  )

  list(wrapped = wrapped, ett_desc = ett_desc[names(wrapped)])
}

#' @noRd
.write_combined_rates <- function(wb, sheet_name, plan, slot, title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  start_row <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
    start_row <- 3L
  }
  prep <- .prepare_combine_data(plan, slot)
  if (is.null(prep)) {
    openxlsx::writeData(wb, sheet_name, "No valid rates results.")
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_rates_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  openxlsx::writeData(wb, sheet_name, dt, startRow = start_row)
}

#' @noRd
.write_combined_irr <- function(wb, sheet_name, plan, slot, title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  start_row <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
    start_row <- 3L
  }
  prep <- .prepare_combine_data(plan, slot)
  if (is.null(prep)) {
    openxlsx::writeData(wb, sheet_name, "No valid IRR results.")
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_irr_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  openxlsx::writeData(wb, sheet_name, dt, startRow = start_row)
}

#' @noRd
.write_consort <- function(wb, sheet_name, ec, eid, label) {
  openxlsx::addWorksheet(wb, sheet_name)
  title <- paste0("Participant flow -- Enrollment ", eid, " (", label, ")")
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)

  att <- ec$attrition
  # Aggregate across trial_ids for overall counts
  overall <- att[,
    .(
      n_person_trials = sum(n_person_trials),
      n_exposed = sum(n_exposed),
      n_unexposed = sum(n_unexposed)
    ),
    by = criterion
  ]
  overall[, criterion := factor(criterion, levels = unique(criterion))]
  data.table::setorder(overall, criterion)

  fmt <- function(x) format(x, big.mark = ",")
  blank <- ""

  rows <- list()
  add <- function(step, rem, rem_exp, rem_unexp, excl, excl_exp, excl_unexp) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      Step = step,
      Remaining = rem,
      `Remaining Exposed` = rem_exp,
      `Remaining Comparator` = rem_unexp,
      Excluded = excl,
      `Excluded Exposed` = excl_exp,
      `Excluded Comparator` = excl_unexp
    )
  }

  all_totals <- overall$n_person_trials
  all_exposed <- overall$n_exposed
  all_unexposed <- overall$n_unexposed

  for (j in seq_len(nrow(overall))) {
    crit <- as.character(overall$criterion[j])
    tot <- all_totals[j]
    exp <- all_exposed[j]
    unexp <- all_unexposed[j]

    if (crit == "before_exclusions") {
      add("Before exclusions", fmt(tot), fmt(exp), fmt(unexp), blank, blank, blank)
    } else {
      d_tot <- all_totals[j - 1L] - tot
      d_exp <- all_exposed[j - 1L] - exp
      d_unexp <- all_unexposed[j - 1L] - unexp
      # Exclusion row (right columns only)
      add(paste0("  Excluded: ", crit), blank, blank, blank,
          fmt(d_tot), fmt(d_exp), fmt(d_unexp))
      # Remaining row (left columns only)
      add(paste0("Remaining"), fmt(tot), fmt(exp), fmt(unexp), blank, blank, blank)
    }
  }

  # Post-matching row
  if (!is.null(ec$matching)) {
    m <- ec$matching
    n_exp <- sum(m$n_exposed_enrolled, na.rm = TRUE)
    n_unexp <- sum(m$n_unexposed_enrolled, na.rm = TRUE)
    add("Post-matching (enrolled)", fmt(n_exp + n_unexp), fmt(n_exp), fmt(n_unexp),
        blank, blank, blank)
  }

  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, sheet_name, dt, startRow = 3L)

  # Bold non-indented rows
  bold_style <- openxlsx::createStyle(textDecoration = "bold")
  for (i in seq_len(nrow(dt))) {
    if (!startsWith(dt$Step[i], "  ")) {
      openxlsx::addStyle(wb, sheet_name, bold_style,
                         rows = i + 3L, cols = 1L)
    }
  }
  openxlsx::setColWidths(wb, sheet_name, cols = 1:7,
                         widths = c(55, 18, 18, 18, 18, 18, 18))
}

#' @noRd
.write_combined_baseline <- function(wb, sheet_name, plan, eid) {
  openxlsx::addWorksheet(wb, sheet_name)
  label <- .enrollment_label(plan, eid)
  title <- paste0("Enrollment ", eid, " (", label, ") -- Baseline characteristics")
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)

  r <- plan$results_enrollment[[eid]]
  if (is.null(r)) {
    openxlsx::writeData(wb, sheet_name, "No results for this enrollment.", startRow = 3L)
    return(invisible(NULL))
  }

  # Convert each tableone to df
  panels <- list(
    Raw = r$table1_raw,
    Unweighted = r$table1_unweighted,
    IPW = r$table1_ipw,
    `IPW Truncated` = r$table1_ipw_trunc
  )

  dfs <- list()
  for (name in names(panels)) {
    if (!is.null(panels[[name]])) {
      dfs[[name]] <- .tableone_to_df(panels[[name]])
    }
  }
  if (length(dfs) == 0L) return(invisible(NULL))

  # Write with merged group headers
  start_col <- 1L
  header_row <- 2L
  data_row <- 3L

  for (name in names(dfs)) {
    df <- dfs[[name]]
    ncols <- ncol(df)
    # Merge cells for group header (row 2)
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
    # Bold the group header
    openxlsx::addStyle(
      wb, sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", halign = "center"),
      rows = header_row, cols = start_col
    )
    # Write data (includes column headers in row 3, data from row 4)
    openxlsx::writeData(
      wb, sheet_name, df,
      startCol = start_col, startRow = data_row
    )
    start_col <- start_col + ncols + 1L  # gap column between panels
  }
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

#' Read skeleton, apply exclusions, optionally derive confounders, set exposure.
#' Shared by `.s1a_worker()` (scout, no confounders) and `.s1b_worker()` (full).
#' @noRd
.s1_prepare_skeleton <- function(
  enrollment_spec,
  file_path,
  spec,
  derive_confounders = TRUE
) {
  baseline_exposed <- rd_exposed <- eligible_valid_exposure <- isoyearweek <- id <- NULL
  data.table::setDTthreads(enrollment_spec$n_threads)
  skeleton <- qs2_read(file_path, nthreads = enrollment_spec$n_threads)
  # Skeleton is already sorted by (id, isoyearweek) from create_skeleton();
  # qs2 preserves row order so setkey is an O(n) verification, not a full sort.
  # Enables binary-search grouping for the repeated by=list(id) calls in
  # exclusion and confounder functions.
  data.table::setkey(skeleton, id, isoyearweek)
  skeleton <- tteplan_apply_exclusions(skeleton, spec, enrollment_spec)
  if (derive_confounders) {
    skeleton <- tteplan_apply_derived_confounders(skeleton, spec)
  }
  x_exp <- enrollment_spec$exposure_impl
  skeleton[,
    rd_exposed := data.table::fcase(
      get(x_exp$variable) == x_exp$exposed_value    , TRUE  ,
      get(x_exp$variable) == x_exp$comparator_value , FALSE ,
      default = NA
    )
  ]
  skeleton[, baseline_exposed := rd_exposed]

  # Mark rows with valid (non-NA) exposure as the first exclusion criterion
  skeleton[, eligible_valid_exposure := !is.na(rd_exposed)]

  # Prepend to eligible_cols so it appears first in attrition reporting
  eligible_cols <- attr(skeleton, "eligible_cols")
  data.table::setattr(skeleton, "eligible_cols", c("eligible_valid_exposure", eligible_cols))

  # Re-combine eligible column to include valid_exposure
  skeleton_eligible_combine(skeleton, attr(skeleton, "eligible_cols"))

  skeleton
}


#' Get all eligible (person_id, trial_id, exposed) tuples from a skeleton.
#' Used by `.s1a_worker()` for scouting and available for direct use.
#' Caller should pre-sort by (pid, trial_id, isoyearweek) for efficiency.
#' @noRd
.s1_eligible_tuples <- function(skeleton, design) {
  . <- rd_exposed <- NULL
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
  # any() not first(): exposure can start at any week within a trial period,

  # not just the first. first() silently drops ~75% of exposed people whose
  # MHT initiation falls mid-period. The no_prior_exposure exclusion criterion
  # handles the new-user restriction (one-time initiation) separately.
  eligible_rows[,
    .(exposed = any(rd_exposed, na.rm = TRUE)),
    by = c(pid, "trial_id")
  ]
}


# --- Attrition helper -------------------------------------------------------

#' Compute cumulative attrition counts per eligibility criterion.
#'
#' Returns a long-format data.table with one row per (trial_id, criterion),
#' showing how many persons and person-trials remain after cumulatively
#' applying each eligibility criterion. Includes a "before_exclusions" row
#' and per-step exposed/unexposed counts for TARGET Item 8 reporting.
#'
#' @param skeleton data.table with trial_id and eligible_* columns assigned.
#' @param eligible_cols Character vector of eligible_* column names in
#'   application order.
#' @param pid Character, person ID column name.
#' @param exposure_var Character, name of the exposure column (default
#'   `"rd_exposed"`).
#' @return data.table with columns: trial_id, criterion, n_persons,
#'   n_person_trials, n_exposed, n_unexposed.
#' @noRd
.s1_compute_attrition <- function(skeleton, eligible_cols, pid,
                                  exposure_var = "rd_exposed") {
  .tte_pid <- .tte_exp <- .tte_exp_any <- trial_id <- . <- criterion <- NULL
  if (is.null(eligible_cols) || length(eligible_cols) == 0L) {
    stop("eligible_cols must be a non-empty character vector")
  }

  # Subset to needed columns for efficiency
  .cols <- c(pid, "trial_id", eligible_cols, exposure_var)
  sk <- skeleton[, .cols, with = FALSE]

  # Alias pid and exposure columns to fixed names for j-expressions
  data.table::setnames(sk, c(pid, exposure_var), c(".tte_pid", ".tte_exp"))

  # "before_exclusions" row --total person-trials and exposure counts.
  # Exposure is classified with any(): a person-trial is "exposed" if ANY
  # week within the trial period has .tte_exp == TRUE. This matches the
  # any() logic in .s1_eligible_tuples().
  pt0 <- sk[, .(
    .tte_exp_any = any(.tte_exp == TRUE, na.rm = TRUE)
  ), by = c(".tte_pid", "trial_id")]
  before_row <- pt0[, .(
    n_persons = data.table::uniqueN(.tte_pid),
    n_person_trials = .N,
    n_exposed = sum(.tte_exp_any == TRUE),
    n_unexposed = sum(.tte_exp_any == FALSE)
  ), by = trial_id]
  before_row[, criterion := "before_exclusions"]

  # For each cumulative criterion level, filter the full skeleton to rows where
  # ALL criteria 1..i pass, then classify exposure per person-trial using
  # any() --a person-trial is "exposed" if ANY eligible week within the
  # trial period has .tte_exp == TRUE. This matches .s1_eligible_tuples().
  rows <- vector("list", length(eligible_cols))
  cumulative_mask <- rep(TRUE, nrow(sk))

  for (i in seq_along(eligible_cols)) {
    cumulative_mask <- cumulative_mask & (sk[[eligible_cols[i]]] == TRUE)
    filtered <- sk[cumulative_mask]
    pt_i <- filtered[, .(
      .tte_exp_any = any(.tte_exp == TRUE, na.rm = TRUE)
    ), by = c(".tte_pid", "trial_id")]
    rows[[i]] <- pt_i[,
      .(
        n_persons = data.table::uniqueN(.tte_pid),
        n_person_trials = .N,
        n_exposed = sum(.tte_exp_any == TRUE),
        n_unexposed = sum(.tte_exp_any == FALSE)
      ),
      by = trial_id
    ][, criterion := eligible_cols[i]]
  }

  # sk is a local copy (column subset), no need to restore names

  data.table::rbindlist(c(list(before_row), rows), use.names = TRUE)
}


# --- s1a: Scout worker (lightweight, no confounders) ------------------------

#' Scout worker for pass 1a: returns eligible tuples and attrition counts.
#'
#' Reads skeleton, applies exclusions (no confounders), sets exposure, then
#' computes per-criterion attrition and extracts eligible tuples.
#'
#' @param enrollment_spec Enrollment spec list.
#' @param file_path Path to a skeleton `.qs2` file.
#' @param spec Parsed study spec.
#' @param cache_path Optional file path to save the prepared skeleton for s1b
#'   reuse. If non-NULL, the skeleton (with exclusions + exposure applied) is
#'   written to this path via `qs2::qs_save()`.
#' @return A list with two elements:
#'   \describe{
#'     \item{tuples}{data.table with columns: person_id_var, trial_id, exposed,
#'       enrollment_person_trial_id.}
#'     \item{attrition}{data.table with columns: trial_id, criterion, n_persons,
#'       n_person_trials, n_exposed, n_unexposed.}
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
#' Reads skeleton, applies exclusions + confounders, sets exposure, then
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
    # Reuse cached skeleton from s1a (already has exclusions + exposure applied)
    data.table::setDTthreads(enrollment_spec$n_threads)
    skeleton <- qs2_read(cache_path, nthreads = enrollment_spec$n_threads)
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
#' @param sep_by_exp Logical, estimate IPCW separately by exposure.
#' @param with_gam Logical, use GAM for IPCW estimation.
#' @return TRUE on success.
#' @noRd
.s2_worker <- function(
  outcome,
  follow_up,
  file_imp_path,
  file_analysis_path,
  n_threads,
  sep_by_exp,
  with_gam
) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(file_imp_path, nthreads = n_threads)
  enrollment$s4_prepare_for_analysis(
    outcome = outcome,
    follow_up = follow_up,
    estimate_ipcw_pp_separately_by_exposure = sep_by_exp,
    estimate_ipcw_pp_with_gam = with_gam
  )
  qs2::qs_save(enrollment, file_analysis_path, nthreads = n_threads)
  TRUE
}


# --- s3_enrollment_worker: Loop 3a enrollment-level baseline worker -----------

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
#' @return A named list with enrollment-level results.
#' @noRd
.s3_enrollment_worker <- function(analysis_path, raw_path, enrollment_id,
                                  n_threads) {
  data.table::setDTthreads(n_threads)
  enrollment <- swereg::qs2_read(analysis_path, nthreads = n_threads)

  table1_unweighted <- enrollment$table1()
  table1_ipw_trunc <- tryCatch(
    enrollment$table1(ipw_col = "ipw_trunc"),
    error = function(e) {
      warning("table1 ipw_trunc failed for ", enrollment_id, ": ",
              conditionMessage(e))
      NULL
    }
  )
  table1_ipw <- tryCatch(
    enrollment$table1(ipw_col = "ipw"),
    error = function(e) {
      warning("table1 ipw failed for ", enrollment_id, ": ",
              conditionMessage(e))
      NULL
    }
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
      enrollment_raw$table1(),
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
    n_baseline = n_baseline,
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
#'   \item Each enrollment must have `id` and `exposure$implementation$variable`
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

  # Validate outcomes
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
  }

  # Validate enrollments
  for (i in seq_along(spec$enrollments)) {
    enr <- spec$enrollments[[i]]
    if (is.null(enr$id)) {
      stop("enrollments[", i, "] is missing 'id'")
    }
    if (is.null(enr$exposure$implementation$variable)) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing exposure$implementation$variable"
      )
    }
    if (is.null(enr$exposure$implementation$matching_ratio)) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing exposure$implementation$matching_ratio"
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
        # Auto-derive variable name from source_variable + window
        spec$confounders[[i]]$implementation$variable <- paste0(
          "rd_no_",
          conf$implementation$source_variable,
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
      }
    }
  }

  # 3. Global exclusion criteria
  for (ec in spec$exclusion_criteria) {
    impl <- ec$implementation

    if (identical(impl$window, "lifetime_before_and_after_baseline")) {
      col_name <- paste0(
        "eligible_no_",
        impl$source_variable,
        "_lifetime_before_and_after_baseline"
      )
      skeleton <- skeleton_eligible_no_events_lifetime_before_and_after_baseline(
        skeleton,
        event_var = impl$source_variable,
        col_name = col_name
      )
    } else if (identical(impl$type, "no_prior_exposure")) {
      window <- impl$window_weeks
      col_name <- paste0(
        "eligible_no_",
        impl$source_variable,
        "_",
        .window_label(window)
      )
      skeleton <- skeleton_eligible_no_observation_in_window_excluding_wk0(
        skeleton,
        var = impl$source_variable,
        value = impl$exposure_value,
        window = window,
        col_name = col_name
      )
    } else {
      window <- impl$window_weeks
      col_name <- paste0(
        "eligible_no_",
        impl$source_variable,
        "_",
        .window_label(window)
      )
      skeleton <- skeleton_eligible_no_events_in_window_excluding_wk0(
        skeleton,
        event_var = impl$source_variable,
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

      if (identical(impl$window, "lifetime_before_and_after_baseline")) {
        col_name <- paste0(
          "eligible_no_",
          impl$source_variable,
          "_lifetime_before_and_after_baseline"
        )
        skeleton <- skeleton_eligible_no_events_lifetime_before_and_after_baseline(
          skeleton,
          event_var = impl$source_variable,
          col_name = col_name
        )
      } else if (identical(impl$type, "no_prior_exposure")) {
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_no_",
          impl$source_variable,
          "_",
          .window_label(window)
        )
        skeleton <- skeleton_eligible_no_observation_in_window_excluding_wk0(
          skeleton,
          var = impl$source_variable,
          value = impl$exposure_value,
          window = window,
          col_name = col_name
        )
      } else {
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_no_",
          impl$source_variable,
          "_",
          .window_label(window)
        )
        skeleton <- skeleton_eligible_no_events_in_window_excluding_wk0(
          skeleton,
          event_var = impl$source_variable,
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
      skeleton <- skeleton_eligible_no_events_in_window_excluding_wk0(
        skeleton,
        event_var = impl$source_variable,
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
    var <- ec$implementation$source_variable
    n_checked <- n_checked + 1L
    if (!var %in% skel_cols) {
      errors <- c(
        errors,
        paste0(
          "exclusion_criteria '",
          ec$name,
          "': source_variable '",
          var,
          "' not found in skeleton"
        )
      )
    }
  }

  # --- Outcomes ---
  for (i in seq_along(spec$outcomes)) {
    out <- spec$outcomes[[i]]
    var <- out$implementation$variable
    n_checked <- n_checked + 1L
    if (!var %in% skel_cols) {
      errors <- c(
        errors,
        paste0(
          "outcomes '",
          out$name,
          "': variable '",
          var,
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
      if (!impl$source_variable %in% skel_cols) {
        errors <- c(
          errors,
          paste0(
            "confounders '",
            conf$name,
            "': source_variable '",
            impl$source_variable,
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
    exp_impl <- enr$exposure$implementation

    # Exposure variable
    n_checked <- n_checked + 1L
    if (!exp_impl$variable %in% skel_cols) {
      errors <- c(
        errors,
        paste0(
          "enrollments '",
          enr$name %||% enr$id,
          "': exposure variable '",
          exp_impl$variable,
          "' not found in skeleton"
        )
      )
    } else {
      # Check exposed_value and comparator_value are present in data
      data_values <- unique(skeleton[[exp_impl$variable]])
      if (!exp_impl$exposed_value %in% data_values) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': exposed_value '",
            exp_impl$exposed_value,
            "' not found in column '",
            exp_impl$variable,
            "'"
          )
        )
      }
      if (!exp_impl$comparator_value %in% data_values) {
        errors <- c(
          errors,
          paste0(
            "enrollments '",
            enr$name %||% enr$id,
            "': comparator_value '",
            exp_impl$comparator_value,
            "' not found in column '",
            exp_impl$variable,
            "'"
          )
        )
      }
    }

    # Additional inclusion variables
    if (!is.null(enr$additional_inclusion)) {
      for (ae in enr$additional_inclusion) {
        if (!is.null(ae$implementation$variable)) {
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
#' follow-up) from the parsed study specification. Also stores each
#' enrollment's exposure implementation details in the ETT data.table so
#' they are available via `plan[[i]]$exposure_impl`.
#'
#' @param spec Character path to a YAML spec file, or a parsed spec list
#'   from [tteplan_read_spec()]. When a path is given, the version extracted
#'   from the filename (`_vNNN.yaml`) is validated against
#'   `spec$study$implementation$version`.
#' @param study A [RegistryStudy] object. The `$skeleton_files` active
#'   binding is used to obtain the skeleton file paths.
#' @param n_skeleton_files Optional integer: if not NULL, only the first
#'   `n_skeleton_files` files are used (for faster dev iterations).
#' @param global_max_isoyearweek Administrative censoring boundary
#'   (isoyearweek string, e.g., "2023-52"). If `NULL` (default), auto-detected
#'   from `max(isoyearweek)` in the first skeleton file. Also runs
#'   [tteplan_validate_spec()] on that skeleton.
#' @param period_width Integer, band width in weeks for enrollment and
#'   time aggregation (default: 4L). Stored on the plan and passed through
#'   to TTEDesign.
#' @return A [TTEPlan] object with the full ETT grid.
#'
#' @family tte_spec
#' @export
tteplan_from_spec_and_registrystudy <- function(
  spec,
  study,

  n_skeleton_files = NULL,
  global_max_isoyearweek = NULL,
  period_width = 4L
) {
  isoyearweek <- exposure_impl <- matching_ratio <- seed <- NULL
  # Resolve spec: path -> parsed list
  if (is.character(spec) && length(spec) == 1) {
    spec_path <- spec
    # Extract version from filename (_vNNN.yaml)
    filename_version <- regmatches(
      basename(spec_path),
      regexec("_(v\\d+)\\.yaml$", basename(spec_path))
    )[[1]][2]
    spec <- tteplan_read_spec(spec_path)
    if (!is.na(filename_version)) {
      spec_version <- spec$study$implementation$version
      if (!identical(spec_version, filename_version)) {
        stop(
          "Version mismatch: filename has '",
          filename_version,
          "' but spec YAML has '",
          spec_version %||% "NULL",
          "'"
        )
      }
    }
  }

  # Resolve skeleton_files from RegistryStudy
  skeleton_files <- study$skeleton_files

  # Apply n_skeleton_files limit
  if (!is.null(n_skeleton_files)) {
    skeleton_files <- utils::head(skeleton_files, n_skeleton_files)
  }
  skeleton_created_at <- NULL
  if (is.null(global_max_isoyearweek)) {
    skeleton <- qs2_read(skeleton_files[1])
    tteplan_validate_spec(spec, skeleton)
    global_max_isoyearweek <- skeleton[, max(isoyearweek, na.rm = TRUE)]
    message("Admin censoring cutoff from skeleton: ", global_max_isoyearweek)
    skeleton_created_at <- attr(skeleton, "created_at")
    rm(skeleton)
  } else if (file.exists(skeleton_files[1])) {
    skeleton <- qs2_read(skeleton_files[1])
    skeleton_created_at <- attr(skeleton, "created_at")
    rm(skeleton)
  }

  project_prefix <- spec$study$implementation$project_prefix

  # Extract confounder variable names
  confounder_vars <- vapply(
    spec$confounders,
    function(c) c$implementation$variable,
    character(1)
  )

  plan <- TTEPlan$new(
    project_prefix = project_prefix,
    skeleton_files = skeleton_files,
    global_max_isoyearweek = global_max_isoyearweek
  )
  plan$period_width <- as.integer(period_width)
  plan$spec <- spec
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
          outcome_var = outcome$implementation$variable,
          outcome_name = outcome$name,
          follow_up = fu$weeks,
          confounder_vars = confounder_vars,
          time_exposure_var = "rd_exposed",
          eligible_var = "eligible",
          argset = list(
            age_group = age_group,
            age_min = age_min,
            age_max = age_max
          )
        )
      }
    }

    # Store exposure implementation in the ETT for this enrollment
    impl <- enrollment$exposure$implementation
    rows <- plan$ett$enrollment_id == enrollment$id
    plan$ett[rows, exposure_impl := list(list(impl))]
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
