# =============================================================================
# TTEPlan R6 class and spec functions
# =============================================================================
# This file contains the planning-side R6 class and all standalone functions
# called by its methods:
#
#   1. TTEPlan R6 class
#   2. .s1_prepare_skeleton(), .s1_eligible_tuples() (shared helpers)
#   3. .s1a_worker(), .s1b_worker() (Loop 1 workers)
#   4. .s3_worker() (Loop 2 IPCW-PP worker, was .s2_worker)
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
    #'       n_persons, n_person_trials) showing cumulative attrition at each
    #'       eligibility step.}
    #'     \item{matching}{data.table (trial_id, n_exposed_total,
    #'       n_unexposed_total, n_exposed_enrolled, n_unexposed_enrolled).}
    #'   }
    enrollment_counts = NULL,

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
      code_lookup <- NULL
      st <- self$code_registry
      if (!is.null(st) && nrow(st) > 0) {
        code_lookup <- new.env(parent = emptyenv())
        for (i in seq_len(nrow(st))) {
          cols <- strsplit(st$generated_columns[i], ", ")[[1]]
          for (col in cols) {
            code_lookup[[col]] <- paste0(st$codes[i], " (", st$type[i], ")")
          }
        }
      }

      # Format a variable name with colors:
      #   code-registry match: cyan var + magenta codes
      #   no match: green var (direct skeleton column)
      fmt_var <- function(var) {
        if (is.null(code_lookup)) {
          return(var)
        }
        info <- code_lookup[[var]]
        if (!is.null(info)) {
          paste0(cyan(var), " <- ", magenta(info))
        } else {
          green(var)
        }
      }

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
      if (!is.null(self$created_at)) {
        cat(
          lbl("Plan created:"),
          format(self$created_at, "%Y-%m-%d %H:%M:%S"),
          "\n",
          sep = ""
        )
      }
      if (!is.null(self$registry_study_created_at)) {
        cat(
          lbl("Study created:"),
          format(self$registry_study_created_at, "%Y-%m-%d %H:%M:%S"),
          "\n",
          sep = ""
        )
      }
      if (!is.null(self$skeleton_created_at)) {
        cat(
          lbl("Skeletons created:"),
          format(self$skeleton_created_at, "%Y-%m-%d %H:%M:%S"),
          "\n",
          sep = ""
        )
      }

      # Skeleton files
      n_skeletons <- length(self$skeleton_files)
      n_expected <- self$expected_skeleton_file_count
      if (!is.null(n_expected) && n_skeletons != n_expected) {
        cat(
          lbl("Skeleton files:"),
          sprintf(
            "%d / %d expected \033[31m** WARNING: incomplete **\033[0m\n",
            n_skeletons,
            n_expected
          ),
          sep = ""
        )
      } else if (!is.null(n_expected)) {
        cat(
          lbl("Skeleton files:"),
          sprintf("%d / %d expected\n", n_skeletons, n_expected),
          sep = ""
        )
      } else {
        cat(lbl("Skeleton files:"), sprintf("%d\n", n_skeletons), sep = "")
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
      item(
        "6",
        "c",
        "Describe the assignment procedures.",
        "Describe how individuals were assigned to treatment strategies in the emulated trial.",
        "Per-band stratified matching with IPW adjustment for residual confounding."
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
          "IPW: logistic regression for P(A=1|L_baseline), stabilized\n",
          "IPCW-PP: GAM/GLM for censoring, marginal stabilization\n",
          "Outcome: weighted Poisson regression with ns(tstop, df=3) and trial_id\n",
          "Truncation: 1st/99th percentile weight winsorization"
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
        "See items 6a-6h above. Key emulation decisions to document:\n- Enrollment window width (period_width) and residual immortal time bias\n- Matching approach vs full-cohort IPW\n- Grace period handling"
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
              .(n_person_trials = sum(n_person_trials)),
              by = criterion
            ]
            lines <- vapply(
              seq_len(nrow(overall)),
              function(j) {
                sprintf(
                  "  After %s: %s person-trials",
                  overall$criterion[j],
                  format(overall$n_person_trials[j], big.mark = ",")
                )
              },
              character(1)
            )
            item8_parts <- c(
              item8_parts,
              paste0("Enrollment '", enr_id, "' attrition (cumulative):"),
              lines
            )
          }
          if (!is.null(ec$matching)) {
            m <- ec$matching
            item8_parts <- c(
              item8_parts,
              sprintf(
                "  Post-matching: %s exposed enrolled, %s unexposed enrolled",
                format(sum(m$n_exposed_enrolled, na.rm = TRUE), big.mark = ","),
                format(
                  sum(m$n_unexposed_enrolled, na.rm = TRUE),
                  big.mark = ","
                )
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
      ett_id <- paste0("ETT", sprintf("%02d", ett_num))
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
    s1_generate_enrollments_and_ipw = function(
      output_dir,
      impute_fn = tteenrollment_impute_confounders,
      stabilize = TRUE,
      n_workers = 3L,
      swereg_dev_path = NULL
    ) {
      # --- Loop 1: Create trial panels from skeleton files ---
      #
      # Two-pass pipeline:
      #
      #   Pass 1a (scout, parallel):
      #     skeleton files -> exclusions -> exposure -> eligible tuples
      #     Returns: (person_id, trial_id, exposed) per file
      #
      #   Match (main process):
      #     Combine all tuples -> per trial_id, keep all exposed,
      #     sample ratio * n_exposed unexposed -> enrolled_ids
      #
      #   Pass 1b (full enrollment, parallel):
      #     skeleton files -> exclusions + confounders -> exposure
      #     -> enroll with pre-matched enrolled_ids (skip matching)
      #     -> TTEEnrollment (panel-expanded)
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

      for (i in seq_len(nrow(ett_loop1))) {
        x_file_raw <- ett_loop1$file_raw[i]
        x_file_imp <- ett_loop1$file_imp[i]

        enrollment_spec <- self$enrollment_spec(i)
        enrollment_spec$n_threads <- n_threads

        # ---- Pass 1a: Scout (parallel) ----
        scout_items <- lapply(files, \(f) {
          list(
            enrollment_spec = enrollment_spec,
            file_path = f,
            spec = spec
          )
        })
        scout_results <- callr_pool(
          items = scout_items,
          worker_fn = .s1a_worker,
          n_workers = n_workers,
          swereg_dev_path = swereg_dev_path,
          p = p,
          item_labels = paste0("s1a:", basename(files))
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
            n_person_trials = sum(n_person_trials)
          ),
          by = .(trial_id, criterion)
        ]

        self$enrollment_counts[[enrollment_spec$enrollment_id]] <- list(
          attrition = attrition_summary,
          matching = matching_counts
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
        enroll_items <- lapply(files, \(f) {
          list(
            enrollment_spec = enrollment_spec,
            file_path = f,
            spec = spec,
            enrolled_ids = enrolled_ids
          )
        })
        results <- callr_pool(
          items = enroll_items,
          worker_fn = .s1b_worker,
          n_workers = n_workers,
          swereg_dev_path = swereg_dev_path,
          p = p,
          item_labels = paste0("s1b:", basename(files))
        )
        rm(enrolled_ids)

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
    s2_generate_analysis_files_and_ipcw_pp = function(
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
        nrow(ett),
        n_workers,
        n_threads
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
        seq_len(nrow(ett)),
        ett$outcome_var,
        ett$follow_up
      )

      p <- progressr::progressor(steps = length(items))
      callr_pool(
        items = items,
        worker_fn = .s3_worker,
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
# Package-level workers for Loop 1 and Loop 2 (not exported)
# =============================================================================

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
  data.table::setDTthreads(enrollment_spec$n_threads)
  skeleton <- qs2_read(file_path, nthreads = enrollment_spec$n_threads)
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
  skeleton
}


#' Get all eligible (person_id, trial_id, exposed) tuples from a skeleton.
#' Used by `.s1a_worker()` for scouting and available for direct use.
#' Caller should pre-sort by (pid, trial_id, isoyearweek) for efficiency.
#' @noRd
.s1_eligible_tuples <- function(skeleton, design) {
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
  eligible_rows[,
    .(exposed = data.table::first(rd_exposed)),
    by = c(pid, "trial_id")
  ]
}


# --- Attrition helper -------------------------------------------------------

#' Compute cumulative attrition counts per eligibility criterion.
#'
#' Returns a long-format data.table with one row per (trial_id, criterion),
#' showing how many persons and person-trials remain after cumulatively
#' applying each eligibility criterion.
#'
#' @param skeleton data.table with trial_id and eligible_* columns assigned.
#' @param eligible_cols Character vector of eligible_* column names in
#'   application order.
#' @param pid Character, person ID column name.
#' @return data.table with columns: trial_id, criterion, n_persons,
#'   n_person_trials.
#' @noRd
.s1_compute_attrition <- function(skeleton, eligible_cols, pid) {
  if (is.null(eligible_cols) || length(eligible_cols) == 0L) {
    stop("eligible_cols must be a non-empty character vector")
  }

  # Work at person-trial baseline level (first week per person-trial).
  # Caller must have already sorted by (pid, trial_id, isoyearweek).
  needed <- c(pid, "trial_id", eligible_cols)
  idx <- skeleton[, .I[1], by = c(pid, "trial_id")]$V1
  pt <- skeleton[idx, ..needed]

  # Alias pid column to avoid repeated get() dispatch in the loop
  has_alias <- pid != ".tte_pid"
  if (has_alias) data.table::setnames(pt, pid, ".tte_pid")

  rows <- vector("list", length(eligible_cols))
  cumulative <- rep(TRUE, nrow(pt))

  for (i in seq_along(eligible_cols)) {
    col <- eligible_cols[[i]]
    cumulative <- cumulative & (pt[[col]] == TRUE)
    rows[[i]] <- pt[
      cumulative,
      .(
        n_persons = data.table::uniqueN(.tte_pid),
        n_person_trials = .N
      ),
      by = trial_id
    ][, criterion := col]
  }

  if (has_alias) data.table::setnames(pt, ".tte_pid", pid)

  data.table::rbindlist(rows, use.names = TRUE)
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
#' @return A list with two elements:
#'   \describe{
#'     \item{tuples}{data.table with columns: person_id_var, trial_id, exposed,
#'       enrollment_person_trial_id.}
#'     \item{attrition}{data.table with columns: trial_id, criterion, n_persons,
#'       n_person_trials.}
#'   }
#' @noRd
.s1a_worker <- function(enrollment_spec, file_path, spec) {
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
#' @return A [TTEEnrollment] object at "trial" level (panel-expanded).
#' @noRd
.s1b_worker <- function(enrollment_spec, file_path, spec, enrolled_ids) {
  skeleton <- .s1_prepare_skeleton(
    enrollment_spec,
    file_path,
    spec,
    derive_confounders = TRUE
  )
  enrollment <- TTEEnrollment$new(
    data = skeleton,
    design = enrollment_spec$design,
    enrolled_ids = enrolled_ids,
    seed = enrollment_spec$seed,
    extra_cols = "isoyearweek"
  )
  rm(skeleton)

  # Prefix enrollment_person_trial_id with enrollment_id
  id_var <- enrollment$design$id_var
  enrollment$data[,
    (id_var) := paste0(enrollment_spec$enrollment_id, ".", get(id_var))
  ]
  enrollment
}


# --- s3_worker (was s2_worker): Loop 2 IPCW-PP worker -----------------------

#' Worker function for Loop 2: per-ETT IPCW-PP + save (internal)
#'
#' Loads an imputed enrollment file, runs `$s4_prepare_for_analysis()`, and saves
#' the analysis-ready file. Called via [callr_pool()] in a fresh R session.
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
.s3_worker <- function(
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

  attr(skeleton, "eligible_cols") <- eligible_cols
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

  issues <- character(0)
  n_checked <- 0L
  skel_cols <- names(skeleton)

  # --- Exclusion criteria ---
  for (i in seq_along(spec$exclusion_criteria)) {
    ec <- spec$exclusion_criteria[[i]]
    var <- ec$implementation$source_variable
    n_checked <- n_checked + 1L
    if (!var %in% skel_cols) {
      issues <- c(
        issues,
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
      issues <- c(
        issues,
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
        issues <- c(
          issues,
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
        issues <- c(
          issues,
          paste0(
            "confounders '",
            conf$name,
            "': variable '",
            impl$variable,
            "' not found in skeleton"
          )
        )
      } else if (!is.null(conf$categories)) {
        # Category check
        data_values <- unique(stats::na.omit(skeleton[[impl$variable]]))
        spec_values <- unlist(conf$categories)
        in_data_not_spec <- setdiff(data_values, spec_values)
        in_spec_not_data <- setdiff(spec_values, data_values)
        if (length(in_data_not_spec) > 0) {
          issues <- c(
            issues,
            paste0(
              "confounders '",
              conf$name,
              "': values in data but not spec: ",
              paste(in_data_not_spec, collapse = ", ")
            )
          )
        }
        if (length(in_spec_not_data) > 0) {
          issues <- c(
            issues,
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
      issues <- c(
        issues,
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
        issues <- c(
          issues,
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
        issues <- c(
          issues,
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
            issues <- c(
              issues,
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
          issues <- c(
            issues,
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

  if (length(issues) > 0) {
    warning(
      "Spec validation: ",
      length(issues),
      " issue(s):\n",
      paste0("  ", seq_along(issues), ". ", issues, collapse = "\n"),
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
