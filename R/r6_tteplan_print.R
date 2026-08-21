# These TTEPlan methods print a plan to the console. The `Methods` banner
# below travels with print_spec_summary() from the class body.

#' @include r6_tteplan.R
#' @description Print the TTEPlan object.
#' @param ... Ignored.
TTEPlan$set("public", "print", function(...) {
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
})

# =========================================================================
# Methods
# =========================================================================

#' @description Print a target trial specification summary.
#' Console-friendly summary derived from the study specification stored
#' on this plan. When `$code_registry` is available, variable names are
#' shown in red and matched code details in blue (ANSI colors).
#' @return `invisible(NULL)`
TTEPlan$set("public", "print_spec_summary", function() {
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
      "      %-34s%s\n",
      "Variable:",
      fmt_var(tx$implementation$variable)
    ))
    cat(sprintf(
      "      %-34s%s <- %s\n",
      "Intervention:",
      tx$arms$intervention,
      yellow(tx$implementation$intervention_value)
    ))
    cat(sprintf(
      "      %-34s%s <- %s\n",
      "Comparator:",
      tx$arms$comparator,
      yellow(tx$implementation$comparator_value)
    ))
    # The spec's number sizes the comparator side of the draw, so it
    # prints on the left of the colon. The label names both sides, because
    # the bare digits read either way.
    cat(sprintf(
      "      %-34s%d:1\n",
      "Comparator-to-intervention ratio:",
      tx$implementation$comparator_to_intervention_ratio
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
})
