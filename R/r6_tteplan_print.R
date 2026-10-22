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
  return(invisible(self))
})

# =========================================================================
# Methods
# =========================================================================

#' @description Print a target trial specification summary.
#' Console-friendly summary derived from the study specification stored
#' on this plan. When `$code_registry` is available, variable names are
#' shown in red and matched code details in blue (ANSI colors).
#' A spec with a `sparse_rule` also prints the rare-condition rule: `min_n`,
#' the driving enrollments, the candidates and the decision. Before s1 the
#' decision reads `not decided yet (s1 has not run)`. A stored decision prints
#' only when the rule in `$spec` still hashes to `$sparse_rule_hash` and s1
#' completed for it. Otherwise the decision reads
#' `not decided yet (s1 has not completed for this rule)`.
#' It also prints `horizon_rule$min_expected_events`, the minimum number of
#' expected events per arm in each follow-up year.
#' @return `invisible(NULL)`
TTEPlan$set("public", "print_spec_summary", function() {
  return(.plan_print_spec_summary(self))
})


# The body of `print_spec_summary()` above. It lived in
# `R/tteplan_reporting.R` until 2026-09-22, and moved here to keep that
# file under the 1000-code-line `static-checks` gate. This file holds the
# method, so the body sits beside its one caller.

#' Print the target trial specification summary
#'
#' The body of `TTEPlan$print_spec_summary()`.
#'
#' @param plan A `TTEPlan`.
#' @return `invisible(NULL)`.
#' @noRd
.plan_print_spec_summary <- function(plan) {
  spec <- plan$spec
  if (is.null(spec)) {
    stop("plan has no spec", call. = FALSE)
  }

  # ANSI color/style helpers
  bold <- function(x) paste0("\033[1m", x, "\033[0m")
  green <- function(x) paste0("\033[92m", x, "\033[0m")
  cyan <- function(x) paste0("\033[36m", x, "\033[0m")
  magenta <- function(x) paste0("\033[95m", x, "\033[0m")
  yellow <- function(x) paste0("\033[93m", x, "\033[0m")

  # Build code lookup if registry available
  cl <- .build_code_lookup(plan, colorize = TRUE)
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
    return(bold(padded))
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
  if (!is.null(plan$registry_study_created_at)) {
    cat(
      lbl("RegistryStudy:"),
      format(plan$registry_study_created_at, "%Y-%m-%d %H:%M:%S"),
      "\n",
      sep = ""
    )
  }

  # Skeletons (nested under RegistryStudy)
  n_skeletons <- length(plan$skeleton_files)
  n_expected <- plan$expected_skeleton_file_count
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
  if (!is.null(plan$skeleton_created_at)) {
    cat(
      skel_label,
      format(plan$skeleton_created_at, "%Y-%m-%d %H:%M:%S"),
      " (",
      skel_detail,
      ")\n",
      sep = ""
    )
  } else {
    cat(skel_label, "(", skel_detail, ")\n", sep = "")
  }

  if (!is.null(plan$created_at)) {
    cat(
      lbl("TTEPlan:"),
      format(plan$created_at, "%Y-%m-%d %H:%M:%S"),
      "\n",
      sep = ""
    )
  }
  if (!is.null(plan$expected_n_ids)) {
    cat(
      lbl("Individuals:"),
      format(plan$expected_n_ids, big.mark = ","),
      " (expected)\n",
      sep = ""
    )
  }
  if (!is.null(plan$global_max_isoyearweek)) {
    cat(
      lbl("Admin censoring:"),
      plan$global_max_isoyearweek,
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

  # Inclusion criteria. The `criteria` container applies to every enrollment,
  # so it prints once here and never inside the enrollment loop below.
  cat(bold("Inclusion criteria (global):"), "\n")
  iso <- spec$inclusion_criteria$isoyears
  cat("  Isoyears: ", iso[1], "-", iso[2], "\n", sep = "")
  for (ic in spec[["inclusion_criteria"]][["criteria"]] %||% list()) {
    cat("  -", ic$name, "\n")
    cat(
      "    Variable:   ",
      fmt_var(
        ic$implementation$source_variable_combined %||%
          ic$implementation$source_variable
      ),
      "\n"
    )
    cat(
      "    Window:     ",
      .tte_inclusion_window_human(ic$implementation),
      "\n"
    )
  }
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

  .plan_print_sparse_rule(plan, bold)
  .plan_print_horizon_rule(plan, bold)

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
        ai_type <- .tte_entry_type(ai)
        rule <- .tte_washout_prose(ai$implementation)
        if (identical(ai_type, "age_range")) {
          cat(sprintf("      %-18s%d-%d\n", "Age range:", ai$min, ai$max))
        } else if (identical(ai_type, "isoyear_range")) {
          cat(sprintf("      %-18s%d-%d\n", "Isoyears:", ai$min, ai$max))
        } else if (isTRUE(ai_type %in% .TTE_INCLUSION_RULE_TYPES)) {
          cat("      -", ai$name %||% rule, "\n")
          cat(
            "        Variable:    ",
            fmt_var(
              ai$implementation$source_variable_combined %||%
                ai$implementation$source_variable
            ),
            "\n"
          )
          if (!is.null(rule)) {
            cat("        Rule:        ", rule, "\n")
          }
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

  return(invisible(NULL))
}


#' Print the rare-condition rule of the spec summary
#'
#' Prints the rows of the Study Specification sheet: `min_n`, the driving
#' enrollments, the candidates, and the decision. The two style arguments of
#' `.tte_sparse_spec_rows()` only mark the header row here. A spec without a
#' `sparse_rule` prints nothing.
#'
#' @param plan A `TTEPlan`.
#' @param bold A function that makes its text bold.
#' @return `invisible(NULL)`.
#' @noRd
.plan_print_sparse_rule <- function(plan, bold) {
  for (rw in .tte_sparse_spec_rows(plan, "header", "label")) {
    if (identical(rw$sa, "header")) {
      cat(bold(paste0(rw$a, ":")), "\n")
    } else if (!is.na(rw$b)) {
      cat("  ", formatC(rw$a, width = -22, flag = "-"), rw$b, "\n", sep = "")
    } else {
      cat("\n")
    }
  }
  return(invisible(NULL))
}


#' Print the horizon rule of the spec summary
#'
#' Prints `horizon_rule$min_expected_events`. A plan saved before the block
#' existed holds no `horizon_rule`. It prints `not declared` and names
#' `$reload_spec()`, which copies the block from the spec file.
#'
#' @param plan A `TTEPlan`.
#' @param bold A function that makes its text bold.
#' @return `invisible(NULL)`.
#' @noRd
.plan_print_horizon_rule <- function(plan, bold) {
  value <- plan$spec$horizon_rule[["min_expected_events"]]
  shown <- if (is.null(value)) {
    "not declared (call $reload_spec() with a spec that declares it)"
  } else {
    paste0(format(value), " per arm, per follow-up year")
  }
  cat(bold("Horizon rule:"), "\n")
  cat(
    "  ",
    formatC("Min expected events", width = -22, flag = "-"),
    shown,
    "\n",
    sep = ""
  )
  cat("\n")
  return(invisible(NULL))
}
