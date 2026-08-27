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
#' @return `invisible(NULL)`
TTEPlan$set("public", "print_spec_summary", function() {
  return(.plan_print_spec_summary(self))
})
