# This file holds the TTEPlan method that prints the TARGET reporting
# checklist.

#' @include r6_tteplan.R
#' @description Print a TARGET-aligned reporting checklist.
#'
#' Generates a self-contained document following the TARGET Statement
#' (Cashin et al., JAMA 2025) 21-item checklist for transparent reporting
#' of target trial emulations. Each item includes the full TARGET
#' description, auto-filled content from the swereg spec where available,
#' and `[FILL IN]` placeholders for PI completion.
#'
#' Item 6h states the minimum-events rule from
#' `spec$min_expected_events_per_arm_per_year`. The method stops, and names
#' `$s3_analyze()`, when a stored result was judged with another value.
#'
#' @return `invisible(NULL)`
TTEPlan$set("public", "print_target_checklist", function() {
  return(.plan_print_target_checklist(self))
})
