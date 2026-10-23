# =============================================================================
# The min_expected_events_per_arm_per_year spec key
# =============================================================================
# Every spec MUST declare the flat key of the minimum-events rule:
#
#   min_expected_events_per_arm_per_year: 5    # number >= 0
#
# `tteplan_read_spec()` refuses a spec without the key, through its vector of
# required sections. `.tte_check_horizon_rule()` below checks the value. The
# key gate in `R/tteplan_spec_schema.R` refuses a mapping under the key, and
# refuses the retired block `horizon_rule`.
# =============================================================================

#' Validate and normalise the minimum-events rule of a parsed specification
#'
#' `tteplan_read_spec()` calls it after its check of the required sections.
#' That check refuses a spec without the key, so this function returns a spec
#' without the key unchanged. It stops on a key with no value, or with a bad
#' value. It returns the specification with
#' `min_expected_events_per_arm_per_year` as a double.
#'
#' @param spec The parsed specification.
#' @return `spec`.
#' @noRd
.tte_check_horizon_rule <- function(spec) {
  if (!"min_expected_events_per_arm_per_year" %in% names(spec)) {
    return(spec)
  }
  value <- spec[["min_expected_events_per_arm_per_year"]]
  # A key written with no value, or with an empty mapping, is a rule the
  # author started and did not finish.
  if (length(value) == 0L) {
    stop(
      "min_expected_events_per_arm_per_year is present but has no value. ",
      "Give it the minimum number of expected events per arm and per ",
      "follow-up year, as in min_expected_events_per_arm_per_year: 5.",
      call. = FALSE
    )
  }
  .tte_check_min_expected_events(value)
  spec[["min_expected_events_per_arm_per_year"]] <- as.numeric(value)
  return(spec)
}

#' Stop unless the minimum-events value is one finite number of at least 0
#'
#' Every message names `min_expected_events_per_arm_per_year`. A logical value
#' is refused, because R would read `true` as 1.
#'
#' @param value The parsed `min_expected_events_per_arm_per_year`.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_check_min_expected_events <- function(value) {
  if (!is.numeric(value) || length(value) != 1L) {
    stop(
      "min_expected_events_per_arm_per_year MUST be one number. It holds: ",
      paste(format(value), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.finite(value)) {
    stop(
      "min_expected_events_per_arm_per_year MUST be a finite number. It ",
      "holds: ",
      format(value),
      ".",
      call. = FALSE
    )
  }
  if (value < 0) {
    stop(
      "min_expected_events_per_arm_per_year MUST be at least 0. It holds: ",
      format(value),
      ".",
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}
