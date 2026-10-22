# =============================================================================
# The horizon_rule spec block
# =============================================================================
# Every spec MUST declare a `horizon_rule` block:
#
#   horizon_rule:
#     min_expected_events: 5    # number >= 0, per arm, per follow-up year
#
# `tteplan_read_spec()` refuses a spec without the block, through its vector
# of required sections. `.tte_check_horizon_rule()` below checks the value.
# The key gate in `R/tteplan_spec_schema.R` refuses any other key in the
# block.
# =============================================================================

#' Validate and normalise the horizon_rule block of a parsed specification
#'
#' `tteplan_read_spec()` calls it after its check of the required sections.
#' That check refuses a spec without the block, so this function returns a spec
#' without the block unchanged. It stops on a block that is empty, is not a
#' mapping, or holds a bad `min_expected_events`. It returns the specification
#' with `min_expected_events` as a double.
#'
#' @param spec The parsed specification.
#' @return `spec`.
#' @noRd
.tte_check_horizon_rule <- function(spec) {
  if (!"horizon_rule" %in% names(spec)) {
    return(spec)
  }
  rule <- spec[["horizon_rule"]]
  # A key written with no value, or with an empty mapping, is a rule the
  # author started and did not finish.
  if (length(rule) == 0L) {
    stop(
      "horizon_rule is present but empty. Give it min_expected_events, as in ",
      "horizon_rule: {min_expected_events: 5}.",
      call. = FALSE
    )
  }
  if (!is.list(rule) || is.null(names(rule))) {
    stop(
      "horizon_rule MUST be a mapping with the one key min_expected_events.",
      call. = FALSE
    )
  }
  value <- rule[["min_expected_events"]]
  if (is.null(value)) {
    stop(
      "horizon_rule has no min_expected_events. Give the minimum number of ",
      "expected events per arm and per follow-up year, as in ",
      "min_expected_events: 5.",
      call. = FALSE
    )
  }
  .tte_check_min_expected_events(value)
  spec[["horizon_rule"]] <- list(min_expected_events = as.numeric(value))
  return(spec)
}

#' Stop unless min_expected_events is one finite number of at least 0
#'
#' Every message names `horizon_rule$min_expected_events`. A logical value is
#' refused, because R would read `true` as 1.
#'
#' @param value The parsed `horizon_rule$min_expected_events`.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_check_min_expected_events <- function(value) {
  if (!is.numeric(value) || length(value) != 1L) {
    stop(
      "horizon_rule$min_expected_events MUST be one number. It holds: ",
      paste(format(value), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.finite(value)) {
    stop(
      "horizon_rule$min_expected_events MUST be a finite number. It holds: ",
      format(value),
      ".",
      call. = FALSE
    )
  }
  if (value < 0) {
    stop(
      "horizon_rule$min_expected_events MUST be at least 0. It holds: ",
      format(value),
      ".",
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}
