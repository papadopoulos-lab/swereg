# =============================================================================
# tteplan_apply_derived_confounders
# =============================================================================

# --- internal: collect computed-confounder grouped specs --------------------
#
# Writes combined source columns to the skeleton (.ensure_combined_column for
# any list-valued source_variable), then returns
#   list(skeleton = ..., grouped_specs = ...)
# ready for .tte_apply_eligibility_batch(). The CALLER MUST take `skeleton`
# from the return value: a write into a table with no free column slot builds
# a new table, and the caller's own binding then holds none of those columns.
# Used by both tteplan_apply_derived_confounders (standalone) and
# .s1_prepare_skeleton (combined batch with exclusions).
.tte_build_confounder_specs <- function(skeleton, spec) {
  if (is.null(spec$confounders)) {
    return(list(skeleton = skeleton, grouped_specs = list()))
  }
  grouped_specs <- list()
  for (conf in spec$confounders) {
    impl <- conf$implementation
    if (!isTRUE(impl$computed)) {
      next
    }
    skeleton <- .ensure_combined_column(skeleton, impl)
    window <- impl$window_weeks
    grouped_specs[[length(grouped_specs) + 1L]] <- list(
      col_name = impl$variable,
      type = "windowed",
      source_var = impl$source_variable_combined,
      window_weeks = if (is.infinite(window)) 99999L else as.integer(window),
      negate_final = FALSE
    )
  }
  return(list(skeleton = skeleton, grouped_specs = grouped_specs))
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
  built <- .tte_build_confounder_specs(skeleton, spec)
  return(.tte_apply_eligibility_batch(
    built$skeleton,
    built$grouped_specs,
    id_col = "id"
  ))
}
