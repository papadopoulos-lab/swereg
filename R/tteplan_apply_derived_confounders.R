# =============================================================================
# tteplan_apply_derived_confounders
# =============================================================================

# --- internal: collect computed-confounder grouped specs --------------------
#
# Mutates skeleton in place (calls .ensure_combined_column for any list-valued
# source_variable), then returns the list of grouped specs ready for
# .tte_apply_eligibility_batch(). Used by both tteplan_apply_derived_confounders
# (standalone) and .s1_prepare_skeleton (combined batch with exclusions).
.tte_build_confounder_specs <- function(skeleton, spec) {
  if (is.null(spec$confounders)) {
    return(list())
  }
  grouped_specs <- list()
  for (conf in spec$confounders) {
    impl <- conf$implementation
    if (!isTRUE(impl$computed)) {
      next
    }
    .ensure_combined_column(skeleton, impl)
    window <- impl$window_weeks
    grouped_specs[[length(grouped_specs) + 1L]] <- list(
      col_name = impl$variable,
      type = "windowed",
      source_var = impl$source_variable_combined,
      window_weeks = if (is.infinite(window)) 99999L else as.integer(window),
      negate_final = FALSE
    )
  }
  grouped_specs
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
  grouped_specs <- .tte_build_confounder_specs(skeleton, spec)
  .tte_apply_eligibility_batch(skeleton, grouped_specs, id_col = "id")
}
