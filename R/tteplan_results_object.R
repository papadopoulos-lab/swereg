# The aggregate results object that `$export_tables()` writes beside the
# workbook. A manuscript reads it, so no analysis project builds its own.

#' Build the aggregate results object of a plan
#'
#' The object holds the accessor tables, one row per enrollment, and the
#' provenance of the run. Every table is aggregate. None holds a person or
#' person-trial identifier, and none holds a row per person or per
#' person-trial.
#'
#' @param plan A `TTEPlan`.
#' @return A named list. See `$export_tables()` for the fields.
#' @noRd
.plan_results_object <- function(plan) {
  baselines <- plan$get_baselines()
  eids <- unique(as.character(plan$ett$enrollment_id))
  enrollments <- data.table::rbindlist(lapply(eids, function(eid) {
    arms <- .lookup_arm_labels(plan$spec, eid)
    arm <- function(which) {
      if (is.null(arms)) {
        return(NA_character_)
      }
      return(as.character(arms[[which]]))
    }
    n_arm <- function(col) {
      return(.baseline_count(baselines, eid, col))
    }
    return(data.table::data.table(
      enrollment_id = eid,
      enrollment_name = .enrollment_label(plan, eid),
      intervention_name = arm("intervention"),
      comparator_name = arm("comparator"),
      n_baseline_intervention = n_arm("n_baseline_intervention"),
      n_baseline_comparator = n_arm("n_baseline_comparator")
    ))
  }))
  s2 <- .tte_s2_run_of(plan$ett)
  return(list(
    estimates = plan$get_estimates(),
    curves = plan$get_curves(),
    subgroups = plan$get_subgroups(),
    baselines = baselines,
    attrition = plan$get_attrition(),
    enrollments = enrollments,
    spec_version = plan$spec_version %||% NA_character_,
    swereg_version = as.character(utils::packageVersion("swereg")),
    swereg_version_computing = .plan_computing_versions(plan),
    s1_run_id = plan$s1_run_id %||% NA_character_,
    s2_run_id = sort(unique(s2[!is.na(s2)]))
  ))
}
