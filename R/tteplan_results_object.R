# The aggregate results object that `$export_tables()` writes beside the
# workbook. A manuscript reads it, so no analysis project builds its own.

#' Build the aggregate results object of a plan
#'
#' The object holds the accessor tables, one row per enrollment, and the
#' provenance of the run. Every table is aggregate. None holds a person or
#' person-trial identifier, and none holds a row per person or per
#' person-trial.
#'
#' The object also holds six fields that a manuscript reads from the plan.
#' They are the comparator-draw counts, the sparse decision, the spec, the
#' ETT grid without its file columns, the protocol table and the design
#' values. A manuscript step therefore needs no plan for them.
#'
#' @param plan A `TTEPlan`.
#' @return A named list. See `$export_tables()` for the fields.
#' @noRd
.plan_results_object <- function(plan) {
  baselines <- plan$get_baselines()
  reported <- .plan_ett_reported(plan)
  eids <- unique(as.character(reported$enrollment_id))
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
  s2 <- .tte_s2_run_of(reported)
  # The four file columns hold paths on the host that ran the plan. They are
  # not results, so the object drops them. A grid without some of them
  # drops the ones it has.
  ett <- data.table::copy(plan$ett)
  if (!is.null(ett)) {
    drop <- intersect(.RESULTS_ETT_FILE_COLS, names(ett))
    if (length(drop) > 0L) {
      ett[, (drop) := NULL]
    }
  }
  # `$get_protocol_table()` stops on a plan without a spec or without a
  # reported ETT. The 13 older fields need neither, so the object holds NULL
  # in those two states. Every other error of the accessor propagates.
  protocol_table <- if (is.null(plan$spec) || NROW(reported) == 0L) {
    NULL
  } else {
    plan$get_protocol_table()
  }
  return(list(
    estimates = plan$get_estimates(),
    curves = plan$get_curves(),
    subgroups = plan$get_subgroups(),
    baselines = baselines,
    attrition = plan$get_attrition(),
    enrollments = enrollments,
    adherence = plan$get_adherence(),
    horizons = plan$get_horizon_windows(),
    spec_version = plan$spec_version %||% NA_character_,
    swereg_version = as.character(utils::packageVersion("swereg")),
    swereg_version_computing = .plan_computing_versions(plan),
    s1_run_id = plan$s1_run_id %||% NA_character_,
    s2_run_id = sort(unique(s2[!is.na(s2)])),
    matching = plan$get_matching(),
    sparse_decision = plan$get_sparse_decision(),
    spec = plan$spec,
    ett = ett,
    protocol_table = protocol_table,
    design = list(
      period_width = plan$period_width,
      global_max_isoyearweek = plan$global_max_isoyearweek
    )
  ))
}

# The columns of `plan$ett` that name a file on the host that ran the plan.
# `.plan_results_object()` leaves them out of its `ett` field.
.RESULTS_ETT_FILE_COLS <- c(
  "file_raw",
  "file_imp",
  "file_analysis",
  "file_analysis_itt"
)
