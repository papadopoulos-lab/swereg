# These helpers report the cohort a plan counted. They name the enrollments
# and the emulated trials it analysed, and the arm labels a sheet prints for
# them.

#' @noRd
#' The emulated trials `$s3_analyze()` has a result entry for.
#'
#' Reads the KEYS of `plan$results_ett` and no value inside it. "Was this trial
#' analysed at all" is a different question from "what does it report". No
#' accessor answers it. An accessor returns rows for what was stored. A trial
#' whose every work item failed stores a skip envelope, and it yields no row.
#'
#' A consumer that must separate "analysed and reported nothing" from "never
#' analysed" calls this. Every consumer that only reports numbers calls an
#' accessor instead.
#'
#' @param plan A TTEPlan.
#' @return A character vector, in stored order.
#' @noRd
.plan_analysed_ett_ids <- function(plan) {
  ids <- names(plan$results_ett)
  if (is.null(ids)) return(character(0)) else return(as.character(ids))
}


#' The enrollments `$s3_analyze()` has a result entry for.
#'
#' The sibling of [.plan_analysed_ett_ids]. It reads the KEYS of
#' `plan$results_enrollment` and no value inside it. A sheet that says "no
#' results for this enrollment" reports that the stage never ran. That is a
#' different statement from "the stage ran and stored no panel".
#'
#' @param plan A TTEPlan.
#' @return A character vector, in stored order.
#' @noRd
.plan_analysed_enrollment_ids <- function(plan) {
  ids <- names(plan$results_enrollment)
  if (is.null(ids)) return(character(0)) else return(as.character(ids))
}


#' The enrollments `$s1_generate_enrollments_and_ipw()` has a counts entry for.
#'
#' The third key reader, beside [.plan_analysed_ett_ids] and
#' [.plan_analysed_enrollment_ids]. It reads the KEYS of
#' `plan$enrollment_counts` and no value inside it. "Did the enrollment stage
#' run for this enrollment" is a different question from "what did it count".
#' No accessor answers it. An entry that stored two empty tables yields no
#' accessor row. That is not the same as no entry at all.
#'
#' @param plan A TTEPlan.
#' @return A character vector, in stored order.
#' @noRd
.plan_counted_enrollment_ids <- function(plan) {
  ids <- names(plan$enrollment_counts)
  if (is.null(ids)) return(character(0)) else return(as.character(ids))
}


#' One enrollment's stored cohort counts, read through the accessors.
#'
#' `.build_cohort_flow()`, `.attrition_overall()` and
#' `.render_consort_sidecars()` all speak the PRODUCER's column names. The two
#' accessors return the same rows under the schema's names, so this renames
#' them back and filters to one enrollment. It selects and renames. It sums
#' nothing, it creates no row, and it fills no gap.
#'
#' @param plan A TTEPlan.
#' @param eid Character(1), the enrollment identifier.
#' @return A list with `attrition` and `matching`. Each is `NULL` when the plan
#'   stores no such table for this enrollment, which is the shape
#'   `.build_cohort_flow()` already tests for.
#' @noRd
.plan_cohort_counts <- function(plan, eid) {
  att <- plan$get_attrition()
  mat <- plan$get_matching()
  a <- att[which(att$enrollment_id == eid)]
  m <- mat[which(mat$enrollment_id == eid)]
  return(list(
    attrition = if (nrow(a) == 0L) {
      NULL
    } else {
      data.table::data.table(
        trial_id = a$trial_id,
        criterion = a$step_name,
        n_persons = a$n_persons,
        n_person_trials = a$n_person_trials,
        n_intervention = a$n_arm_intervention,
        n_comparator = a$n_arm_comparator
      )
    },
    matching = if (nrow(m) == 0L) {
      NULL
    } else {
      data.table::data.table(
        trial_id = m$trial_id,
        n_intervention_total = m$n_intervention_total,
        n_comparator_total = m$n_comparator_total,
        n_intervention_enrolled = m$n_intervention_enrolled,
        n_comparator_enrolled = m$n_comparator_enrolled
      )
    }
  ))
}


.enrollment_label <- function(plan, eid) {
  if (is.null(plan$spec)) {
    return(eid)
  }
  for (enr in plan$spec$enrollments) {
    if (enr$id == eid) {
      if (!is.null(enr$name) && nzchar(enr$name)) return(enr$name)
    }
  }
  return(eid)
}

#' Look up the (comparator, intervention) arm labels for an enrollment id from
#' the study spec. Returns NULL when the spec has no usable arm names.
#' @noRd
.lookup_arm_labels <- function(spec, enrollment_id) {
  if (is.null(spec) || is.null(spec$enrollments)) {
    return(NULL)
  }
  for (enr in spec$enrollments) {
    if (isTRUE(enr$id == enrollment_id)) {
      arms <- enr$treatment$arms
      if (is.null(arms)) {
        return(NULL)
      }
      intervention <- arms$intervention
      comparator <- arms$comparator
      if (is.null(intervention) || is.null(comparator)) {
        return(NULL)
      }
      return(c(
        comparator = as.character(comparator),
        intervention = as.character(intervention)
      ))
    }
  }
  return(NULL)
}
