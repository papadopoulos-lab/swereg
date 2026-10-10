# These writers produce the two overview sheets. One carries a row per
# enrollment, and the other a row per emulated trial.

#' @noRd
.write_enrollment_overview <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "Enrollments")
  enrollment_ids <- unique(plan$ett$enrollment_id)
  baselines <- plan$get_baselines()
  rows <- lapply(enrollment_ids, function(eid) {
    label <- .enrollment_label(plan, eid)
    n_base <- .baseline_count(baselines, eid, "n_baseline")
    # Treatment info from spec
    tx_info <- list(
      variable = NA,
      intervention = NA,
      comparator = NA,
      ratio = NA
    )
    row <- plan$ett[plan$ett$enrollment_id == eid][1]
    if (
      "treatment_impl" %in% names(plan$ett) && !is.null(row$treatment_impl[[1]])
    ) {
      impl <- row$treatment_impl[[1]]
      tx_info$variable <- impl$variable %||% NA
      tx_info$intervention <- impl$intervention_value %||% NA
      tx_info$comparator <- impl$comparator_value %||% NA
    }
    if ("comparator_to_intervention_ratio" %in% names(plan$ett)) {
      tx_info$ratio <- row$comparator_to_intervention_ratio
    }
    return(data.table::data.table(
      enrollment_id = eid,
      additional_criteria = label,
      treatment_variable = tx_info$variable,
      intervention_value = tx_info$intervention,
      comparator_value = tx_info$comparator,
      comparator_to_intervention_ratio = tx_info$ratio,
      n_baseline = n_base
    ))
  })
  # With a sparse_rule decision, a last column says whether each enrollment
  # drove the decision or inherited it.
  dt <- .tte_sparse_role_column(data.table::rbindlist(rows), plan)
  return(openxlsx::writeData(wb, "Enrollments", dt))
}

#' @noRd
.write_ett_overview <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "ETTs")
  # `n_events` repeats on every estimate row of an emulated trial, so the first
  # row carries it. A trial that stored no estimate at all yields no row and
  # therefore no count.
  est <- plan$get_estimates()
  rows <- lapply(seq_len(nrow(plan$ett)), function(i) {
    r <- plan$ett[i]
    ett_id <- r$ett_id
    hit <- which(est$ett_id == ett_id)
    return(data.table::data.table(
      ett_id = ett_id,
      enrollment_id = r$enrollment_id,
      outcome_var = r$outcome_var,
      outcome_name = r$outcome_name,
      follow_up = r$follow_up,
      description = r$description,
      n_events = if (length(hit) > 0L) est$n_events[hit[1L]] else NA
    ))
  })
  dt <- .tte_sparse_role_column(data.table::rbindlist(rows), plan)
  return(openxlsx::writeData(wb, "ETTs", dt))
}
