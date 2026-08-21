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
    data.table::data.table(
      enrollment_id = eid,
      additional_criteria = label,
      treatment_variable = tx_info$variable,
      intervention_value = tx_info$intervention,
      comparator_value = tx_info$comparator,
      comparator_to_intervention_ratio = tx_info$ratio,
      n_baseline = n_base
    )
  })
  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, "Enrollments", dt)
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
    data.table::data.table(
      ett_id = ett_id,
      enrollment_id = r$enrollment_id,
      outcome_var = r$outcome_var,
      outcome_name = r$outcome_name,
      follow_up = r$follow_up,
      description = r$description,
      n_events = if (length(hit) > 0L) est$n_events[hit[1L]] else NA
    )
  })
  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(wb, "ETTs", dt)
}

#' The description of each emulated trial, read from `plan$ett`.
#'
#' `plan$ett` is an INPUT and it holds one row per emulated trial, so every
#' identifier has a description whatever the analysis stored.
#'
#' The stored result carries a `description` field too. Reading THAT over the
#' whole result list stopped an export. One trial's copy could be absent, or
#' could be more than one string. A single stale entry then blocked the trials
#' the caller had asked for. `$reload_spec()` no longer refreshes the stored
#' copy, so the field is now more likely to be absent.
#'
#' @param plan A TTEPlan.
#' @param ett_ids Character vector of identifiers, in the wanted order.
#' @return A named character vector as long as `ett_ids`. An identifier the
#'   grid does not carry falls back to the identifier itself.
#' @noRd
.ett_descriptions <- function(plan, ett_ids) {
  ett_ids <- as.character(ett_ids)
  out <- stats::setNames(ett_ids, ett_ids)
  ett <- plan$ett
  if (
    is.null(ett) ||
      nrow(ett) == 0L ||
      !all(c("ett_id", "description") %in% names(ett))
  ) {
    return(out)
  }
  hit <- match(ett_ids, as.character(ett$ett_id))
  desc <- as.character(ett$description)[hit]
  ok <- !is.na(hit) & !is.na(desc)
  out[ok] <- desc[ok]
  out
}
