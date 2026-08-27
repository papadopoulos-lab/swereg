# These writers produce the combined rates sheets. The treatment legend
# beside them names the arms each sheet reports.

#' @noRd
.prepare_combine_data <- function(plan, slot, keep_ett_ids = NULL) {
  results <- plan$results_ett
  if (!is.null(keep_ett_ids)) {
    results <- results[names(results) %in% keep_ett_ids]
  }
  results_list <- lapply(results, function(r) {
    val <- r[[slot]]
    if (is.null(val) || isTRUE(val$skipped)) {
      return(NULL)
    }
    return(list(x = val))
  })
  results_list <- Filter(Negate(is.null), results_list)
  if (length(results_list) == 0L) {
    return(NULL)
  }

  combine_input <- lapply(results_list, `[[`, "x")
  names(combine_input) <- names(results_list)

  wrapped <- lapply(names(combine_input), function(n) {
    lst <- list()
    lst[[slot]] <- combine_input[[n]]
    return(lst)
  })
  names(wrapped) <- names(combine_input)

  ett_desc <- .ett_descriptions(plan, names(wrapped))

  if (!is.null(keep_ett_ids)) {
    # Reorder to follow the user-specified ETT order
    keep <- intersect(keep_ett_ids, names(wrapped))
    wrapped <- wrapped[keep]
    ett_desc <- ett_desc[keep]
  }

  return(list(wrapped = wrapped, ett_desc = ett_desc))
}

#' Build a "Treatment definitions" data.table for the unique enrollments
#' touched by a set of ETT ids. Returns NULL when no enrollment metadata
#' is available.
#' @noRd
.build_treatment_legend <- function(plan, ett_ids = NULL) {
  ett <- plan$ett
  if (!is.null(ett_ids)) {
    ett <- ett[ett$ett_id %in% ett_ids]
  }
  if (nrow(ett) == 0L) {
    return(NULL)
  }
  enrollment_ids <- unique(ett$enrollment_id)
  rows <- lapply(enrollment_ids, function(eid) {
    enr <- NULL
    if (!is.null(plan$spec) && !is.null(plan$spec$enrollments)) {
      for (e in plan$spec$enrollments) {
        if (isTRUE(e$id == eid)) {
          enr <- e
          break
        }
      }
    }
    arms <- if (!is.null(enr)) enr$treatment$arms else NULL
    return(data.table::data.table(
      enrollment_id = eid,
      name = if (!is.null(enr$name)) enr$name else .enrollment_label(plan, eid),
      intervention = arms$intervention %||% NA_character_,
      comparator = arms$comparator %||% NA_character_,
      description = enr$treatment$description %||% NA_character_
    ))
  })
  return(data.table::rbindlist(rows))
}

#' Decide whether to relabel the generic Intervention/Comparator column suffixes
#' to spec-derived arm labels. Only does so when every featured ETT shares the
#' same (intervention, comparator) labels.
#' @noRd
.unique_arm_labels <- function(legend) {
  if (is.null(legend) || nrow(legend) == 0L) {
    return(NULL)
  }
  int <- unique(stats::na.omit(legend$intervention))
  cmp <- unique(stats::na.omit(legend$comparator))
  if (length(int) != 1L || length(cmp) != 1L) {
    return(NULL)
  }
  return(c(intervention = int, comparator = cmp))
}

#' Rename `*_Intervention` / `*_Comparator` column suffixes on a combined
#' rates data.table to use spec-derived arm labels. No-op when labels can't
#' be resolved.
#' @noRd
.rename_treatment_columns <- function(dt, legend) {
  arms <- .unique_arm_labels(legend)
  if (is.null(arms)) {
    return(dt)
  }
  nm <- names(dt)
  nm <- gsub("_Intervention$", paste0("_", arms[["intervention"]]), nm)
  nm <- gsub("_Comparator$", paste0("_", arms[["comparator"]]), nm)
  data.table::setnames(dt, nm)
  return(dt)
}

#' Write a treatment-definitions block to a worksheet at the given row, then
#' return the next free row.
#' @noRd
.write_treatment_legend <- function(wb, sheet_name, legend, start_row) {
  if (is.null(legend) || nrow(legend) == 0L) {
    return(start_row)
  }
  openxlsx::writeData(
    wb,
    sheet_name,
    "Treatment definitions",
    startRow = start_row,
    startCol = 1L
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold"),
    rows = start_row,
    cols = 1L
  )
  start_row <- start_row + 1L
  openxlsx::writeData(
    wb,
    sheet_name,
    legend,
    startRow = start_row,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  return(start_row + nrow(legend) + 2L)
}

#' @noRd
.write_combined_rates <- function(
  wb,
  sheet_name,
  plan,
  slot,
  title = NULL,
  keep_ett_ids = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  prep <- .prepare_combine_data(plan, slot, keep_ett_ids = keep_ett_ids)
  if (is.null(prep)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid rates results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }
  dt <- tryCatch(
    tteenrollment_rates_combine(prep$wrapped, slot, prep$ett_desc),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  dt <- .rename_treatment_columns(dt, legend)
  return(openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  ))
}

#' Merge rates and IRR results for the same set of ETTs into one sheet.
#'
#' Uses [tteenrollment_combined_combine()] under the hood, then applies
#' `.rename_treatment_columns()` so the `_Intervention`/`_Comparator` suffixes
#' pick up spec-derived arm labels when the featured ETTs share one enrollment.
#' @noRd
.write_combined_rates_irr <- function(
  wb,
  sheet_name,
  plan,
  rates_slot,
  irr_slot,
  title = NULL,
  keep_ett_ids = NULL
) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  legend <- .build_treatment_legend(plan, keep_ett_ids)
  row_ptr <- .write_treatment_legend(wb, sheet_name, legend, row_ptr)

  # Keep only ETTs that have BOTH rates and IRR results. This avoids a
  # size-mismatch recycling warning in the merge step.
  results <- plan$results_ett
  if (!is.null(keep_ett_ids)) {
    results <- results[names(results) %in% keep_ett_ids]
  }
  keep_ids <- Filter(
    function(eid) {
      r <- results[[eid]]
      if (is.null(r)) {
        return(FALSE)
      }
      rv <- r[[rates_slot]]
      iv <- r[[irr_slot]]
      return(!is.null(rv) && !isTRUE(rv$skipped) && !is.null(iv) && !isTRUE(iv$skipped))
    },
    names(results)
  )
  if (length(keep_ids) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid combined results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }
  results <- results[keep_ids]
  if (!is.null(keep_ett_ids)) {
    # Preserve user-specified order
    keep_ordered <- intersect(keep_ett_ids, names(results))
    results <- results[keep_ordered]
  }

  ett_desc <- .ett_descriptions(plan, names(results))

  dt <- tryCatch(
    tteenrollment_combined_combine(
      results,
      rates_slot,
      irr_slot,
      ett_desc
    ),
    error = function(e) data.table::data.table(error = conditionMessage(e))
  )
  dt <- .rename_treatment_columns(dt, legend)
  return(openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  ))
}
