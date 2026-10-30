# The treatment legend names the arms a results sheet reports.

#' Build a "Treatment definitions" data.table for the unique enrollments
#' touched by a set of ETT ids. Returns NULL when no enrollment metadata
#' is available.
#' @noRd
.build_treatment_legend <- function(plan, ett_ids = NULL) {
  ett <- .plan_ett_reported(plan)
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
