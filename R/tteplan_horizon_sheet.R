# The "Minimum events" sheet of the results workbook. It shows the window
# table of the minimum-events rule, one row per ETT, estimand and follow-up
# year.

#' Column headers and Excel number formats of the `Minimum events` sheet
#'
#' The names are the headers, in sheet order. A value of `NA` leaves the
#' column without a number format.
#' @noRd
.HORIZON_SHEET_COLS <- c(
  "ETT" = NA_character_,
  "Enrollment" = NA_character_,
  "Outcome" = NA_character_,
  "Follow-up (weeks)" = "0",
  "Estimand" = NA_character_,
  "Window (weeks)" = NA_character_,
  "PY (int)" = "#,##0.0",
  "PY (cmp)" = "#,##0.0",
  "Pooled rate per 100,000 PY" = "#,##0.0",
  "Expected events (int)" = "0.00",
  "Expected events (cmp)" = "0.00",
  "Events (int)" = "0.00",
  "Events (cmp)" = "0.00",
  "Window passes" = NA_character_,
  "Reportable horizon (weeks)" = "0"
)


#' The rows of the `Minimum events` sheet
#'
#' Reads `$get_horizon_windows()`, which holds one row per window and arm, and
#' puts the two arms of a window on one row. All counts are weighted, with the
#' weight of the estimand.
#'
#' @param plan A TTEPlan.
#' @return A data.frame with the columns of `.HORIZON_SHEET_COLS`, ordered by
#'   the ETT order of `plan$ett`, then per-protocol before intention-to-treat,
#'   then window. It has no rows when no result stores a window table.
#' @noRd
.horizon_sheet_rows <- function(plan) {
  hw <- plan$get_horizon_windows()
  cols <- names(.HORIZON_SHEET_COLS)
  if (nrow(hw) == 0L) {
    out <- as.data.frame(stats::setNames(
      replicate(length(cols), character(0), simplify = FALSE),
      cols
    ))
    return(out)
  }
  ett <- plan$ett
  int <- hw[hw$arm == "intervention"]
  cmp <- hw[hw$arm == "comparator"]
  key <- c("ett_id", "estimand", "window_lo")
  data.table::setkeyv(int, key)
  data.table::setkeyv(cmp, key)
  if (!identical(int[, key, with = FALSE], cmp[, key, with = FALSE])) {
    stop(
      "Minimum events sheet: a stored window lacks one of its two arms.",
      call. = FALSE
    )
  }
  ett_row <- match(int$ett_id, ett$ett_id)
  ord <- order(
    ett_row,
    match(int$estimand, names(.TTE_ESTIMANDS)),
    int$window_lo
  )
  int <- int[ord]
  cmp <- cmp[ord]
  ett_row <- ett_row[ord]
  weeks <- function(x) format(x, trim = TRUE, scientific = FALSE)
  out <- data.frame(
    ETT = int$ett_id,
    Enrollment = vapply(
      ett$enrollment_id[ett_row],
      function(eid) .enrollment_label(plan, eid),
      character(1)
    ),
    Outcome = ett$outcome_name[ett_row],
    `Follow-up (weeks)` = as.integer(ett$follow_up[ett_row]),
    Estimand = unname(.TTE_ESTIMANDS[int$estimand]),
    `Window (weeks)` = paste0(weeks(int$window_lo), "-", weeks(int$window_hi)),
    `PY (int)` = int$py_weighted,
    `PY (cmp)` = cmp$py_weighted,
    `Pooled rate per 100,000 PY` = int$rate_pooled * 1e5,
    `Expected events (int)` = int$events_expected,
    `Expected events (cmp)` = cmp$events_expected,
    `Events (int)` = int$events_weighted,
    `Events (cmp)` = cmp$events_weighted,
    `Window passes` = ifelse(int$pass, "yes", "no"),
    `Reportable horizon (weeks)` = int$horizon_reportable_weeks,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  return(out)
}


#' Write the `Minimum events` sheet
#'
#' Row 1 is the title, which names the threshold. Row 2 states how the sheet
#' computes the expected events. The header is row 4. A plan with no stored
#' window table gets the sheet with one line in place of the table.
#'
#' @param wb An openxlsx workbook.
#' @param sheet_name Character(1), the sheet name.
#' @param plan A TTEPlan.
#' @return `invisible(NULL)`.
#' @noRd
.write_horizon_sheet <- function(wb, sheet_name, plan) {
  openxlsx::addWorksheet(wb, sheet_name)
  rows <- .horizon_sheet_rows(plan)
  rule <- plan$spec[["min_expected_events_per_arm_per_year"]]
  openxlsx::writeData(
    wb,
    sheet_name,
    paste0(
      "Expected events per arm and follow-up year (minimum-events rule, ",
      "min_expected_events_per_arm_per_year = ",
      if (is.null(rule)) "not set" else format(rule),
      ")"
    ),
    startRow = 1L
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    paste0(
      "All counts use the weight of the estimand. The expected events of an ",
      "arm are computed under the null: the pooled weighted rate of both ",
      "arms times the weighted person-years of that arm. A year passes when ",
      "both arms reach min_expected_events_per_arm_per_year. The pooled rate ",
      "holds the observed events, so the decision depends on the outcome ",
      "data."
    ),
    startRow = 2L
  )
  if (nrow(rows) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      paste0(
        "No stored result holds a window table of the minimum-events rule. ",
        "Run $s3_analyze()."
      ),
      startRow = 4L
    )
    return(invisible(NULL))
  }
  header_row <- 4L
  openxlsx::writeData(wb, sheet_name, rows, startRow = header_row)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    ),
    rows = header_row,
    cols = seq_along(rows),
    gridExpand = TRUE
  )
  data_rows <- header_row + seq_len(nrow(rows))
  for (j in which(!is.na(.HORIZON_SHEET_COLS))) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(numFmt = .HORIZON_SHEET_COLS[[j]]),
      rows = data_rows,
      cols = j,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_along(rows),
    widths = c(12, 30, 30, 12, 20, 14, rep(16, 9))
  )
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = header_row + 1L)
  return(invisible(NULL))
}
