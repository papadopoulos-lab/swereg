# This writer produces the combined baseline sheet. It stacks one baseline
# panel per enrollment on a single worksheet.

# .write_consort() / .write_consort_text() / .write_consort_flowchart() live
# in R/consort.R. The dispatcher tries the flowchart path and falls back to
# the text table when DiagrammeR/DiagrammeRsvg/rsvg are unavailable or
# rendering errors out.

#' @noRd
.write_combined_baseline <- function(wb, sheet_name, plan, eid) {
  openxlsx::addWorksheet(wb, sheet_name)
  label <- .enrollment_label(plan, eid)
  title <- paste0(
    "Enrollment ",
    eid,
    " (",
    label,
    ") -- Baseline characteristics"
  )
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )

  # Summary sentence: unique persons + sequential-TTE person-trial counts
  # pulled from the attrition table + the baseline row count after the draw.
  # Surfacing both numbers protects against the common reviewer confusion
  # where a 22M-person-week figure is mistaken for 22M participants.
  summary_line <- .format_enrollment_summary(plan, eid)
  header_row <- 2L
  data_row <- 3L
  if (!is.null(summary_line)) {
    openxlsx::writeData(wb, sheet_name, summary_line, startRow = 2L)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(fontSize = 10, textDecoration = "italic"),
      rows = 2L,
      cols = 1L
    )
    header_row <- 4L
    data_row <- 5L
  }

  if (!eid %in% .plan_analysed_enrollment_ids(plan)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No results for this enrollment.",
      startRow = header_row + 1L
    )
    return(invisible(NULL))
  }

  baselines <- plan$get_baselines()
  arm_labels <- .baseline_arm_labels(baselines, eid)
  panel <- function(imputation, weighting) {
    .baseline_panel(
      baselines,
      eid,
      imputation,
      weighting,
      "supplementary",
      arm_labels
    )
  }
  panels <- list(
    `Unimputed and unweighted` = panel("raw", "none"),
    `Imputed and unweighted` = panel("imputed", "none"),
    `Imputed and IPW` = panel("imputed", "ipw"),
    `Imputed and IPW truncated` = panel("imputed", "ipw_trunc")
  )

  panels <- Filter(Negate(is.null), panels)
  # smd_numeric is a programmatic contract, not a display column. Strip it
  # before ncol() decides the merged header width for each panel.
  panels <- lapply(panels, .t1_drop_numeric)
  if (length(panels) == 0L) {
    return(invisible(NULL))
  }

  start_col <- 1L

  bold_centre <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center"
  )
  table_header <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )

  for (name in names(panels)) {
    df <- panels[[name]]
    ncols <- ncol(df)
    if (ncols > 1) {
      openxlsx::mergeCells(
        wb,
        sheet_name,
        cols = start_col:(start_col + ncols - 1L),
        rows = header_row
      )
    }
    openxlsx::writeData(
      wb,
      sheet_name,
      name,
      startCol = start_col,
      startRow = header_row
    )
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = bold_centre,
      rows = header_row,
      cols = start_col
    )
    openxlsx::writeData(
      wb,
      sheet_name,
      df,
      startCol = start_col,
      startRow = data_row,
      headerStyle = table_header
    )
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = start_col,
      widths = 50
    )
    if (ncols > 1) {
      openxlsx::setColWidths(
        wb,
        sheet_name,
        cols = (start_col + 1L):(start_col + ncols - 1L),
        widths = 18
      )
    }
    start_col <- start_col + ncols + 1L
  }
}
