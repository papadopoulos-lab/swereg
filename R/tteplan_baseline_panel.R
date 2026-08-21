# These helpers read the stored baseline characteristics table. One reports
# whether it is stale, one builds the panel, and one writes a panel to a
# worksheet.

#' (removed) -- main Table 1 is now stored separately by the enrollment
#' worker as `table1_ipw_trunc_main`, so no on-the-fly stripping is needed.

#' Is one enrollment's cached baseline result too old to export?
#'
#' `$export_tables()` calls this over `self$results_enrollment` and re-runs
#' `$recompute_baselines()` for every enrollment it marks stale. Three
#' generations of cache fail here:
#'
#' * **Pre-`swereg_table1`**: the panel is a `tableone` object, so it does not
#'   carry the `swereg_table1` class.
#' * **Pre-`smd_numeric`**: the panel is a `swereg_table1` built before
#'   `.swereg_table1()` emitted the unrounded `smd_numeric` column. The class
#'   test alone declares it current, so the Love plot would receive no numeric
#'   SMDs and no error would be raised.
#' * **Pre-SMD main panel**: the worker built `table1_ipw_trunc_main` with
#'   `include_smd = FALSE`, so the headline Table 1 carries no SMD column.
#'   The worker builds the four supplementary panels with `include_smd = TRUE`.
#'   A predicate that reads only the first present panel therefore calls this
#'   cache current.
#'
#' The check runs on EVERY panel the cached result holds, not on the first one
#' it finds. Each present panel MUST be a `swereg_table1` and MUST carry
#' `smd_numeric`. One failing panel marks the whole result stale.
#'
#' Absence is not failure. A panel the worker never produced is `NULL`, and the
#' check skips it. `table1_raw` is `NULL` when no raw file sits on disk.
#' `table1_ipw_trunc_main` is `NULL` when the enrollment has no `ipw_trunc`
#' column. A result with no panel at all is not stale: there is nothing to
#' refresh.
#'
#' The lookup uses `[[` and not `$`. `table1_ipw` is a strict prefix of
#' `table1_ipw_trunc`, so `$` partial matching would return the truncated panel
#' under the untruncated name.
#'
#' @param r One element of `plan$results_enrollment`, or `NULL`.
#' @return `TRUE` when the cached panels must be recomputed.
#' @noRd
.baseline_panel_is_stale <- function(r) {
  if (is.null(r)) {
    return(FALSE)
  }
  panel_names <- c(
    "table1_ipw_trunc",
    "table1_ipw_trunc_main",
    "table1_unweighted",
    "table1_ipw",
    "table1_raw"
  )
  panels <- lapply(intersect(panel_names, names(r)), function(nm) r[[nm]])
  panels <- Filter(Negate(is.null), panels)
  if (length(panels) == 0L) {
    return(FALSE)
  }
  is_current <- vapply(
    panels,
    function(p) {
      inherits(p, "swereg_table1") && "smd_numeric" %in% names(p)
    },
    logical(1)
  )
  !all(is_current)
}

#' Write a swereg_table1 data.table to a worksheet with bold header styling
#' and a fitted Variable column.
#' @noRd
.write_tableone_sheet <- function(wb, sheet_name, t1_dt, title = NULL) {
  # smd_numeric is a programmatic contract, not a display column.
  t1_dt <- .t1_drop_numeric(t1_dt)
  openxlsx::addWorksheet(wb, sheet_name)
  start_row <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = 1L,
      cols = 1L
    )
    start_row <- 3L
  }
  if (is.null(t1_dt) || nrow(t1_dt) == 0L) {
    openxlsx::writeData(wb, sheet_name, "(no data)", startRow = start_row)
    return(invisible(NULL))
  }
  openxlsx::writeData(
    wb,
    sheet_name,
    t1_dt,
    startRow = start_row,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  ncols <- ncol(t1_dt)
  widths <- c(50, 16, rep(22, max(0, ncols - 2L)))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncols), widths = widths)
}

#' The baseline count of one enrollment, read through `$get_baselines()`.
#'
#' `$get_baselines()` repeats the three enrollment counts on every row of that
#' enrollment's panels, so the first row carries them. An enrollment that
#' stored no panel yields no row and therefore no count.
#'
#' @param baselines A `$get_baselines()` table.
#' @param eid Character(1), the enrollment identifier.
#' @param field Character(1), one of `n_baseline`, `n_baseline_intervention`
#'   and `n_baseline_comparator`.
#' @return Numeric(1), `NA_real_` when the enrollment stored no panel.
#' @noRd
.baseline_count <- function(baselines, eid, field = "n_baseline") {
  if (is.null(baselines) || nrow(baselines) == 0L) {
    return(NA_real_)
  }
  hit <- which(baselines$enrollment_id == eid)
  if (length(hit) == 0L) {
    return(NA_real_)
  }
  as.numeric(baselines[[field]][hit[1L]])
}


#' The two arm labels a rendered baseline panel heads its columns with.
#'
#' Read from the panel that `$s3_analyze()` STORED, through
#' `$get_baselines()`. The panel was built with the arm labels the
#' specification held when the analysis ran. The stored header is therefore the
#' header those numbers belong to.
#'
#' The specification is NOT re-read here. A specification edited between the
#' analysis and the export would otherwise head yesterday's numbers with
#' today's labels. A specification that names no arms would replace a real
#' header with the two values of the treatment variable.
#'
#' @param baselines A `$get_baselines()` table.
#' @param eid Character(1), the enrollment identifier.
#' @return A named character(2), `comparator` and `intervention`. Both are
#'   `NA_character_` when the plan stores no panel for this enrollment.
#' @noRd
.baseline_arm_labels <- function(baselines, eid) {
  out <- c(comparator = NA_character_, intervention = NA_character_)
  if (is.null(baselines) || nrow(baselines) == 0L) {
    return(out)
  }
  hit <- which(
    baselines$enrollment_id == eid & !is.na(baselines$comparator_label)
  )
  if (length(hit) == 0L) {
    return(out)
  }
  c(
    comparator = as.character(baselines$comparator_label[hit[1L]]),
    intervention = as.character(baselines$intervention_label[hit[1L]])
  )
}


#' Rebuild one rendered baseline panel from `$get_baselines()`.
#'
#' `$get_baselines()` returns the stored cells and drops two rendering
#' conventions. This function restores both, which is the consumer's work:
#' \itemize{
#'   \item the variable name prints once per block. The accessor carries it
#'     down every row, so this blanks the repeats.
#'   \item the `SMD` column is a display string. The accessor keeps the
#'     unrounded double, so this formats it with `.t1_fmt_smd()`, the one
#'     formatter the producer used.
#' }
#'
#' The `SMD` column is composed only when the panel carries at least one
#' standardised mean difference. A panel built with `include_smd = FALSE`
#' carries none, and it had no such column.
#'
#' @param baselines A `$get_baselines()` table.
#' @param eid Character(1), the enrollment identifier.
#' @param imputation,weighting,variant The three panel keys.
#' @param arm_labels As returned by [.baseline_arm_labels].
#' @return A data.table with the rendered columns, or `NULL` when the plan
#'   stores no such panel.
#' @noRd
.baseline_panel <- function(
  baselines,
  eid,
  imputation,
  weighting,
  variant,
  arm_labels
) {
  if (is.null(baselines) || nrow(baselines) == 0L) {
    return(NULL)
  }
  hit <- which(
    baselines$enrollment_id == eid &
      baselines$imputation == imputation &
      baselines$weighting == weighting &
      baselines$variant == variant
  )
  if (length(hit) == 0L) {
    return(NULL)
  }
  rows <- baselines[hit]
  variable <- as.character(rows$variable)
  variable[is.na(variable)] <- ""
  n <- length(variable)
  if (n > 1L) {
    repeated <- c(FALSE, variable[-1L] == variable[-n])
    variable[repeated] <- ""
  }
  out <- data.table::data.table(
    Variable = variable,
    Level = as.character(rows$level),
    Overall = as.character(rows$overall)
  )
  data.table::set(
    out,
    j = arm_labels[["comparator"]],
    value = as.character(rows$comparator)
  )
  data.table::set(
    out,
    j = arm_labels[["intervention"]],
    value = as.character(rows$intervention)
  )
  # The stored SHAPE. A panel built with `include_smd = TRUE` carries the
  # column whatever its values, and a panel whose every standardised mean
  # difference is `NA` still heads a blank `SMD` column.
  if (isTRUE(rows$smd_stored[1L])) {
    data.table::set(
      out,
      j = "SMD",
      value = vapply(rows$smd_numeric, .t1_fmt_smd, character(1))
    )
  }
  out[]
}
