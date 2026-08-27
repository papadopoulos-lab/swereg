# These writers produce the CONSORT attrition sheet. The enrollment summary
# beside it is the one-line sentence that heads a results sheet.

#' Render a one-line enrollment summary sentence for the top of a results
#' sheet. Pulls unique-person and person-trial counts from `$get_attrition()`
#' (final criterion row) and the post-draw baseline count from
#' `$get_baselines()`. Returns NULL when the required fields are absent.
#' @noRd
.format_enrollment_summary <- function(plan, eid) {
  ec <- .plan_cohort_counts(plan, eid)
  if (is.null(ec$attrition) || nrow(ec$attrition) == 0L) {
    return(NULL)
  }
  overall <- .attrition_overall(ec$attrition)
  if (is.null(overall) || nrow(overall) == 0L) {
    return(NULL)
  }
  last <- overall[nrow(overall)]
  n_baseline <- .baseline_count(plan$get_baselines(), eid, "n_baseline")
  fmt <- function(x) format(x, big.mark = ",")
  parts <- c(
    sprintf(
      "Cohort: %s unique persons contributed %s sequential trial enrollments (intervention: %s / comparator: %s person-trials).",
      fmt(last$n_persons),
      fmt(last$n_person_trials),
      fmt(last$n_intervention),
      fmt(last$n_comparator)
    )
  )
  # The true post-draw count comes from the `matching` counts slot (enrolled
  # intervention + comparator person-trials), NOT from n_baseline.
  if (!is.null(ec$matching)) {
    m <- ec$matching
    n_int <- sum(m$n_intervention_enrolled, na.rm = TRUE)
    n_cmp <- sum(m$n_comparator_enrolled, na.rm = TRUE)
    if ((n_int + n_cmp) > 0L) {
      parts <- c(
        parts,
        sprintf(
          "After the comparator draw: %s person-trials entered baseline (intervention: %s / comparator: %s).",
          fmt(n_int + n_cmp),
          fmt(n_int),
          fmt(n_cmp)
        )
      )
    }
  }
  # n_baseline is the per-protocol analysis dataset. It holds the enrolled
  # person-trials minus those censored in the first period for protocol
  # deviation or loss to follow-up. It is NOT the post-draw count.
  # `.baseline_count()` reports an absent count as `NA`, so the guard tests for
  # a true comparison rather than for a non-NULL value.
  if (isTRUE(n_baseline > 0)) {
    parts <- c(
      parts,
      sprintf(
        "Analysis dataset (per-protocol): %s person-trials, after first-period censoring (protocol deviation or loss to follow-up; accounted for by IPCW).",
        fmt(n_baseline)
      )
    )
  }
  return(paste(parts, collapse = " "))
}


#' Write the CONSORT attrition numbers for one enrollment to a sheet.
#' Carries `criterion`, `n_persons`, `n_person_trials`, `n_intervention`,
#' and `n_comparator`, aggregated across trial_ids. Companion to the
#' CONSORT PNG/PDF sidecars: readers can cite exact numbers without
#' measuring pixels. The counts come from `$get_attrition()` and
#' `$get_matching()`.
#'
#' @return `TRUE` when the sheet was added to `wb`, and `FALSE` when it was
#'   not. The caller MUST read this before it names the sheet in the table of
#'   contents. Two states write nothing: an absent attrition table, and an
#'   attrition table that `.build_cohort_flow()` refuses.
#' @noRd
.write_attrition_sheet <- function(wb, sheet_name, plan, eid) {
  ec <- .plan_cohort_counts(plan, eid)
  if (is.null(ec$attrition) || nrow(ec$attrition) == 0L) {
    return(invisible(FALSE))
  }
  # Same single source of truth as the CONSORT diagram, so the sheet and the
  # picture cannot disagree. Includes the comparator draw (selection) and per-
  # protocol analysis (censoring) steps, each tagged by `kind`/`change_kind`
  # so the draw and analysis reductions are NOT mislabelled as exclusions.
  baselines <- plan$get_baselines()
  analysis_n <- .baseline_count(baselines, eid, "n_baseline")
  flow <- .build_cohort_flow(
    ec,
    # `.build_cohort_flow()` treats an absent size as `NULL`, and
    # `.baseline_count()` reports it as `NA`.
    analysis_n = if (is.na(analysis_n)) NULL else analysis_n,
    analysis_n_intervention = .baseline_count(
      baselines,
      eid,
      "n_baseline_intervention"
    ),
    analysis_n_comparator = .baseline_count(
      baselines,
      eid,
      "n_baseline_comparator"
    )
  )
  if (is.null(flow) || nrow(flow) == 0L) {
    return(invisible(FALSE))
  }

  openxlsx::addWorksheet(wb, sheet_name)
  label <- .enrollment_label(plan, eid)
  title <- paste0(
    "Enrollment ",
    eid,
    " (",
    label,
    ") -- cohort derivation (CONSORT)"
  )
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )

  out <- data.table::copy(flow)
  data.table::setcolorder(
    out,
    c(
      "step",
      "kind",
      "n_persons",
      "n_person_trials",
      "change_persons",
      "change_person_trials",
      "change_kind",
      "n_intervention",
      "n_comparator"
    )
  )

  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    out,
    startRow = 3L,
    headerStyle = header_style
  )
  # Counts are already real numbers (writeData on a numeric data.table); add a
  # thousands-separator display format so they read cleanly. Columns 1/2/7 are
  # text (step / kind / change_kind).
  if (nrow(out) > 0L) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(numFmt = "#,##0"),
      rows = 4L:(3L + nrow(out)),
      cols = c(3L, 4L, 5L, 6L, 8L, 9L),
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  openxlsx::setColWidths(wb, sheet_name, cols = 1L, widths = 45)
  openxlsx::setColWidths(wb, sheet_name, cols = 2L:9L, widths = 18)
  return(invisible(TRUE))
}
