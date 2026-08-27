# These helpers produce the weight-truncation sensitivity sheet. They build
# one measurement block per arm, apply its Excel number formats, and mark
# whether the incidence rate ratio is estimable.

#' Pull a one-row measurement block (events/PY/rate per arm + IRR + CI +
#' p-value) for a single emulated trial and a single estimand and weighting
#' combination.
#'
#' Reads `$get_estimates()`, never a result slot. Column names use generic
#' suffixes (`events_intervention`, `rate_cmp`, etc.) since the arm identities
#' are carried in the separate id columns of the sensitivity sheet.
#'
#' Returns `NULL` when the combination has nothing to report. Three states give
#' that answer, and `$get_estimates()` reports all three as absent rows or as
#' `NA`:
#' \itemize{
#'   \item the combination stored neither rates nor a ratio, so it has no row;
#'   \item the rates are unusable, which is every per-arm field `NA`. A stored
#'     rates table with no arm column, or with the wrong number of arm rows,
#'     reads this way;
#'   \item the ratio is unusable, which is every ratio field `NA`.
#' }
#'
#' @param est A `$get_estimates()` table.
#' @param ett_id Character(1).
#' @param slot Character(1), any slot name of the wanted combination.
#' @return A named list of eleven fields, or `NULL`.
#' @noRd
.sensitivity_row_measurements <- function(est, ett_id, slot) {
  combo <- .tte_slot_combo(slot)
  hit <- which(
    est$ett_id == ett_id &
      est$estimand == combo[["estimand"]] &
      est$weights == combo[["weights"]]
  )
  if (length(hit) == 0L) {
    return(NULL)
  }
  row <- est[hit[1L]]
  # The stored SHAPE, not the stored values. A combination whose rates table
  # holds `NA` numbers still reports its identifiers and its ratio, with blank
  # rate cells. A combination that has no usable rates table reports nothing.
  if (!isTRUE(row$rates_stored)) {
    return(NULL)
  }
  # The stored SHAPE, not the stored values. A combination whose ratio failed
  # still reports its arm counts, and a combination that has no ratio slot
  # reports nothing.
  if (!isTRUE(row$irr_stored) || !isTRUE(row$irr_interval_stored)) {
    return(NULL)
  }

  return(list(
    events_intervention = row$events_int,
    py_intervention = row$py_int,
    rate_intervention = row$rate_int,
    events_cmp = row$events_cmp,
    py_cmp = row$py_cmp,
    rate_cmp = row$rate_cmp,
    irr = row$irr,
    lo = row$irr_lo,
    hi = row$irr_hi,
    pvalue = row$irr_pvalue,
    irr_estimable = row$irr_estimable
  ))
}


#' Excel number formats for the 9 fixed measurement columns. `NA` marks a
#' column that stays a human-formatted display string (IRR, 95% CI) -- those
#' are inherently composite, like Table 1's "n (%)". Every other column is
#' written as a bare number and formatted in Excel so it sorts and sums and
#' never trips the "number stored as text" warning.
#' @noRd
.MEASUREMENT_NUMFMT <- c(
  "Events (int)" = "0.0",
  "PY (int)" = "#,##0",
  "Rate/100k (int)" = "0.0",
  "Events (cmp)" = "0.0",
  "PY (cmp)" = "#,##0",
  "Rate/100k (cmp)" = "0.0",
  "IRR" = NA,
  "95% CI" = NA,
  "p-value" = "[<0.001]\"<0.001\";0.000"
)


#' Apply the measurement-column number formats to one side-by-side block whose
#' first measurement column sits at `block_start`, over body rows `data_rows`.
#' Numeric columns get their Excel numFmt; the IRR/CI display strings are left
#' alone. Styles are stacked so existing fills (e.g. block shading) survive.
#' @noRd
.apply_measurement_numfmt <- function(wb, sheet_name, block_start, data_rows) {
  if (length(data_rows) == 0L) {
    return(invisible(NULL))
  }
  fmts <- .MEASUREMENT_NUMFMT
  for (j in seq_along(fmts)) {
    f <- fmts[[j]]
    if (is.na(f)) {
      next
    }
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(numFmt = f),
      rows = data_rows,
      cols = block_start + j - 1L,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  return(invisible(NULL))
}


#' Is an incidence rate ratio estimable?
#'
#' The ONE place the package answers that question. `$s3_analyze()` calls it and
#' stores the answer as the `irr_estimable` column, beside the ratio it belongs
#' to. `.sensitivity_row_fmt()` calls it to decide whether to print the ratio.
#' Two copies of this test could drift apart, and a results sheet and a figure
#' would then disagree about the same ratio.
#'
#' An arm with no event gives a ratio of exactly 0, which is FINITE. An
#' `is.finite()` guard alone lets it print as `"0.00"` beside a zero-width
#' interval `"0.00 to 0.00"`. That reads as a point estimate of no risk, known
#' perfectly. It is neither: the ratio is inestimable.
#'
#' Every display reads the STORED answer through
#' `.tte_irr_estimable_stored()`. This function is the producer's rule and the
#' fallback for a result stored before the column existed.
#'
#' @param irr Numeric, the stored ratio. `NA` and `NaN` are not estimable.
#' @return A logical vector as long as `irr`.
#' @noRd
.tte_irr_estimable <- function(irr) {
  irr <- suppressWarnings(as.numeric(irr))
  return(is.finite(irr) & irr >= 0.01)
}


#' The estimability decision for ONE stored incidence rate ratio.
#'
#' Reads the stored `irr_estimable` column. `$s3_analyze()` decides it once,
#' beside the ratio, and `$get_estimates()` carries it. A formatter that
#' re-tested the threshold would be a second decision site, and two displays of
#' one ratio could then disagree.
#'
#' A result stored before that column existed passes `NA`. The rule is then
#' applied here, by the ONE function that holds it. That is the consumer
#' deriving what the producer did not store, and three live projects hold such
#' results. Rendering nothing for them would blank a ratio that used to print.
#'
#' @param irr Numeric(1), the stored ratio.
#' @param irr_estimable Logical(1), the stored decision, or `NA`.
#' @return Logical(1).
#' @noRd
.tte_irr_estimable_stored <- function(irr, irr_estimable) {
  if (length(irr_estimable) == 1L && !is.na(irr_estimable)) {
    return(isTRUE(as.logical(irr_estimable)))
  }
  return(isTRUE(.tte_irr_estimable(irr)))
}


#' Attach the estimability decision to one stored incidence rate ratio.
#'
#' The DECISION is data, and `$s3_analyze()` stores it. A reader of
#' `plan$results_ett` then sees whether the ratio may be printed, without
#' repeating the rule. This mirrors `nnt_direction` on the risk-difference row.
#'
#' A value that is not a table with an `IRR` column passes through unchanged.
#' That covers the skip envelope a failed worker returns.
#'
#' @param value One `$irr()` return value, or a skip envelope.
#' @return The same object, with an `irr_estimable` column when it carries one.
#' @noRd
.s3_mark_irr_estimable <- function(value) {
  if (!data.table::is.data.table(value) || !"IRR" %in% names(value)) {
    return(value)
  }
  data.table::set(
    value,
    j = "irr_estimable",
    value = .tte_irr_estimable(value$IRR)
  )
  return(value)
}


#' Format a single measurement block for one row of a results / sensitivity
#' sheet. Returns a named list of **typed** cells keyed by internal
#' disambiguating column names (`col_key_prefix` prepended to the 9 fixed
#' column names): events / PY / rate / p-value are bare numerics (formatted in
#' Excel via [.apply_measurement_numfmt]); IRR and 95% CI stay display strings.
#' Display headers are written separately by the sheet writer, so the prefix
#' never appears in the worksheet.
#' @noRd
.sensitivity_row_fmt <- function(m, col_key_prefix) {
  display_names <- names(.MEASUREMENT_NUMFMT)
  if (is.null(m)) {
    cells <- list(
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_character_,
      NA_character_,
      NA_real_
    )
  } else {
    # The STORED decision. `$s3_analyze()` makes it once, beside the ratio.
    # See `.tte_irr_estimable()` for why a ratio of exactly 0 is inestimable
    # rather than zero.
    irr_estimable <- .tte_irr_estimable_stored(m$irr, m$irr_estimable)
    ci <- if (
      irr_estimable &&
        is.finite(m$lo) &&
        is.finite(m$hi) &&
        m$lo > 0 &&
        m$hi > 0
    ) {
      sprintf("%.2f to %.2f", m$lo, m$hi)
    } else {
      NA_character_
    }
    cells <- list(
      as.numeric(m$events_intervention),
      as.numeric(m$py_intervention),
      as.numeric(m$rate_intervention),
      as.numeric(m$events_cmp),
      as.numeric(m$py_cmp),
      as.numeric(m$rate_cmp),
      if (irr_estimable) sprintf("%.2f", m$irr) else NA_character_,
      ci,
      as.numeric(m$pvalue)
    )
  }
  return(setNames(cells, paste0(col_key_prefix, display_names)))
}


#' Write the "Full results" sheet: one row per ETT, with 5
#' identifier columns (Enrollment | Intervention | Comparator | Outcome |
#' Follow-up) and two side-by-side measurement blocks.
#'
#' Order: **truncated weights on the left, untruncated weights on the
#' right**. The untruncated block is shaded light grey to emphasise the
#' side-by-side comparison. Column headers within each block are just
#' `Events (int)`, `PY (int)`, etc. (no `[truncated]`/`[untruncated]`
#' suffix) -- the merged group header row carries the distinction.
#'
#' @noRd
.write_combined_sensitivity <- function(
  wb,
  sheet_name,
  plan,
  trunc_rates_slot,
  trunc_irr_slot,
  untrunc_rates_slot,
  untrunc_irr_slot,
  title = NULL,
  left_label = "Truncated weights",
  right_label = "Untruncated weights"
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

  # Each side names ONE estimand and weighting combination, and the two slot
  # arguments of a side MUST agree about which. `$get_estimates()` keys the
  # result on the combination, so a mismatched pair would silently report the
  # rates of one weighting beside the ratio of another.
  for (pair in list(
    c(trunc_rates_slot, trunc_irr_slot),
    c(untrunc_rates_slot, untrunc_irr_slot)
  )) {
    if (!identical(.tte_slot_combo(pair[1]), .tte_slot_combo(pair[2]))) {
      stop(
        "'",
        pair[1],
        "' and '",
        pair[2],
        "' name different estimand and weighting combinations",
        call. = FALSE
      )
    }
  }

  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No ETTs to report.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  display_names <- c(
    "Events (int)",
    "PY (int)",
    "Rate/100k (int)",
    "Events (cmp)",
    "PY (cmp)",
    "Rate/100k (cmp)",
    "IRR",
    "95% CI",
    "p-value"
  )

  # Build one row per ETT. Truncated columns come first, then untruncated.
  est <- plan$get_estimates()
  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    untrunc_m <- .sensitivity_row_measurements(est, eid, untrunc_irr_slot)
    trunc_m <- .sensitivity_row_measurements(est, eid, trunc_irr_slot)
    if (is.null(trunc_m) && is.null(untrunc_m)) {
      next
    }

    enr_id <- ett$enrollment_id[i]
    enr_name <- .enrollment_label(plan, enr_id)
    arms <- .lookup_arm_labels(plan$spec, enr_id)
    intervention_name <- if (!is.null(arms)) {
      arms[["intervention"]]
    } else {
      "Intervention"
    }
    comparator_name <- if (!is.null(arms)) {
      arms[["comparator"]]
    } else {
      "Comparator"
    }

    id_cols <- list(
      Enrollment = enr_name,
      Intervention = intervention_name,
      Comparator = comparator_name,
      Outcome = ett$outcome_name[i],
      `Follow-up (weeks)` = as.integer(ett$follow_up[i])
    )
    left_cols <- .sensitivity_row_fmt(trunc_m, "t_")
    right_cols <- .sensitivity_row_fmt(untrunc_m, "u_")
    rows[[length(rows) + 1L]] <- c(id_cols, left_cols, right_cols)
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No valid sensitivity results.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)

  # Layout constants
  n_id <- 5L
  n_block <- length(display_names)
  trunc_cols_start <- n_id + 1L
  trunc_cols_end <- n_id + n_block
  untrunc_cols_start <- trunc_cols_end + 1L
  untrunc_cols_end <- trunc_cols_end + n_block

  group_header_row <- row_ptr
  col_header_row <- row_ptr + 1L
  data_start_row <- row_ptr + 2L

  # --- Styles ---
  group_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    fontSize = 12,
    fgFill = "#D9D9D9",
    border = "TopBottom"
  )
  group_header_untrunc_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    fontSize = 12,
    fgFill = "#BFBFBF",
    border = "TopBottom"
  )
  id_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )
  col_header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#EFEFEF",
    border = "bottom"
  )
  col_header_untrunc_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#DDDDDD",
    border = "bottom"
  )
  body_untrunc_style <- openxlsx::createStyle(fgFill = "#F2F2F2")

  # --- Group header row ---
  openxlsx::mergeCells(
    wb,
    sheet_name,
    cols = untrunc_cols_start:untrunc_cols_end,
    rows = group_header_row
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    right_label,
    startCol = untrunc_cols_start,
    startRow = group_header_row
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = group_header_untrunc_style,
    rows = group_header_row,
    cols = untrunc_cols_start
  )

  openxlsx::mergeCells(
    wb,
    sheet_name,
    cols = trunc_cols_start:trunc_cols_end,
    rows = group_header_row
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    left_label,
    startCol = trunc_cols_start,
    startRow = group_header_row
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = group_header_style,
    rows = group_header_row,
    cols = trunc_cols_start
  )

  # --- Column header row (id cols + display names for both blocks) ---
  id_names <- c(
    "Enrollment",
    "Intervention",
    "Comparator",
    "Outcome",
    "Follow-up (weeks)"
  )
  header_row <- c(id_names, display_names, display_names)
  for (k in seq_along(header_row)) {
    openxlsx::writeData(
      wb,
      sheet_name,
      header_row[k],
      startCol = k,
      startRow = col_header_row
    )
  }
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = id_header_style,
    rows = col_header_row,
    cols = seq_len(n_id),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = col_header_untrunc_style,
    rows = col_header_row,
    cols = untrunc_cols_start:untrunc_cols_end,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = col_header_style,
    rows = col_header_row,
    cols = trunc_cols_start:trunc_cols_end,
    gridExpand = TRUE
  )

  # --- Body: write the data without its own header row ---
  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = data_start_row,
    colNames = FALSE
  )

  data_end_row <- data_start_row + nrow(dt) - 1L
  if (nrow(dt) > 0L) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = body_untrunc_style,
      rows = data_start_row:data_end_row,
      cols = untrunc_cols_start:untrunc_cols_end,
      gridExpand = TRUE,
      stack = TRUE
    )
    body_rows <- data_start_row:data_end_row
    .apply_measurement_numfmt(wb, sheet_name, trunc_cols_start, body_rows)
    .apply_measurement_numfmt(wb, sheet_name, untrunc_cols_start, body_rows)
  }

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(untrunc_cols_end),
    widths = c(
      30,
      20,
      20,
      30,
      12,
      rep(14, n_block),
      rep(14, n_block)
    )
  )
  return(openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = data_start_row,
    firstActiveCol = n_id + 1L
  ))
}
