# These helpers produce the single-estimand results sheet. Three of them
# build the risk-difference columns it carries after the measurement block.

#' Excel number formats for the three NUMERIC risk-difference columns that the
#' single-estimand results sheets carry after the measurement block. The
#' interval is a fourth column and stays a display string, like `95% CI`.
#'
#' The risk-difference format prints an explicit `+` on a positive value. The
#' sign is the clinical direction, so it is not decoration. `+4.88` and `-4.88`
#' are opposite results, and a reader must not have to look for a minus.
#' @noRd
.RD_SHEET_NUMFMT <- c(
  "Persons with event (int)" = "#,##0",
  "Persons with event (cmp)" = "#,##0",
  "Risk difference per 10,000" = "+0.00;-0.00;0.00"
)


#' Build the `rd_lookup` a forest figure draws its risk-difference columns
#' from, out of `$get_estimates()`.
#'
#' `.forest_rd_map()` keys the lookup on `ett_id` and reads six required
#' columns plus the two decision columns. `$get_estimates()` carries all eight
#' under the accessor's own names, so this renames rather than computes.
#'
#' An emulated trial gets a row when the plan stored a risk difference for that
#' estimand and weighting. Every risk-difference field `NA` means the plan
#' stored none, and the trial then gets no row and renders an empty cell.
#'
#' @param plan A TTEPlan.
#' @param rd_slot Character(1), the risk-difference slot naming the wanted
#'   combination.
#' @param keep_ett_ids Character vector of the identifiers the figure draws.
#' @return A data.table, or `NULL` when nothing was stored.
#' @noRd
.tte_rd_lookup <- function(plan, rd_slot, keep_ett_ids) {
  est <- .tte_estimates_for_slot(plan, rd_slot)
  if (nrow(est) == 0L) {
    return(NULL)
  }
  # The stored SHAPE. A risk-difference row whose values are `NA` still gets a
  # lookup entry, as it did when this read the slot directly.
  hit <- which(est$rd_stored & est$ett_id %in% keep_ett_ids)
  if (length(hit) == 0L) {
    return(NULL)
  }
  data.table::data.table(
    ett_id = as.character(est$ett_id[hit]),
    rd = est$rd[hit],
    rd_lo = est$rd_lo[hit],
    rd_hi = est$rd_hi[hit],
    nnt = est$nnt[hit],
    nnt_direction = est$nnt_direction[hit],
    n_persons_with_event_intervention = est$persons_event_int[hit],
    n_persons_with_event_comparator = est$persons_event_cmp[hit],
    conf_level = est$conf_level[hit]
  )
}


#' Build the four risk-difference cells for one row of a results sheet.
#'
#' The two counts are distinct PEOPLE who had the outcome, unweighted. They are
#' NOT the `Events (int)` / `Events (cmp)` columns in the measurement block.
#' Those are weighted sums over event ROWS, and they count one woman twice when
#' she carries the event in two of her sequential trials. The headers say which
#' is which.
#'
#' The risk difference keeps its sign and is scaled to 10,000 people, matching
#' the forest figure.
#'
#' @param rd_row A one-row table carrying the `$get_estimates()` risk-difference
#'   columns (`persons_event_int`, `persons_event_cmp`, `rd`, `rd_lo` and
#'   `rd_hi`), or NULL.
#' @return An unnamed list of four cells: two counts, the risk difference, and
#'   its interval as a display string.
#' @noRd
.rd_sheet_cells <- function(rd_row) {
  if (is.null(rd_row) || nrow(rd_row) == 0L) {
    return(list(NA_real_, NA_real_, NA_real_, NA_character_))
  }
  per <- 10000
  pick <- function(nm) as.numeric(rd_row[[nm]])[1]
  rd <- pick("rd")
  lo <- pick("rd_lo")
  hi <- pick("rd_hi")
  list(
    pick("persons_event_int"),
    pick("persons_event_cmp"),
    if (is.finite(rd)) rd * per else NA_real_,
    if (is.finite(lo) && is.finite(hi)) {
      sprintf("%+.2f to %+.2f", lo * per, hi * per)
    } else {
      NA_character_
    }
  )
}


#' Resolve the confidence level the risk-difference interval header states.
#'
#' One header covers the whole column, so every interval under it must have
#' been computed at one level. This keeps the contract [.forest_rd_conf_level()]
#' sets for the figure: refuse rather than print a level the numbers do not
#' have.
#'
#' @param levels Numeric vector of per-row confidence levels, `NA` allowed.
#' @return A character(1) percentage with no percent sign. Falls back to `"95"`
#'   when no row recorded a level.
#' @noRd
.rd_sheet_conf_pct <- function(levels) {
  seen <- unique(as.numeric(levels))
  seen <- seen[!is.na(seen)]
  if (length(seen) == 0L) {
    return(.ff_conf_pct(0.95))
  }
  if (length(seen) > 1L) {
    stop(
      "the risk differences on this sheet mix confidence levels (",
      paste(seen, collapse = ", "),
      "); one column cannot carry two."
    )
  }
  .ff_conf_pct(seen)
}


#' Write a single-estimand results sheet: one row per ETT with the 5 identifier
#' columns and one measurement block (events / PY / rate per arm + IRR + 95% CI
#' + p-value). Numbers are real (Excel numFmt via [.apply_measurement_numfmt]);
#' IRR and 95% CI are display strings. Used for "PP results" and "ITT results".
#'
#' `rd_slot` names the per-ETT list element holding the risk-difference row
#' (`"rd_pp_trunc"` or `"rd_itt"`, written by `$s3_analyze()` for every ETT).
#' When at least one ETT carries one, four more columns follow the measurement
#' block.
#' Those are the per-arm distinct-person event counts, the signed risk
#' difference per 10,000 people, and its interval. When no ETT carries one, the
#' four columns are left out rather than heading a block of empty cells.
#' @noRd
.write_results_single <- function(
  wb,
  sheet_name,
  plan,
  rates_slot,
  irr_slot,
  rd_slot = NULL,
  title = NULL
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

  if (!identical(.tte_slot_combo(rates_slot), .tte_slot_combo(irr_slot))) {
    stop(
      "'",
      rates_slot,
      "' and '",
      irr_slot,
      "' name different estimand and weighting combinations"
    )
  }
  # The risk difference belongs to the SAME combination as the rates and the
  # ratio, so `$get_estimates()` already carries it on the same row.
  if (
    !is.null(rd_slot) &&
      !identical(.tte_slot_combo(rd_slot), .tte_slot_combo(irr_slot))
  ) {
    stop(
      "'",
      rd_slot,
      "' and '",
      irr_slot,
      "' name different estimand and weighting combinations"
    )
  }

  est <- plan$get_estimates()
  combo <- .tte_slot_combo(irr_slot)
  display_names <- names(.MEASUREMENT_NUMFMT)
  rd_names <- names(.RD_SHEET_NUMFMT)
  rd_cells <- list()
  rd_levels <- numeric(0)
  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    m <- .sensitivity_row_measurements(est, eid, irr_slot)
    if (is.null(m)) {
      next
    }
    hit <- which(
      est$ett_id == eid &
        est$estimand == combo[["estimand"]] &
        est$weights == combo[["weights"]]
    )
    rd_row <- if (
      is.null(rd_slot) ||
        length(hit) == 0L ||
        !isTRUE(est$rd_stored[hit[1L]])
    ) {
      NULL
    } else {
      est[hit[1L]]
    }
    rd_cells[[length(rd_cells) + 1L]] <- .rd_sheet_cells(rd_row)
    if (!is.null(rd_row)) {
      rd_levels <- c(rd_levels, as.numeric(rd_row[["conf_level"]])[1])
    }
    enr_id <- ett$enrollment_id[i]
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
      Enrollment = .enrollment_label(plan, enr_id),
      Intervention = intervention_name,
      Comparator = comparator_name,
      Outcome = ett$outcome_name[i],
      `Follow-up (weeks)` = as.integer(ett$follow_up[i])
    )
    rows[[length(rows) + 1L]] <- c(id_cols, .sensitivity_row_fmt(m, ""))
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(wb, sheet_name, "No valid results.", startRow = row_ptr)
    return(invisible(NULL))
  }

  # The four risk-difference columns are composed only when something populated
  # them. A header over a block of empty cells claims a quantity that was never
  # computed, and computing it costs minutes per ETT, so most exports have none.
  has_rd <- any(vapply(
    rd_cells,
    function(cells) is.finite(cells[[3]]),
    logical(1)
  ))
  if (has_rd) {
    rd_headers <- c(
      rd_names,
      paste0("Risk difference ", .rd_sheet_conf_pct(rd_levels), "% CI")
    )
    for (k in seq_along(rows)) {
      rows[[k]] <- c(rows[[k]], setNames(rd_cells[[k]], rd_headers))
    }
  } else {
    # A caller that named an `rd_slot` asked for the risk difference and got
    # nothing. $s3_analyze() writes that slot for every ETT, so a cold cache
    # now means s3 has not run against this plan, or every ETT failed. Say so.
    # Dropping four columns in silence is how a stale results file stays
    # invisible: the sheet still looks complete.
    if (!is.null(rd_slot) && length(rd_cells) > 0L) {
      message(
        "No cached risk difference for '",
        rd_slot,
        "', so this sheet omits ",
        "the risk-difference columns. Run $s3_analyze() before ",
        "$export_tables()."
      )
    }
    rd_headers <- character(0)
  }

  dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  n_id <- 5L
  col_header_row <- row_ptr
  data_start_row <- row_ptr + 1L

  id_names <- c(
    "Enrollment",
    "Intervention",
    "Comparator",
    "Outcome",
    "Follow-up (weeks)"
  )
  header_row <- c(id_names, display_names, rd_headers)
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
    style = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    ),
    rows = col_header_row,
    cols = seq_along(header_row),
    gridExpand = TRUE
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = data_start_row,
    colNames = FALSE
  )
  data_end_row <- data_start_row + nrow(dt) - 1L
  .apply_measurement_numfmt(
    wb,
    sheet_name,
    n_id + 1L,
    data_start_row:data_end_row
  )
  if (has_rd) {
    rd_start <- n_id + length(display_names) + 1L
    for (j in seq_along(.RD_SHEET_NUMFMT)) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        style = openxlsx::createStyle(numFmt = .RD_SHEET_NUMFMT[[j]]),
        rows = data_start_row:data_end_row,
        cols = rd_start + j - 1L,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_along(header_row),
    widths = c(
      30,
      20,
      20,
      30,
      12,
      rep(14, length(display_names)),
      rep(24, length(rd_headers))
    )
  )
  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = data_start_row,
    firstActiveCol = n_id + 1L
  )
  invisible(NULL)
}
