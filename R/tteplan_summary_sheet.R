# The Summary sheet of the results workbook. It holds one row per ETT, with
# the intention-to-treat and the per-protocol estimates side by side.
#
# Every number on the sheet comes from the sources the "ITT results" and
# "PP results" sheets read: `$get_estimates()` through
# `.sensitivity_row_measurements()`, `.sensitivity_row_fmt()`,
# `.results_rd_row()` and `.rd_sheet_cells()`. The sheet computes no estimate.
# It formats the values of those two sheets as display strings.

# The result slots of the two estimands. The per-protocol slots are the
# truncated-weight combination, which is the one the "PP results" sheet shows.
.SUMMARY_ITT_SLOTS <- c(irr = "irr_itt", rd = "rd_itt")
.SUMMARY_PP_SLOTS <- c(irr = "irr_pp_trunc", rd = "rd_pp_trunc")


#' Read the `family` of one enrollment from the plan's spec
#'
#' @param plan A `TTEPlan`.
#' @param eid Character(1), the enrollment id.
#' @return Character(1), or `NA_character_` when the enrollment sets no
#'   family.
#' @noRd
.enrollment_family <- function(plan, eid) {
  for (enr in plan$spec$enrollments) {
    if (isTRUE(as.character(enr$id) == eid)) {
      family <- enr[["family"]]
      if (is.character(family) && length(family) == 1L) {
        return(family)
      }
    }
  }
  return(NA_character_)
}


#' Format a count the way the results sheets show it
#'
#' The results sheets format a person count with `#,##0`.
#'
#' @param x Numeric vector.
#' @return Character vector.
#' @noRd
.summary_count <- function(x) {
  return(format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}


#' Format a risk difference the way the results sheets show it
#'
#' The results sheets format the risk difference with `+0.00;-0.00;0.00`, so
#' a positive value carries an explicit `+`.
#'
#' @param x Numeric(1), a finite risk difference per 10,000 people.
#' @return Character(1).
#' @noRd
.summary_signed <- function(x) {
  if (x == 0) {
    return("0.00")
  }
  return(sprintf("%+.2f", x))
}


#' Build the Summary cells of one estimand and weighting combination
#'
#' The rules follow `.write_results_single()`. An ETT without usable rates or
#' a usable ratio, and without a stored risk difference, has no row on the
#' results sheet, so all its cells here are `NA`. The event counts and the
#' risk difference show only when the results sheet carries its
#' risk-difference columns. A ratio or a risk difference that the minimum-events
#' rule withheld reads `NE`, and its event counts stay.
#'
#' @param est A `$get_estimates()` table.
#' @param ett_ids Character vector, the ETTs in sheet order.
#' @param slots Named character(2): `irr` and `rd`, the result slots.
#' @return A list of five vectors, each as long as `ett_ids`: `irr_ci`,
#'   `events` and `rd` are display strings, and `py_int` and `py_cmp` are the
#'   person-years of each arm.
#' @noRd
.summary_estimand_cells <- function(est, ett_ids, slots) {
  combo <- .tte_slot_combo(slots[["irr"]])
  if (!identical(.tte_slot_combo(slots[["rd"]]), combo)) {
    stop(
      "'",
      slots[["rd"]],
      "' and '",
      slots[["irr"]],
      "' name different estimand and weighting combinations",
      call. = FALSE
    )
  }
  n <- length(ett_ids)
  out <- list(
    irr_ci = rep(NA_character_, n),
    events = rep(NA_character_, n),
    rd = rep(NA_character_, n),
    py_int = rep(NA_real_, n),
    py_cmp = rep(NA_real_, n)
  )
  rd_cells <- vector("list", n)
  on_sheet <- logical(n)
  for (i in seq_len(n)) {
    m <- .sensitivity_row_measurements(est, ett_ids[i], slots[["irr"]])
    hit <- which(
      est$ett_id == ett_ids[i] &
        est$estimand == combo[["estimand"]] &
        est$weights == combo[["weights"]]
    )
    rd_row <- .results_rd_row(est, hit, slots[["rd"]])
    # The rule of `.write_results_single()`: a stored risk difference keeps
    # the row on the sheet when the rates or the ratio are missing.
    if (is.null(m) && is.null(rd_row)) {
      next
    }
    on_sheet[i] <- TRUE
    fmt <- .sensitivity_row_fmt(m, "")
    irr <- fmt[["IRR"]]
    ci <- fmt[["95% CI"]]
    if (isTRUE(m$withheld)) {
      out$irr_ci[i] <- "NE"
    } else if (!is.na(irr)) {
      out$irr_ci[i] <- if (is.na(ci)) irr else paste0(irr, " (", ci, ")")
    }
    out$py_int[i] <- fmt[["PY (int)"]]
    out$py_cmp[i] <- fmt[["PY (cmp)"]]
    rd_cells[[i]] <- .rd_sheet_cells(rd_row)
  }
  if (!.results_has_rd(rd_cells[on_sheet])) {
    return(out)
  }
  for (i in which(on_sheet)) {
    cells <- rd_cells[[i]]
    if (is.finite(cells[[1]]) && is.finite(cells[[2]])) {
      out$events[i] <- paste(
        .summary_count(cells[[1]]),
        "/",
        .summary_count(cells[[2]])
      )
    }
    if (isTRUE(attr(cells, "withheld"))) {
      out$rd[i] <- "NE"
    } else if (is.finite(cells[[3]])) {
      point <- .summary_signed(cells[[3]])
      out$rd[i] <- if (is.na(cells[[4]])) {
        point
      } else {
        paste0(point, " (", cells[[4]], ")")
      }
    }
  }
  return(out)
}


#' Format the per-protocol person-years as a share of the intention-to-treat
#' person-years, per arm
#'
#' @param pp,itt Lists from `.summary_estimand_cells()`.
#' @return Character vector, one `"56% / 99%"` cell per ETT, or `NA` when an
#'   arm lacks a finite ratio.
#' @noRd
.summary_retention <- function(pp, itt) {
  r_int <- pp$py_int / itt$py_int
  r_cmp <- pp$py_cmp / itt$py_cmp
  ok <- is.finite(r_int) & is.finite(r_cmp)
  out <- rep(NA_character_, length(r_int))
  out[ok] <- sprintf("%.0f%% / %.0f%%", 100 * r_int[ok], 100 * r_cmp[ok])
  return(out)
}


#' Write the "Summary" sheet: one row per ETT, in `plan$ett` order
#'
#' The header is row 1, with an autofilter on every column, and the header
#' row is frozen. The `Family` column shows only when an enrollment of the
#' spec sets `family`.
#'
#' @param wb An openxlsx workbook.
#' @param plan A `TTEPlan`.
#' @param sheet_name Character(1), the sheet name.
#' @return `invisible(NULL)`.
#' @noRd
.write_summary_sheet <- function(wb, plan, sheet_name = "Summary") {
  openxlsx::addWorksheet(wb, sheet_name)
  ett <- plan$ett
  if (is.null(ett)) {
    ett <- data.table::data.table(
      ett_id = character(0),
      enrollment_id = character(0),
      outcome_name = character(0),
      follow_up = integer(0)
    )
  }
  ett_ids <- as.character(ett$ett_id)
  eids <- as.character(ett$enrollment_id)
  est <- plan$get_estimates()
  itt <- .summary_estimand_cells(est, ett_ids, .SUMMARY_ITT_SLOTS)
  pp <- .summary_estimand_cells(est, ett_ids, .SUMMARY_PP_SLOTS)

  contrast <- vapply(
    eids,
    function(eid) {
      return(paste0(eid, ": ", .enrollment_label(plan, eid)))
    },
    character(1),
    USE.NAMES = FALSE
  )
  dt <- data.frame(
    ETT = ett_ids,
    Contrast = contrast,
    Outcome = as.character(ett$outcome_name),
    `Follow-up (weeks)` = as.integer(ett$follow_up),
    `Events int/cmp (ITT)` = itt$events,
    `ITT IRR (CI)` = itt$irr_ci,
    `Events int/cmp (PP)` = pp$events,
    `PP IRR (CI)` = pp$irr_ci,
    `PP PY retention (int / cmp)` = .summary_retention(pp, itt),
    `PP RD per 10,000 (CI)` = pp$rd,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  widths <- c(11, 36, 30, 10, 16, 22, 16, 22, 18, 26)

  spec_families <- vapply(
    plan$spec$enrollments,
    function(enr) {
      return("family" %in% names(enr))
    },
    logical(1)
  )
  if (any(spec_families)) {
    family <- vapply(
      eids,
      function(eid) {
        return(.enrollment_family(plan, eid))
      },
      character(1),
      USE.NAMES = FALSE
    )
    dt <- cbind(dt[1L], Family = family, dt[-1L], stringsAsFactors = FALSE)
    widths <- c(widths[1L], 20, widths[-1L])
  }

  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = 1L,
    startCol = 1L,
    withFilter = TRUE,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom",
      wrapText = TRUE
    )
  )
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_along(widths),
    widths = widths
  )
  return(invisible(NULL))
}
