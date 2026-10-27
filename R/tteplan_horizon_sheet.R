# The two "Minimum events" sheets of the results workbook and the
# feasibility workbook. Both are wide: one row per outcome and comparison,
# one column per follow-up horizon, per-protocol only. The
# intention-to-treat estimate follows the per-protocol decision, so it needs
# no column of its own.
#
# "Minimum events (Y or -)" shows the decision at the spec's cut. "Minimum
# events (#)" shows the largest cut of `.HORIZON_CUT_GRID` at which the
# estimate is still reported, so a different cut can be read without a
# rerun. Neither sheet shows an event count, a rate or expected events.

#' The sheet names. Excel does not allow "/" in a sheet name.
#' @noRd
.SHEET_MIN_EVENTS_YN <- "Minimum events (Y or -)"

#' @noRd
.SHEET_MIN_EVENTS_CUT <- "Minimum events (#)"

#' The fills of a reported and a withheld cell.
#' @noRd
.MIN_EVENTS_FILL <- c(reported = "#BDD7EE", withheld = "#F4B6B6")


#' The wide table of the per-protocol decisions
#'
#' @param plan A TTEPlan.
#' @param records A list keyed by ETT id. Each element is the per-protocol
#'   horizon record of that ETT. An ETT without a record gets an empty cell.
#' @return A list. `table` is a data.frame with the columns `Outcome`, `#`,
#'   `Comparison` and one column per follow-up horizon, named by the spec's
#'   follow-up label. `yn` holds `"Y"` or `"-"`, `cut` the largest passing cut
#'   or `"-"`. A cell without an ETT record is `NA`, and `Outcome` is `NA`
#'   below the first row of each outcome. `withheld` is a logical matrix with the shape of the horizon
#'   columns, `NA` where an ETT has no record. `table` has no rows when the
#'   plan has no ETT.
#' @noRd
.minimum_events_wide <- function(plan, records) {
  ett <- plan$ett
  fu <- plan$spec$follow_up %||% list()
  fu_weeks <- vapply(fu, function(f) as.numeric(f$weeks), numeric(1))
  fu_label <- vapply(fu, function(f) as.character(f$label), character(1))
  weeks <- sort(unique(as.numeric(ett$follow_up)))
  label <- fu_label[match(weeks, fu_weeks)]
  label[is.na(label)] <- paste(weeks[is.na(label)], "weeks")
  enr <- .osf_comparison_numbers(plan$spec)
  enr_ids <- unique(c(names(enr), unique(ett$enrollment_id)))
  enr_ids <- enr_ids[enr_ids %in% ett$enrollment_id]
  outcomes <- unique(ett$outcome_name)

  n <- length(outcomes) * length(enr_ids)
  yn <- matrix(NA_character_, n, length(weeks))
  cut <- matrix(NA_character_, n, length(weeks))
  withheld <- matrix(NA, n, length(weeks))
  out_col <- rep(NA_character_, n)
  num_col <- integer(n)
  cmp_col <- character(n)
  r <- 0L
  for (o in outcomes) {
    for (eid in enr_ids) {
      r <- r + 1L
      if (eid == enr_ids[1L]) {
        out_col[r] <- o
      }
      num_col[r] <- match(eid, enr_ids)
      cmp_col[r] <- .enrollment_label(plan, eid)
      for (j in seq_along(weeks)) {
        k <- which(
          ett$enrollment_id == eid &
            ett$outcome_name == o &
            as.numeric(ett$follow_up) == weeks[j]
        )
        if (length(k) > 1L) {
          stop(
            "Minimum events sheets: ",
            length(k),
            " ETTs (",
            paste(ett$ett_id[k], collapse = ", "),
            ") share enrollment ",
            eid,
            ", outcome '",
            o,
            "' and follow-up ",
            weeks[j],
            " weeks, so they cannot share one cell. Give each outcome a ",
            "distinct name.",
            call. = FALSE
          )
        }
        if (length(k) == 0L) {
          next
        }
        rec <- records[[ett$ett_id[k]]]
        if (!is.list(rec)) {
          next
        }
        if (!"max_passing_cut" %in% names(rec)) {
          stop(
            "Minimum events sheets: the record of ETT ",
            ett$ett_id[k],
            " holds no max_passing_cut. It was stored before swereg 27.3.0. ",
            "Rerun the step that wrote it.",
            call. = FALSE
          )
        }
        withheld[r, j] <- isTRUE(rec$withheld)
        yn[r, j] <- if (rec$withheld) "-" else "Y"
        cut[r, j] <- if (is.na(rec$max_passing_cut)) {
          "-"
        } else {
          format(rec$max_passing_cut, trim = TRUE)
        }
      }
    }
  }
  base <- data.frame(
    Outcome = out_col,
    `#` = num_col,
    Comparison = cmp_col,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  as_table <- function(m) {
    colnames(m) <- label
    return(cbind(base, as.data.frame(m, stringsAsFactors = FALSE)))
  }
  return(list(yn = as_table(yn), cut = as_table(cut), withheld = withheld))
}


#' Write one wide Minimum events sheet
#'
#' @param wb An openxlsx workbook.
#' @param sheet_name Character(1).
#' @param title,note Character(1), rows 1 and 2.
#' @param table The data.frame to write from row 4.
#' @param withheld The logical matrix of `.minimum_events_wide()`.
#' @return `invisible(NULL)`.
#' @noRd
.write_minimum_events_one <- function(
  wb,
  sheet_name,
  title,
  note,
  table,
  withheld
) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, title, startRow = 1L)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(wb, sheet_name, note, startRow = 2L)
  if (nrow(table) == 0L) {
    openxlsx::writeData(wb, sheet_name, "(no ETT)", startRow = 4L)
    return(invisible(NULL))
  }
  header_row <- 4L
  openxlsx::writeData(wb, sheet_name, table, startRow = header_row)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    ),
    rows = header_row,
    cols = seq_along(table),
    gridExpand = TRUE
  )
  first <- ncol(table) - ncol(withheld)
  for (state in c("reported", "withheld")) {
    hit <- which(
      !is.na(withheld) & withheld == (state == "withheld"),
      arr.ind = TRUE
    )
    if (nrow(hit) == 0L) {
      next
    }
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(
        fgFill = .MIN_EVENTS_FILL[[state]],
        halign = "center"
      ),
      rows = header_row + hit[, 1L],
      cols = first + hit[, 2L],
      gridExpand = FALSE
    )
  }
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold"),
    rows = header_row + which(!is.na(table$Outcome)),
    cols = 1L,
    stack = TRUE
  )
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_along(table),
    widths = c(30, 4, 60, rep(12, ncol(withheld)))
  )
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = header_row + 1L)
  return(invisible(NULL))
}


#' Write the two Minimum events sheets
#'
#' @param wb An openxlsx workbook.
#' @param plan A TTEPlan.
#' @param records A list keyed by ETT id: the per-protocol horizon record of
#'   each ETT.
#' @return `invisible(NULL)`.
#' @noRd
.write_minimum_events_sheets <- function(wb, plan, records) {
  k <- plan$spec[["min_expected_events_per_arm_per_year"]]
  k <- if (is.null(k)) "not set" else format(k, trim = TRUE)
  wide <- .minimum_events_wide(plan, records)
  .write_minimum_events_one(
    wb,
    .SHEET_MIN_EVENTS_YN,
    "Minimum-events rule: per-protocol estimates reported (Y) or withheld (-)",
    paste0(
      "Y: at least ",
      k,
      " expected events per arm in every follow-up year up to the horizon. ",
      "-: withheld. The intention-to-treat estimate follows the per-protocol ",
      "decision."
    ),
    wide$yn,
    wide$withheld
  )
  .write_minimum_events_one(
    wb,
    .SHEET_MIN_EVENTS_CUT,
    "Minimum-events rule: the largest cut at which each per-protocol estimate is reported",
    paste0(
      "Each cell is the largest cut, from ",
      paste(format(.HORIZON_CUT_GRID, trim = TRUE), collapse = ", "),
      " expected events per arm per follow-up year, at which the estimate is ",
      "reported. -: withheld even at ",
      format(min(.HORIZON_CUT_GRID), trim = TRUE),
      ". The fill shows the decision at the current cut, ",
      k,
      "."
    ),
    wide$cut,
    wide$withheld
  )
  return(invisible(NULL))
}
