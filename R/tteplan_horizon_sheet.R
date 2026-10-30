# The two "Minimum events" sheets of the results workbook and the
# feasibility workbook. Both are wide: one row per outcome and comparison,
# one column per whole follow-up year, per-protocol only. The
# intention-to-treat estimate follows the per-protocol decision, so it needs
# no column of its own.
#
# "Minimum events (Y or -)" shows the verdict of the rule at the spec's cut.
# "Minimum events (#)" shows the largest cut of `.HORIZON_CUT_GRID` at which
# the estimate still passes the rule, so a different cut can be read without
# a rerun. Neither sheet shows an event count, a rate or expected events.

#' The sheet names. Excel does not allow "/" in a sheet name.
#' @noRd
.SHEET_MIN_EVENTS_YN <- "Minimum events (Y or -)"

#' @noRd
.SHEET_MIN_EVENTS_CUT <- "Minimum events (#)"

#' The fills of a reported and a withheld cell.
#' @noRd
.MIN_EVENTS_FILL <- c(reported = "#BDD7EE", withheld = "#F4B6B6")


#' The per-year table of a horizon record
#'
#' A record of swereg 27.3.1 holds `by_year`. A record of `$s3_analyze()`
#' from swereg 27.3.0 holds none, and its window table still holds
#' `events_expected`, so the table is derived from the windows. A record
#' with neither is a feasibility record of swereg 27.3.0: its windows lost
#' `events_expected` in the worker, and the function stops.
#'
#' @param rec A horizon record.
#' @param ett_id Character(1), for the message.
#' @return The `by_year` table of `.tte_horizon_by_year()`.
#' @noRd
.minimum_events_by_year <- function(rec, ett_id) {
  if (data.table::is.data.table(rec$by_year)) {
    return(rec$by_year)
  }
  win <- rec$windows
  if (is.data.frame(win) && "events_expected" %in% names(win)) {
    return(.tte_horizon_by_year(data.table::as.data.table(win)))
  }
  stop(
    "Minimum events sheets: the record of ETT ",
    ett_id,
    " holds no decision for each follow-up year. It is a feasibility record ",
    "stored before swereg 27.3.1. Rerun the feasibility step, ",
    "$s3_feasibility().",
    call. = FALSE
  )
}


#' The column label of a whole follow-up year
#'
#' @param year Integer, the follow-up years.
#' @param spec The study specification, or `NULL`.
#' @return Character, the label of the spec follow-up with `52 * year`
#'   weeks, and otherwise `"1 year"` or `"<k> years"`.
#' @noRd
.minimum_events_year_label <- function(year, spec) {
  fu <- spec$follow_up %||% list()
  fu_weeks <- vapply(fu, function(f) as.numeric(f$weeks), numeric(1))
  fu_label <- vapply(fu, function(f) as.character(f$label), character(1))
  label <- fu_label[match(year * .HORIZON_WINDOW_WEEKS, fu_weeks)]
  own <- ifelse(year == 1L, "1 year", paste(year, "years"))
  label[is.na(label)] <- own[is.na(label)]
  return(label)
}


#' The wide table of the per-protocol decisions
#'
#' The table has one column per whole follow-up year k = 1 to K. K is the
#' longest follow-up of all ETTs, feasibility-horizon ETTs included, in whole
#' years of `.HORIZON_WINDOW_WEEKS` weeks. No column stands for a follow-up
#' that is not a whole number of years.
#'
#' A cell of year k reads the ETT of its outcome and comparison with a
#' follow-up of `52 * k` weeks. It shows the decision of that record. When no such
#' ETT exists, the cell reads row k of `by_year` of the shortest ETT with a
#' longer follow-up. When no ETT of the row reaches year k, the cell is empty.
#' So an ETT shorter than one year appears in no cell.
#'
#' @param plan A TTEPlan.
#' @param records A list keyed by ETT id. Each element is the per-protocol
#'   horizon record of that ETT. An ETT without a record gets an empty cell.
#' @return A list. `table` is a data.frame with the columns `Outcome`, `#`,
#'   `Comparison` and one column per whole follow-up year. A year column
#'   takes the spec's follow-up label when the spec has a follow-up of that
#'   length, and otherwise `"1 year"` or `"<k> years"`. `yn` holds `"Y"` or `"-"`, `cut`
#'   the largest passing cut or `"-"`. A cell without an ETT record is `NA`,
#'   and `Outcome` is `NA` below the first row of each outcome. `withheld` is
#'   a logical matrix with the shape of the year columns, `NA` where a cell
#'   is empty. `table` has no rows when the plan has no ETT. Every cell shows
#'   the computed verdict of the rule, also for an exempt enrollment. The
#'   `Comparison` of an enrollment that the spec exempts ends with
#'   `" (exempt)"`. `any_exempt` is `TRUE` when at least one enrollment of a
#'   row is exempt. `any_longer` is `TRUE` when at least one cell reads the
#'   `by_year` of a longer ETT. `feasibility_weeks` is the longest follow-up
#'   of a feasibility-horizon ETT, or `NA` when the plan has none. A
#'   horizon-only ETT of `.tte_ceiling_follow_ups()` does not count there.
#'   `ceiling_note` is the note of `.minimum_events_ceilings()`.
#' @noRd
.minimum_events_wide <- function(plan, records) {
  ett <- plan$ett
  width <- .HORIZON_WINDOW_WEEKS
  ett_fu <- as.numeric(ett$follow_up)
  n_years <- if (length(ett_fu) == 0L) {
    0L
  } else {
    as.integer(floor(max(ett_fu) / width))
  }
  years <- seq_len(n_years)
  label <- .minimum_events_year_label(years, plan$spec)
  enr <- .osf_comparison_numbers(plan$spec)
  enr_ids <- unique(c(names(enr), unique(ett$enrollment_id)))
  enr_ids <- enr_ids[enr_ids %in% ett$enrollment_id]
  outcomes <- unique(ett$outcome_name)
  is_feas <- if ("feasibility_horizon" %in% names(ett)) {
    ett$feasibility_horizon %in% TRUE
  } else {
    rep(FALSE, nrow(ett))
  }
  # A horizon-only ETT is either the feasibility horizon or the whole year
  # that decides a part-year horizon.
  ceilings <- .minimum_events_ceilings(plan, is_feas)
  is_feas[ceilings$rows] <- FALSE

  # Every record of an ETT of the plan MUST carry `max_passing_cut` and a
  # per-year table.
  for (i in seq_len(nrow(ett))) {
    rec <- records[[ett$ett_id[i]]]
    if (!is.list(rec)) {
      next
    }
    if (!"max_passing_cut" %in% names(rec)) {
      stop(
        "Minimum events sheets: the record of ETT ",
        ett$ett_id[i],
        " holds no max_passing_cut. It was stored before swereg 27.3.0. ",
        "Rerun the step that wrote it.",
        call. = FALSE
      )
    }
    .minimum_events_by_year(rec, ett$ett_id[i])
  }

  n <- length(outcomes) * length(enr_ids)
  yn <- matrix(NA_character_, n, n_years)
  cut <- matrix(NA_character_, n, n_years)
  withheld <- matrix(NA, n, n_years)
  out_col <- rep(NA_character_, n)
  num_col <- integer(n)
  cmp_col <- character(n)
  any_longer <- FALSE
  r <- 0L
  for (o in outcomes) {
    for (eid in enr_ids) {
      r <- r + 1L
      if (eid == enr_ids[1L]) {
        out_col[r] <- o
      }
      num_col[r] <- match(eid, enr_ids)
      cmp_col[r] <- .enrollment_label(plan, eid)
      if (.tte_enrollment_rule_exempt(plan$spec, eid)) {
        cmp_col[r] <- paste0(cmp_col[r], " (exempt)")
      }
      rows <- which(ett$enrollment_id == eid & ett$outcome_name == o)
      for (w in unique(ett_fu[rows])) {
        k <- rows[ett_fu[rows] == w]
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
            w,
            " weeks, so they cannot share one cell. Give each outcome a ",
            "distinct name.",
            call. = FALSE
          )
        }
      }
      for (j in seq_along(years)) {
        target <- width * years[j]
        k <- rows[ett_fu[rows] == target]
        own <- length(k) == 1L
        if (!own) {
          longer <- rows[ett_fu[rows] > target]
          if (length(longer) == 0L) {
            next
          }
          k <- longer[which.min(ett_fu[longer])]
        }
        rec <- records[[ett$ett_id[k]]]
        if (!is.list(rec)) {
          next
        }
        if (own) {
          fails <- isTRUE(rec$withheld)
          best <- rec$max_passing_cut
        } else {
          by_year <- .minimum_events_by_year(rec, ett$ett_id[k])
          row_k <- by_year[by_year$year == years[j], ]
          fails <- !isTRUE(row_k$reportable)
          best <- row_k$max_passing_cut
          any_longer <- TRUE
        }
        withheld[r, j] <- fails
        yn[r, j] <- if (fails) "-" else "Y"
        cut[r, j] <- if (is.na(best)) {
          "-"
        } else {
          format(best, trim = TRUE)
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
  any_exempt <- any(vapply(
    enr_ids,
    function(eid) .tte_enrollment_rule_exempt(plan$spec, eid),
    logical(1)
  ))
  return(list(
    yn = as_table(yn),
    cut = as_table(cut),
    withheld = withheld,
    any_exempt = n > 0L && any_exempt,
    any_longer = any_longer,
    feasibility_weeks = if (any(is_feas)) max(ett_fu[is_feas]) else NA_real_,
    ceiling_note = ceilings$note
  ))
}


#' The horizon-only ETTs that decide a part-year horizon
#'
#' `.tteplan_add_spec_etts()` adds them through `.tte_ceiling_follow_ups()`.
#' A horizon-only ETT is one of them when its follow-up is a whole year that
#' `.tte_ceiling_follow_ups()` returns for its enrollment.
#'
#' @param plan A TTEPlan.
#' @param is_feas Logical, `TRUE` on each horizon-only row of `plan$ett`.
#' @return A list. `rows` holds the row numbers of those ETTs in `plan$ett`.
#'   `note` holds one sentence per whole year, or is `NULL` when the plan has
#'   no such ETT.
#' @noRd
.minimum_events_ceilings <- function(plan, is_feas) {
  ett <- plan$ett
  rows <- integer(0)
  decides <- list()
  for (i in which(is_feas)) {
    for (fu in .tte_ceiling_follow_ups(plan$spec, ett$enrollment_id[i])) {
      if (fu$weeks == as.numeric(ett$follow_up[i])) {
        rows <- c(rows, i)
        key <- as.character(fu$weeks)
        decides[[key]] <- sort(unique(c(decides[[key]], fu$decides)))
      }
    }
  }
  weeks <- function(x) {
    return(paste0(format(x, trim = TRUE, scientific = FALSE), "-week"))
  }
  note <- vapply(
    names(decides),
    function(key) {
      return(paste0(
        "The ",
        weeks(as.numeric(key)),
        " follow-up is the whole year that decides the ",
        .osf_and(weeks(decides[[key]])),
        " follow-up; no estimate is reported for the ",
        weeks(as.numeric(key)),
        " follow-up."
      ))
    },
    character(1),
    USE.NAMES = FALSE
  )
  return(list(
    rows = rows,
    note = if (length(note) > 0L) paste(note, collapse = " ")
  ))
}


#' Write one wide Minimum events sheet
#'
#' @param wb An openxlsx workbook.
#' @param sheet_name Character(1).
#' @param title,note Character(1), rows 1 and 2.
#' @param table The data.frame to write from row 4.
#' @param withheld The logical matrix of `.minimum_events_wide()`.
#' @param year_note Character(1) or `NULL`, row 3. `NULL` writes no row 3.
#' @return `invisible(NULL)`.
#' @noRd
.write_minimum_events_one <- function(
  wb,
  sheet_name,
  title,
  note,
  table,
  withheld,
  year_note = NULL
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
  if (!is.null(year_note)) {
    openxlsx::writeData(wb, sheet_name, year_note, startRow = 3L)
  }
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
#' The cells and the fills show the verdict of the rule. The title and the
#' note of the "Minimum events (Y or -)" sheet say so. When an enrollment
#' of the sheet is exempt, the note of each sheet ends with one sentence on
#' the exemption.
#'
#' Row 3 of each sheet explains the year columns, and it is empty when none
#' of its sentences applies. The first sentence appears when a cell reads the
#' `by_year` table of a longer ETT. The second appears when the plan holds a
#' feasibility-horizon ETT, and it names the weeks of that horizon. Then one
#' sentence follows for each whole year that the grid adds to decide a
#' part-year horizon. It names that whole year and the part-year horizons.
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
  year_note <- c(
    if (isTRUE(wide$any_longer)) {
      paste0(
        "Years without their own follow-up are read from the shortest ",
        "longer follow-up."
      )
    },
    if (!is.na(wide$feasibility_weeks)) {
      paste0(
        "Years beyond the analysis follow-up come from the feasibility ",
        "horizon of ",
        format(wide$feasibility_weeks, trim = TRUE, scientific = FALSE),
        " weeks (feasibility_follow_up_weeks); no estimate is reported for ",
        "them."
      )
    },
    wide$ceiling_note
  )
  if (!is.null(year_note)) {
    year_note <- paste(year_note, collapse = " ")
  }
  exempt_note <- if (isTRUE(wide$any_exempt)) {
    paste0(
      " Exempt: the specification exempts this comparison; its estimates ",
      "are reported whatever the rule decides."
    )
  }
  .write_minimum_events_one(
    wb,
    .SHEET_MIN_EVENTS_YN,
    "Minimum-events rule: per-protocol estimates pass (Y) or fail (-) the rule",
    paste0(
      "Y: at least ",
      k,
      " expected events per arm in every follow-up year up to the horizon. ",
      "-: fails the rule; the estimate is withheld unless the comparison is ",
      "exempt. The intention-to-treat estimate follows the per-protocol ",
      "decision.",
      exempt_note
    ),
    wide$yn,
    wide$withheld,
    year_note
  )
  .write_minimum_events_one(
    wb,
    .SHEET_MIN_EVENTS_CUT,
    "Minimum-events rule: the largest cut at which each per-protocol estimate passes the rule",
    paste0(
      "Each cell is the largest cut, from ",
      paste(format(.HORIZON_CUT_GRID, trim = TRUE), collapse = ", "),
      " expected events per arm per follow-up year, at which the estimate ",
      "passes the rule. -: fails the rule even at ",
      format(min(.HORIZON_CUT_GRID), trim = TRUE),
      ". The fill shows the decision at the current cut, ",
      k,
      ".",
      exempt_note
    ),
    wide$cut,
    wide$withheld,
    year_note
  )
  return(invisible(NULL))
}
