# The overview sheets of feasibility.xlsx and tables.xlsx.
# `.write_feasibility_sheets()` in r6_tteplan_feasibility.R writes them into
# both workbooks.

# ---- Overview sheets ------------------------------------------------------------
#
# Five sheets, each one grid per question, with one comparison per row or
# column, colour fills and a legend:
#   Minimum events       comparisons x (outcome, follow-up year), largest cut
#   Longest reportable   comparisons x (outcome, threshold), longest horizon
#   Attrition by arm     steps x (comparison, arm), remaining and removed
#   Rare conditions by comparison   candidates x (decision, comparisons),
#                        smallest arm
#   Table 1 (filterable) one long table with an autofilter, SMD coloured

#' The overview sheet names, in workbook order.
#' @noRd
.SHEETS_OVERVIEW <- c(
  min_events = "Minimum events",
  longest = "Longest reportable",
  attrition = "Attrition by arm",
  rare = "Rare conditions by comparison",
  table1 = "Table 1 (filterable)"
)

#' The fills of the overview sheets.
#' @noRd
.OVERVIEW_FILL <- c(
  red = "#F4B6B6",
  orange = "#F8CBAD",
  yellow = "#FFE699",
  lightgreen = "#D9EAD3",
  green = "#A9D18E",
  grey = "#EFEFEF",
  darkgrey = "#BFBFBF",
  black = "#000000"
)

#' The note line that explains the black and grey cells.
#' @noRd
.OVERVIEW_NOT_IN_DATA_NOTE <- paste(
  "Black: the outcome is not in the data (0 persons in the whole dataset);",
  "see Coverage (whole dataset). Grey comparison: one of its criteria or",
  "confounders reads a column with 0 persons in the whole dataset; see",
  "Coverage (whole dataset)."
)

#' The thresholds of the Longest reportable sheet.
#' @noRd
.LONGEST_THRESHOLDS <- c(5, 10, 20)

#' Write a title, a note and a legend at the top of an overview sheet
#'
#' @param legend A named character vector: fill name to its meaning.
#' @return The first free row.
#' @noRd
.overview_head <- function(wb, sht, title, note, legend = NULL) {
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(wb, sht, title, startRow = 1L)
  openxlsx::addStyle(
    wb,
    sht,
    openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(wb, sht, note, startRow = 2L)
  row <- 3L
  for (k in names(legend)) {
    openxlsx::writeData(wb, sht, legend[[k]], startCol = 2L, startRow = row)
    openxlsx::addStyle(
      wb,
      sht,
      openxlsx::createStyle(fgFill = .OVERVIEW_FILL[[k]]),
      rows = row,
      cols = 1L
    )
    row <- row + 1L
  }
  return(row + 1L)
}

#' Fill cells of a sheet
#' @param cells A two-column matrix of (row, col) in the sheet.
#' @noRd
.overview_fill <- function(wb, sht, cells, fill) {
  if (length(cells) == 0L || nrow(cells) == 0L) {
    return(invisible(NULL))
  }
  openxlsx::addStyle(
    wb,
    sht,
    openxlsx::createStyle(fgFill = .OVERVIEW_FILL[[fill]], halign = "center"),
    rows = cells[, 1L],
    cols = cells[, 2L],
    gridExpand = FALSE,
    stack = TRUE
  )
  return(invisible(NULL))
}

#' The fill of a largest passing cut
#' @noRd
.overview_cut_fill <- function(cut) {
  n <- suppressWarnings(as.numeric(cut))
  out <- rep(NA_character_, length(cut))
  out[!is.na(cut) & cut == "-"] <- "red"
  out[!is.na(n) & n < 5] <- "orange"
  out[!is.na(n) & n >= 5 & n < 10] <- "yellow"
  out[!is.na(n) & n >= 10 & n < 20] <- "lightgreen"
  out[!is.na(n) & n >= 20] <- "green"
  return(out)
}


#' The minimum-events cuts, one row per comparison
#'
#' @param plan A TTEPlan.
#' @param records A list keyed by ETT id: the per-protocol horizon records.
#' @return A list: `ids` and `names` of the comparisons, `outcomes`,
#'   `years` (the column labels), and `cut`, a list by outcome of a character
#'   matrix comparisons x years with the largest passing cut, `"-"` for none
#'   and `NA` for no ETT. `yn` has the same shape and holds the verdict of
#'   the specification's own threshold, `"Y"` or `"-"`.
#' @noRd
.overview_cuts <- function(plan, records) {
  wide <- .minimum_events_wide(plan, records)
  tab <- wide$cut
  tab_yn <- wide$yn
  ett <- plan$ett
  enr <- .osf_comparison_numbers(plan$spec)
  enr_ids <- unique(c(names(enr), unique(ett$enrollment_id)))
  enr_ids <- enr_ids[enr_ids %in% ett$enrollment_id]
  years <- setdiff(names(tab), c("Outcome", "#", "Comparison"))
  outcome <- tab$Outcome
  for (i in seq_along(outcome)) {
    if (is.na(outcome[i]) && i > 1L) {
      outcome[i] <- outcome[i - 1L]
    }
  }
  outcomes <- unique(outcome)
  pivot <- function(src) {
    res <- lapply(outcomes, function(o) {
      m <- matrix(NA_character_, length(enr_ids), length(years))
      rows <- which(outcome == o)
      for (r in rows) {
        m[src[["#"]][r], ] <- as.character(unlist(src[r, years]))
      }
      return(m)
    })
    names(res) <- outcomes
    return(res)
  }
  cut <- pivot(tab)
  yn <- pivot(tab_yn)
  return(list(
    ids = enr_ids,
    # An exempt comparison shows the verdict of the rule, and its name says
    # that s3 reports it anyway.
    names = vapply(
      enr_ids,
      function(e) {
        label <- .enrollment_label(plan, e)
        if (.tte_enrollment_rule_exempt(plan$spec, e)) {
          label <- paste0(label, " (exempt)")
        }
        return(label)
      },
      character(1)
    ),
    outcomes = outcomes,
    years = years,
    cut = cut,
    yn = yn
  ))
}


#' Write the Minimum events and Longest reportable sheets
#' @noRd
.write_overview_min_events <- function(wb, plan, records) {
  x <- .overview_cuts(plan, records)
  nid <- .tte_not_in_data(plan)
  k <- plan$spec[["min_expected_events_per_arm_per_year"]]

  # Minimum events: one row per comparison, one column per outcome and year.
  sht <- .SHEETS_OVERVIEW[["min_events"]]
  start <- .overview_head(
    wb,
    sht,
    "Minimum events: the largest cut each per-protocol estimate passes",
    paste0(
      "Each cell is the largest number of expected events per arm in every ",
      "follow-up year up to that horizon, from 1, 2, 3, 4, 5, 10 and 20. ",
      "The current cut is ",
      if (is.null(k)) "not set" else format(k),
      ". An empty cell has no ETT. A 6-month horizon takes the 1-year verdict. ",
      "(exempt): the specification exempts the comparison, so s3 reports its ",
      "estimates whatever the cell shows. ",
      .OVERVIEW_NOT_IN_DATA_NOTE
    ),
    c(
      green = "20 or more",
      lightgreen = "10",
      yellow = "5",
      orange = "1 to 4",
      red = "- : fails even at 1"
    )
  )
  n_cmp <- length(x$ids)
  if (n_cmp == 0L) {
    openxlsx::writeData(wb, sht, "(no ETT)", startRow = start)
    return(invisible(NULL))
  }
  # Two header rows: the outcome over its years, then the years.
  hdr1 <- c("ID", "Comparison")
  hdr2 <- c("ID", "Comparison")
  body <- data.frame(ID = x$ids, Comparison = unname(x$names), stringsAsFactors = FALSE)
  fills <- list()
  col <- 2L
  for (o in x$outcomes) {
    m <- x$cut[[o]]
    shown <- m
    shown[!is.na(m) & m == "20"] <- "\u226520"
    absent <- o %in% nid$outcomes
    for (j in seq_along(x$years)) {
      col <- col + 1L
      hdr1 <- c(hdr1, if (j == 1L) o else "")
      hdr2 <- c(hdr2, x$years[j])
      body[[paste0("c", col)]] <- if (absent) rep("", n_cmp) else shown[, j]
      f <- if (absent) rep("black", n_cmp) else .overview_cut_fill(m[, j])
      for (r in which(!is.na(f))) {
        fills[[f[r]]] <- rbind(fills[[f[r]]], c(start + 1L + r, col))
      }
    }
  }
  openxlsx::writeData(wb, sht, t(hdr1), startRow = start, colNames = FALSE)
  openxlsx::writeData(wb, sht, t(hdr2), startRow = start + 1L, colNames = FALSE)
  openxlsx::writeData(wb, sht, body, startRow = start + 2L, colNames = FALSE)
  openxlsx::addStyle(
    wb,
    sht,
    openxlsx::createStyle(textDecoration = "bold", fgFill = .OVERVIEW_FILL[["grey"]], halign = "center"),
    rows = start + 0:1,
    cols = seq_len(col),
    gridExpand = TRUE
  )
  for (j in seq_along(x$outcomes)) {
    from <- 2L + (j - 1L) * length(x$years) + 1L
    if (length(x$years) > 1L) {
      openxlsx::mergeCells(wb, sht, cols = from:(from + length(x$years) - 1L), rows = start)
    }
  }
  grey_rows <- which(x$ids %in% nid$enrollments)
  if (length(grey_rows) > 0L) {
    fills[["darkgrey"]] <- rbind(fills[["darkgrey"]], cbind(start + 1L + grey_rows, 2L))
  }
  for (f in names(fills)) {
    .overview_fill(wb, sht, fills[[f]], f)
  }
  openxlsx::setColWidths(wb, sht, cols = seq_len(col), widths = c(8, 50, rep(8, col - 2L)))
  openxlsx::freezePane(wb, sht, firstActiveRow = start + 2L, firstActiveCol = 3L)

  # Longest reportable: the horizon each threshold would report.
  sht <- .SHEETS_OVERVIEW[["longest"]]
  thr <- sort(unique(c(.LONGEST_THRESHOLDS, if (!is.null(k)) as.numeric(k))))
  start <- .overview_head(
    wb,
    sht,
    "Longest reportable horizon at each threshold",
    paste0(
      "The longest follow-up up to which every year passes the threshold, in ",
      "expected events per arm per year. none: the first year fails. The ",
      "specification uses ",
      if (is.null(k)) "no threshold" else format(k),
      ". (exempt): the specification exempts the comparison, so s3 reports ",
      "its estimates to the full follow-up whatever the cell shows. ",
      .OVERVIEW_NOT_IN_DATA_NOTE
    ),
    c(green = "Reported to the longest horizon", yellow = "Reported to a shorter horizon", red = "none")
  )
  hdr1 <- c("ID", "Comparison")
  hdr2 <- c("ID", "Comparison")
  body <- data.frame(ID = x$ids, Comparison = unname(x$names), stringsAsFactors = FALSE)
  fills <- list()
  col <- 2L
  for (o in x$outcomes) {
    m <- x$cut[[o]]
    m_yn <- x$yn[[o]]
    for (th in thr) {
      # A cut is a value of the coarse grid, so it decides only a threshold
      # of that grid. The specification's own threshold reads the stored
      # verdict of the rule instead, which holds for any threshold.
      own <- !is.null(k) && th == as.numeric(k)
      col <- col + 1L
      hdr1 <- c(hdr1, if (th == thr[1L]) o else "")
      hdr2 <- c(hdr2, paste0("threshold ", format(th)))
      val <- character(n_cmp)
      for (r in seq_len(n_cmp)) {
        n <- suppressWarnings(as.numeric(m[r, ]))
        avail <- which(!is.na(m[r, ]))
        last <- 0L
        for (j in seq_along(x$years)) {
          fails <- if (own) {
            !identical(m_yn[r, j], "Y")
          } else {
            is.na(m[r, j]) || is.na(n[j]) || n[j] < th
          }
          if (fails) {
            break
          }
          last <- j
        }
        val[r] <- if (length(avail) == 0L) "" else if (last == 0L) "none" else x$years[last]
        f <- if (length(avail) == 0L) NA else if (last == 0L) "red" else if (last == max(avail)) "green" else "yellow"
        if (o %in% nid$outcomes) {
          val[r] <- ""
          f <- "black"
        }
        if (!is.na(f)) {
          fills[[f]] <- rbind(fills[[f]], c(start + 1L + r, col))
        }
      }
      body[[paste0("c", col)]] <- val
    }
  }
  openxlsx::writeData(wb, sht, t(hdr1), startRow = start, colNames = FALSE)
  openxlsx::writeData(wb, sht, t(hdr2), startRow = start + 1L, colNames = FALSE)
  openxlsx::writeData(wb, sht, body, startRow = start + 2L, colNames = FALSE)
  openxlsx::addStyle(
    wb,
    sht,
    openxlsx::createStyle(textDecoration = "bold", fgFill = .OVERVIEW_FILL[["grey"]], halign = "center"),
    rows = start + 0:1,
    cols = seq_len(col),
    gridExpand = TRUE
  )
  for (j in seq_along(x$outcomes)) {
    from <- 2L + (j - 1L) * length(thr) + 1L
    if (length(thr) > 1L) {
      openxlsx::mergeCells(wb, sht, cols = from:(from + length(thr) - 1L), rows = start)
    }
  }
  grey_rows <- which(x$ids %in% nid$enrollments)
  if (length(grey_rows) > 0L) {
    fills[["darkgrey"]] <- rbind(fills[["darkgrey"]], cbind(start + 1L + grey_rows, 2L))
  }
  for (f in names(fills)) {
    .overview_fill(wb, sht, fills[[f]], f)
  }
  openxlsx::setColWidths(wb, sht, cols = seq_len(col), widths = c(8, 50, rep(12, col - 2L)))
  openxlsx::freezePane(wb, sht, firstActiveRow = start + 2L, firstActiveCol = 3L)
  return(invisible(NULL))
}


#' A readable label for an attrition step
#'
#' @param step The stored criterion, such as `eligible_no_osd_z907_lifetime`.
#' @param spec The study specification.
#' @param eid The enrollment id, for its own criteria.
#' @return Character(1).
#' @noRd
.overview_step_label <- function(step, spec, eid) {
  if (identical(step, "before_exclusions")) {
    return("Start: all person-trials")
  }
  if (!startsWith(step, "eligible_")) {
    return(step)
  }
  enr <- NULL
  for (e in spec$enrollments) {
    if (isTRUE(e$id == eid)) enr <- e
  }
  cands <- list()
  add <- function(prefix, crits) {
    for (cr in crits) {
      impl <- cr$implementation
      sv <- as.character(unlist(
        impl[["source_variable_combined"]] %||% impl[["source_variable"]] %||% impl[["variable"]]
      ))
      for (v in sv) {
        cands[[length(cands) + 1L]] <<- list(v = v, label = paste0(prefix, cr$name))
      }
    }
    return(invisible(NULL))
  }
  add("Incl. ", spec$inclusion_criteria$criteria)
  add("Incl. ", enr$additional_inclusion)
  add("Excl. ", spec$exclusion_criteria)
  add("Excl. ", enr$additional_exclusion)
  best <- NULL
  for (cd in cands) {
    if (grepl(cd$v, step, fixed = TRUE) && (is.null(best) || nchar(cd$v) > nchar(best$v))) {
      best <- cd
    }
  }
  if (!is.null(best)) {
    return(best$label)
  }
  if (grepl("isoyear", step, fixed = TRUE)) {
    return("Incl. enrollment calendar years")
  }
  return(step)
}


#' Write the Attrition by arm sheet
#' @noRd
.write_overview_attrition <- function(wb, plan) {
  enrollment_period_id <- step_order <- enrollment_id <- NULL
  label <- step_name <- NULL
  sht <- .SHEETS_OVERVIEW[["attrition"]]
  start <- .overview_head(
    wb,
    sht,
    "Attrition by arm: person-trials left after each step, and removed by it",
    paste0(
      "One column per comparison and arm, before the comparator draw. Steps ",
      "run top to bottom, so a step removes only what the steps above it left. ",
      "A row in red removes nobody in any comparison: its variable may be ",
      "missing from the data (see the Coverage sheets)."
    ),
    c(red = "Removes nobody in any comparison")
  )
  att <- plan$get_attrition()
  if (nrow(att) == 0L) {
    openxlsx::writeData(wb, sht, "(no attrition rows)", startRow = start)
    return(invisible(NULL))
  }
  glob <- att[is.na(enrollment_period_id)]
  eids <- unique(glob$enrollment_id)
  glob[,
    label := mapply(
      .overview_step_label,
      step_name,
      eid = enrollment_id,
      MoreArgs = list(spec = plan$spec),
      USE.NAMES = FALSE
    )
  ]
  steps <- character(0)
  for (eid in eids) {
    g <- glob[enrollment_id == eid][order(step_order)]
    steps <- unique(c(steps, g$label))
  }
  left <- matrix(NA_real_, length(steps), 2L * length(eids))
  removed <- left
  for (i in seq_along(eids)) {
    g <- glob[enrollment_id == eids[i]][order(step_order)]
    r <- match(g$label, steps)
    left[r, 2L * i - 1L] <- g$n_arm_intervention
    left[r, 2L * i] <- g$n_arm_comparator
    removed[r, 2L * i - 1L] <- c(NA, -diff(g$n_arm_intervention))
    removed[r, 2L * i] <- c(NA, -diff(g$n_arm_comparator))
  }
  cols <- as.vector(rbind(paste(eids, "intervention"), paste(eids, "comparator")))
  write_block <- function(title, m, row) {
    openxlsx::writeData(wb, sht, title, startRow = row)
    openxlsx::addStyle(wb, sht, openxlsx::createStyle(textDecoration = "bold"), rows = row, cols = 1L)
    d <- data.frame(Step = seq_along(steps), Criterion = steps, stringsAsFactors = FALSE)
    for (j in seq_along(cols)) {
      d[[cols[j]]] <- m[, j]
    }
    openxlsx::writeData(
      wb,
      sht,
      d,
      startRow = row + 1L,
      headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = .OVERVIEW_FILL[["grey"]], wrapText = TRUE)
    )
    openxlsx::addStyle(
      wb,
      sht,
      openxlsx::createStyle(numFmt = "#,##0"),
      rows = row + 1L + seq_along(steps),
      cols = 2L + seq_along(cols),
      gridExpand = TRUE,
      stack = TRUE
    )
    nobody <- which(apply(removed, 1L, function(x) all(is.na(x) | x == 0)) & seq_along(steps) > 1L)
    if (length(nobody) > 0L) {
      openxlsx::addStyle(
        wb,
        sht,
        openxlsx::createStyle(fgFill = .OVERVIEW_FILL[["red"]]),
        rows = row + 1L + nobody,
        cols = seq_len(2L + length(cols)),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    return(row + length(steps) + 3L)
  }
  row <- write_block("Person-trials left after the step", left, start)
  write_block("Person-trials removed by the step", removed, row)
  openxlsx::setColWidths(wb, sht, cols = seq_len(2L + length(cols)), widths = c(6, 55, rep(13, length(cols))))
  openxlsx::freezePane(wb, sht, firstActiveCol = 3L)
  return(invisible(NULL))
}


#' Write the Rare conditions by comparison sheet
#'
#' Reads `plan$sparse_counts`, the s1b counts of every enrollment. A plan
#' from before swereg 27.3.13 holds the driving enrollments only, through the
#' decision table.
#' @noRd
.write_overview_rare <- function(wb, plan) {
  sht <- .SHEETS_OVERVIEW[["rare"]]
  rule <- plan$spec$rare_condition_rule
  if (is.null(rule)) {
    return(invisible(FALSE))
  }
  min_n <- as.numeric(rule$min_n)
  start <- .overview_head(
    wb,
    sht,
    "Rare conditions by comparison: persons in the smaller arm who hold each condition at baseline",
    paste0(
      "Eligible persons before the comparator draw, without follow-up ",
      "outcomes. c: the comparator is the smaller arm. The rule decides from ",
      paste(unlist(rule$decided_by), collapse = ", "),
      " only; the other comparisons show what it would find there. ",
      "1 to 4 show as <5."
    ),
    stats::setNames(
      c(
        paste0("At least ", 2 * min_n),
        paste0(min_n, " to ", 2 * min_n - 1),
        paste0("Below ", min_n, ": excluded if it were the deciding comparison")
      ),
      c("lightgreen", "orange", "red")
    )
  )
  cands <- as.character(unlist(rule$candidates))
  # The s1b decision of each condition: adjusted, or excluded.
  dec <- plan$sparse_decision
  decision <- if (data.table::is.data.table(dec)) {
    as.character(dec$decision[match(cands, dec$variable)])
  } else {
    rep(NA_character_, length(cands))
  }
  decision[is.na(decision)] <- ""
  labels <- vapply(
    cands,
    function(v) {
      for (cf in plan$spec$confounders) {
        if (identical(cf$implementation[["variable"]], v)) return(as.character(cf$name))
      }
      return(v)
    },
    character(1)
  )
  counts <- plan$sparse_counts
  if (!data.table::is.data.table(counts) || nrow(counts) == 0L) {
    openxlsx::writeData(
      wb,
      sht,
      "(no counts per comparison: rerun s1 with swereg 27.3.13 or later)",
      startRow = start
    )
    if (data.table::is.data.table(dec)) {
      openxlsx::writeData(
        wb,
        sht,
        data.frame(Condition = unname(labels), Decision = decision),
        startRow = start + 2L
      )
    }
    return(invisible(TRUE))
  }
  enr <- .osf_comparison_numbers(plan$spec)
  eids <- names(enr)[names(enr) %in% counts$enrollment_id]
  shown <- matrix("", length(cands), length(eids))
  n_small <- matrix(NA_real_, length(cands), length(eids))
  for (i in seq_along(cands)) {
    for (j in seq_along(eids)) {
      ni <- counts$n_persons[counts$enrollment_id == eids[j] & counts$variable == cands[i] & counts$arm == "intervention"]
      nc <- counts$n_persons[counts$enrollment_id == eids[j] & counts$variable == cands[i] & counts$arm == "comparator"]
      if (length(ni) != 1L || length(nc) != 1L) {
        next
      }
      s <- min(ni, nc)
      n_small[i, j] <- s
      shown[i, j] <- paste0(.coverage_count(s), if (nc < ni) " c" else "")
    }
  }
  below <- !is.na(n_small) & n_small < min_n
  d <- data.frame(
    Condition = unname(labels),
    Decision = decision,
    stringsAsFactors = FALSE
  )
  hdr <- ifelse(eids %in% unlist(rule$decided_by), paste0(eids, " (decides)"), eids)
  for (j in seq_along(eids)) {
    d[[hdr[j]]] <- shown[, j]
  }
  d[[paste0("Comparisons < ", min_n)]] <- rowSums(below)
  tot <- data.frame(
    Condition = paste0("Conditions < ", min_n),
    Decision = "",
    t(colSums(below)),
    0,
    check.names = FALSE
  )
  names(tot) <- names(d)
  tot[[ncol(tot)]] <- NA
  d <- rbind(d, tot)
  openxlsx::writeData(
    wb,
    sht,
    d,
    startRow = start,
    headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = .OVERVIEW_FILL[["grey"]], halign = "center")
  )
  for (f in c("lightgreen", "orange", "red")) {
    hit <- switch(f,
      lightgreen = !is.na(n_small) & n_small >= 2 * min_n,
      orange = !is.na(n_small) & n_small >= min_n & n_small < 2 * min_n,
      red = below
    )
    w <- which(hit, arr.ind = TRUE)
    if (nrow(w) > 0L) {
      .overview_fill(wb, sht, cbind(start + w[, 1L], 2L + w[, 2L]), f)
    }
  }
  openxlsx::addStyle(
    wb,
    sht,
    openxlsx::createStyle(textDecoration = "bold"),
    rows = start + nrow(d),
    cols = seq_len(ncol(d)),
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::setColWidths(wb, sht, cols = seq_len(ncol(d)), widths = c(45, 11, rep(11, ncol(d) - 2L)))
  return(invisible(TRUE))
}


#' Split a Table 1 cell such as "14,964 (7.5%)" into its count and share
#' @noRd
.overview_split_cell <- function(x) {
  x <- as.character(x)
  m <- regmatches(x, regexec("^\\s*([0-9][0-9,]*)\\s*\\(([0-9.]+)%\\)\\s*$", x))
  n <- vapply(m, function(z) if (length(z) == 3L) z[2L] else NA_character_, character(1))
  p <- vapply(m, function(z) if (length(z) == 3L) z[3L] else NA_character_, character(1))
  value <- ifelse(is.na(n), x, n)
  return(list(value = value, pct = p))
}


#' Write the Table 1 (filterable) sheet
#'
#' One long table over every enrollment and both panels, with an autofilter.
#' @param baselines Named list by enrollment id of the two panels.
#' @noRd
.write_overview_table1 <- function(wb, plan, baselines) {
  sht <- .SHEETS_OVERVIEW[["table1"]]
  start <- .overview_head(
    wb,
    sht,
    "Table 1, unweighted, every comparison in one table",
    "Filter on Enrollment and File. SMD: standardised mean difference between the arms.",
    c(orange = "SMD 0.1 to 0.2", red = "SMD 0.2 or more")
  )
  panels <- c(table1_raw = "s1 (unimputed)", table1_unweighted = "s2 (imputed)")
  rows <- list()
  for (eid in names(baselines)) {
    for (slot in names(panels)) {
      t1 <- baselines[[eid]][[slot]]
      if (is.null(t1) || nrow(t1) == 0L) {
        next
      }
      t1 <- data.table::as.data.table(t1)
      nm <- names(t1)
      fixed <- c("Variable", "Level", "Overall", "SMD", "smd_numeric")
      arms <- setdiff(nm, fixed)
      if (length(arms) != 2L) {
        next
      }
      # The writer orders the arm columns comparator, then intervention.
      cmp <- .overview_split_cell(t1[[arms[1L]]])
      int <- .overview_split_cell(t1[[arms[2L]]])
      var <- as.character(t1$Variable)
      for (i in seq_along(var)) {
        if ((is.na(var[i]) || !nzchar(var[i])) && i > 1L) var[i] <- var[i - 1L]
      }
      smd <- if ("smd_numeric" %in% nm) {
        as.numeric(t1$smd_numeric)
      } else {
        suppressWarnings(as.numeric(t1$SMD))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        Enrollment = eid,
        `Short name` = .enrollment_label(plan, eid),
        File = panels[[slot]],
        Covariate = var,
        Level = as.character(t1$Level),
        Intervention = int$value,
        `Intervention %` = suppressWarnings(as.numeric(int$pct)),
        Comparator = cmp$value,
        `Comparator %` = suppressWarnings(as.numeric(cmp$pct)),
        SMD = round(smd, 3),
        smd_raw = smd,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    openxlsx::writeData(wb, sht, "(no Table 1)", startRow = start)
    return(invisible(NULL))
  }
  d <- do.call(rbind, rows)
  # The colour band reads the unrounded SMD: 0.0996 shows as 0.1 and stays
  # uncoloured.
  smd_raw <- d$smd_raw
  d$smd_raw <- NULL
  openxlsx::writeDataTable(
    wb,
    sht,
    d,
    startRow = start,
    tableStyle = "TableStyleLight1",
    withFilter = TRUE
  )
  for (f in c("orange", "red")) {
    hit <- which(!is.na(smd_raw) & if (f == "red") smd_raw >= 0.2 else smd_raw >= 0.1 & smd_raw < 0.2)
    if (length(hit) > 0L) {
      .overview_fill(wb, sht, cbind(start + hit, ncol(d)), f)
    }
  }
  openxlsx::setColWidths(wb, sht, cols = seq_len(ncol(d)), widths = c(11, 40, 16, 30, 26, 14, 10, 14, 10, 8))
  openxlsx::freezePane(wb, sht, firstActiveRow = start + 1L)
  return(invisible(NULL))
}

