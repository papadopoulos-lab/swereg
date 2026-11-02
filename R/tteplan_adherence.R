# Protocol adherence: how many person-trials of each arm are still observed,
# and how many of those are still on protocol, at every week of follow-up.
# `.s3_enrollment_worker()` computes the table from the raw enrollment file,
# and `$get_adherence()` returns it.

#' Count the observed and on-protocol person-trials at every week
#'
#' The function reads the source panel. That is the raw enrollment object,
#' which `.s1e_worker()` writes before imputation, before the fill and before
#' any outcome. The function reads `data` and `design` of that object and
#' nothing else. It reads no outcome column. The counts are unweighted.
#'
#' The unit is one person-trial: one row with `tstart_var == 0` and a non-NA
#' `treatment_var`. `TRUE` is the intervention arm and `FALSE` is the
#' comparator arm. A person with several trials counts once per trial.
#'
#' Each person-trial has four boundaries. Each is an integer week, or NA when
#' the panel holds no such boundary:
#'
#' * `d`, from `weeks_to_protocol_deviation`.
#' * `r`, from `weeks_to_record_end`.
#' * `g`, from `weeks_to_observation_gap`.
#' * `a`, the administrative end. The formula is the `weeks_to_admin_end`
#'   formula of `s5_prepare_outcome()`. It is NA when
#'   `design$admin_censor_isoyearweek` is NULL.
#'
#' A person-trial is observed at week `t` when `t <= min(r, g, a)`, with NA
#' ignored. It is on protocol at week `t` when it is observed at `t`, and
#' `t <= d` or `d` is NA. A deviation at time zero, `d <= 0`, is off protocol
#' at every week. The comparison is inclusive because a row of a risk set
#' spans `tstart < t <= tstop` (see `.tte_span_risk_sets()`).
#'
#' @param enrollment_raw The raw enrollment object, a `TTEEnrollment`.
#' @return `NULL` when one of the three columns `weeks_to_protocol_deviation`,
#'   `weeks_to_record_end` and `weeks_to_observation_gap` is absent. Also
#'   `NULL` when `design$admin_censor_isoyearweek` is set and `isoyearweek` is
#'   absent. Otherwise a data.table with the columns `arm` (character,
#'   `"intervention"` or `"comparator"`), `week`, `n_observed` and
#'   `n_on_protocol` (all integer). It holds one row per arm and week, for
#'   every week from 1 to `max(tstop_var)` of the panel. The intervention
#'   rows come first, and the weeks ascend within each arm. An arm without a
#'   person-trial gets zero counts.
#' @noRd
.tte_adherence_counts <- function(enrollment_raw) {
  data <- enrollment_raw$data
  design <- enrollment_raw$design
  if (!all(.TTE_ADHERENCE_COLS %in% names(data))) {
    return(NULL)
  }
  admin_end <- design$admin_censor_isoyearweek
  if (!is.null(admin_end) && !"isoyearweek" %in% names(data)) {
    return(NULL)
  }

  tstop <- data[[design$tstop_var]]
  last_week <- if (length(tstop) == 0L) {
    0L
  } else {
    as.integer(max(tstop, na.rm = TRUE))
  }
  weeks <- seq_len(last_week)

  tx <- data[[design$treatment_var]]
  unit <- data[[design$tstart_var]] == 0 & !is.na(tx)
  unit <- unit %in% TRUE
  tx <- tx[unit]
  d <- as.integer(data[["weeks_to_protocol_deviation"]][unit])
  r <- as.integer(data[["weeks_to_record_end"]][unit])
  g <- as.integer(data[["weeks_to_observation_gap"]][unit])
  a <- rep(NA_integer_, sum(unit))
  if (!is.null(admin_end)) {
    # The `weeks_to_admin_end` formula of `s5_prepare_outcome()`, read on the
    # baseline row of each person-trial. `difftime()` counts the whole weeks
    # between the two last dates, and the `+ 1L` adds the administrative week.
    study_end_date <- cstime::isoyearweek_to_last_date(admin_end)
    a <- as.integer(difftime(
      study_end_date,
      cstime::isoyearweek_to_last_date(data[["isoyearweek"]][unit]),
      units = "weeks"
    )) +
      1L
  }

  end_observed <- pmin(r, g, a, na.rm = TRUE)
  end_protocol <- pmin(end_observed, d, na.rm = TRUE)

  arms <- list(intervention = tx %in% TRUE, comparator = tx %in% FALSE)
  out <- lapply(names(arms), function(arm) {
    keep <- arms[[arm]]
    return(data.table::data.table(
      arm = arm,
      week = weeks,
      n_observed = .tte_adherence_count_to(end_observed[keep], weeks),
      n_on_protocol = .tte_adherence_count_to(end_protocol[keep], weeks)
    ))
  })
  return(data.table::rbindlist(out))
}


#' The three boundary columns `.tte_adherence_counts()` needs
#' @noRd
.TTE_ADHERENCE_COLS <- c(
  "weeks_to_protocol_deviation",
  "weeks_to_record_end",
  "weeks_to_observation_gap"
)


#' Count the person-trials whose last counted week is at or after each week
#'
#' @param end Integer, the last counted week of each person-trial. NA means
#'   that no boundary stops it inside the panel.
#' @param weeks Integer, the weeks to count at.
#' @return Integer, one count per element of `weeks`.
#' @noRd
.tte_adherence_count_to <- function(end, weeks) {
  # One entry per distinct end. The ends are integer weeks, so they take far
  # fewer distinct values than a registry panel holds person-trials. Each
  # week then reads the short table.
  tab <- data.table::data.table(end = as.integer(end))[, .N, by = "end"]
  ends <- tab$end
  n <- tab$N
  return(vapply(
    weeks,
    function(t) {
      return(sum(n[is.na(ends) | t <= ends]))
    },
    integer(1L)
  ))
}


#' The milestone weeks of the adherence table
#'
#' Week 13, week 26 and every multiple of 52. A week is kept only when it is
#' at most `last_week`.
#'
#' @param last_week Integer(1), the last stored week.
#' @return Integer, ascending.
#' @noRd
.tte_adherence_milestones <- function(last_week) {
  last_week <- as.integer(last_week)
  weeks <- c(13L, 26L, 52L * seq_len(max(0L, last_week %/% 52L)))
  return(weeks[weeks <= last_week])
}


#' The labels of the milestone weeks of the adherence table
#'
#' A multiple of 52 takes the label of `.minimum_events_year_label()`. That
#' label is the spec `follow_up` label of the same length when one exists,
#' and otherwise `"1 year"` or `"<k> years"`. Every other week reads
#' `"<w> weeks"`, so week 13 reads `"13 weeks"` and week 26 reads
#' `"26 weeks"`.
#'
#' @param weeks Integer, from `.tte_adherence_milestones()`.
#' @param spec The study specification, or `NULL`.
#' @return Character, one label per element of `weeks`.
#' @noRd
.tte_adherence_milestone_labels <- function(weeks, spec) {
  weeks <- as.integer(weeks)
  label <- paste(weeks, "weeks")
  is_year <- weeks %% .HORIZON_WINDOW_WEEKS == 0L
  if (any(is_year)) {
    label[is_year] <- .minimum_events_year_label(
      weeks[is_year] %/% .HORIZON_WINDOW_WEEKS,
      spec
    )
  }
  return(label)
}


#' Test whether a stored enrollment result is missing adherence
#'
#' A result is missing adherence exactly when it holds no element named
#' `adherence`. A stored `adherence = NULL` is present. It means the worker
#' ran and could not compute the table, so no refresh recomputes it unless
#' the caller passes `force = TRUE`.
#'
#' @param result One element of `plan$results_enrollment`, or `NULL`.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tte_adherence_missing <- function(result) {
  return(!"adherence" %in% names(result))
}


#' Which stored results the export refreshes for adherence
#'
#' `$export_tables()` calls this function for its lazy refresh. An
#' enrollment is refreshed when all three conditions hold:
#'
#' 1. Its stored result is missing adherence, see `.tte_adherence_missing()`.
#' 2. An output directory resolves: `output_dir`, else `plan$output_dir`.
#' 3. The `file_raw` of the enrollment exists in that directory. The file is
#'    the one of its first reported ETT, the file `$recompute_baselines()`
#'    reads.
#'
#' Otherwise the enrollment is not refreshed for adherence. The function
#' then gives no message, no warning and no error.
#'
#' @param plan A `TTEPlan`.
#' @param output_dir Character(1) or NULL, the `output_dir` argument of
#'   `$export_tables()`.
#' @return Logical, one element per element of `plan$results_enrollment`.
#' @noRd
.tte_adherence_refresh_wanted <- function(plan, output_dir) {
  dir <- output_dir %||% plan$output_dir
  ett <- .plan_ett_reported(plan)
  results <- plan$results_enrollment
  out <- vapply(
    seq_along(results),
    function(i) {
      if (!.tte_adherence_missing(results[[i]])) {
        return(FALSE)
      }
      if (length(dir) != 1L || is.na(dir)) {
        return(FALSE)
      }
      raw <- ett$file_raw[ett$enrollment_id == names(results)[i]]
      if (length(raw) == 0L || is.na(raw[1L])) {
        return(FALSE)
      }
      return(file.exists(file.path(dir, raw[1L])))
    },
    logical(1L)
  )
  names(out) <- names(results)
  return(out)
}


#' The name of the adherence sheet of `tables.xlsx` and `feasibility.xlsx`
#' @noRd
.SHEET_ADHERENCE <- "Protocol adherence"


#' The stored adherence tables of a plan, one per enrollment
#'
#' The function reads `plan$get_adherence()`. An enrollment with a stored
#' `adherence = NULL`, or without the name, is absent from the list.
#'
#' @param plan A `TTEPlan`.
#' @return A named list by enrollment id. Each element is a data.table with
#'   the columns `arm`, `week`, `n_observed` and `n_on_protocol`.
#' @noRd
.tte_adherence_by_enrollment <- function(plan) {
  adh <- plan$get_adherence()
  cols <- c("arm", "week", "n_observed", "n_on_protocol")
  ids <- unique(adh$enrollment_id)
  out <- lapply(ids, function(id) {
    return(adh[adh$enrollment_id == id, cols, with = FALSE])
  })
  names(out) <- ids
  return(out)
}


#' The wide table of the Protocol adherence sheet
#'
#' The table has one row per enrollment and arm. The enrollments follow
#' `.osf_comparison_numbers(plan$spec)`, and an enrollment that the spec does
#' not name follows them. The intervention row comes before the comparator
#' row. `#` is the position of the enrollment in that order, so it is the
#' comparison number of the spec. `#` and `Comparison` are on the first row
#' of each enrollment only.
#'
#' The milestones are the union of `.tte_adherence_milestones()` over the
#' enrollments, each up to its own last stored week. Each milestone has
#' three columns: `n_observed`, `n_on_protocol` and the percentage
#' `round(100 * n_on_protocol / n_observed)`. The percentage is `NA` when
#' `n_observed` is 0. All three are `NA` past the last stored week of the
#' enrollment.
#'
#' @param plan A `TTEPlan`.
#' @param adherence A named list by enrollment id, each element a stored
#'   adherence table or `NULL`.
#' @return `NULL` when no element is a table with rows. Otherwise a list:
#'   `table` is a data.frame with the columns `#`, `Comparison`, `Arm` and
#'   three numeric columns per milestone. `labels` holds the label of each
#'   milestone.
#' @noRd
.tte_adherence_wide <- function(plan, adherence) {
  has_table <- vapply(
    adherence,
    function(x) data.table::is.data.table(x) && nrow(x) > 0L,
    logical(1)
  )
  adherence <- adherence[has_table]
  if (length(adherence) == 0L) {
    return(NULL)
  }
  order <- unique(c(
    names(.osf_comparison_numbers(plan$spec)),
    names(adherence)
  ))
  ids <- order[order %in% names(adherence)]
  weeks <- sort(unique(unlist(lapply(ids, function(id) {
    return(.tte_adherence_milestones(max(adherence[[id]]$week)))
  }))))
  rows <- list()
  for (id in ids) {
    adh <- adherence[[id]]
    arm_labels <- .lookup_arm_labels(plan$spec, id)
    for (arm in c("intervention", "comparator")) {
      first <- arm == "intervention"
      cells <- as.numeric(unlist(lapply(weeks, function(w) {
        hit <- which(adh$arm == arm & adh$week == w)
        if (length(hit) != 1L) {
          return(rep(NA_real_, 3L))
        }
        itt <- as.numeric(adh$n_observed[hit])
        pp <- as.numeric(adh$n_on_protocol[hit])
        pct <- if (itt > 0) round(100 * pp / itt) else NA_real_
        return(c(itt, pp, pct))
      })))
      rows[[length(rows) + 1L]] <- c(
        list(
          `#` = if (first) match(id, order) else NA_integer_,
          Comparison = if (first) .enrollment_label(plan, id) else NA,
          Arm = if (is.null(arm_labels)) arm else arm_labels[[arm]]
        ),
        as.list(stats::setNames(cells, sprintf("v%d", seq_along(cells))))
      )
    }
  }
  table <- data.table::rbindlist(rows)
  data.table::setDF(table)
  # `.tte_adherence_milestone_labels()` returns one label for no week, so
  # it is not called then.
  labels <- character(0)
  if (length(weeks) > 0L) {
    labels <- .tte_adherence_milestone_labels(weeks, plan$spec)
  }
  return(list(table = table, labels = labels))
}


#' Write the Protocol adherence sheet
#'
#' `.plan_export_tables()` and `.write_feasibility_xlsx()` both call it, so
#' the sheet of `tables.xlsx` and of `feasibility.xlsx` is the same. The
#' layout follows the Minimum events sheets. Row 1 holds the title and row 2
#' the note. Row 4 holds `#`, `Comparison`, `Arm` and the label of each
#' milestone, merged across its three columns. Row 5 holds `N (ITT)`,
#' `N (PP)` and `%` under each milestone. The data start at row 6, and every
#' count and percentage is a numeric cell. A blank cell is `NA` in
#' `.tte_adherence_wide()`.
#'
#' An enrollment whose table is `NULL` or absent gets no row. When no
#' enrollment has a table, the function writes no sheet. When the tables
#' hold no milestone week, the sheet holds the three leading columns only.
#'
#' @param wb An openxlsx workbook.
#' @param plan A `TTEPlan`.
#' @param adherence A named list by enrollment id, each element a stored
#'   adherence table or `NULL`.
#' @return Invisibly, a named character: the description of the sheet for a
#'   table of contents, named by the sheet. A zero-length character when the
#'   function wrote no sheet.
#' @noRd
.write_adherence_sheet <- function(wb, plan, adherence) {
  wide <- .tte_adherence_wide(plan, adherence)
  if (is.null(wide)) {
    return(invisible(character(0)))
  }
  sh <- .SHEET_ADHERENCE
  table <- wide$table
  n_ms <- length(wide$labels)
  openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(
    wb,
    sh,
    paste0(
      "Protocol adherence: person-trials observed and on protocol at each ",
      "milestone week"
    ),
    startRow = 1L
  )
  openxlsx::addStyle(
    wb,
    sh,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(
    wb,
    sh,
    paste0(
      "Unweighted counts of person-trials, counted before any outcome. ",
      "N (ITT): the person-trials not yet at a record end, an observation ",
      "gap or the administrative end. N (PP): those of them not yet deviated ",
      "from protocol. %: 100 x N (PP) / N (ITT), blank when N (ITT) is 0. ",
      "A blank milestone is past the follow-up of the comparison."
    ),
    startRow = 2L
  )
  first <- 3L + 3L * (seq_len(n_ms) - 1L) + 1L
  head4 <- c("#", "Comparison", "Arm", rep(NA_character_, 3L * n_ms))
  head4[first] <- wide$labels
  head5 <- c(rep(NA_character_, 3L), rep(c("N (ITT)", "N (PP)", "%"), n_ms))
  header_row <- 4L
  openxlsx::writeData(
    wb,
    sh,
    as.data.frame(rbind(head4, head5), stringsAsFactors = FALSE),
    startRow = header_row,
    colNames = FALSE
  )
  for (col in first) {
    openxlsx::mergeCells(wb, sh, cols = col + 0:2, rows = header_row)
  }
  openxlsx::addStyle(
    wb,
    sh,
    style = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    ),
    rows = header_row + 0:1,
    cols = seq_along(table),
    gridExpand = TRUE
  )
  openxlsx::writeData(
    wb,
    sh,
    table,
    startRow = header_row + 2L,
    colNames = FALSE
  )
  openxlsx::addStyle(
    wb,
    sh,
    style = openxlsx::createStyle(textDecoration = "bold"),
    rows = header_row + 1L + which(!is.na(table$`#`)),
    cols = 1L,
    stack = TRUE
  )
  openxlsx::setColWidths(
    wb,
    sh,
    cols = seq_along(table),
    widths = c(4, 60, 30, rep(12, 3L * n_ms))
  )
  openxlsx::freezePane(wb, sh, firstActiveRow = header_row + 2L)
  desc <- paste0(
    "Person-trials observed (N (ITT)) and on protocol (N (PP)) at each ",
    "milestone week, by comparison and arm"
  )
  return(invisible(stats::setNames(desc, sh)))
}
