# =============================================================================
# Entry-window snapshots
# =============================================================================

#' The prefix that marks an entry-window snapshot column.
#'
#' A confounder name MUST NOT start with it. [TTEDesign] rejects one that does.
#' @noRd
.TTE_ENTRY_PREFIX <- ".tte_entry__"

#' Name the entry-window snapshot column of each confounder.
#'
#' @param vars Character vector of confounder names.
#' @return A character vector of the same length as `vars`.
#' @noRd
.tte_entry_col <- function(vars) {
  if (length(vars) == 0L) {
    return(character(0))
  }
  return(paste0(.TTE_ENTRY_PREFIX, vars))
}

#' Stop on a confounder name that takes the reserved prefix.
#'
#' @param vars Character vector of confounder names.
#' @return `vars`, invisibly.
#' @noRd
.tte_check_entry_names <- function(vars) {
  if (length(vars) == 0L) {
    return(invisible(vars))
  }
  bad <- vars[startsWith(as.character(vars), .TTE_ENTRY_PREFIX)]
  if (length(bad) > 0L) {
    stop(
      "A confounder name MUST NOT start with '",
      .TTE_ENTRY_PREFIX,
      "'. swereg reserves that prefix for the entry-window snapshot of each ",
      "confounder. Rename: ",
      paste(bad, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(invisible(vars))
}

#' Report whether a trial panel carries a complete entry-window snapshot.
#'
#' The answer is `TRUE` when every confounder has its `.tte_entry__` column. It
#' is `FALSE` when no confounder has one. A partial set stops the run, because
#' baseline adjustment MUST read every confounder at the same instant.
#'
#' A panel with no snapshot reads the collapsed value of the follow-up band.
#' That is what every release before this one did. Two panels reach that state:
#' one built by an earlier release, and one whose entry rows carried no
#' `recruit_week_index`.
#'
#' @param data A data.table.
#' @param confounder_vars Character vector of confounder names.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tte_has_entry_snapshot <- function(data, confounder_vars) {
  if (length(confounder_vars) == 0L) {
    return(FALSE)
  }
  cols <- .tte_entry_col(confounder_vars)
  present <- cols %in% names(data)
  if (all(present)) {
    return(TRUE)
  }
  if (!any(present)) {
    return(FALSE)
  }
  stop(
    "The trial panel holds an entry-window snapshot for some confounders and ",
    "not for others. Missing: ",
    paste(cols[!present], collapse = ", "),
    ". Baseline adjustment MUST read every confounder at the same instant.",
    call. = FALSE
  )
}

#' Read every confounder at the recruiting week of each person-trial.
#'
#' The recruiting week is the earliest week of the entry band that is both
#' eligible and in an arm. `.band_baseline_treatment()` computes it, and
#' `entry_dt` carries it as `recruit_week_index`.
#'
#' The first week of the entry window is the wrong instant. A woman need not be
#' eligible there, and she need not be in an arm there. A covariate read there
#' can describe a woman who was not yet in the trial.
#'
#' A person-trial with no row at its recruiting week reads `NA`. This function
#' never substitutes a nearby week.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id` and `id_var`. It MUST also carry `recruit_week_index`,
#'   or this function returns `NULL`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param confounder_vars Character vector of confounder names.
#' @param id_var Character, the person-trial identifier column.
#' @return A data.table keyed by `id_var`, with one `.tte_entry__<v>` column
#'   per confounder. `NULL` when there is nothing to read.
#' @noRd
.tte_entry_snapshot <- function(
  entry_dt,
  data_enrolled,
  person_id_col,
  confounder_vars,
  id_var
) {
  .tte_pid <- .tte_week <- NULL # nolint
  conf <- intersect(confounder_vars, names(data_enrolled))
  if (length(conf) == 0L) {
    return(NULL)
  }
  if (!"recruit_week_index" %in% names(entry_dt)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }

  # The instant each person-trial is read at. It is the recruiting week, and
  # not the first week of the entry window.
  week_index <- as.integer(entry_dt[["recruit_week_index"]])

  # Match on the week STRING rather than on a week index. `data_enrolled` can
  # hold millions of rows, and an index column would allocate one integer
  # vector that long. `.tte_week_index0()` defines the inverse mapping.
  want <- data.table::data.table(
    .tte_pid = entry_dt[[".tte_person_id"]],
    .tte_week = cstime::dates_by_isoyearweek$isoyearweek[week_index + 1L]
  )
  row_of <- data_enrolled[
    want,
    on = stats::setNames(
      c(".tte_pid", ".tte_week"),
      c(person_id_col, "isoyearweek")
    ),
    which = TRUE,
    mult = "first"
  ]

  # `data_enrolled[NA_integer_]` returns a row of NA, so a person-trial with no
  # row at its recruiting week reads NA on every confounder.
  out <- data_enrolled[row_of, conf, with = FALSE]
  data.table::setnames(out, conf, .tte_entry_col(conf))
  data.table::set(out, j = id_var, value = entry_dt[[id_var]])
  data.table::setkeyv(out, id_var)
  return(out[])
}
