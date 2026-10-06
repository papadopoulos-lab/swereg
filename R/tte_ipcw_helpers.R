# Helpers the censoring model uses: the missing-confounder guard, the
# time-term helper that every model shares, and the entry-window slice.

#' Stop when a time-updated confounder is missing on the IPCW fitting rows.
#'
#' `$s6_ipcw_pp()` fits censoring on the follow-up rows, so it reads the
#' time-updated confounder. An `NA` there makes `stats::predict()` return `NA`,
#' and `cumprod()` carries that `NA` through the rest of the person-trial. The
#' weight then reaches the survey fit as `NA`, far from the cause.
#'
#' swereg MUST NOT overwrite an observed follow-up value with the
#' `.tte_entry__` value. That value describes the recruiting week, and reading
#' it during follow-up is the confounding that a time-zero design removes.
#'
#' `$s1b_fill_followup_confounders()` supplies a missing follow-up value from
#' the last observed value of the same person-trial. s1d runs it before this
#' step.
#'
#' @param data The rows the censoring model fits.
#' @param confounder_vars Character vector of confounder names.
#' @param id_var Character, the person-trial identifier column.
#' @return `invisible(NULL)`, or an error.
#' @noRd
.tte_stop_on_missing_ipcw_confounders <- function(
  data,
  confounder_vars,
  id_var
) {
  cols <- intersect(confounder_vars, names(data))
  if (length(cols) == 0L || nrow(data) == 0L) {
    return(invisible(NULL))
  }
  n_missing <- vapply(cols, function(v) sum(is.na(data[[v]])), integer(1))
  if (all(n_missing == 0L)) {
    return(invisible(NULL))
  }

  ids <- data[[id_var]]
  n_trials <- data.table::uniqueN(ids)
  detail <- vapply(
    cols[n_missing > 0L],
    function(v) {
      na_rows <- is.na(data[[v]])
      return(sprintf(
        "  %s: %d of %d rows, %d of %d person-trials",
        v,
        sum(na_rows),
        nrow(data),
        data.table::uniqueN(ids[na_rows]),
        n_trials
      ))
    },
    character(1)
  )
  stop(
    "s6_ipcw_pp() cannot fit the censoring model.\n",
    "A time-updated confounder is missing on the rows it fits:\n",
    paste(detail, collapse = "\n"),
    "\nAn NA there gives an NA weight, and cumprod() carries it through the ",
    "rest of the person-trial.\n",
    "Fill those follow-up values before this step, or drop the affected ",
    "person-trials.\n",
    "$s1b_fill_followup_confounders() supplies a missing follow-up value from ",
    "the last observed value of the same person-trial. s1d runs it before ",
    "this step.\n",
    "swereg MUST NOT overwrite an observed follow-up value with the ",
    "entry-window value. That value describes the recruiting week.",
    call. = FALSE
  )
}

#' Name the time term of one model.
#'
#' Every time term of the outcome, heterogeneity and censoring models comes
#' from this one function. The ladder steps down as the fit sees fewer
#' distinct values. `mgcv::s()` asks for 10 basis functions by default, and it
#' stops when the covariate holds fewer than 10 distinct values. A natural
#' cubic spline of 3 degrees of freedom needs 4. A factor needs 2.
#'
#' `s()` fits only inside `mgcv::bam()`, so every `survey::svyglm()` and
#' `stats::glm()` fit passes `gam = FALSE`.
#'
#' @param var Character, the column the term reads.
#' @param n Integer, the number of distinct values of `var` in the rows that
#'   the model is fitted to.
#' @param gam Logical. `TRUE` asks for a penalised spline.
#' @return A character scalar. It is `""` when fewer than 2 distinct values
#'   leave nothing to fit.
#' @noRd
.tte_time_term <- function(var, n, gam) {
  if (gam && n >= 10L) {
    return(paste0("s(", var, ")"))
  }
  if (n >= 4L) {
    return(paste0("splines::ns(", var, ", df = 3)"))
  }
  if (n >= 2L) {
    return(paste0("factor(", var, ")"))
  }
  return("")
}

#' Read the confounders of a baseline slice at the entry window.
#'
#' The returned table names each confounder exactly as the design does, and
#' holds its entry-window value under that name. Every step that fits or
#' tabulates baseline confounders MUST read the panel through this function.
#'
#' The rename is local to the returned table. The panel keeps the follow-up
#' value under the confounder name, and the entry-window value under the
#' `.tte_entry__` name.
#'
#' @param data A data.table, one row per person-trial.
#' @param confounder_vars Character vector of confounder names.
#' @param keep_cols Character vector of other columns to carry, such as the
#'   identifier, the treatment column and a weight column.
#' @return A new data.table. It shares no column with `data`.
#' @noRd
.tte_entry_view <- function(data, confounder_vars, keep_cols = character(0)) {
  use_entry <- .tte_has_entry_snapshot(data, confounder_vars)
  conf <- intersect(confounder_vars, names(data))
  entry <- .tte_entry_col(conf)
  cols <- unique(c(keep_cols, conf, if (use_entry) entry))
  out <- data.table::copy(data[, intersect(cols, names(data)), with = FALSE])
  if (use_entry) {
    for (i in seq_along(conf)) {
      data.table::set(out, j = conf[i], value = out[[entry[i]]])
    }
  }
  return(out)
}
