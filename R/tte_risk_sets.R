# Risk sets over the reporting times: which times a panel row spans, and the
# weighted risk sets, events and head counts at each time.

#' The reporting times one panel row spans
#'
#' A row spans time `t` when `tstart < t <= tstop`. The interval is half open:
#' the row covers the weeks from `tstart` to `tstop - 1`, and the event of the
#' row lands at `tstop`.
#'
#' A survival risk set at `t` therefore holds every row that spans `t`, and not
#' only the rows that stop at `t`. The two sets agreed while every stop sat on
#' the band grid. `s5_prepare_outcome()` clips the terminal row at the exact
#' censoring week, so a stop now falls between two band boundaries and the two
#' sets differ.
#'
#' @param tstart Numeric, the exclusive start of each row.
#' @param tstop Numeric, the inclusive stop of each row.
#' @param times Numeric, the reporting times. Sorted, unique, without `NA`.
#' @return A list of two integer vectors. `lo` is the first position in `times`
#'   that the row spans, and `hi` is the last one. `hi < lo` means the row
#'   spans no reporting time.
#' @noRd
.tte_span_index <- function(tstart, tstop, times) {
  list(
    lo = findInterval(tstart, times) + 1L,
    hi = findInterval(tstop, times)
  )
}

#' The exclusive start of every panel row
#'
#' Reads `tstart_var` where the panel carries it. Every panel that `$enroll()`
#' builds carries it, so that is the production path.
#'
#' A panel built by hand can omit the column, and it then states no interval at
#' all. The row is read as covering the one step that ends at its own stop. The
#' start is then the previous reporting time, and 0 for the first. The
#' estimator read every row that way before this release, so a panel with no
#' start column keeps the numbers it had.
#'
#' @param data A data.table at trial level, one row per person-trial-band.
#' @param tstart_var Character, the period start column.
#' @param tstop_var Character, the period stop column.
#' @param times Numeric, the reporting times. Sorted, unique, and holding every
#'   value of `tstop_var`.
#' @return A numeric vector, one element per row of `data`.
#' @noRd
.tte_interval_start <- function(data, tstart_var, tstop_var, times) {
  if (tstart_var %in% names(data)) {
    return(as.numeric(data[[tstart_var]]))
  }
  as.numeric(c(0, times)[match(data[[tstop_var]], times)])
}

#' Weighted risk sets, weighted events and head counts at every reporting time
#'
#' The ONE site that decides which rows enter a survival risk set.
#'
#' `Y_a(t) = sum_i w_i * I(A_i = a, tstart_i < t <= tstop_i)` is the weighted
#' risk set. It is a weighted COUNT of the person-trials at risk at `t`, and it
#' is never a sum of person-time. `$rates()` owns the person-time quantity and
#' forms it as `sum(person_weeks * w)`.
#'
#' `d_a(t) = sum_i w_i * I(A_i = a, event_i = 1, tstop_i = t)` is the weighted
#' event count. Note the asymmetry against `Y_a(t)` and keep it: the risk set
#' SPANS the time, and the event LANDS at the stop of its own row.
#'
#' `N_a(t)` counts the distinct people who span `t`. A person holds several
#' sequential trials, so her rows are merged into runs first and she is then
#' counted once.
#'
#' Every arm gets a row at every reporting time, including a time where it
#' holds no row of its own. That is what lets a survival curve carry its latest
#' exact value forward. It also lets both arms of a risk difference be read at
#' one time.
#'
#' @param arm A vector of arm labels, one element per panel row.
#' @param person A vector of person labels, one element per panel row.
#' @param weight Numeric, the analysis weight of each panel row.
#' @param event Numeric or integer, the 0/1 outcome indicator of each row.
#' @param tstart Numeric, the exclusive start of each row.
#' @param tstop Numeric, the inclusive stop of each row.
#' @param times Numeric, the reporting times. Sorted, unique, and holding every
#'   value of `tstop`.
#' @return A data.table with one row per arm and reporting time, sorted by arm
#'   and then by time. Columns `arm`, `time`, `events`, `at_risk` and
#'   `n_persons_at_risk`.
#' @noRd
.tte_span_risk_sets <- function(
  arm,
  person,
  weight,
  event,
  tstart,
  tstop,
  times
) {
  . <- arm_i <- t_i <- lo <- hi <- w <- ev <- dw <- dn <- run <- NULL # nolint
  events <- at_risk <- n_persons_at_risk <- person_i <- t_event <- NULL # nolint
  i.events <- i.dw <- i.dn <- NULL # nolint

  arms <- sort(unique(arm), na.last = TRUE)
  n_arm <- length(arms)
  n_time <- length(times)
  span <- .tte_span_index(tstart, tstop, times)

  d <- data.table::data.table(
    arm_i = match(arm, arms),
    person_i = person,
    w = as.numeric(weight),
    ev = as.numeric(event),
    t_event = match(tstop, times),
    lo = span$lo,
    hi = span$hi
  )
  if (anyNA(d$t_event)) {
    stop("every 'tstop' must be one of the reporting times")
  }

  out <- data.table::CJ(arm_i = seq_len(n_arm), t_i = seq_len(n_time))
  out[, `:=`(events = 0, dw = 0, dn = 0L)]

  # The event lands at the stop of its own row.
  e <- d[ev > 0, .(events = sum(w * ev)), keyby = .(arm_i, t_i = t_event)]
  out[e, events := i.events, on = c("arm_i", "t_i")]

  # The risk set spans. One `+w` where the row enters and one `-w` after it
  # leaves; the running sum is then the risk set at every reporting time. The
  # panel is millions of rows, so this stays linear in the rows.
  s <- d[hi >= lo]
  edges <- data.table::rbindlist(list(
    s[, .(arm_i, t_i = lo, dw = w)],
    s[, .(arm_i, t_i = hi + 1L, dw = -w)]
  ))[t_i <= n_time, .(dw = sum(dw)), keyby = .(arm_i, t_i)]
  out[edges, dw := i.dw, on = c("arm_i", "t_i")]
  out[, at_risk := cumsum(dw), by = "arm_i"]

  # The head count spans over the UNION of a person's rows. Merging her rows
  # into runs first is what stops two overlapping trials counting her twice.
  # The guard skips the grouping on an empty table: data.table evaluates
  # `min()` once on the empty group to type the result, and that warns.
  if (nrow(s)) {
    data.table::setorder(s, arm_i, person_i, lo, hi)
    s[,
      run := cumsum(lo > data.table::shift(cummax(hi), fill = 0L)),
      by = c("arm_i", "person_i")
    ]
    runs <- s[,
      .(lo = min(lo), hi = max(hi)),
      by = c("arm_i", "person_i", "run")
    ]
    head_edges <- data.table::rbindlist(list(
      runs[, .(arm_i, t_i = lo, dn = 1L)],
      runs[, .(arm_i, t_i = hi + 1L, dn = -1L)]
    ))[t_i <= n_time, .(dn = sum(dn)), keyby = .(arm_i, t_i)]
    out[head_edges, dn := i.dn, on = c("arm_i", "t_i")]
  }
  out[, n_persons_at_risk := cumsum(dn), by = "arm_i"]

  # A running sum over weights leaves a residue of about 1e-16 where the risk
  # set is empty. The head count is an integer and is exact, so it decides.
  out[n_persons_at_risk == 0L, at_risk := 0]

  out[, `:=`(arm = arms[arm_i], time = times[t_i])]
  out[, c("arm_i", "t_i", "dw", "dn") := NULL]
  data.table::setcolorder(
    out,
    c("arm", "time", "events", "at_risk", "n_persons_at_risk")
  )
  out[]
}
