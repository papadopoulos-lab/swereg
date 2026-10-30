# =============================================================================
# The minimum-events rule at s3: expected events per follow-up year, and
# withholding
# =============================================================================
# s3 cuts the follow-up of each ETT into follow-up years of
# `.HORIZON_WINDOW_WEEKS` weeks. The last window ends at the ETT's
# `follow_up` and can be partial. In each window, the expected events of an
# arm are computed under the null: the pooled weighted rate of both arms
# times the weighted person-time of that arm. A window passes when both arms
# reach `min_expected_events_per_arm_per_year`.
#
# The reportable horizon is the end of the last window before the first
# failing window. When a window fails, s3 withholds the incidence rate
# ratio, the risk difference and the number needed to treat of that
# estimand. Events, person-years, rates and the survival curve stay.
#
# The decision depends on the observed events through the pooled rate. It
# is a prespecified reporting rule. It is not a precision guarantee.
#
# An enrollment that sets `minimum_events_rule_exempt: true` is exempt. s3
# computes and stores the decision of its ETTs, and withholds none of their
# estimates. Their status is `.HORIZON_STATUS_EXEMPT`.
#
# The RD worker item of `.s3_ett_dispatch()` computes the window table
# outside `safe_call()`, so an error stops the item. The parent merge in
# `$s3_analyze()` calls `.s3_apply_horizon_rule()`, which stops when an ETT
# has no decision.
# =============================================================================

# The width of one window, in weeks. A package constant, not a spec value.
.HORIZON_WINDOW_WEEKS <- 52L

# The start of every `interval_status` and `horizon_status` value that the
# rule writes for a withheld estimand. Match it with `startsWith()`.
.HORIZON_STATUS_PREFIX <- "below minimum-events rule"

# The status text that swereg 27.2.14 stored for a withheld estimand.
.HORIZON_STATUS_PREFIX_LEGACY <- "below horizon rule"

# The `horizon_status` of an estimand whose windows all pass.
.HORIZON_STATUS_PASS <- "all windows pass"

# The `horizon_status` of every estimand of an ETT whose enrollment sets
# `minimum_events_rule_exempt: true`. It does not start with
# `.HORIZON_STATUS_PREFIX`, so `.tte_horizon_withheld()` reads it as reported.
.HORIZON_STATUS_EXEMPT <- "exempt from minimum-events rule"

# The cuts that `max_passing_cut` reports: the largest of them at which an
# estimand would still pass. A coarse grid, so the workbooks never show the
# expected events themselves.
.HORIZON_CUT_GRID <- c(1, 2, 3, 4, 5, 10, 20)

# The decision fields of a horizon record. The intention-to-treat record
# takes these from the per-protocol record.
.HORIZON_DECISION_FIELDS <- c(
  "reportable_weeks",
  "withheld",
  "fail_lo",
  "fail_hi",
  "fail_arm",
  "status",
  "max_passing_cut",
  "exempt",
  "by_year"
)

# The result slots of each estimand that the rule withholds. The subgroup
# and interaction-test slots end in `_pp` or `_itt`, and
# `.tte_horizon_withhold()` finds them by that suffix.
.HORIZON_SLOTS <- list(
  pp = list(
    irr = c("irr_pp_trunc", "irr_pp"),
    rd = "rd_pp_trunc",
    curve = "rd_curve_pp_trunc"
  ),
  itt = list(irr = "irr_itt", rd = "rd_itt", curve = "rd_curve_itt")
)


#' Does the spec exempt an enrollment from the minimum-events rule?
#'
#' An enrollment is exempt when it sets `minimum_events_rule_exempt: true`.
#' An enrollment without the key, and an id that the spec does not hold, are
#' not exempt. `tteplan_read_spec()` refuses every value that is not one
#' logical.
#'
#' @param spec `plan$spec`, or `NULL`.
#' @param enrollment_id The enrollment id, compared as text.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tte_enrollment_rule_exempt <- function(spec, enrollment_id) {
  if (length(enrollment_id) != 1L || is.na(enrollment_id)) {
    return(FALSE)
  }
  for (enr in spec$enrollments %||% list()) {
    if (identical(as.character(enr$id), as.character(enrollment_id))) {
      return(isTRUE(enr[["minimum_events_rule_exempt"]]))
    }
  }
  return(FALSE)
}


#' The names of the enrollments that the spec exempts, in spec order
#'
#' An exempt enrollment without a `name` is named by its id.
#'
#' @param spec `plan$spec`.
#' @return A character vector, empty when no enrollment is exempt.
#' @noRd
.tte_exempt_enrollment_names <- function(spec) {
  out <- character(0)
  for (enr in spec$enrollments %||% list()) {
    if (isTRUE(enr[["minimum_events_rule_exempt"]])) {
      nm <- enr$name
      ok <- is.character(nm) && length(nm) == 1L && !is.na(nm) && nzchar(nm)
      out <- c(out, if (ok) nm else as.character(enr$id))
    }
  }
  return(out)
}


#' The sentence of the generated prose that names the exempt enrollments
#'
#' TARGET item 6h, the OSF Inference criteria field and the Analysis plan
#' row of the protocol table each carry it after their withholding sentence.
#'
#' @param spec `plan$spec`.
#' @return A character(1) that ends with a full stop, or `NULL` when no
#'   enrollment is exempt.
#' @noRd
.tte_exempt_rule_sentence <- function(spec) {
  nm <- .tte_exempt_enrollment_names(spec)
  if (length(nm) == 0L) {
    return(NULL)
  }
  return(paste0(
    "The specification exempts ",
    .osf_and(nm),
    " from the rule, so the rule withholds none of ",
    if (length(nm) == 1L) "its" else "their",
    " estimates."
  ))
}


#' Is a stored status a status of the minimum-events rule?
#'
#' A result that swereg 27.2.14 stored carries the legacy prefix. It counts
#' as withheld too, so its estimates still read `NE` on export.
#'
#' @param status Character vector, `NULL` allowed.
#' @return A logical vector as long as `status`. `NA` gives `FALSE`.
#' @noRd
.tte_horizon_withheld <- function(status) {
  status <- as.character(status)
  return(
    !is.na(status) &
      (startsWith(status, .HORIZON_STATUS_PREFIX) |
        startsWith(status, .HORIZON_STATUS_PREFIX_LEGACY))
  )
}


#' One numeric column of a panel, checked for missing and non-finite values
#'
#' @param data The panel.
#' @param col Character(1), the column name.
#' @param what Character(1), the role of the column for the message.
#' @return The column as a double vector.
#' @noRd
.tte_horizon_col <- function(data, col, what) {
  if (!col %in% names(data)) {
    stop(
      "minimum-events rule: the ",
      what,
      " column '",
      col,
      "' is not in the panel.",
      call. = FALSE
    )
  }
  x <- suppressWarnings(as.numeric(data[[col]]))
  bad <- sum(!is.finite(x))
  if (bad > 0L) {
    stop(
      "minimum-events rule: the ",
      what,
      " column '",
      col,
      "' holds a missing or non-finite value on ",
      bad,
      " row(s). The expected events cannot be computed.",
      call. = FALSE
    )
  }
  return(x)
}


#' Weighted person-time, events and expected events per window and arm
#'
#' The window table of the minimum-events rule, for one ETT and one weight.
#' Rows are
#' half-open `(tstart, tstop]`. In each window, a row adds its weight times
#' the weeks of its interval inside the window, divided by 52.25, to the
#' weighted person-years. Its event counts in the window that holds its
#' `tstop`.
#'
#' The expected events of an arm are `rate_pooled * py_weighted`. An arm's own
#' events enter only through the pooled rate. A window with no pooled
#' person-time has a pooled rate of 0, and expected events of 0 in both arms.
#'
#' @param data The analysis panel, one row per person-trial interval.
#' @param weight_col Character(1), the weight column of the estimand.
#' @param follow_up Numeric(1), the follow-up of the ETT, in weeks. The last
#'   window ends here.
#' @param rule A list holding `min_expected_events`, the value of the spec
#'   key `min_expected_events_per_arm_per_year`. The stored horizon record
#'   keeps this field name, so a plan saved by swereg 27.2.14 still loads.
#' @param treatment_var Character(1), the arm column. `TRUE` is the
#'   intervention arm.
#' @param tstart_var,tstop_var,event_var Character(1), the column names of
#'   the interval start, the interval stop and the event indicator.
#' @return A data.table with one row per window and arm: `window_lo`,
#'   `window_hi`, `arm` (`"intervention"` or `"comparator"`), `py_weighted`,
#'   `events_weighted`, `rate_pooled`, `events_expected` and `pass`. `pass`
#'   is the decision of the window, so both arm rows of a window carry the
#'   same value.
#' @noRd
.tte_horizon_windows <- function(
  data,
  weight_col,
  follow_up,
  rule,
  treatment_var,
  tstart_var = "tstart",
  tstop_var = "tstop",
  event_var = "event"
) {
  follow_up <- suppressWarnings(as.numeric(follow_up))
  if (length(follow_up) != 1L || !is.finite(follow_up) || follow_up <= 0) {
    stop(
      "minimum-events rule: follow_up MUST be one finite number of weeks above 0.",
      call. = FALSE
    )
  }
  if (!is.list(rule) || is.null(rule[["min_expected_events"]])) {
    stop(
      "minimum-events rule: the rule MUST be a list that holds ",
      "min_expected_events.",
      call. = FALSE
    )
  }
  .tte_check_min_expected_events(rule[["min_expected_events"]])
  min_events <- as.numeric(rule[["min_expected_events"]])
  if (!treatment_var %in% names(data)) {
    stop(
      "minimum-events rule: the arm column '",
      treatment_var,
      "' is not in the panel.",
      call. = FALSE
    )
  }
  arm <- as.logical(data[[treatment_var]])
  if (anyNA(arm)) {
    stop(
      "minimum-events rule: the arm column '",
      treatment_var,
      "' holds a missing value.",
      call. = FALSE
    )
  }
  w <- .tte_horizon_col(data, weight_col, "weight")
  t0 <- .tte_horizon_col(data, tstart_var, "interval start")
  t1 <- .tte_horizon_col(data, tstop_var, "interval stop")
  ev <- .tte_horizon_col(data, event_var, "event")

  width <- .HORIZON_WINDOW_WEEKS
  k <- seq_len(ceiling(follow_up / width))
  lo <- as.numeric((k - 1L) * width)
  hi <- pmin(k * width, follow_up)

  rows <- lapply(seq_along(k), function(i) {
    overlap <- pmax(0, pmin(t1, hi[i]) - pmax(t0, lo[i]))
    in_window <- t1 > lo[i] & t1 <= hi[i]
    py <- c(sum(w[arm] * overlap[arm]), sum(w[!arm] * overlap[!arm])) / 52.25
    events <- c(
      sum(w[arm & in_window] * ev[arm & in_window]),
      sum(w[!arm & in_window] * ev[!arm & in_window])
    )
    rate_pooled <- if (sum(py) > 0) sum(events) / sum(py) else 0
    events_expected <- rate_pooled * py
    return(data.table::data.table(
      window_lo = lo[i],
      window_hi = hi[i],
      arm = c("intervention", "comparator"),
      py_weighted = py,
      events_weighted = events,
      rate_pooled = rate_pooled,
      events_expected = events_expected,
      pass = all(events_expected >= min_events)
    ))
  })
  return(data.table::rbindlist(rows))
}


#' Decide the reportable horizon from a window table
#'
#' @param windows The table `.tte_horizon_windows()` returns.
#' @param rule The rule it was computed with, a list holding
#'   `min_expected_events`.
#' @return A named list. `reportable_weeks` is the end of the last window
#'   before the first failing window. It is 0 when the first window fails,
#'   and the follow-up when no window fails. `withheld` is `TRUE` when a
#'   window fails. `fail_lo` and `fail_hi` name the first failing window.
#'   `fail_arm` names the arm that failed it: `"intervention"`,
#'   `"comparator"` or `"both"`. The three are `NA` when no window fails.
#'   `status` is the `horizon_status`. `max_passing_cut` is the largest cut
#'   of `.HORIZON_CUT_GRID` at which every window would pass, or `NA` when
#'   none would.
#' @noRd
.tte_horizon_decision <- function(windows, rule) {
  min_events <- as.numeric(rule[["min_expected_events"]])
  max_passing_cut <- .tte_horizon_max_passing_cut(windows$events_expected)
  win <- unique(windows[, c("window_lo", "window_hi", "pass"), with = FALSE])
  fail <- which(!win$pass)
  if (length(fail) == 0L) {
    return(list(
      reportable_weeks = max(win$window_hi),
      withheld = FALSE,
      fail_lo = NA_real_,
      fail_hi = NA_real_,
      fail_arm = NA_character_,
      status = .HORIZON_STATUS_PASS,
      max_passing_cut = max_passing_cut
    ))
  }
  first <- win[fail[1L]]
  in_first <- windows$window_lo == first$window_lo
  low <- windows$arm[in_first & windows$events_expected < min_events]
  arm <- if (length(low) == 1L) low else "both"
  weeks <- function(x) format(x, trim = TRUE, scientific = FALSE)
  return(list(
    reportable_weeks = first$window_lo,
    withheld = TRUE,
    fail_lo = first$window_lo,
    fail_hi = first$window_hi,
    fail_arm = arm,
    status = paste0(
      .HORIZON_STATUS_PREFIX,
      ": ",
      if (arm == "both") "both arms" else paste(arm, "arm"),
      ", weeks ",
      weeks(first$window_lo),
      "-",
      weeks(first$window_hi)
    ),
    max_passing_cut = max_passing_cut
  ))
}


#' The largest cut of the grid at which every window would pass
#'
#' A window passes at cut k when the expected events of both arms reach k.
#' So every window passes at k exactly when the smallest expected events
#' reach k.
#'
#' @param events_expected Numeric vector, the expected events of every window
#'   and arm.
#' @return Numeric(1), a value of `.HORIZON_CUT_GRID`, or `NA` when the
#'   smallest expected events are below the smallest cut.
#' @noRd
.tte_horizon_max_passing_cut <- function(events_expected) {
  ok <- .HORIZON_CUT_GRID[.HORIZON_CUT_GRID <= min(events_expected)]
  if (length(ok) == 0L) {
    return(NA_real_)
  }
  return(max(ok))
}


#' The decision of the rule up to each whole follow-up year
#'
#' Year k covers every window that ends at or before week `52 * k`. The
#' partial last window ends at the follow-up, so it is in the last year.
#' The Minimum events sheets read row k for a year that has no ETT of its
#' own.
#'
#' @param windows The table `.tte_horizon_windows()` returns. It MUST hold
#'   `events_expected`.
#' @return A data.table with one row per year, 1 to the number of windows.
#'   It has three columns: `year`, `reportable` and `max_passing_cut`.
#'   `reportable` is `TRUE` when every window of the year passes.
#'   `max_passing_cut` is `.tte_horizon_max_passing_cut()` over the expected
#'   events of those windows. The table holds no expected events.
#' @noRd
.tte_horizon_by_year <- function(windows) {
  width <- .HORIZON_WINDOW_WEEKS
  years <- seq_len(ceiling(max(windows$window_hi) / width))
  rows <- lapply(years, function(k) {
    sel <- windows$window_hi <= width * k
    return(data.table::data.table(
      year = k,
      reportable = all(windows$pass[sel]),
      max_passing_cut = .tte_horizon_max_passing_cut(
        windows$events_expected[sel]
      )
    ))
  })
  return(data.table::rbindlist(rows))
}


#' Give the intention-to-treat record the per-protocol decision
#'
#' The intention-to-treat estimate of an ETT is reported exactly when its
#' per-protocol estimate is, to the same horizon. The record keeps its own
#' window table, and `decided_by` is `"pp"`.
#'
#' @param pp,itt The horizon records of the two estimands of one ETT.
#' @return `itt`, with the decision fields of `pp`.
#' @noRd
.tte_horizon_follow_pp <- function(pp, itt) {
  for (f in .HORIZON_DECISION_FIELDS) {
    itt[f] <- list(pp[[f]])
  }
  itt$decided_by <- "pp"
  return(itt)
}


#' The horizon record of one ETT and one estimand
#'
#' The RD worker item computes it and returns it as `horizon_<estimand>`. The
#' record holds the rule that made the decision, so the writers can refuse a
#' result that a different rule judged.
#'
#' The record keeps the computed decision of the rule for every ETT:
#' `reportable_weeks`, `withheld`, the `fail_*` fields and `max_passing_cut`.
#' `rule$exempt` and `exempt` are `TRUE` when the rule holds `exempt = TRUE`,
#' that is, when the spec exempts the enrollment of the ETT. An exempt record
#' then has the status `.HORIZON_STATUS_EXEMPT`, which carries no
#' `.HORIZON_STATUS_PREFIX`. A record that is not exempt has `exempt = FALSE`.
#' Its `rule` holds no `exempt`, and its status is the status of
#' `.tte_horizon_decision()`. So it is the record of swereg 27.3.0 with one
#' more field.
#'
#' @param data,weight_col,follow_up,treatment_var,tstart_var,tstop_var
#'   As for `.tte_horizon_windows()`.
#' @param rule A list holding `min_expected_events` and, optionally,
#'   `exempt`. A missing `exempt` counts as `FALSE`.
#' @return A named list: `rule` (`min_expected_events`, and `exempt = TRUE`
#'   for an exempt record), `weight_col`, `follow_up`, `windows`, the fields
#'   of `.tte_horizon_decision()`, `exempt`, and `by_year`, the table of
#'   `.tte_horizon_by_year()`. `by_year` is a decision field, so the
#'   intention-to-treat record takes the `by_year` of the per-protocol
#'   record.
#' @noRd
.tte_horizon_record <- function(
  data,
  weight_col,
  follow_up,
  rule,
  treatment_var,
  tstart_var = "tstart",
  tstop_var = "tstop"
) {
  windows <- .tte_horizon_windows(
    data,
    weight_col,
    follow_up,
    rule,
    treatment_var,
    tstart_var,
    tstop_var
  )
  exempt <- isTRUE(rule$exempt)
  decision <- .tte_horizon_decision(windows, rule)
  if (exempt) {
    decision$status <- .HORIZON_STATUS_EXEMPT
  }
  decision$exempt <- exempt
  decision$by_year <- .tte_horizon_by_year(windows)
  # The rule of a record that is not exempt is the rule of swereg 27.3.0.
  # `.tte_assert_horizon_rule_current()` reads a missing `exempt` as FALSE.
  stored_rule <- list(
    min_expected_events = as.numeric(rule$min_expected_events)
  )
  if (exempt) {
    stored_rule$exempt <- TRUE
  }
  return(c(
    list(
      rule = stored_rule,
      weight_col = weight_col,
      follow_up = as.numeric(follow_up),
      windows = windows
    ),
    decision
  ))
}


#' Withhold the effect estimates of one estimand of one ETT result
#'
#' Sets the ratios, the risk difference and the number needed to treat of the
#' estimand to `NA`, with its subgroup ratios and interaction tests. Events,
#' person-years, rates and the survival columns stay. A skip envelope stays
#' as it is.
#'
#' @param res One element of `plan$results_ett`.
#' @param estimand Character(1), `"pp"` or `"itt"`.
#' @param record The horizon record of that estimand.
#' @return `res`, with the estimates withheld.
#' @noRd
.tte_horizon_withhold <- function(res, estimand, record) {
  slots <- .HORIZON_SLOTS[[estimand]]
  is_table <- function(x) data.table::is.data.table(x) && nrow(x) > 0L
  na_cols <- function(x, cols, value = NA_real_) {
    for (nm in intersect(cols, names(x))) {
      data.table::set(x, j = nm, value = value)
    }
    return(x)
  }
  ratio_cols <- c("IRR", "IRR_lower", "IRR_upper", "IRR_pvalue")
  for (s in slots$irr) {
    if (is_table(res[[s]])) {
      v <- na_cols(data.table::copy(res[[s]]), ratio_cols)
      data.table::set(v, j = "irr_estimable", value = FALSE)
      res[[s]] <- v
    }
  }
  if (is_table(res[[slots$rd]])) {
    res[[slots$rd]] <- .tte_rd_withhold_rows(
      data.table::copy(res[[slots$rd]]),
      TRUE,
      record$status
    )
  }
  curve <- res[[slots$curve]]
  if (is_table(curve)) {
    time_col <- setdiff(names(curve), .ACC_CURVE_KNOWN_COLS)
    time_col <- if ("tstop" %in% time_col) "tstop" else time_col[1L]
    res[[slots$curve]] <- .tte_rd_withhold_rows(
      data.table::copy(curve),
      curve[[time_col]] > record$reportable_weeks,
      record$status
    )
  }
  sfx <- paste0("_", estimand, "$")
  for (s in grep(paste0("^subgroup_.+", sfx), names(res), value = TRUE)) {
    if (is_table(res[[s]])) {
      v <- na_cols(data.table::copy(res[[s]]), ratio_cols)
      if ("irr_estimable" %in% names(v)) {
        data.table::set(v, j = "irr_estimable", value = FALSE)
      }
      for (a in c("em_pvalue", "ratio_of_irrs")) {
        if (!is.null(attr(v, a))) data.table::setattr(v, a, NA_real_)
      }
      res[[s]] <- v
    }
  }
  for (s in grep(paste0("^emtest_.+", sfx), names(res), value = TRUE)) {
    em <- res[[s]]
    if (is.list(em) && !isTRUE(em$skipped)) {
      for (nm in c("p_value", "ratio_of_irrs", "ratio_lower", "ratio_upper")) {
        if (!is.null(em[[nm]])) em[[nm]] <- NA_real_
      }
      if (is_table(em$interaction_coefs)) {
        em$interaction_coefs <- na_cols(
          data.table::copy(em$interaction_coefs),
          c("estimate", "se", "p")
        )
      }
      res[[s]] <- em
    }
  }
  return(res)
}


#' Apply the horizon decisions of an s3 run to the stored results
#'
#' The parent merge of `$s3_analyze()` calls it after it stores every worker
#' return. It stops when an ETT of the run has no horizon record for an
#' estimand. A failed or skipped RD worker can then never leave reported
#' estimates behind. The intention-to-treat record then takes the
#' per-protocol decision, through `.tte_horizon_follow_pp()`.
#'
#' It withholds an estimand when its record has `withheld = TRUE` and is not
#' exempt. An exempt record keeps `withheld = TRUE` when the rule fails, and
#' the function withholds none of its estimates.
#'
#' @param results_ett `plan$results_ett`.
#' @param ett_ids Character vector, the ETTs this run computed.
#' @return `results_ett`, with every withheld estimand withheld.
#' @noRd
.s3_apply_horizon_rule <- function(results_ett, ett_ids) {
  for (eid in ett_ids) {
    res <- results_ett[[eid]]
    for (est in names(.TTE_ESTIMANDS)) {
      rec <- res[[paste0("horizon_", est)]]
      if (!is.list(rec) || !is.logical(rec$withheld) || is.na(rec$withheld)) {
        stop(
          "s3_analyze(): ETT ",
          eid,
          " has no decision of the minimum-events rule for the ",
          .TTE_ESTIMANDS[[est]],
          " estimand. Its risk-difference worker failed or was skipped, ",
          "so its estimates cannot be reported. Fix the cause and rerun ",
          "$s3_analyze().",
          call. = FALSE
        )
      }
    }
    res$horizon_itt <- .tte_horizon_follow_pp(res$horizon_pp, res$horizon_itt)
    for (est in names(.TTE_ESTIMANDS)) {
      rec <- res[[paste0("horizon_", est)]]
      if (rec$withheld && !isTRUE(rec$exempt)) {
        res <- .tte_horizon_withhold(res, est, rec)
      }
    }
    results_ett[[eid]] <- res
  }
  return(results_ett)
}


#' Stop unless the spec carries a valid min_expected_events_per_arm_per_year
#'
#' @param spec `plan$spec`.
#' @param where Character(1), the method name for the message.
#' @return The rule, a list holding `min_expected_events`. That is the field
#'   name of the stored horizon record.
#' @noRd
.tte_assert_horizon_rule_present <- function(spec, where) {
  value <- spec[["min_expected_events_per_arm_per_year"]]
  if (is.null(value)) {
    stop(
      where,
      ": the plan's spec has no min_expected_events_per_arm_per_year. s3 ",
      "needs it to decide the reportable horizon. Add ",
      "min_expected_events_per_arm_per_year: 5 to the spec file and call ",
      "$reload_spec().",
      call. = FALSE
    )
  }
  .tte_check_min_expected_events(value)
  return(list(min_expected_events = as.numeric(value)))
}


#' Stop unless every stored result holds a decision of the current rule
#'
#' Each horizon record holds a decision and the rule that made it. The
#' writers stop when a stored ETT result has no such record for an estimand.
#' A result stored before swereg 27.2.14 has none. They also stop when the
#' recorded `rule$min_expected_events` is not `identical()` to the spec value
#' `min_expected_events_per_arm_per_year`, with both values as doubles and no
#' tolerance. They stop, too, when an intention-to-treat record was decided
#' on its own, as before swereg 27.3.0, rather than by the per-protocol
#' decision.
#'
#' They also stop when a stored record's `rule$exempt` differs from the
#' current exemption of its ETT. The function finds the enrollment of each
#' ETT in `plan$ett$enrollment_id`, and reads `minimum_events_rule_exempt` of
#' that enrollment in `plan$spec`. A record without `rule$exempt`, as swereg
#' 27.3.0 stored it, counts as not exempt. So a result of a spec without the
#' key stays current.
#'
#' @param plan A `TTEPlan`.
#' @param where Character(1), the method name for the message.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_assert_horizon_rule_current <- function(plan, where) {
  want <- plan$spec[["min_expected_events_per_arm_per_year"]]
  want <- if (is.numeric(want) && length(want) == 1L) as.numeric(want) else NULL
  absent <- character(0)
  stale <- character(0)
  seen <- numeric(0)
  own_itt <- character(0)
  flipped <- character(0)
  ett_enrollment <- stats::setNames(
    as.character(plan$ett$enrollment_id),
    as.character(plan$ett$ett_id)
  )
  for (eid in names(plan$results_ett)) {
    enr <- if (eid %in% names(ett_enrollment)) {
      ett_enrollment[[eid]]
    } else {
      NA_character_
    }
    want_exempt <- .tte_enrollment_rule_exempt(plan$spec, enr)
    itt <- plan$results_ett[[eid]][["horizon_itt"]]
    if (is.list(itt) && !identical(itt$decided_by, "pp")) {
      own_itt <- c(own_itt, eid)
    }
    for (est in names(.TTE_ESTIMANDS)) {
      rec <- plan$results_ett[[eid]][[paste0("horizon_", est)]]
      got <- if (is.list(rec)) rec$rule$min_expected_events else NULL
      w <- if (is.list(rec)) rec$withheld else NULL
      decided <- is.logical(w) && length(w) == 1L && !is.na(w)
      if (!decided || !is.numeric(got) || length(got) != 1L) {
        absent <- c(absent, paste0(eid, " (", est, ")"))
        next
      }
      if (!identical(as.numeric(got), want)) {
        stale <- c(stale, eid)
        seen <- c(seen, as.numeric(got))
      }
      got_exempt <- isTRUE(rec$rule$exempt)
      if (!identical(got_exempt, want_exempt)) {
        flipped <- c(
          flipped,
          paste0(
            eid,
            " (enrollment ",
            enr,
            ": stored ",
            tolower(got_exempt),
            ", spec ",
            tolower(want_exempt),
            ")"
          )
        )
      }
    }
  }
  flipped <- unique(flipped)
  if (length(absent) > 0L) {
    stop(
      where,
      ": ",
      length(absent),
      " stored ETT estimand(s) hold no decision of the minimum-events rule: ",
      paste(utils::head(absent, 5L), collapse = ", "),
      ". A result stored before swereg 27.2.14 holds none. Rerun ",
      "$s3_analyze() to apply the current rule.",
      call. = FALSE
    )
  }
  if (length(own_itt) > 0L) {
    stop(
      where,
      ": the stored intention-to-treat results of ",
      length(own_itt),
      " ETT(s) (",
      paste(utils::head(own_itt, 5L), collapse = ", "),
      ") were judged on their own, as before swereg 27.3.0. The ",
      "intention-to-treat estimate now follows the per-protocol decision. ",
      "Rerun $s3_analyze() to apply it.",
      call. = FALSE
    )
  }
  if (length(stale) > 0L) {
    stop(
      where,
      ": the stored results of ",
      length(unique(stale)),
      " ETT(s) (",
      paste(utils::head(unique(stale), 5L), collapse = ", "),
      ") were judged with min_expected_events_per_arm_per_year = ",
      paste(format(sort(unique(seen)), digits = 15L), collapse = ", "),
      ", and the spec holds ",
      if (is.null(want)) {
        "no min_expected_events_per_arm_per_year"
      } else {
        format(want, digits = 15L)
      },
      ". Rerun $s3_analyze() to apply the current rule.",
      call. = FALSE
    )
  }
  if (length(flipped) > 0L) {
    stop(
      where,
      ": the stored results of ",
      length(flipped),
      " ETT(s) were judged with another minimum_events_rule_exempt than ",
      "the spec now gives their enrollment: ",
      paste(utils::head(flipped, 5L), collapse = ", "),
      ". Rerun $s3_analyze() to apply the current exemption.",
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}


#' Name every withheld estimand of an s3 run on stdout
#'
#' The worker output reaches nobody, so the driver states the decision in the
#' job log. An estimand of an exempt enrollment is never named as withheld.
#' When the run holds one, a second line names each such estimand as exempt.
#' A run without an exempt estimand prints the one line of swereg 27.3.0.
#'
#' @param results_ett `plan$results_ett`.
#' @param ett_ids Character vector, the ETTs this run computed.
#' @return `invisible(NULL)`.
#' @noRd
.s3_horizon_report <- function(results_ett, ett_ids) {
  if (length(ett_ids) == 0L) {
    return(invisible(NULL))
  }
  hits <- character(0)
  exempt <- character(0)
  for (eid in ett_ids) {
    for (est in names(.TTE_ESTIMANDS)) {
      rec <- results_ett[[eid]][[paste0("horizon_", est)]]
      if (isTRUE(rec$exempt)) {
        exempt <- c(exempt, paste0(eid, " (", est, ")"))
      } else if (isTRUE(rec$withheld)) {
        hits <- c(
          hits,
          paste0(eid, " (", est, ", ", rec$reportable_weeks, " w)")
        )
      }
    }
  }
  n <- length(ett_ids) * length(.TTE_ESTIMANDS)
  if (length(hits) == 0L) {
    cat(sprintf(
      "s3: the minimum-events rule withheld none of %d ETT estimands.\n",
      n
    ))
  } else {
    cat(sprintf(
      "s3: the minimum-events rule withheld %d of %d ETT estimands (reportable weeks): %s\n",
      length(hits),
      n,
      paste(hits, collapse = ", ")
    ))
  }
  if (length(exempt) > 0L) {
    cat(sprintf(
      "s3: the specification exempts %d of %d ETT estimands from the minimum-events rule, so the rule withheld none of their estimates: %s\n",
      length(exempt),
      n,
      paste(exempt, collapse = ", ")
    ))
  }
  return(invisible(NULL))
}
