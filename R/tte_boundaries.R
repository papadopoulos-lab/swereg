# =============================================================================
# Deviation boundary
# =============================================================================

#' Place the deviation boundary of every enrolled person-trial.
#'
#' The boundary is the week that follow-up stops at, counted from time zero.
#' It is exact to the week, and it comes from the weekly assessments. It is an
#' exclusive stop. See the interval convention section of [TTEDesign].
#'
#' `enroll()` collapses each follow-up interval to one row, so the weekly
#' sequence is gone by the time `s5_prepare_outcome()` runs. This function reads
#' the sequence here, where it still exists. It returns one integer per
#' person-trial, and never a weekly panel.
#'
#' @section Discordance and the arm tolerance:
#'
#' An assessment is discordant when `design$time_treatment_var` does not hold
#' the assigned arm of that person-trial. `NA` is discordant in both arms.
#'
#' A tolerance is the number of CONSECUTIVE discordant assessments an arm
#' allows. A concordant assessment resets the run. Each arm reads its own
#' tolerance: `design$intervention_tolerance_weeks` and
#' `design$comparator_tolerance_weeks`.
#'
#' For tolerance `k`, the boundary is the right edge of the `(k + 1)`th
#' consecutive discordant week. A run that starts at week `u0` therefore gives
#' `(u0 + k + 1) - L`, where `L` is the time-zero week. A tolerance of 0 censors
#' at the first discordant week.
#'
#' A run that starts before time zero counts only its weeks at or after it.
#' The `u >= L + k` test below is what enforces that.
#'
#' @section Loss of observation:
#'
#' Loss of observation is not discordance, and this boundary does not hold it.
#' `.tte_observation_gap_boundary()` places the first absent week, and
#' `s5_prepare_outcome()` stops follow-up there under both estimands. The
#' event-priority rule of a deviation therefore never applies to a gap.
#'
#' @section Runs are read over the observed weeks only:
#'
#' A run is a set of discordant weeks with consecutive week indices. An absent
#' week therefore breaks a run. A run that ends after a gap starts after it,
#' so its boundary falls after the gap boundary.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `enrollment_period_id`, `baseline_tx` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_intervals Integer, the number of follow-up intervals the
#'   panel holds. A boundary past the last interval reads `NA`.
#' @return A data.table keyed by `id_var`, with one integer column
#'   `weeks_to_protocol_deviation`. `NULL` when the design cannot support the
#'   weekly read.
#' @noRd
.tte_deviation_boundary <- function(
  entry_dt,
  data_enrolled,
  design,
  person_id_col,
  id_var,
  n_follow_up_intervals
) {
  dv_pid <- dv_week <- dv_run <- dv_start <- NULL # nolint
  dv_len <- dv_hit <- q_week <- NULL # nolint

  tx_col <- design$time_treatment_var
  # Without an observation encoding swereg cannot tell an absent week from a
  # week outside the study, so it cannot report a gap. Phase 8 gates landmark
  # qualification on the same field, and the two MUST agree.
  if (is.null(design$observed_var)) {
    return(NULL)
  }
  if (is.null(tx_col) || !tx_col %in% names(data_enrolled)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }
  if (nrow(entry_dt) == 0L) {
    return(NULL)
  }

  period_width <- as.integer(design$period_width)
  span <- as.integer(n_follow_up_intervals) * period_width

  # --- the observed weekly assessments -------------------------------------
  # A row that fails the observation test is dropped here, so a week is
  # present in `w` if and only if the person was under observation in it. The
  # `row_presence` sentinel keeps every row, because the caller has already
  # deleted the unobserved ones.
  observed_col <- .tte_observed_column(design$observed_var)
  week_index <- .tte_week_index0(data_enrolled[["isoyearweek"]])
  keep <- !is.na(week_index)
  if (!is.null(observed_col)) {
    keep <- keep & .tte_is_true(data_enrolled[[observed_col]])
  }
  w <- data.table::data.table(
    dv_pid = data_enrolled[[person_id_col]][keep],
    dv_week = week_index[keep],
    dv_tx = data_enrolled[[tx_col]][keep]
  )
  data.table::setkeyv(w, c("dv_pid", "dv_week"))

  # --- the person-trials ---------------------------------------------------
  # Time zero of a person-trial is the first week after its enrollment period.
  lm_week <- (as.integer(entry_dt[["enrollment_period_id"]]) + 1L) *
    period_width
  arm <- .tte_is_true(entry_dt[["baseline_tx"]])
  n_pt <- length(lm_week)
  stop_week <- rep(NA_integer_, n_pt)

  # An observation gap is not read here. `.tte_observation_gap_boundary()`
  # reads it, and `s5_prepare_outcome()` treats it as loss of observation.

  # --- discordant runs, one arm at a time ---------------------------------- A
  # person can be an initiator in one enrollment period and a comparator in
  # another, so the runs are read per arm and not per person.
  for (this_arm in c(TRUE, FALSE)) {
    idx <- which(arm == this_arm)
    if (length(idx) == 0L) {
      next
    }
    k <- as.integer(
      if (this_arm) {
        design$intervention_tolerance_weeks
      } else {
        design$comparator_tolerance_weeks
      }
    )

    # Concordance for the intervention arm is `TRUE`, and for the comparator
    # arm it is `FALSE`. Every other value, `NA` included, is discordant.
    is_disc <- if (this_arm) {
      !.tte_is_true(w[["dv_tx"]])
    } else {
      !.tte_is_false(w[["dv_tx"]])
    }
    dw <- unique(w[is_disc, list(dv_pid, dv_week)], by = c("dv_pid", "dv_week"))
    if (nrow(dw) == 0L) {
      next
    }
    data.table::setkeyv(dw, c("dv_pid", "dv_week"))

    # A run starts at a discordant week whose previous week is not the week
    # before it. That covers a concordant week and an absent week alike,
    # because neither reaches `dw`.
    dw[,
      dv_start := {
        prev <- data.table::shift(dv_week)
        is.na(prev) | prev != dv_week - 1L
      },
      by = dv_pid
    ]
    # The first discordant week of every person starts a run, so the running
    # sum never joins two people.
    dw[, dv_run := cumsum(dv_start)]
    dw[, dv_len := seq_len(.N), by = dv_run]

    # A week qualifies when the run ending there is at least `k + 1` weeks
    # long. The boundary is the right edge of the earliest qualifying week
    # that is at or after `L + k`, which is the earliest week whose whole run
    # of `k + 1` sits inside follow-up.
    qk <- dw[dv_len >= k + 1L, list(dv_pid, dv_week)]
    if (nrow(qk) == 0L) {
      next
    }
    data.table::setkeyv(qk, c("dv_pid", "dv_week"))
    qk[, dv_hit := dv_week]
    q <- data.table::data.table(dv_pid = entry_dt[[".tte_person_id"]][idx])
    q[, q_week := lm_week[idx] + k]
    hit <- qk[q, on = c("dv_pid", dv_week = "q_week"), roll = -Inf, dv_hit]
    stop_week[idx] <- pmin(stop_week[idx], hit + 1L, na.rm = TRUE)
  }

  # --- the boundary, counted from the landmark -----------------------------
  weeks <- stop_week - lm_week
  weeks[!is.na(weeks) & weeks > span] <- NA_integer_

  out <- data.table::data.table(weeks_to_protocol_deviation = weeks)
  data.table::set(out, j = id_var, value = entry_dt[[id_var]])
  data.table::setkeyv(out, id_var)
  return(out[])
}


#' Find the first absent week at or after a query week.
#'
#' A gap opens at `u + 1` when the next observed week of the same person is
#' later than `u + 1`. The last week of a record has no next week, so it opens
#' no gap here.
#'
#' @param obs_pid,obs_week The person and the 0-based week index of every
#'   observed person-week.
#' @param query_pid,query_week One person and one week per query.
#' @return An integer vector with one element per query. It holds the first
#'   gap week at or after `query_week`, or `NA` when the person has none.
#' @noRd
.tte_first_gap_week <- function(obs_pid, obs_week, query_pid, query_week) {
  gp_pid <- gp_week <- gp_next <- gp_hit <- NULL # nolint
  out <- rep(NA_integer_, length(query_pid))
  w <- data.table::data.table(gp_pid = obs_pid, gp_week = obs_week)
  data.table::setkeyv(w, c("gp_pid", "gp_week"))
  w[, gp_next := data.table::shift(gp_week, type = "lead"), by = gp_pid]
  gaps <- w[
    !is.na(gp_next) & gp_next > gp_week + 1L,
    list(gp_pid, gp_week = gp_week + 1L)
  ]
  # A duplicated person-week would otherwise duplicate a gap, and a duplicate
  # in the joined table below would return two rows for one query.
  gaps <- unique(gaps, by = c("gp_pid", "gp_week"))
  if (nrow(gaps) == 0L) {
    return(out)
  }
  data.table::setkeyv(gaps, c("gp_pid", "gp_week"))
  gaps[, gp_hit := gp_week]
  q <- data.table::data.table(gp_pid = query_pid, gp_week = query_week)
  out <- gaps[q, on = c("gp_pid", "gp_week"), roll = -Inf, gp_hit]
  return(out)
}


#' Place the observation-gap boundary of every enrolled person-trial.
#'
#' The boundary is the first absent week after time zero, counted from time
#' zero. It is an exclusive stop. See the interval convention section of
#' [TTEDesign].
#'
#' A gap is loss of observation, and it stops follow-up under both estimands.
#' This function reads it without the treatment variable, so an ITT design
#' without `time_treatment_var` still finds it.
#'
#' @inheritParams .tte_record_end_boundary
#' @return A data.table keyed by `id_var`, with one integer column
#'   `weeks_to_observation_gap`. `NULL` when the design cannot support the
#'   weekly read.
#' @noRd
.tte_observation_gap_boundary <- function(
  entry_dt,
  data_enrolled,
  design,
  person_id_col,
  id_var,
  n_follow_up_intervals
) {
  # The same gate as `.tte_record_end_boundary()`.
  if (is.null(design$observed_var)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }
  if (nrow(entry_dt) == 0L) {
    return(NULL)
  }

  period_width <- as.integer(design$period_width)
  span <- as.integer(n_follow_up_intervals) * period_width

  observed_col <- .tte_observed_column(design$observed_var)
  week_index <- .tte_week_index0(data_enrolled[["isoyearweek"]])
  keep <- !is.na(week_index)
  if (!is.null(observed_col)) {
    keep <- keep & .tte_is_true(data_enrolled[[observed_col]])
  }

  lm_week <- (as.integer(entry_dt[["enrollment_period_id"]]) + 1L) *
    period_width
  gap_week <- .tte_first_gap_week(
    obs_pid = data_enrolled[[person_id_col]][keep],
    obs_week = week_index[keep],
    query_pid = entry_dt[[".tte_person_id"]],
    query_week = lm_week
  )

  # The same cut as `.tte_deviation_boundary()`.
  weeks <- gap_week - lm_week
  weeks[!is.na(weeks) & weeks > span] <- NA_integer_

  out <- data.table::data.table(weeks_to_observation_gap = weeks)
  data.table::set(out, j = id_var, value = entry_dt[[id_var]])
  data.table::setkeyv(out, id_var)
  return(out[])
}


#' Write the observation-gap boundary into an enrolled panel
#'
#' Types the column first, so an empty panel still carries it and
#' `tteenrollment_rbind()` sees one column set across the chunks.
#'
#' @param panel The enrolled panel. It is changed by reference.
#' @param gap_dt The return of `.tte_observation_gap_boundary()`, or `NULL`.
#' @param id_var Character, the person-trial identifier column.
#' @return `panel`, invisibly.
#' @noRd
.tte_write_observation_gap <- function(panel, gap_dt, id_var) {
  weeks_to_observation_gap <- i.weeks_to_observation_gap <- NULL # nolint
  if (is.null(gap_dt)) {
    return(invisible(panel))
  }
  data.table::set(panel, j = "weeks_to_observation_gap", value = NA_integer_)
  if (nrow(panel) > 0L) {
    panel[
      gap_dt,
      weeks_to_observation_gap := i.weeks_to_observation_gap,
      on = id_var
    ]
  }
  return(invisible(panel))
}


# The warning for a panel enrolled before swereg wrote
# `weeks_to_observation_gap`. `.tte_gap_record_end()` gives it in
# `s5_prepare_outcome()`.
.TTE_LEGACY_GAP_WARNING <- paste(
  "This enrollment was enrolled before swereg 26.15.0, so its panel has no",
  "`weeks_to_observation_gap` column. Gaps in observation cannot be detected",
  "in it, and an outcome after such a gap may be counted. Re-run s1",
  "(`$s1_generate_enrollments_and_ipw()`) to remove this limitation."
)


#' Test whether an enrollment panel predates `weeks_to_observation_gap`
#'
#' `enroll()` writes `weeks_to_observation_gap` whenever the design declares
#' `observed_var`. A non-empty panel that `enroll()` built with
#' `observed_var` and that lacks the column was enrolled before swereg
#' 26.15.0.
#'
#' @param data The panel.
#' @param design The [TTEDesign] of the enrollment, or `NULL`.
#' @param steps_completed Character, the `steps_completed` of the enrollment.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tte_is_legacy_gap_panel <- function(data, design, steps_completed) {
  if (!is.data.frame(data) || is.null(design)) {
    return(FALSE)
  }
  return(
    "enroll" %in% steps_completed &&
      !is.null(design$observed_var) &&
      nrow(data) > 0L &&
      !"weeks_to_observation_gap" %in% names(data)
  )
}


#' Stop follow-up at the first absent week
#'
#' Both estimands stop at the first absent week after time zero, with no
#' tolerance. The gap is loss of observation, so this function moves the
#' record end of `s5_prepare_outcome()` to it. A gap is not a deviation, so
#' the event-priority rule never clears it. An outcome after the gap is never
#' counted, even when it falls in the same follow-up interval as the gap.
#'
#' A whole missing follow-up interval has no panel row, and every later row
#' is numbered one interval too early. All of those rows open at or after the
#' gap, so `s5_prepare_outcome()` drops them.
#'
#' A panel enrolled before swereg 26.15.0 lacks `weeks_to_observation_gap`
#' (see `.tte_is_legacy_gap_panel()`). Its weekly rows are gone, so swereg
#' cannot recompute the gap. This function then leaves the record end alone,
#' and warns with `.TTE_LEGACY_GAP_WARNING`. The warning says three
#' things:
#'
#' - Gaps in observation cannot be detected in the panel.
#' - An outcome after such a gap may be counted.
#' - A re-run of s1 removes the limitation.
#'
#' [qs2_read()] refuses an enrollment saved before schema 6, so this panel
#' reaches the function only from an object in memory.
#'
#' @param data The panel inside `s5_prepare_outcome()`. It MUST carry
#'   `.record_end`. It is changed by reference.
#' @param design The [TTEDesign] of the enrollment.
#' @param steps_completed Character, the `steps_completed` of the enrollment.
#' @return `data`, invisibly.
#' @noRd
.tte_gap_record_end <- function(data, design, steps_completed) {
  .record_end <- weeks_to_observation_gap <- NULL # nolint
  if (!"weeks_to_observation_gap" %in% names(data)) {
    if (.tte_is_legacy_gap_panel(data, design, steps_completed)) {
      warning(.TTE_LEGACY_GAP_WARNING, call. = FALSE)
    }
    return(invisible(data))
  }
  data[,
    .record_end := pmin(.record_end, weeks_to_observation_gap, na.rm = TRUE)
  ]
  return(invisible(data))
}


#' Place the record-end boundary of every enrolled person-trial.
#'
#' The boundary is the week the weekly record stops at, counted from time
#' zero. It is exact to the week, and it comes from the weekly sequence. It
#' is an exclusive stop. See the interval convention section of [TTEDesign].
#'
#' A record that simply ends carries no internal gap, so
#' `.tte_deviation_boundary()` never reports it. `s5_prepare_outcome()` reports
#' it as `weeks_to_loss`, and reads `.max_tstop` for the value. `.max_tstop` is
#' the stop of the LAST follow-up interval, so a record that ends inside an
#' interval overshoots by up to `period_width - 1` weeks. This function reads
#' the exact week instead.
#'
#' A record that reaches the end of the panel returns `NA`. Nothing is left for
#' it to stop, and the person completed the follow-up the panel holds.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `enrollment_period_id` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_intervals Integer, the number of follow-up intervals the
#'   panel holds.
#' @return A data.table keyed by `id_var`, with one integer column
#'   `weeks_to_record_end`. `NULL` when the design cannot support the weekly
#'   read.
#' @noRd
.tte_record_end_boundary <- function(
  entry_dt,
  data_enrolled,
  design,
  person_id_col,
  id_var,
  n_follow_up_intervals
) {
  re_pid <- re_week <- NULL # nolint
  re_last <- NULL # nolint

  # The same gate as `.tte_deviation_boundary()`. Without an observation
  # encoding swereg cannot say whether the record ended or the person is
  # simply absent from these weeks, and the two boundaries MUST agree on that.
  if (is.null(design$observed_var)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }
  if (nrow(entry_dt) == 0L) {
    return(NULL)
  }

  period_width <- as.integer(design$period_width)
  span <- as.integer(n_follow_up_intervals) * period_width

  observed_col <- .tte_observed_column(design$observed_var)
  week_index <- .tte_week_index0(data_enrolled[["isoyearweek"]])
  keep <- !is.na(week_index)
  if (!is.null(observed_col)) {
    keep <- keep & .tte_is_true(data_enrolled[[observed_col]])
  }
  if (!any(keep)) {
    return(NULL)
  }

  last_week <- data.table::data.table(
    re_pid = data_enrolled[[person_id_col]][keep],
    re_week = week_index[keep]
  )[, list(re_last = max(re_week)), by = re_pid]
  data.table::setkeyv(last_week, "re_pid")

  lm_week <- (as.integer(entry_dt[["enrollment_period_id"]]) + 1L) *
    period_width
  hit <- last_week[
    data.table::data.table(re_pid = entry_dt[[".tte_person_id"]]),
    on = "re_pid",
    re_last
  ]

  # A week is a half-open interval, so a record whose last observed week is
  # `u` stops at `u + 1`.
  weeks <- (hit + 1L) - lm_week
  weeks[!is.na(weeks) & weeks >= span] <- NA_integer_

  out <- data.table::data.table(weeks_to_record_end = weeks)
  data.table::set(out, j = id_var, value = entry_dt[[id_var]])
  data.table::setkeyv(out, id_var)
  return(out[])
}


#' Place the outcome boundary of every enrolled person-trial.
#'
#' The boundary is the week the outcome falls in, counted from time zero. It
#' is exact to the week, and it comes from the weekly sequence. It is an
#' exclusive stop. See the interval convention section of [TTEDesign].
#'
#' The collapse keeps one outcome flag per follow-up interval. After it the
#' week is gone, and the only boundary left to read is the stop of the
#' interval. That
#' overshoots by up to `period_width - 1` weeks. It also disagrees with
#' `weeks_to_record_end` and `weeks_to_protocol_deviation`, which are exact.
#' The disagreement changes the winner. A woman whose record ends in week 10,
#' and whose outcome falls in week 10, loses her event to the record end.
#'
#' The active outcome is chosen later, in `s5_prepare_outcome()`, so this
#' returns one column per outcome the design names.
#'
#' An outcome week before time zero is not a follow-up event and never
#' becomes the boundary. A boundary past the last follow-up interval of the
#' panel reads `NA`.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `enrollment_period_id` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col`, `isoyearweek` and the outcome columns.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_intervals Integer, the number of follow-up intervals the
#'   panel holds.
#' @return A data.table keyed by `id_var`, with one integer column
#'   `weeks_to_event_<outcome>` per outcome column. `NULL` when the design
#'   cannot support the weekly read.
#' @noRd
.tte_event_boundary <- function(
  entry_dt,
  data_enrolled,
  design,
  person_id_col,
  id_var,
  n_follow_up_intervals
) {
  ev_pid <- ev_week <- ev_hit <- q_week <- NULL # nolint

  # The same gate as `.tte_deviation_boundary()` and
  # `.tte_record_end_boundary()`. Without an observation encoding swereg
  # cannot say whether a week without the outcome was observed at all, and the
  # three boundaries MUST agree on that.
  if (is.null(design$observed_var)) {
    return(NULL)
  }
  if (!"isoyearweek" %in% names(data_enrolled)) {
    return(NULL)
  }
  if (nrow(entry_dt) == 0L) {
    return(NULL)
  }
  outcome_cols <- intersect(design$outcome_vars, names(data_enrolled))
  if (length(outcome_cols) == 0L) {
    return(NULL)
  }

  period_width <- as.integer(design$period_width)
  span <- as.integer(n_follow_up_intervals) * period_width

  observed_col <- .tte_observed_column(design$observed_var)
  week_index <- .tte_week_index0(data_enrolled[["isoyearweek"]])
  keep <- !is.na(week_index)
  if (!is.null(observed_col)) {
    keep <- keep & .tte_is_true(data_enrolled[[observed_col]])
  }
  if (!any(keep)) {
    return(NULL)
  }

  pid_kept <- data_enrolled[[person_id_col]][keep]
  week_kept <- week_index[keep]
  lm_week <- (as.integer(entry_dt[["enrollment_period_id"]]) + 1L) *
    period_width
  q <- data.table::data.table(
    ev_pid = entry_dt[[".tte_person_id"]],
    q_week = lm_week
  )

  out <- data.table::data.table(seq_len(nrow(entry_dt)))
  data.table::set(out, j = 1L, value = entry_dt[[id_var]])
  data.table::setnames(out, 1L, id_var)

  for (col in outcome_cols) {
    weeks <- rep(NA_integer_, nrow(entry_dt))
    hit_rows <- .tte_is_true(data_enrolled[[col]][keep])
    if (any(hit_rows)) {
      # The first outcome week at or after the landmark. A duplicated
      # person-week would return two rows for one person-trial.
      ew <- unique(data.table::data.table(
        ev_pid = pid_kept[hit_rows],
        ev_week = week_kept[hit_rows]
      ))
      data.table::setkeyv(ew, c("ev_pid", "ev_week"))
      ew[, ev_hit := ev_week]
      hit <- ew[q, on = c("ev_pid", ev_week = "q_week"), roll = -Inf, ev_hit]
      # A week is a half-open interval, so an outcome in week `u` stops at
      # `u + 1`.
      weeks <- (hit + 1L) - lm_week
      weeks[!is.na(weeks) & weeks > span] <- NA_integer_
    }
    data.table::set(
      out,
      j = paste0("weeks_to_event_", col),
      value = as.integer(weeks)
    )
  }
  data.table::setkeyv(out, id_var)
  return(out[])
}
