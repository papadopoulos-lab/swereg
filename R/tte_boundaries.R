# =============================================================================
# Deviation boundary
# =============================================================================

#' Place the deviation boundary of every enrolled person-trial.
#'
#' The boundary is the week that follow-up stops at, counted from the landmark.
#' It is exact to the week, and it comes from the weekly assessments. It is an
#' exclusive stop. See the interval convention section of [TTEDesign].
#'
#' `enroll()` collapses each band to one row, so the weekly sequence is gone by
#' the time `s5_prepare_outcome()` runs. This function reads the sequence here,
#' where it still exists. It returns one integer per person-trial, and never a
#' weekly panel.
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
#' `(u0 + k + 1) - L`, where `L` is the landmark week. A tolerance of 0 censors
#' at the first discordant week.
#'
#' A run that starts before the landmark counts only its weeks at or after it.
#' The `u >= L + k` test below is what enforces that.
#'
#' @section Loss of observation:
#'
#' Loss of observation is not discordance, and no tolerance applies to it. An
#' internal gap in the weekly sequence stops follow-up at the first absent
#' week. The person may return in a later week. She is censored at the gap.
#'
#' A record that simply ends carries no internal gap. `s5_prepare_outcome()`
#' already reports that case as `weeks_to_loss`, read from the panel itself.
#'
#' @section Runs are read over the observed weeks only:
#'
#' A run is a set of discordant weeks with consecutive week indices. An absent
#' week therefore breaks a run, and the gap boundary always falls at or before
#' the boundary the unbroken run would give.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `entry_band_id`, `baseline_tx` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_bands Integer, the number of follow-up bands the panel
#'   holds. A boundary past the last band reads `NA`.
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
  n_follow_up_bands
) {
  dv_pid <- dv_week <- dv_next <- dv_run <- dv_start <- NULL # nolint
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
  span <- as.integer(n_follow_up_bands) * period_width

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
  # The landmark of a person-band is the first week of the band after it.
  lm_week <- (as.integer(entry_dt[["entry_band_id"]]) + 1L) * period_width
  arm <- .tte_is_true(entry_dt[["baseline_tx"]])
  n_pt <- length(lm_week)
  stop_week <- rep(NA_integer_, n_pt)

  # --- internal observation gaps -------------------------------------------
  # `dv_next` is the next OBSERVED week of the same person. A gap opens at
  # `dv_week + 1` when that next week is further away than one week. The last
  # week of a record has no next week, so it opens no gap here.
  w[, dv_next := data.table::shift(dv_week, type = "lead"), by = dv_pid]
  gaps <- w[
    !is.na(dv_next) & dv_next > dv_week + 1L,
    list(dv_pid, dv_week = dv_week + 1L)
  ]
  w[, dv_next := NULL]
  # A duplicated person-week would otherwise duplicate a gap, and a duplicate
  # in the joined table below would return two rows for one person-trial.
  gaps <- unique(gaps, by = c("dv_pid", "dv_week"))
  if (nrow(gaps) > 0L) {
    data.table::setkeyv(gaps, c("dv_pid", "dv_week"))
    gaps[, dv_hit := dv_week]
    q <- data.table::data.table(dv_pid = entry_dt[[".tte_person_id"]])
    q[, q_week := lm_week]
    stop_week <- gaps[
      q,
      on = c("dv_pid", dv_week = "q_week"),
      roll = -Inf,
      dv_hit
    ]
  }

  # --- discordant runs, one arm at a time ----------------------------------
  # A person can be an initiator in one band and a comparator in another, so
  # the runs are read per arm and not per person.
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
  out[]
}


#' Place the record-end boundary of every enrolled person-trial.
#'
#' The boundary is the week the weekly record stops at, counted from the
#' landmark. It is exact to the week, and it comes from the weekly sequence. It
#' is an exclusive stop. See the interval convention section of [TTEDesign].
#'
#' A record that simply ends carries no internal gap, so
#' `.tte_deviation_boundary()` never reports it. `s5_prepare_outcome()` reports
#' it as `weeks_to_loss`, and reads `.max_tstop` for the value. `.max_tstop` is
#' the stop of the LAST BAND, so a record that ends inside a band overshoots by
#' up to `period_width - 1` weeks. This function reads the exact week instead.
#'
#' A record that reaches the end of the panel returns `NA`. Nothing is left for
#' it to stop, and the person completed the follow-up the panel holds.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `entry_band_id` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col` and `isoyearweek`.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_bands Integer, the number of follow-up bands the panel
#'   holds.
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
  n_follow_up_bands
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
  span <- as.integer(n_follow_up_bands) * period_width

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

  lm_week <- (as.integer(entry_dt[["entry_band_id"]]) + 1L) * period_width
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
  out[]
}


#' Place the outcome boundary of every enrolled person-trial.
#'
#' The boundary is the week the outcome falls in, counted from the landmark. It
#' is exact to the week, and it comes from the weekly sequence. It is an
#' exclusive stop. See the interval convention section of [TTEDesign].
#'
#' The band collapse keeps one outcome flag per band. After it the week is
#' gone, and the only boundary left to read is the stop of the band. That
#' overshoots by up to `period_width - 1` weeks. It also disagrees with
#' `weeks_to_record_end` and `weeks_to_protocol_deviation`, which are exact.
#' The disagreement changes the winner. A woman whose record ends in week 10,
#' and whose outcome falls in week 10, loses her event to the record end.
#'
#' The active outcome is chosen later, in `s5_prepare_outcome()`, so this
#' returns one column per outcome the design names.
#'
#' An outcome week before the landmark is not a follow-up event and never
#' becomes the boundary. A boundary past the last band of the panel reads `NA`.
#'
#' @param entry_dt One row per enrolled person-trial. It MUST carry
#'   `.tte_person_id`, `entry_band_id` and `id_var`.
#' @param data_enrolled The person-week rows of the enrolled persons. It MUST
#'   carry `person_id_col`, `isoyearweek` and the outcome columns.
#' @param design A [TTEDesign].
#' @param person_id_col Character, the person identifier column of
#'   `data_enrolled`.
#' @param id_var Character, the person-trial identifier column.
#' @param n_follow_up_bands Integer, the number of follow-up bands the panel
#'   holds.
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
  n_follow_up_bands
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
  span <- as.integer(n_follow_up_bands) * period_width

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
  lm_week <- (as.integer(entry_dt[["entry_band_id"]]) + 1L) * period_width
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
  out[]
}
