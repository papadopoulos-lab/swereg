# The fixtures of `test-adherence.R`. Each builds a source panel: a
# trial-level enrollment that carries the three boundary columns
# `.tte_adherence_counts()` reads, `isoyearweek` and a design. The panel
# holds no outcome, as the raw file `.s1e_worker()` writes holds none.

ADH_PW <- 4L

# The design of every fixture panel. `admin` is the value of
# `admin_censor_isoyearweek`, or NULL.
.adh_design <- function(admin = NULL, follow_up = 24L) {
  return(swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "trial_id",
    treatment_var = "tx",
    time_treatment_var = "on_tx",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = follow_up,
    period_width = ADH_PW,
    admin_censor_isoyearweek = admin
  ))
}

# The ISO year-week `k` weeks after `start`.
.adh_week_after <- function(start, k) {
  wk <- cstime::dates_by_isoyearweek$isoyearweek
  return(wk[match(start, wk) + k])
}

# One row per person-trial in, one row per follow-up interval out.
#
# `trials` holds `id`, `trial_id`, `tx`, `d`, `r`, `g` and `baseline`, the
# ISO year-week of time zero. `stop` is the stop of the last row of each
# person-trial; NA gives every person-trial rows up to `last_week`.
.adh_panel <- function(trials, last_week = 24L) {
  rows <- lapply(seq_len(nrow(trials)), function(i) {
    tr <- trials[i]
    stop_i <- if ("stop" %in% names(tr) && !is.na(tr$stop)) {
      tr$stop
    } else {
      last_week
    }
    tstart <- seq(0L, stop_i - ADH_PW, by = ADH_PW)
    return(data.table::data.table(
      id = tr$id,
      trial_id = tr$trial_id,
      tstart = tstart,
      tstop = tstart + ADH_PW,
      tx = tr$tx,
      on_tx = tr$tx,
      age = 40 + i,
      death = 0L,
      isoyearweek = .adh_week_after(tr$baseline, tstart),
      weeks_to_protocol_deviation = as.integer(tr$d),
      weeks_to_record_end = as.integer(tr$r),
      weeks_to_observation_gap = as.integer(tr$g)
    ))
  })
  return(data.table::rbindlist(rows))
}

# The source-panel enrollment of `panel` under `design`.
.adh_enrollment <- function(panel, design) {
  return(swereg::TTEEnrollment$new(
    data.table::copy(panel),
    design,
    data_level = "trial"
  ))
}

# The person-trials of the hand-built panel of test 1. Seven per arm, and one
# with an NA treatment. The administrative end is 2020-30:
#
# - a = 30 for a trial that starts in 2020-01, so it falls after week 24.
# - a = 16 for a trial that starts in 2020-15.
.adh_test1_trials <- function() {
  na <- NA_integer_
  return(data.table::data.table(
    id = sprintf("p%02d", 1:15),
    trial_id = sprintf("p%02d.1", 1:15),
    tx = c(rep(TRUE, 7L), rep(FALSE, 7L), NA),
    # Intervention, in row order:
    # - I1: no boundary.
    # - I2: deviation at time zero.
    # - I3: deviation at 10.
    # - I4: record end 8, before deviation 12.
    # - I5: gap 14.
    # - I6: deviation and record end, both 6.
    # - I7: administrative end 16.
    # Comparator, in row order:
    # - C1: no boundary.
    # - C2: deviation at time zero.
    # - C3: deviation at 20.
    # - C4: record end 3, before deviation 5.
    # - C5: gap 1.
    # - C6: deviation and record end, both 22.
    # - C7: administrative end 16, before deviation 18.
    # The last row has an NA treatment and a deviation at 2.
    d = c(na, 0L, 10L, 12L, na, 6L, na, na, 0L, 20L, 5L, na, 22L, 18L, 2L),
    r = c(na, na, na, 8L, na, 6L, na, na, na, na, 3L, na, 22L, na, na),
    g = c(na, na, na, na, 14L, na, na, na, na, na, na, 1L, na, na, na),
    baseline = c(
      rep("2020-01", 6L),
      "2020-15",
      rep("2020-01", 6L),
      "2020-15",
      "2020-01"
    )
  ))
}

# The person-trials of the equality test 2. Person p01 holds two
# overlapping trials, p01.1 from 2020-01 and p01.2 from 2020-09. The
# administrative end is 2020-40:
#
# - a = 40 for a trial that starts in 2020-01.
# - a = 32 for a trial that starts in 2020-09.
# - a = 28 for a trial that starts in 2020-13.
#
# All three fall inside the 48 weeks of the panel. So the administrative end
# binds for each trial without an earlier record end or gap. The rows of a
# trial with a record end stop at the first interval stop at or after it, as
# `enroll()` builds them.
.adh_test2_trials <- function() {
  na <- NA_integer_
  return(data.table::data.table(
    id = c("p01", "p01", sprintf("p%02d", 2:14)),
    trial_id = c("p01.1", "p01.2", sprintf("p%02d.1", 2:14)),
    tx = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),
    d = c(na, 7L, 0L, 30L, 9L, na, 13L, na, 0L, 41L, na, 5L, 18L, 36L, 3L),
    r = c(na, 20L, na, na, 9L, 17L, na, na, na, 26L, na, 25L, na, na, na),
    g = c(na, na, na, 22L, na, na, 13L, na, na, na, 11L, na, na, 30L, 2L),
    baseline = c(
      "2020-01",
      "2020-09",
      "2020-01",
      "2020-13",
      "2020-01",
      "2020-01",
      "2020-09",
      "2020-01",
      "2020-01",
      "2020-09",
      "2020-13",
      "2020-01",
      "2020-13",
      "2020-09",
      "2020-01"
    ),
    stop = c(na, 20L, na, na, 12L, 20L, na, na, na, 28L, na, 28L, na, na, na)
  ))
}

# Save `enrollment` as the raw file of `plan` and as every analysis file
# `plan$ett` names, under `dir`. The worker reads the smallest analysis file,
# so one object serves them all.
.adh_write_files <- function(plan, dir, enrollment) {
  for (f in unique(plan$ett$file_raw)) {
    qs2::qs_save(enrollment, file.path(dir, f))
  }
  for (f in unique(plan$ett$file_analysis)) {
    qs2::qs_save(enrollment, file.path(dir, f))
  }
  return(invisible(dir))
}
