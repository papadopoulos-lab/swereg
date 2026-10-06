# Follow-up stops at the first absent week after time zero, under both
# estimands.
#
# An absent week is a week with no row under the `row_presence` sentinel. The
# person is not under observation in it, so an outcome there is never
# recorded. ITT used to continue through the gap to the end of the record. PP
# stopped at the gap, but its event-priority rule counted an outcome later in
# the same follow-up interval. A gap that covers a whole follow-up interval
# also leaves that interval without a panel row, and `enroll()` numbers the
# rows by position. Every later row then opened one interval too early.
#
# This file pins five properties.
#
# 1. ITT stops at the first absent week, and the stop row is censored.
# 2. PP stops at the first absent week, and an outcome after the gap is never
#    counted, even in the same follow-up interval.
# 3. A treatment deviation never stops ITT. Under PP, an outcome in the same
#    follow-up interval as a deviation is still counted.
# 4. A panel without `weeks_to_observation_gap` warns once, and is read as
#    having no gap. A read of such a panel warns on read, and s4 does not
#    warn again.
# 5. Without a gap, ITT and PP output is unchanged. The fingerprints were
#    measured on swereg before this change.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

.ig_pw <- 4L
.ig_n_fu <- 12L

# Sixteen consecutive ISO year-weeks, starting on a follow-up interval boundary.
# Under `period_width = 4` they make one enrollment period and three follow-up
# intervals.
.ig_weeks <- function(n_weeks = 16L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .ig_pw == 0L
  ]$idx[1]
  wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)]
}

# One person, one row per week. The three vector arguments name 1-indexed
# FOLLOW-UP weeks. `absent_fu` deletes the row, which is a gap under the
# `row_presence` sentinel.
.ig_person <- function(
  id,
  weeks,
  arm,
  discordant_fu = integer(0),
  event_fu = integer(0),
  absent_fu = integer(0)
) {
  n <- length(weeks)
  fu <- seq_len(n) - .ig_pw
  on_tx <- rep(arm, n)
  on_tx[fu %in% discordant_fu] <- !arm
  d <- data.table::data.table(
    id = id,
    isoyearweek = weeks,
    exposed = rep(arm, n),
    eligible = seq_len(n) <= .ig_pw,
    died = fu %in% event_fu,
    on_tx = on_tx,
    age = 50 + seq_len(n)
  )
  d[!(fu %in% absent_fu)]
}

# Fillers for the propensity model. FI1 has an outcome, and FC1 and FC2
# deviate, so the fingerprints cover an event row and a deviation row. A ratio
# of 2 draws every comparator, so no test depends on the seeded draw.
.ig_fillers <- function(weeks) {
  data.table::rbindlist(c(
    lapply(1:8, function(i) {
      .ig_person(
        paste0("FI", i),
        weeks,
        arm = TRUE,
        event_fu = if (i == 1L) 6L else integer(0)
      )
    }),
    lapply(1:12, function(i) {
      .ig_person(
        paste0("FC", i),
        weeks,
        arm = FALSE,
        discordant_fu = if (i <= 2L) 3L:9L else integer(0)
      )
    })
  ))
}

.ig_design <- function(time_treatment_var = "on_tx") {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = time_treatment_var,
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = .ig_n_fu,
    period_width = .ig_pw,
    intervention_tolerance_weeks = 0L,
    comparator_tolerance_weeks = 3L
  )
}

# The fixtures are small, so the models separate and warn. The warnings are
# about the toy data and not about the boundary.
.ig_run <- function(d, estimand, design = .ig_design()) {
  trial <- TTEEnrollment$new(
    data = data.table::copy(d),
    design = design,
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died",
      follow_up = .ig_n_fu,
      estimand = estimand,
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  trial$data
}

.ig_rows <- function(d, who) {
  d[id == who][order(tstart)]
}

# WHOLEGAP has no rows in follow-up weeks 5 to 8, which is the whole of
# follow-up interval 2. Her outcome is in follow-up week 10, after the gap.
# PARTGAP has no row in follow-up week 6 only, and her outcome is in follow-up
# week 7. Both weeks fall in follow-up interval 2, which runs from week 4 to
# week 8. SWGAP deviates in follow-up week 2, and has no row in follow-up
# week 9. COLLIDE has no gap. She deviates in follow-up week 6 and has the
# outcome in follow-up week 7, both in follow-up interval 2.
.ig_gap_data <- function() {
  weeks <- .ig_weeks()
  data.table::rbindlist(list(
    .ig_person("WHOLEGAP", weeks, TRUE, absent_fu = 5L:8L, event_fu = 10L),
    .ig_person("PARTGAP", weeks, TRUE, absent_fu = 6L, event_fu = 7L),
    .ig_person("SWGAP", weeks, TRUE, discordant_fu = 2L, absent_fu = 9L),
    .ig_person("COLLIDE", weeks, TRUE, discordant_fu = 6L, event_fu = 7L),
    .ig_fillers(weeks)
  ))
}

# The rows `s4_prepare_for_analysis()` returns, as one text line per row.
.ig_fingerprint <- function(d) {
  cols <- c(
    "id",
    "trial_id",
    "tstart",
    "tstop",
    "person_weeks",
    "event",
    "censor_this_period",
    "weeks_to_event",
    "weeks_to_protocol_deviation",
    "weeks_to_loss"
  )
  k <- d[order(id, tstart), cols, with = FALSE]
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(do.call(paste, c(as.list(k), sep = "|")), f)
  unname(tools::md5sum(f))
}


# ---------------------------------------------------------------------------
# PROOF 1
# ---------------------------------------------------------------------------

test_that("ITT stops at the first absent week and censors the stop row", {
  out <- .ig_run(.ig_gap_data(), "itt")

  # The gap opens at follow-up week 5, so the boundary is week 4. The outcome
  # in week 10 is not counted, and the renumbered row after the gap is gone.
  got <- .ig_rows(out, "WHOLEGAP")
  expect_identical(unique(got$weeks_to_observation_gap), 4L)
  expect_identical(got$tstop, 4L)
  expect_identical(got$event, 0L)
  expect_identical(got$censor_this_period, 1L)
  expect_identical(unique(got$weeks_to_loss), 4L)
  expect_identical(unique(got$weeks_to_protocol_deviation), NA_integer_)

  # A gap inside a follow-up interval clips that interval at the gap. The
  # outcome after the gap in the same interval is not counted.
  got <- .ig_rows(out, "PARTGAP")
  expect_identical(got$tstop, c(4L, 5L))
  expect_identical(got$person_weeks, c(4L, 1L))
  expect_identical(got$event, c(0L, 0L))
  expect_identical(got$censor_this_period, c(0L, 1L))
  expect_identical(unique(got$weeks_to_loss), 5L)

  # The deviation in week 2 does not stop ITT. The gap at week 8 does.
  got <- .ig_rows(out, "SWGAP")
  expect_identical(got$tstop, c(4L, 8L))
  expect_identical(got$censor_this_period, c(0L, 1L))
  expect_identical(unique(got$weeks_to_loss), 8L)
})

test_that("ITT reads the gap without a time-varying treatment variable", {
  out <- .ig_run(
    .ig_gap_data(),
    "itt",
    design = .ig_design(time_treatment_var = NULL)
  )
  got <- .ig_rows(out, "WHOLEGAP")
  expect_identical(got$tstop, 4L)
  expect_identical(got$event, 0L)
  expect_identical(got$censor_this_period, 1L)
})


# ---------------------------------------------------------------------------
# PROOF 2
# ---------------------------------------------------------------------------

test_that("PP stops at the first absent week and never counts an outcome after it", {
  out <- .ig_run(.ig_gap_data(), "pp")

  # The gap is loss of observation, and not a deviation.
  got <- .ig_rows(out, "WHOLEGAP")
  expect_identical(got$tstop, 4L)
  expect_identical(got$event, 0L)
  expect_identical(got$censor_this_period, 1L)
  expect_identical(unique(got$weeks_to_loss), 4L)
  expect_identical(unique(got$weeks_to_protocol_deviation), NA_integer_)

  # The gap at week 5 and the outcome at week 7 share follow-up interval 2.
  # The event-priority rule does not apply to a gap, so she stops at week 5.
  got <- .ig_rows(out, "PARTGAP")
  expect_identical(got$tstop, c(4L, 5L))
  expect_identical(got$person_weeks, c(4L, 1L))
  expect_identical(got$event, c(0L, 0L))
  expect_identical(got$censor_this_period, c(0L, 1L))
  expect_identical(unique(got$weeks_to_loss), 5L)
})


# ---------------------------------------------------------------------------
# Supporting behaviour
# ---------------------------------------------------------------------------

test_that("a deviation keeps its PP event priority and never stops ITT", {
  pp <- .ig_run(.ig_gap_data(), "pp")

  # The deviation at week 2 comes before the gap at week 8, so PP stops there.
  got <- .ig_rows(pp, "SWGAP")
  expect_identical(got$tstop, 2L)
  expect_identical(got$censor_this_period, 1L)
  expect_identical(unique(got$weeks_to_protocol_deviation), 2L)

  # The outcome at week 7 shares follow-up interval 2 with the deviation at
  # week 6, so the outcome wins and is counted.
  got <- .ig_rows(pp, "COLLIDE")
  expect_identical(got$tstop, c(4L, 7L))
  expect_identical(got$event, c(0L, 1L))
  expect_identical(got$censor_this_period, c(0L, 0L))
  expect_identical(unique(got$weeks_to_protocol_deviation), 6L)

  # ITT ignores the deviation, and counts the outcome at week 7.
  itt <- .ig_run(.ig_gap_data(), "itt")
  got <- .ig_rows(itt, "COLLIDE")
  expect_identical(got$tstop, c(4L, 7L))
  expect_identical(got$event, c(0L, 1L))
  expect_identical(got$censor_this_period, c(0L, 0L))
})

# Runs s2 and s4 on `trial`, and returns every warning message they give.
.ig_warnings <- function(trial, estimand) {
  msgs <- character()
  withCallingHandlers(
    {
      trial$s2_ipw(stabilize = TRUE)
      trial$s4_prepare_for_analysis(
        outcome = "died",
        follow_up = .ig_n_fu,
        estimand = estimand,
        estimate_ipcw_pp_with_gam = FALSE
      )
    },
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  return(msgs)
}

.ig_legacy_text <- paste(
  "This enrollment was enrolled before swereg 26.15.0, so its panel has no",
  "`weeks_to_observation_gap` column. Gaps in observation cannot be detected",
  "in it, and an outcome after such a gap may be counted. Re-run s1",
  "(`$s1_generate_enrollments_and_ipw()`) to remove this limitation."
)

test_that("a panel without weeks_to_observation_gap warns once and keeps the old rule", {
  enrol <- function() {
    return(TTEEnrollment$new(
      data = data.table::copy(.ig_gap_data()),
      design = .ig_design(),
      ratio = 2,
      seed = 4,
      extra_cols = "isoyearweek"
    ))
  }

  # A legacy panel is a panel that `enroll()` built without the column.
  trial <- enrol()
  trial$data[, weeks_to_observation_gap := NULL]
  msgs <- .ig_warnings(trial, "itt")
  legacy <- msgs[grepl("26.15.0", msgs, fixed = TRUE)]
  expect_identical(legacy, .ig_legacy_text)

  # ITT then runs through the gap to the end of the panel, as before the
  # column existed.
  got <- .ig_rows(trial$data, "PARTGAP")
  expect_identical(got$tstop, c(4L, 7L))
  expect_identical(got$event, c(0L, 1L))

  # A panel that carries the column gives no such warning.
  msgs <- .ig_warnings(enrol(), "itt")
  expect_false(any(grepl("26.15.0", msgs, fixed = TRUE)))
})

test_that("a read legacy panel warns once on read and not again in s4", {
  trial <- TTEEnrollment$new(
    data = data.table::copy(.ig_gap_data()),
    design = .ig_design(),
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
  trial$data[, weeks_to_observation_gap := NULL]
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(trial, path)

  # The read gives the warning, and sets the private flag.
  msgs <- character()
  read <- withCallingHandlers(qs2_read(path), warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_identical(msgs, .ig_legacy_text)
  expect_true(read$.__enclos_env__$private$.legacy_gap_warned)

  # s4 on the same object does not warn again.
  msgs <- .ig_warnings(read, "itt")
  expect_false(any(grepl("26.15.0", msgs, fixed = TRUE)))
})

test_that("without a gap, ITT and PP output is unchanged", {
  weeks <- .ig_weeks()
  d <- data.table::rbindlist(list(
    .ig_person("SWITCH", weeks, TRUE, discordant_fu = 2L),
    .ig_person("EVENT", weeks, TRUE, event_fu = 10L),
    .ig_person("TAILCUT", weeks, TRUE, absent_fu = 11L:12L),
    .ig_fillers(weeks)
  ))

  itt <- .ig_run(d, "itt")
  expect_identical(unique(itt$weeks_to_observation_gap), NA_integer_)
  expect_identical(nrow(itt), 68L)
  expect_identical(.ig_fingerprint(itt), "56a5137b869d3b45b0fd0dfac38c6375")

  pp <- .ig_run(d, "pp")
  expect_identical(nrow(pp), 64L)
  expect_identical(.ig_fingerprint(pp), "c070fe93072bf58afc74fb681c9b6ea7")
})
