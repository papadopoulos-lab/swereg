# A subgroup level is the value at the recruiting week, which the snapshot
# column `.tte_entry__rd_sg` holds. The current-week column `rd_sg` can
# change during follow-up. In this panel, 10 person-trials per arm enter at
# level "a", and their `rd_sg` then reads "c". No person-trial holds "c" at
# baseline. So level "c" MUST NOT appear, and their events count in "a".

# One row per 4-week interval to week 104. A person-trial stops at its first
# event. The shape follows `.hl_panel()` in helper-horizon-subgroup.R.
.sb_panel <- function(seed = 23L) {
  set.seed(seed)
  n_int <- 104L %/% 4L
  k <- 0L
  one <- function(arm, entry, ev, switch_at = NA_integer_) {
    k <<- k + 1L
    id <- sprintf("p%04d", k)
    keep <- if (any(ev == 1L)) seq_len(which(ev == 1L)[1L]) else seq_len(n_int)
    tstart <- (keep - 1L) * 4L
    sg <- rep(entry, length(keep))
    if (!is.na(switch_at)) {
      sg[tstart >= switch_at] <- "c"
    }
    age <- 50 + k %% 10L
    return(data.table::data.table(
      id = id,
      enrollment_person_trial_id = paste0(id, "_t1"),
      rd_intervention = arm,
      tstart = tstart,
      tstop = keep * 4L,
      event = ev[keep],
      person_weeks = 4,
      rd_age_continuous = age,
      rd_sg = sg,
      .tte_entry__rd_sg = entry,
      .tte_entry__rd_age_continuous = age
    ))
  }
  rows <- list()
  for (arm in c(TRUE, FALSE)) {
    # 20 person-trials at "a" on every row. Random events.
    for (j in seq_len(20L)) {
      rows[[length(rows) + 1L]] <- one(arm, "a", stats::rbinom(n_int, 1L, 0.02))
    }
    # 10 person-trials that enter at "a" and read "c" later. Each has one
    # event in the interval with tstart 12, which is the 4th interval. Five
    # switch from tstart 0 and five from tstart 8, after time zero.
    for (j in seq_len(10L)) {
      ev <- integer(n_int)
      ev[4L] <- 1L
      rows[[length(rows) + 1L]] <- one(
        arm,
        "a",
        ev,
        switch_at = if (j <= 5L) 0L else 8L
      )
    }
    # 20 person-trials at "b" on every row. Random events.
    for (j in seq_len(20L)) {
      rows[[length(rows) + 1L]] <- one(arm, "b", stats::rbinom(n_int, 1L, 0.02))
    }
  }
  d <- data.table::rbindlist(rows)
  for (w in c("analysis_weight_pp_trunc", "analysis_weight_pp", "ipw")) {
    data.table::set(d, j = w, value = 1)
  }
  data.table::set(d, j = "ipw_trunc", value = 1)
  return(d)
}

# The TTEEnrollment of C and D, built as `.hl_build()` builds its fixture.
.sb_enrollment <- function(d) {
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 104L
  )
  return(swereg::TTEEnrollment$new(d, design, data_level = "trial"))
}

# The events of the person-trials that enter at "a". All weights are 1, so
# the weighted sum equals the raw count.
.sb_entry_a_events <- function(d) {
  return(sum(
    d$event[d$.tte_entry__rd_sg == "a"] *
      d$analysis_weight_pp_trunc[
        d$.tte_entry__rd_sg == "a"
      ]
  ))
}

test_that("A: the horizon records hold the baseline levels and their events", {
  d <- .sb_panel()
  rec <- swereg:::.tte_horizon_subgroup_records(
    d,
    "rd_sg",
    "analysis_weight_pp_trunc",
    104,
    list(min_expected_events = 5, exempt = FALSE),
    "rd_intervention",
    "tstart",
    "tstop"
  )
  expect_identical(names(rec), c("a", "b"))
  # The windows of a horizon record do not overlap, so their sum is the
  # event total of the level.
  expect_equal(
    sum(rec[["a"]]$windows$events_weighted),
    .sb_entry_a_events(d)
  )
})

test_that("B: the baseline counts hold levels a and b with 30 and 20 per arm", {
  d <- .sb_panel()
  counts <- swereg:::.tte_subgroup_baseline_counts(
    d[tstart == 0],
    "rd_sg",
    "rd_intervention"
  )
  expect_identical(counts$level, c("a", "b"))
  expect_identical(counts$n_intervention, c(30L, 20L))
  expect_identical(counts$n_comparator, c(30L, 20L))
})

test_that("C: irr_by_subgroup reports no level c and counts its events in a", {
  skip_if_not_installed("survey")
  d <- .sb_panel()
  enr <- .sb_enrollment(d)
  res <- suppressWarnings(
    enr$irr_by_subgroup("analysis_weight_pp_trunc", "rd_sg")
  )
  expect_false("c" %in% res$level)
  row_a <- res[res$level == "a"]
  expect_equal(
    row_a$events_intervention + row_a$events_comparator,
    .sb_entry_a_events(d)
  )
})

test_that("D: the effect-modification test sees two levels", {
  skip_if_not_installed("survey")
  d <- .sb_panel()
  enr <- .sb_enrollment(d)
  res <- suppressWarnings(
    enr$effect_modification_test("analysis_weight_pp_trunc", "rd_sg")
  )
  expect_identical(res$n_levels, 2L)
})
