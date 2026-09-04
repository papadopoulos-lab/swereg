# `make_rowind_first_occurrence()` and the six `skeleton_eligible_*()`
# functions write their new column into the caller's table with `:=`, and
# their help pages call that change by reference. The claim holds only while
# the table keeps one spare column slot per new column. Past the last spare
# slot data.table allocates a longer column list. A longer list is a new R
# object, so the caller keeps the old one and never sees the new column.
# data.table 1.18.4 reports nothing.
#
# Each function here adds one column, so it needs zero spare slots to fail.
# Two of them hold two columns at once, so they need one spare slot to fail:
# `make_rowind_first_occurrence()` holds `temp` beside the new column, and
# `skeleton_eligible_no_observation_in_window_excluding_wk0()` holds a
# `.temp_obs_*` column beside it. At one spare slot the caller of
# `make_rowind_first_occurrence()` keeps `temp` and never gets the new column.

skip_if_not_installed("data.table")

# A data.table with exactly `spare` free column slots. Serialization drops
# data.table's over-allocation, which is what a qs2 file on disk does to a
# table, and `setalloccol()` then sets the exact number back. `setalloccol()`
# never shrinks, so it cannot produce this on its own.
.shr <- function(dt, spare) {
  dt <- unserialize(serialize(dt, NULL))
  return(data.table::setalloccol(dt, spare))
}

# A three-person skeleton over one quarter. `ev` is TRUE for person 1 from
# ISO week 2021-05 onward, and FALSE everywhere else, so every eligibility
# rule below separates person 1 from persons 2 and 3.
.shr_skeleton <- function(spare = 0L) {
  sk <- swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31")
  sk[, ev := id == 1L & isoyearweek >= "2021-05"]
  return(.shr(sk, spare))
}

# The same skeleton with the slots to spare. Every value assertion below
# compares the out-of-slots run against this run, so the growth path and the
# in-place path must agree.
.shr_reference <- function(fn, ...) {
  sk <- .shr_skeleton(200L)
  fn(sk, ...)
  return(sk)
}

test_that("skeleton_eligible_isoyears reaches the caller out of slots", {
  sk <- .shr_skeleton(0L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  swereg::skeleton_eligible_isoyears(sk, 2021)

  expect_true("eligible_isoyears" %in% names(sk))
  ref <- .shr_reference(swereg::skeleton_eligible_isoyears, 2021)
  expect_identical(sk$eligible_isoyears, ref$eligible_isoyears)
  expect_identical(sk$eligible_isoyears, sk$isoyear == 2021)
  expect_true(any(sk$eligible_isoyears))
  expect_false(all(sk$eligible_isoyears))
})

test_that("skeleton_eligible_age_range reaches the caller out of slots", {
  sk <- .shr_skeleton(0L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  swereg::skeleton_eligible_age_range(sk, "id", min_age = 2, max_age = 3)

  expect_true("eligible_age" %in% names(sk))
  ref <- .shr_reference(
    swereg::skeleton_eligible_age_range,
    "id",
    min_age = 2,
    max_age = 3
  )
  expect_identical(sk$eligible_age, ref$eligible_age)
  expect_false(any(sk[id == 1L]$eligible_age))
  expect_true(all(sk[id == 2L]$eligible_age))
})

test_that("skeleton_eligible_no_events_in_window reaches the caller", {
  sk <- .shr_skeleton(0L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  swereg::skeleton_eligible_no_events_in_window_excluding_wk0(sk, "ev", window = 4)

  expect_true("eligible_no_ev_4wk" %in% names(sk))
  ref <- .shr_reference(
    swereg::skeleton_eligible_no_events_in_window_excluding_wk0,
    "ev",
    window = 4
  )
  expect_identical(sk$eligible_no_ev_4wk, ref$eligible_no_ev_4wk)
  expect_true(all(sk[id == 2L]$eligible_no_ev_4wk))
  expect_false(all(sk[id == 1L]$eligible_no_ev_4wk))
})

test_that("skeleton_eligible_no_observation holds two columns at once", {
  # This one writes `.temp_obs_ev`, then the eligibility column, then deletes
  # `.temp_obs_ev`. Both are on the table at once, so it needs two slots.
  sk <- .shr_skeleton(1L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 1L)

  swereg::skeleton_eligible_no_observation_in_window_excluding_wk0(sk, "ev", TRUE)

  expect_true("eligible_no_ev_ever" %in% names(sk))
  expect_false(".temp_obs_ev" %in% names(sk))
  ref <- .shr_reference(
    swereg::skeleton_eligible_no_observation_in_window_excluding_wk0,
    "ev",
    TRUE
  )
  expect_identical(sk$eligible_no_ev_ever, ref$eligible_no_ev_ever)
  expect_true(all(sk[id == 2L]$eligible_no_ev_ever))
})

test_that("skeleton_eligible_no_events_lifetime reaches the caller", {
  sk <- .shr_skeleton(0L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  swereg::skeleton_eligible_no_events_lifetime_before_and_after_baseline(sk, "ev")

  col <- "eligible_no_ev_lifetime_before_and_after_baseline"
  expect_true(col %in% names(sk))
  ref <- .shr_reference(
    swereg::skeleton_eligible_no_events_lifetime_before_and_after_baseline,
    "ev"
  )
  expect_identical(sk[[col]], ref[[col]])
  expect_false(any(sk[id == 1L][[col]]))
  expect_true(all(sk[id == 2L][[col]]))
})

test_that("skeleton_eligible_combine reaches the caller out of slots", {
  sk <- .shr_skeleton(1L)
  sk[, other := id <= 2L]
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  swereg::skeleton_eligible_combine(sk, c("ev", "other"))

  expect_true("eligible" %in% names(sk))
  expect_identical(sk$eligible, sk$ev & sk$other)
  expect_true(any(sk[id == 1L]$eligible))
  expect_false(any(sk[id == 3L]$eligible))
})

test_that("make_rowind_first_occurrence reaches the caller out of slots", {
  # `temp` and the new column are on the table at once, so one spare slot is
  # not enough. Before the repair the caller kept `temp` and never got
  # `ri_first`, which then collided with the next call.
  sk <- .shr_skeleton(1L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 1L)

  swereg::make_rowind_first_occurrence(sk, "ev == TRUE", "isoyearweek", "ri_first")

  expect_true("ri_first" %in% names(sk))
  expect_false("temp" %in% names(sk))
  got <- sk[, .(v = unique(ri_first)), keyby = id]
  expect_identical(got$v, c("2021-05", NA_character_, NA_character_))
})

test_that("make_rowind_first_occurrence reaches the caller with no slots", {
  sk <- .shr_skeleton(0L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  swereg::make_rowind_first_occurrence(sk, "ev == TRUE", "isoyearweek", "ri_first")

  expect_true("ri_first" %in% names(sk))
  expect_false("temp" %in% names(sk))
})

test_that("skeleton_eligible_isoyears mutates in place when it has the slots", {
  sk <- .shr_skeleton(200L)
  before <- data.table::address(sk)

  swereg::skeleton_eligible_isoyears(sk, 2021)

  expect_identical(data.table::address(sk), before)
  expect_true("eligible_isoyears" %in% names(sk))
})

test_that("the growth shares the column vectors", {
  # R cannot grow a list in place, so the caller's binding must hold a new
  # object after the growth. The column vectors move with it by reference,
  # which is what makes the growth cheap.
  sk <- .shr_skeleton(0L)
  before_table <- data.table::address(sk)
  before_id <- data.table::address(sk$id)

  swereg::skeleton_eligible_isoyears(sk, 2021)

  expect_identical(data.table::address(sk$id), before_id)
  expect_false(identical(data.table::address(sk), before_table))
  expect_identical(data.table::truelength(sk) - ncol(sk), 4096L)
})

test_that("skeleton_eligible_combine reaches a table in an R6 field", {
  sk <- swereg:::Skeleton$new(data = .shr_skeleton(0L), batch_number = 1L)

  swereg::skeleton_eligible_combine(sk$data, "ev")

  expect_true("eligible" %in% names(sk$data))
  expect_identical(sk$data$eligible, sk$data$ev)
})

test_that("make_rowind_first_occurrence warns when it cannot reach the caller", {
  # `identity(sk)` is a call, so there is no binding to write the grown table
  # back to. The columns reach the return value and nothing else.
  sk <- .shr_skeleton(0L)

  expect_warning(
    got <- swereg::make_rowind_first_occurrence(
      identity(sk),
      "ev == TRUE",
      "isoyearweek",
      "ri_first"
    ),
    "Free column slots"
  )
  expect_true("ri_first" %in% names(got))
  expect_false("ri_first" %in% names(sk))
})
