# `any_events_prior_to()` looks back over CALENDAR weeks, not rows.
#
# A skeleton row is either one ISO week ("2008-01") or one whole ISO year
# ("2004-**"). A window of N weeks before row i covers the N ISO weeks before
# the first week of row i. A prior row is inside the window when any week of it
# is. An annual row therefore counts whole when its year overlaps the window,
# because the week of an event inside that year is unknown.
#
# Every expected value below is derived by hand from the ISO calendar, not from
# the implementation. ISO years 2004 and 2009 have a week 53. 2005 and 2008 do
# not.

skip_if_not_installed("data.table")

aep <- function(x, n, iyw) {
  return(any_events_prior_to(x, window_excluding_wk0 = n, isoyearweek = iyw))
}

# --- the window counts weeks, not rows --------------------------------------

test_that("an event in a person's first rows is seen by the rows after it", {
  # The row-count version returned FALSE here until N rows existed.
  iyw <- c("2008-01", "2008-02", "2008-03", "2008-04", "2008-05")
  x <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  # 2008-04 looks at 2008-01..2008-03; 2008-05 looks at 2008-02..2008-04.
  expect_identical(aep(x, 3L, iyw), c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("the current week is never in its own window", {
  iyw <- c("2008-01", "2008-02", "2008-03")
  expect_identical(aep(c(FALSE, TRUE, FALSE), 52L, iyw), c(FALSE, FALSE, TRUE))
  expect_identical(aep(c(TRUE, TRUE, TRUE), 52L, iyw), c(FALSE, TRUE, TRUE))
})

test_that("a gap in the weekly rows does not stretch the window", {
  # 2008-10 looks at 2008-01..2008-09, which holds the event.
  # 2008-11 looks at 2008-02..2008-10, which does not.
  # A 9-row window would have seen the event from both.
  iyw <- c("2008-01", "2008-10", "2008-11")
  expect_identical(aep(c(TRUE, FALSE, FALSE), 9L, iyw), c(FALSE, TRUE, FALSE))
})

test_that("week 53 is a week", {
  # From 2010-02, five weeks back is 2009-50: 2010-01, 2009-53, 2009-52,
  # 2009-51, 2009-50. A 52-week-year assumption would put it four back.
  iyw <- c("2009-50", "2010-02")
  x <- c(TRUE, FALSE)
  expect_identical(aep(x, 5L, iyw), c(FALSE, TRUE))
  expect_identical(aep(x, 4L, iyw), c(FALSE, FALSE))
})

# --- annual rows --------------------------------------------------------------

test_that("an annual row counts whole when its year overlaps the window", {
  # From 2005-02: N = 1 looks at 2005-01 only; N = 2 adds 2004-53, a week of
  # 2004, so the 2004 annual row is inside.
  iyw <- c("2004-**", "2005-01", "2005-02")
  x <- c(TRUE, FALSE, FALSE)
  expect_identical(aep(x, 1L, iyw), c(FALSE, TRUE, FALSE))
  expect_identical(aep(x, 2L, iyw), c(FALSE, TRUE, TRUE))
})

test_that("an annual row counts as a whole year, not as one week", {
  # 2007-01 with N = 52 looks at 2006-01..2006-52. The 2004 annual row is
  # outside, although it sits only two rows back.
  iyw <- c("2004-**", "2005-**", "2007-01")
  expect_identical(aep(c(TRUE, FALSE, FALSE), 52L, iyw), c(FALSE, TRUE, FALSE))
})

test_that("an annual current row looks back from the first week of its year", {
  # "2004-**" with N = 1 looks at 2003-52, a week of 2003.
  iyw <- c("2003-**", "2004-**")
  expect_identical(aep(c(TRUE, FALSE), 1L, iyw), c(FALSE, TRUE))
})

test_that("an annual row and a weekly row of the same year are refused", {
  # create_skeleton() no longer builds this shape, so it can only come from a
  # skeleton built some other way. An annual row means a whole ISO year.
  expect_error(aep(c(TRUE, FALSE), 1L, c("1999-**", "1999-52")), "order")
  expect_error(aep(c(TRUE, FALSE), 1L, c("2004-**", "2004-53")), "order")
})

# --- lifetime window ------------------------------------------------------------

test_that("the lifetime window sees every earlier row, annual or weekly", {
  iyw <- c("1990-**", "2004-**", "2005-01", "2020-30")
  expect_identical(aep(c(TRUE, FALSE, FALSE, FALSE), 99999L, iyw), c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(aep(c(FALSE, FALSE, FALSE, FALSE), 99999L, iyw), c(FALSE, FALSE, FALSE, FALSE))
})

test_that("mixed annual and weekly rows at the production window lengths", {
  # From 2005-01 and 2005-52, 52 weeks back reaches 2004-53. From 2006-01,
  # 104 weeks back reaches 2004 (2005 has 52 weeks, 2004 has 53). From 2007-01,
  # 156 weeks back reaches 2004-02.
  iyw <- c("2004-**", "2005-01", "2005-52", "2006-**", "2007-01")
  x <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  expect_identical(aep(x, 52L, iyw), c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(aep(x, 104L, iyw), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(aep(x, 156L, iyw), c(FALSE, TRUE, TRUE, TRUE, TRUE))
})

test_that("across several annual rows, week 53 decides the boundary", {
  # From 2009-01: 2008, 2007, 2006 and 2005 hold 208 weeks; week 209 is 2004-53.
  iyw <- c("2004-**", "2005-**", "2006-**", "2007-**", "2008-**", "2009-01")
  x <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  expect_false(aep(x, 208L, iyw)[6])
  expect_true(aep(x, 209L, iyw)[6])
})

test_that("an annual row never counts its own event", {
  expect_identical(aep(TRUE, 1L, "2004-**"), FALSE)
  expect_identical(aep(TRUE, 99999L, "2004-**"), FALSE)
})

test_that("a double, a large or an infinite window is accepted", {
  iyw <- c("2004-01", "2008-01")
  x <- c(TRUE, FALSE)
  expect_identical(aep(x, 52, iyw), c(FALSE, FALSE))
  expect_identical(aep(x, Inf, iyw), c(FALSE, TRUE))
  expect_identical(aep(x, 100000, iyw), c(FALSE, TRUE))
  expect_identical(aep(x, 2147483648, iyw), c(FALSE, TRUE))
})

# --- missing values follow any() ----------------------------------------------

test_that("NA in the window gives NA unless an event is also in the window", {
  iyw <- c("2008-01", "2008-02", "2008-03", "2008-04", "2008-05", "2008-06")
  # N = 2: 2008-03 and 2008-04 hold the NA week in their window; 2008-05 and
  # 2008-06 do not.
  x <- c(FALSE, NA, FALSE, FALSE, FALSE, FALSE)
  expect_identical(aep(x, 2L, iyw), c(FALSE, FALSE, NA, NA, FALSE, FALSE))
  # An observed event decides the answer, as in any(c(TRUE, NA)).
  x <- c(TRUE, NA, FALSE, FALSE, FALSE, FALSE)
  expect_identical(aep(x, 2L, iyw), c(FALSE, TRUE, TRUE, NA, FALSE, FALSE))
})

test_that("in the lifetime window an NA stays until an event decides it", {
  iyw <- c("2008-01", "2008-02", "2008-03", "2008-04")
  expect_identical(aep(c(FALSE, NA, FALSE, FALSE), 99999L, iyw), c(FALSE, FALSE, NA, NA))
  expect_identical(aep(c(FALSE, NA, TRUE, FALSE), 99999L, iyw), c(FALSE, FALSE, NA, TRUE))
})

# --- refusals -------------------------------------------------------------------

test_that("isoyearweek is required", {
  expect_error(any_events_prior_to(c(TRUE, FALSE), window_excluding_wk0 = 52L), "isoyearweek")
})

test_that("rows out of calendar order are refused", {
  expect_error(aep(c(TRUE, FALSE), 52L, c("2008-02", "2008-01")), "order")
  # An annual row after a weekly row of the same year is out of order too.
  expect_error(aep(c(TRUE, FALSE), 52L, c("2005-01", "2005-**")), "order")
  # An annual row followed by week 01 of its own year would cover no week.
  expect_error(aep(c(TRUE, FALSE), 52L, c("2004-**", "2004-01")), "order")
})

test_that("a repeated week is refused", {
  expect_error(aep(c(TRUE, FALSE), 52L, c("2008-01", "2008-01")), "order")
  expect_error(aep(c(TRUE, FALSE), 52L, c("2004-**", "2004-**")), "order")
})

test_that("a week that does not exist is refused", {
  expect_error(aep(c(TRUE, FALSE), 52L, c("2008-01", "2008-53")), "2008-53")
  expect_error(aep(c(TRUE, FALSE), 52L, c("2008-01", "2008-xx")), "2008-xx")
})

test_that("a window that is not one whole non-negative number is refused", {
  iyw <- c("2008-01", "2008-02")
  for (w in list(-1L, 0.5, NA_integer_, NA_real_, "52", c(52L, 104L), integer(0))) {
    expect_error(aep(c(TRUE, FALSE), w, iyw), "window_excluding_wk0")
  }
})

test_that("a window of zero weeks is empty", {
  expect_identical(aep(c(TRUE, TRUE), 0L, c("2008-01", "2008-02")), c(FALSE, FALSE))
})

test_that("x and isoyearweek must have the same length", {
  expect_error(aep(c(TRUE, FALSE), 52L, "2008-01"), "length")
})

test_that("an empty input gives an empty result", {
  expect_identical(aep(logical(0), 52L, character(0)), logical(0))
})

# --- the callers pass the calendar --------------------------------------------

.cal_fixture <- function() {
  # Person 1 has a gap; person 2 starts with an annual row.
  return(data.table::data.table(
    id = c(1L, 1L, 1L, 2L, 2L, 2L),
    isoyearweek = c("2008-01", "2008-10", "2008-11", "2004-**", "2005-01", "2005-02"),
    ev = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    grp = c("a", "b", "b", "a", "b", "b")
  ))
}

test_that("skeleton_eligible_no_events_in_window_excluding_wk0 uses calendar weeks", {
  dt <- .cal_fixture()
  skeleton_eligible_no_events_in_window_excluding_wk0(dt, "ev", window = 9, col_name = "elig9")
  skeleton_eligible_no_events_in_window_excluding_wk0(dt, "ev", window = 1, col_name = "elig1")
  # Person 1 as in the gap test. Person 2: N = 9 from 2005-02 reaches 2004.
  expect_identical(dt$elig9, !c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(dt$elig1, !c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
})

test_that("skeleton_eligible_no_observation_in_window_excluding_wk0 uses calendar weeks", {
  dt <- .cal_fixture()
  skeleton_eligible_no_observation_in_window_excluding_wk0(dt, "grp", value = "a", window = 9, col_name = "elig9")
  expect_identical(dt$elig9, !c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("the batch evaluator uses calendar weeks for every windowed type", {
  dt <- .cal_fixture()
  specs <- list(
    list(col_name = "w", type = "windowed", source_var = "ev", window_weeks = 9L),
    list(col_name = "w_neg", type = "windowed", source_var = "ev", window_weeks = 9L, negate_final = TRUE),
    list(col_name = "no_obs", type = "windowed_no_obs", source_var = "grp", value = "a", window_weeks = 9L),
    list(col_name = "only_obs", type = "windowed_only_obs", source_var = "grp", value = "b", window_weeks = 9L)
  )
  dt <- swereg:::.tte_apply_eligibility_batch(dt, specs, id_col = "id")
  seen <- c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
  expect_identical(dt$w, !seen)
  expect_identical(dt$w_neg, seen)
  expect_identical(dt$no_obs, !seen)
  # "only b before baseline": the "a" rows are the violations.
  expect_identical(dt$only_obs, !seen)
})

test_that("each person's events stay with that person", {
  # Person 2 has no event; person 1's event MUST NOT reach person 2's rows.
  dt <- data.table::data.table(
    id = c(1L, 1L, 2L, 2L, 2L),
    isoyearweek = c("2008-01", "2008-02", "2008-01", "2008-10", "2008-11"),
    ev = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  skeleton_eligible_no_events_in_window_excluding_wk0(dt, "ev", window = 9, col_name = "elig")
  expect_identical(dt$elig, c(TRUE, FALSE, TRUE, TRUE, TRUE))
  dt2 <- swereg:::.tte_apply_eligibility_batch(
    data.table::copy(dt[, .(id, isoyearweek, ev)]),
    list(list(col_name = "w", type = "windowed", source_var = "ev", window_weeks = 9L)),
    id_col = "id"
  )
  expect_identical(dt2$w, c(TRUE, FALSE, TRUE, TRUE, TRUE))
})

test_that("the helper treats a window too large for an integer as lifetime", {
  dt <- data.table::data.table(id = c(1L, 1L), isoyearweek = c("2004-01", "2008-01"), ev = c(TRUE, FALSE))
  skeleton_eligible_no_events_in_window_excluding_wk0(dt, "ev", window = 2147483648, col_name = "elig")
  expect_identical(dt$elig, c(TRUE, FALSE))
})

test_that("negate_final keeps NA as NA", {
  # N = 2: 2008-02 sees only the NA week; 2008-03 and 2008-04 see the event.
  dt <- data.table::data.table(
    id = 1L, isoyearweek = c("2008-01", "2008-02", "2008-03", "2008-04"),
    ev = c(NA, TRUE, FALSE, FALSE)
  )
  specs <- list(
    list(col_name = "pos", type = "windowed", source_var = "ev", window_weeks = 2L, negate_final = TRUE),
    list(col_name = "neg", type = "windowed", source_var = "ev", window_weeks = 2L)
  )
  dt <- swereg:::.tte_apply_eligibility_batch(dt, specs, id_col = "id")
  expect_identical(dt$pos, c(FALSE, NA, TRUE, TRUE))
  expect_identical(dt$neg, c(TRUE, NA, FALSE, FALSE))
})

test_that("the helpers refuse a table without isoyearweek", {
  dt <- data.table::data.table(id = c(1L, 1L), ev = c(TRUE, FALSE), grp = c("a", "b"))
  expect_error(skeleton_eligible_no_events_in_window_excluding_wk0(dt, "ev", window = 9), "isoyearweek")
  expect_error(skeleton_eligible_no_observation_in_window_excluding_wk0(dt, "grp", value = "a", window = 9), "isoyearweek")
})

test_that("the helper accepts the skeleton create_skeleton() builds", {
  dt <- create_skeleton(ids = 1L, isoyear_min = 2000L, isoyearweek_max = "2000-09")
  dt[, ev := isoyearweek == "1999-**"]
  skeleton_eligible_no_events_in_window_excluding_wk0(dt, "ev", window = 1, col_name = "elig")
  # 2000-01 looks at 1999-52, a week of the 1999 annual row; 2000-02 looks at 2000-01.
  expect_false(dt[isoyearweek == "2000-01", elig])
  expect_true(dt[isoyearweek == "2000-02", elig])
})
