# Pin two add_rx() behaviours that the add_diagnoses family already had:
#
#   1. The "YYYY-**" annual-row remap. create_skeleton() builds an annual spine
#      ("<year>-**", is_isoyear == TRUE) for every year before the weekly
#      period. add_rx() now remaps BOTH interval endpoints that fall before the
#      first weekly row onto that annual spine, so a prescription starting
#      before the study period is no longer silently lost.
#
#   2. Duration arithmetic. foverlaps(type = "any") matches inclusively at both
#      endpoints, so the derived interval end is edatum + round(fddd) - 1.
#      Rows whose duration is missing, non-finite or not positive are dropped
#      BEFORE the ISO week conversion, with one warning.
#
# Both apply only on the derived path: a caller who supplies stop_date,
# start_isoyearweek or stop_isoyearweek keeps full control.

skip_if_not_installed("data.table")

# Weekly spine starts 2020-01-06 (isoyearweek "2020-02"); annual spine covers
# 1900:2019.
.rxa_skeleton <- function(ids = 1L) {
  swereg::create_skeleton(
    ids = ids,
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
}

.rxa_table <- function(...) {
  data.table::data.table(...)
}

# Collect warning messages without letting them fail the test.
.rxa_warnings <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(
    expr,
    warning = function(cond) {
      msgs <<- c(msgs, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  msgs
}

# ---- Repair 12: the annual-row remap -------------------------------------

test_that("add_rx: prescription wholly before the weekly spine marks the annual row", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2018-05-01"),
    atc = "N06AB10",
    fddd = 30
  )
  swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  expect_true(all(skel[is_isoyear == TRUE & isoyear == 2018L, rx_n06a]))
  expect_false(any(skel[is_isoyear == TRUE & isoyear != 2018L, rx_n06a]))
  expect_false(any(skel[is_isoyear == FALSE, rx_n06a]))
})

test_that("add_rx: multi-year pre-weekly prescription marks every intersected annual row", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2016-05-01"),
    atc = "N06AB10",
    fddd = 800 # into 2018
  )
  swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  expect_true(all(skel[is_isoyear == TRUE & isoyear %in% 2016:2018, rx_n06a]))
  expect_false(any(skel[is_isoyear == TRUE & isoyear == 2015L, rx_n06a]))
  expect_false(any(skel[is_isoyear == TRUE & isoyear == 2019L, rx_n06a]))
  expect_false(any(skel[is_isoyear == FALSE, rx_n06a]))
})

test_that("add_rx: prescription spanning the weekly boundary marks the annual row AND the weekly rows", {
  skel <- .rxa_skeleton(1L)
  # 2019-12-20 (isoyearweek 2019-51) + 60 days => 2020-02-17 (isoyearweek 2020-08)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2019-12-20"),
    atc = "N06AB10",
    fddd = 60
  )
  swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  # Pre-weekly portion collapses onto the annual rows it spans. create_skeleton()
  # builds the annual spine up to the ISO year of date_min - 1, which is 2020
  # here (2020-01-05 belongs to isoyear 2020), so the days 2019-12-20..2020-01-05
  # land on both the 2019 and the 2020 annual row.
  expect_true(all(skel[is_isoyear == TRUE & isoyear %in% c(2019L, 2020L), rx_n06a]))
  expect_false(any(skel[is_isoyear == TRUE & !isoyear %in% c(2019L, 2020L), rx_n06a]))

  # Weekly portion keeps weekly resolution: 2020-02 .. 2020-08 TRUE, rest FALSE.
  expect_true(all(
    skel[is_isoyear == FALSE & isoyearweek %in% sprintf("2020-%02d", 2:8), rx_n06a]
  ))
  expect_false(any(
    skel[is_isoyear == FALSE & !isoyearweek %in% sprintf("2020-%02d", 2:8), rx_n06a]
  ))
})

test_that("add_rx: prescription wholly inside the weekly spine marks no annual row", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-06-01"),
    atc = "N06AB10",
    fddd = 30
  )
  swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  expect_false(any(skel[is_isoyear == TRUE, rx_n06a]))
  expect_true(any(skel[is_isoyear == FALSE, rx_n06a]))
})

test_that("add_rx: skeleton with no weekly rows skips the remap", {
  skel <- .rxa_skeleton(1L)[is_isoyear == TRUE]
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2018-05-01"),
    atc = "N06AB10",
    fddd = 30
  )
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )

  expect_false(any(grepl("no non-missing arguments", msgs, fixed = TRUE)))
  expect_type(skel$rx_n06a, "logical")
  expect_false(any(skel$rx_n06a))
})

# ---- Repair 13: inclusive end and the pre-conversion duration filter -------

test_that("add_rx: a duration of N days covers N days, not N + 1", {
  skel <- .rxa_skeleton(1L)
  # 2020-03-02 is the Monday of isoyearweek 2020-10; 7 days ends 2020-03-08 (Sun).
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-02"),
    atc = "N06AB10",
    fddd = 7
  )
  swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  expect_true(all(skel[isoyearweek == "2020-10", rx_n06a]))
  expect_false(any(skel[isoyearweek == "2020-11", rx_n06a]))
  expect_equal(sum(skel$rx_n06a), 1L)
})

test_that("add_rx: fddd of zero is dropped before the ISO conversion, with a warning", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-02"),
    atc = "N06AB10",
    fddd = 0
  )
  expect_warning(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A")),
    regexp = "1 prescription rows dropped before ISO week conversion"
  )
  expect_false(any(skel$rx_n06a))
})

test_that("add_rx: negative fddd inside one ISO week is dropped before the ISO conversion", {
  skel <- .rxa_skeleton(1L)
  # fddd = -1 from a Wednesday stays inside isoyearweek 2020-10, so the
  # post-conversion inverted-interval filter cannot see it.
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-04"),
    atc = "N06AB10",
    fddd = -1
  )
  expect_warning(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A")),
    regexp = "1 prescription rows dropped before ISO week conversion"
  )
  expect_false(any(skel$rx_n06a))
})

test_that("add_rx: NA fddd is dropped by the pre-conversion duration filter", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = c(1L, 1L),
    edatum = as.Date(c("2020-03-02", "2020-06-01")),
    atc = c("N06AB10", "N06AB10"),
    fddd = c(NA_real_, 7)
  )
  expect_warning(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A")),
    regexp = "1 prescription rows dropped before ISO week conversion because fddd is missing, non-finite, or not positive"
  )
  # The surviving row is untouched by the filter.
  expect_true(any(skel$rx_n06a))
})

# ---- Caller-supplied columns keep control ---------------------------------

test_that("add_rx: caller-supplied stop_date is neither filtered nor shortened", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-02"),
    atc = "N06AB10",
    fddd = 0, # would be dropped on the derived path
    stop_date = as.Date("2020-03-09") # Monday of isoyearweek 2020-11
  )
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )

  expect_false(any(grepl("dropped before ISO week conversion", msgs, fixed = TRUE)))
  expect_true(all(skel[isoyearweek == "2020-10", rx_n06a]))
  expect_true(all(skel[isoyearweek == "2020-11", rx_n06a]))
})

test_that("add_rx: caller-supplied ISO week columns are not remapped", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2019-12-20"),
    atc = "N06AB10",
    fddd = 7,
    start_isoyearweek = "2019-51",
    stop_isoyearweek = "2019-52"
  )
  swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  # "2019-51"/"2019-52" match no skeleton row and MUST NOT be collapsed to
  # "2019-**": the caller asked for those exact weeks.
  expect_false(any(skel[is_isoyear == TRUE, rx_n06a]))
  expect_false(any(skel$rx_n06a))
})

# ---- The caller's lmed is read, never written ----------------------------
#
# add_rx() derives start_date, stop_date, start_isoyearweek and stop_isoyearweek
# on a local working copy. Writing them back made the cached ISO week values
# skeleton-dependent, and the "column already present" guards then reused one
# skeleton's remap on a later call.

test_that("add_rx: the caller's lmed gains no columns", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-02"),
    atc = "N06AB10",
    fddd = 7
  )
  # data.table `:=` edits the names attribute in place, and names() can return
  # that very vector -- copy it, or the "before" snapshot silently updates.
  before <- data.table::copy(names(rx))
  swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  expect_identical(names(rx), before)
  expect_true(any(skel$rx_n06a))
})

test_that("add_rx: one lmed reused across two skeletons is remapped per skeleton", {
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2019-03-04"),
    atc = "N06AB10",
    fddd = 35 # 2019-03-04 .. 2019-04-07, isoyearweeks 2019-10 .. 2019-14
  )

  # First skeleton: weekly spine starts 2020, so 2019 is pre-weekly.
  skel_2020 <- .rxa_skeleton(1L)
  swereg::add_rx(skel_2020, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  expect_true(all(skel_2020[is_isoyear == TRUE & isoyear == 2019L, rx_n06a]))

  # Second skeleton: weekly spine covers 2019, so the same prescription must
  # mark weekly rows and no annual row.
  skel_2019 <- swereg::create_skeleton(
    ids = 1L, date_min = as.Date("2019-01-07"), date_max = as.Date("2019-12-29")
  )
  swereg::add_rx(skel_2019, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  expect_equal(
    sort(skel_2019[rx_n06a == TRUE]$isoyearweek),
    sprintf("2019-%02d", 10:14)
  )
  expect_false(any(skel_2019[is_isoyear == TRUE, rx_n06a]))
})

test_that("add_rx: the caller's lmed schema does not depend on dropped rows", {
  a <- .rxa_table(
    lopnr = c(1L, 1L), edatum = as.Date("2020-03-02"),
    atc = "N06AB10", fddd = c(0, 7)
  )
  b <- .rxa_table(
    lopnr = c(1L, 1L), edatum = as.Date("2020-03-02"),
    atc = "N06AB10", fddd = c(1, 7)
  )
  original <- data.table::copy(names(a))

  invisible(.rxa_warnings(
    swereg::add_rx(.rxa_skeleton(1L), a, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  ))
  invisible(.rxa_warnings(
    swereg::add_rx(.rxa_skeleton(1L), b, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  ))

  expect_identical(names(a), names(b))
  expect_identical(names(a), original)
})

test_that("add_rx: a stop_date cached by an earlier call cannot bypass the duration filter", {
  rx <- .rxa_table(
    lopnr = 1L, edatum = as.Date("2020-03-02"), atc = "N06AB10", fddd = 7
  )
  swereg::add_rx(.rxa_skeleton(1L), rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))

  # Change the duration to one that MUST be dropped, then call again.
  rx[, fddd := 0]
  skel <- .rxa_skeleton(1L)
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )

  expect_true(any(grepl("fddd is missing, non-finite, or not positive", msgs, fixed = TRUE)))
  expect_false(any(skel$rx_n06a))
})

# ---- The interval is validated as dates, before the remap -----------------

test_that("add_rx: an inverted caller-supplied stop_date before the weekly spine is dropped", {
  skel <- .rxa_skeleton(1L)
  # Both endpoints are pre-weekly and in the same ISO year, so the annual remap
  # would collapse them to one equal pair and hide the inversion.
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2018-05-10"),
    atc = "N06AB10",
    fddd = 7,
    stop_date = as.Date("2018-05-01")
  )
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )

  expect_true(any(grepl("invalid as dates", msgs, fixed = TRUE)))
  expect_false(any(skel[is_isoyear == TRUE & isoyear == 2018L, rx_n06a]))
  expect_false(any(skel$rx_n06a))
})

test_that("add_rx: a caller-supplied stop_date of NA is dropped before the ISO conversion", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-02"),
    atc = "N06AB10",
    fddd = 7,
    stop_date = as.Date(NA)
  )
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )

  expect_true(any(grepl("invalid as dates", msgs, fixed = TRUE)))
  expect_false(any(skel$rx_n06a))
})

# ---- Duration edge cases --------------------------------------------------

test_that("add_rx: NaN, Inf and -Inf fddd are all dropped", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-02"),
    atc = "N06AB10",
    fddd = c(NaN, Inf, -Inf)
  )
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )

  expect_true(any(grepl("3 prescription rows dropped", msgs, fixed = TRUE)))
  expect_false(any(skel$rx_n06a))
})

test_that("add_rx: fddd of 1 covers exactly one week and fddd of 0.5 rounds to zero and is dropped", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L, edatum = as.Date("2020-03-02"), atc = "N06AB10", fddd = 1
  )
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )
  expect_length(msgs, 0L)
  expect_equal(skel[rx_n06a == TRUE]$isoyearweek, "2020-10")

  # round() is ties-to-even, so round(0.5) is 0.
  expect_equal(round(0.5), 0)
  skel2 <- .rxa_skeleton(1L)
  rx2 <- .rxa_table(
    lopnr = 1L, edatum = as.Date("2020-03-02"), atc = "N06AB10", fddd = 0.5
  )
  msgs2 <- .rxa_warnings(
    swereg::add_rx(skel2, rx2, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )
  expect_true(any(grepl(
    "1 prescription rows dropped before ISO week conversion because fddd is missing, non-finite, or not positive",
    msgs2,
    fixed = TRUE
  )))
  expect_false(any(skel2$rx_n06a))
})

test_that("add_rx: several dropped rows produce exactly one warning", {
  skel <- .rxa_skeleton(1L)
  rx <- .rxa_table(
    lopnr = 1L,
    edatum = as.Date("2020-03-02"),
    atc = "N06AB10",
    fddd = c(0, -1, NA_real_, 7)
  )
  msgs <- .rxa_warnings(
    swereg::add_rx(skel, rx, id_name = "lopnr", codes = list("rx_n06a" = "N06A"))
  )

  expect_length(msgs, 1L)
  expect_true(any(grepl("3 prescription rows dropped", msgs, fixed = TRUE)))
  expect_true(any(skel$rx_n06a)) # the valid row survives
})
