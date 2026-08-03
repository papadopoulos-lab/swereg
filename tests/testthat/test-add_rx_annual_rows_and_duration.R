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
