# `create_skeleton(ids, isoyear_min, isoyearweek_max)` builds annual rows for
# every ISO year before `isoyear_min` and weekly rows from week 01 of
# `isoyear_min` to `isoyearweek_max`. No ISO year holds both kinds of row, so
# an annual row always means a whole ISO year.
#
# Every expected value below is derived by hand from the ISO calendar. ISO
# years 2004, 2009, 2015 and 2020 have a week 53 in the range used here.

skip_if_not_installed("data.table")

test_that("annual rows end the year before isoyear_min, weekly rows start at its week 01", {
  sk <- create_skeleton(ids = 1L, isoyear_min = 2005L, isoyearweek_max = "2005-03")
  # 1900..2004 is 105 annual rows, then three weeks.
  expect_identical(nrow(sk), 108L)
  expect_identical(sk$isoyearweek[104:108], c("2003-**", "2004-**", "2005-01", "2005-02", "2005-03"))
  expect_identical(sk$is_isoyear, c(rep(TRUE, 105L), rep(FALSE, 3L)))
  expect_identical(sk[is_isoyear == TRUE, max(isoyear)], 2004L)
})

test_that("no ISO year holds both an annual and a weekly row", {
  sk <- create_skeleton(ids = 1L, isoyear_min = 2000L, isoyearweek_max = "2000-05")
  both <- intersect(sk[is_isoyear == TRUE, isoyear], sk[is_isoyear == FALSE, isoyear])
  expect_length(both, 0L)
  expect_false("1999-52" %in% sk$isoyearweek)
  expect_identical(sk[is_isoyear == FALSE, min(isoyearweek)], "2000-01")
})

test_that("week 53 is included where the ISO year has one", {
  sk <- create_skeleton(ids = 1L, isoyear_min = 2004L, isoyearweek_max = "2005-01")
  # 2004-01..2004-53 and 2005-01.
  expect_identical(sk[is_isoyear == FALSE, .N], 54L)
  expect_true("2004-53" %in% sk$isoyearweek)
})

test_that("the end week may fall anywhere in a year", {
  sk <- create_skeleton(ids = 1L, isoyear_min = 2000L, isoyearweek_max = "2000-01")
  expect_identical(sk[is_isoyear == FALSE, isoyearweek], "2000-01")
  sk <- create_skeleton(ids = 1L, isoyear_min = 2020L, isoyearweek_max = "2020-53")
  expect_identical(sk[is_isoyear == FALSE, .N], 53L)
})

test_that("the production grid of 2005 to 2025-01 has the expected rows", {
  sk <- create_skeleton(ids = 1L, isoyear_min = 2005L, isoyearweek_max = "2025-01")
  # 2005..2024 hold 20 * 52 + 3 = 1043 weeks (2009, 2015, 2020 have week 53),
  # plus 2025-01.
  expect_identical(sk[is_isoyear == FALSE, .N], 1044L)
  expect_identical(sk[is_isoyear == TRUE, .N], 105L)
  expect_identical(sk[is_isoyear == FALSE, max(isoyearweek)], "2025-01")
})

test_that("an ISO year sums to exactly one person-year of annual rows", {
  sk <- create_skeleton(ids = 1L, isoyear_min = 2000L, isoyearweek_max = "2000-05")
  # 1999 is one annual row worth 1 person-year, with no weekly row added.
  expect_identical(sk[isoyear == 1999L, sum(personyears)], 1)
})

test_that("every id gets the same spine, sorted by id and isoyearweek", {
  sk <- create_skeleton(ids = c(2L, 1L), isoyear_min = 2005L, isoyearweek_max = "2005-02")
  expect_identical(sk$id, rep(c(1L, 2L), each = 107L))
  expect_identical(sk[id == 1L, isoyearweek], sk[id == 2L, isoyearweek])
  expect_false(is.unsorted(sk[id == 1L, isoyearweek]))
})

test_that("the retired date arguments are refused by name, with directions", {
  expect_error(create_skeleton(ids = 1L, date_min = "2000-01-01", date_max = "2023-12-31"), "no longer takes date_min")
})

test_that("the retired date arguments are refused by position, with directions", {
  expect_error(create_skeleton(1L, "2000-01-01", "2023-12-31"), "no longer takes date_min")
})

test_that("a date-time passed by position is refused with directions", {
  expect_error(create_skeleton(1L, as.POSIXct("2000-01-01", tz = "UTC"), "2023-12-31"), "no longer takes date_min")
})

test_that("invalid bounds are refused", {
  # The end before the start.
  expect_error(create_skeleton(ids = 1L, isoyear_min = 2000L, isoyearweek_max = "1999-52"), "isoyearweek_max")
  # Not a week.
  expect_error(create_skeleton(ids = 1L, isoyear_min = 2000L, isoyearweek_max = "2008-53"), "2008-53")
  expect_error(create_skeleton(ids = 1L, isoyear_min = 2000L, isoyearweek_max = "2005-**"), "2005-\\*\\*")
  # Not one whole year.
  expect_error(create_skeleton(ids = 1L, isoyear_min = 2000.5, isoyearweek_max = "2005-01"), "isoyear_min")
  expect_error(create_skeleton(ids = 1L, isoyear_min = c(2000L, 2001L), isoyearweek_max = "2005-01"), "isoyear_min")
  expect_error(create_skeleton(ids = 1L, isoyear_min = NA_integer_, isoyearweek_max = "2005-01"), "isoyear_min")
})

test_that("a whole-number double is accepted as isoyear_min", {
  sk <- create_skeleton(ids = 1L, isoyear_min = 2005, isoyearweek_max = "2005-01")
  expect_identical(sk[is_isoyear == FALSE, isoyearweek], "2005-01")
})
