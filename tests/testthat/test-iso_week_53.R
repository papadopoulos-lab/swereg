# ISO 8601 has a week 53 in years where January 1st falls on a
# Thursday or in leap years where Jan 1st falls on Wednesday.
# Recent / upcoming ISO-53 years: 2015, 2020, 2026, 2032.
#
# Mishandling W53 manifests as silently dropped data in that final
# week, which trickles into the rate denominator and the analysis
# follow-up window. Pin the invariant.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

test_that("create_skeleton produces 53 weeks for ISO 2020", {
  skel <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2019-12-30"),  # ISO 2020-W01 Monday
    date_max = as.Date("2021-01-03")   # ISO 2020-W53 Sunday
  )
  weeks_2020 <- skel[is_isoyear == FALSE & isoyear == 2020L,
                     sort(unique(isoyearweek))]
  expected <- sprintf("2020-%02d", 1:53)
  expect_setequal(weeks_2020, expected)
})

test_that("create_skeleton produces 52 weeks for non-ISO-53 years (2021)", {
  skel <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2021-01-04"),  # ISO 2021-W01 Monday
    date_max = as.Date("2022-01-02")   # ISO 2021-W52 Sunday
  )
  weeks_2021 <- skel[is_isoyear == FALSE & isoyear == 2021L,
                     sort(unique(isoyearweek))]
  expected <- sprintf("2021-%02d", 1:52)
  expect_setequal(weeks_2021, expected)
  expect_false("2021-53" %in% weeks_2021)
})

test_that("create_skeleton produces 53 weeks for ISO 2026", {
  # ISO 2026 also has 53 weeks. This is the upcoming case for studies
  # currently being run -- verify it doesn't silently drop W53.
  skel <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2025-12-29"),  # ISO 2026-W01 Monday
    date_max = as.Date("2027-01-03")   # ISO 2026-W53 Sunday
  )
  weeks_2026 <- skel[is_isoyear == FALSE & isoyear == 2026L,
                     sort(unique(isoyearweek))]
  expect_true("2026-53" %in% weeks_2026,
              info = "ISO 2026 has 53 weeks; W53 must appear in the skeleton")
  expect_equal(length(weeks_2026), 53L)
})

test_that("dates inside ISO W53 are correctly classified", {
  skel <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2020-12-28"),  # Monday of ISO 2020-W53
    date_max = as.Date("2021-01-03")   # Sunday of ISO 2020-W53
  )
  weekly <- skel[is_isoyear == FALSE]
  # Every weekly row should be in 2020-W53. Dates 2020-12-28 through
  # 2021-01-03 all belong to ISO 2020 W53.
  expect_setequal(unique(weekly$isoyearweek), "2020-53")
  expect_setequal(unique(weekly$isoyear), 2020L)
})
