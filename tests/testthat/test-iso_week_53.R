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
    isoyear_min = 2020,              # weekly rows from ISO 2020-W01
    isoyearweek_max = "2020-53"      # to ISO 2020-W53
  )
  weeks_2020 <- skel[is_isoyear == FALSE & isoyear == 2020L,
                     sort(unique(isoyearweek))]
  expected <- sprintf("2020-%02d", 1:53)
  expect_setequal(weeks_2020, expected)
})

test_that("create_skeleton produces 52 weeks for non-ISO-53 years (2021)", {
  skel <- create_skeleton(
    ids = 1L,
    isoyear_min = 2021,              # weekly rows from ISO 2021-W01
    isoyearweek_max = "2021-52"      # to ISO 2021-W52
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
    isoyear_min = 2026,              # weekly rows from ISO 2026-W01
    isoyearweek_max = "2026-53"      # to ISO 2026-W53
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
    isoyear_min = 2020,              # weekly rows start at ISO 2020-W01
    isoyearweek_max = "2020-53"      # and end at ISO 2020-W53
  )
  weekly <- skel[is_isoyear == FALSE]
  # The weekly rows can no longer start inside a year, so W53 is the last
  # weekly row. Dates 2020-12-28 through 2021-01-03 all belong to ISO 2020
  # W53, so the row belongs to ISO year 2020 and ends on Sunday 2021-01-03.
  expect_identical(utils::tail(weekly$isoyearweek, 1L), "2020-53")
  expect_identical(weekly[isoyearweek == "2020-53", isoyear], 2020L)
  expect_identical(
    weekly[isoyearweek == "2020-53", isoyearweeksun],
    as.Date("2021-01-03")
  )
  expect_setequal(unique(weekly$isoyear), 2020L)
})
