# Pins the `personyears` denominator that every rate / IRR in the
# results workbook ultimately divides by.
#
# `create_skeleton()` emits two row classes:
#   - is_isoyear == TRUE  -> annual rows (1900..date_min-1's isoyear),
#                            personyears = 1 each
#   - is_isoyear == FALSE -> weekly rows (date_min..date_max),
#                            personyears = 1/52.25 each
#
# Rate computations downstream consume the weekly rows. Getting the
# weekly accounting wrong by 1% scales every reported rate by 1%.

skip_if_not_installed("data.table")

test_that("each weekly row contributes exactly 1/52.25 personyears", {
  skel <- create_skeleton(
    ids = 1:3,
    date_min = as.Date("2021-01-04"),  # ISO Monday
    date_max = as.Date("2021-12-26")
  )
  weekly <- skel[is_isoyear == FALSE]
  expect_true(all(abs(weekly$personyears - 1 / 52.25) < 1e-12))
})

test_that("each annual row contributes exactly 1 personyear", {
  skel <- create_skeleton(
    ids = 1:2,
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
  yearly <- skel[is_isoyear == TRUE]
  expect_true(all(yearly$personyears == 1))
})

test_that("sum(personyears) over weekly rows equals n_weeks / 52.25 per id", {
  skel <- create_skeleton(
    ids = c("A", "B", "C"),
    date_min = as.Date("2021-01-04"),
    date_max = as.Date("2021-12-26")
  )
  per_id <- skel[is_isoyear == FALSE,
                 .(py = sum(personyears), n = .N), by = id]
  expect_true(all(abs(per_id$py - per_id$n / 52.25) < 1e-12))
  expect_true(length(unique(per_id$n)) == 1L,
              info = "weekly row count differs across ids -- skeleton not balanced")
})

test_that("ISO-53 years contribute one extra week of personyears", {
  # ISO year 2020 has 53 weeks (the next such year is 2026, which is
  # also currently in scope for active studies).
  skel_2020 <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2020-01-06"),  # ISO 2020-W02 Monday (skip year-boundary)
    date_max = as.Date("2020-12-27")   # ISO 2020-W52 end
  )
  weeks_2020 <- skel_2020[is_isoyear == FALSE & isoyear == 2020L,
                          unique(isoyearweek)]
  # In 2020 we should see weeks 02..52 if we stayed inside that range,
  # i.e. 51 weeks. The W53 boundary case is the test below.
  expect_gte(length(weeks_2020), 51L)

  # Now span the full ISO year 2020 including W53. ISO 2020 runs from
  # 2019-12-30 (W01 Monday) to 2021-01-03 (W53 Sunday).
  skel_full <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2019-12-30"),
    date_max = as.Date("2021-01-03")
  )
  weeks_2020_full <- skel_full[is_isoyear == FALSE & isoyear == 2020L,
                               unique(isoyearweek)]
  expect_true("2020-53" %in% weeks_2020_full,
              info = "ISO week 53 of 2020 is missing from the skeleton")
  expect_equal(length(weeks_2020_full), 53L,
               info = "ISO year 2020 should have exactly 53 weeks in the skeleton")
})

test_that("personyears sum across multiple isoyears is additive", {
  skel <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2019-01-07"),  # ISO 2019-W02
    date_max = as.Date("2021-12-26")   # late ISO 2021
  )
  per_year <- skel[is_isoyear == FALSE,
                   .(py = sum(personyears), n = .N), by = isoyear]
  expect_true(all(abs(per_year$py - per_year$n / 52.25) < 1e-12))
  # Cross-year totals should also satisfy the identity.
  total <- skel[is_isoyear == FALSE, sum(personyears)]
  total_n <- skel[is_isoyear == FALSE, .N]
  expect_true(abs(total - total_n / 52.25) < 1e-12)
})
