# Pin date-precision and sentinel handling in make_lowercase_names()
# (the in-place version) -- the entrypoint that runs before every
# add_*() call. Mishandling a sentinel like "0000" or "00" silently
# shifts events by months or drops them entirely, which then ripples
# through every downstream rate / IRR.
#
# Documented contract (per swereg CLAUDE.md and the function source):
#   - YYYY (4 chars)        -> add default_month_day = "0701"
#   - YYYYMM (6 chars)      -> add default_day = "15"
#   - YYYYMMDD (8 chars)    -> use as-is
#   - "0000" / "9999"       -> treated as missing year sentinels
#   - "00" (month or day)   -> replaced by default
#   - "" / "NA"             -> NA Date

skip_if_not_installed("data.table")

test_that("make_lowercase_names: lowercases column names in place", {
  dt <- data.table::data.table(LopNr = 1:3, INDATUM = "20200315",
                               OTHER = c("a", "b", "c"))
  swereg::make_lowercase_names(dt) |> suppressMessages() |> suppressWarnings()
  expect_setequal(names(dt), c("lopnr", "indatum", "other"))
})

test_that("make_lowercase_names: 8-char YYYYMMDD parses as-is", {
  dt <- data.table::data.table(LopNr = 1L, INDATUM = "20200315")
  swereg::make_lowercase_names(dt, date_columns = "indatum") |>
    suppressMessages() |> suppressWarnings()
  expect_s3_class(dt$indatum, "Date")
  expect_equal(dt$indatum, as.Date("2020-03-15"))
})

test_that("make_lowercase_names: 6-char YYYYMM appends default day", {
  dt <- data.table::data.table(LopNr = 1L, INDATUM = "202003")
  swereg::make_lowercase_names(dt, date_columns = "indatum",
                               default_day = "15") |>
    suppressMessages() |> suppressWarnings()
  expect_equal(dt$indatum, as.Date("2020-03-15"))
})

test_that("make_lowercase_names: 4-char YYYY appends default month-day", {
  dt <- data.table::data.table(LopNr = 1L, INDATUM = "2020")
  swereg::make_lowercase_names(dt, date_columns = "indatum",
                               default_month_day = "0701") |>
    suppressMessages() |> suppressWarnings()
  expect_equal(dt$indatum, as.Date("2020-07-01"))
})

test_that("make_lowercase_names: '0000' year produces a literal year-0 date (not NA)", {
  # Surprising behaviour worth pinning: "0000" is NOT in the default
  # `na_strings`, so it parses to a literal year-0 date with the
  # default month-day appended. If a registry uses "0000" as a
  # sentinel for missing year, callers must add it to na_strings
  # explicitly.
  dt <- data.table::data.table(LopNr = 1:2,
                               INDATUM = c("20200315", "0000"))
  swereg::make_lowercase_names(dt, date_columns = "indatum") |>
    suppressMessages() |> suppressWarnings()
  expect_equal(dt$indatum[1], as.Date("2020-03-15"))
  expect_false(is.na(dt$indatum[2]),
    info = "'0000' should NOT be treated as NA by default")
})

test_that("make_lowercase_names: '9999' full-year sentinel becomes NA", {
  dt <- data.table::data.table(LopNr = 1:2,
                               INDATUM = c("20200315", "9999"))
  swereg::make_lowercase_names(dt, date_columns = "indatum") |>
    suppressMessages() |> suppressWarnings()
  expect_true(is.na(dt$indatum[2]))
})

test_that("make_lowercase_names: '99999999' full-length sentinel becomes NA", {
  dt <- data.table::data.table(LopNr = 1:2,
                               INDATUM = c("20200315", "99999999"))
  swereg::make_lowercase_names(dt, date_columns = "indatum") |>
    suppressMessages() |> suppressWarnings()
  expect_true(is.na(dt$indatum[2]))
})

test_that("make_lowercase_names: empty string and 'NA' literal become NA", {
  dt <- data.table::data.table(LopNr = 1:3,
                               INDATUM = c("20200315", "", "NA"))
  swereg::make_lowercase_names(dt, date_columns = "indatum") |>
    suppressMessages() |> suppressWarnings()
  expect_equal(dt$indatum[1], as.Date("2020-03-15"))
  expect_true(is.na(dt$indatum[2]))
  expect_true(is.na(dt$indatum[3]))
})

test_that("make_lowercase_names: idempotent over already-lowercase names", {
  dt <- data.table::data.table(lopnr = 1L, indatum = "20200315")
  expect_silent(suppressWarnings(suppressMessages(
    swereg::make_lowercase_names(dt, date_columns = "indatum")
  )))
  expect_equal(dt$indatum, as.Date("2020-03-15"))
})

test_that("make_lowercase_names: mixed precisions in one column round-trip cleanly", {
  dt <- data.table::data.table(
    LopNr = 1:4,
    INDATUM = c("20200315", "202003", "2020", "")
  )
  swereg::make_lowercase_names(dt, date_columns = "indatum",
                               default_month_day = "0701",
                               default_day = "15") |>
    suppressMessages() |> suppressWarnings()
  expect_s3_class(dt$indatum, "Date")
  expect_equal(dt$indatum[1], as.Date("2020-03-15"))
  expect_equal(dt$indatum[2], as.Date("2020-03-15"))
  expect_equal(dt$indatum[3], as.Date("2020-07-01"))
  expect_true(is.na(dt$indatum[4]))
})
