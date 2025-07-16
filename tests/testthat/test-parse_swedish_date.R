test_that("parse_swedish_date handles different date formats", {
  # Test basic formats
  dates <- c("2020", "202003", "20200315")
  result <- parse_swedish_date(dates)
  
  expect_equal(result[1], as.Date("2020-07-01"))  # Year only -> July 1st
  expect_equal(result[2], as.Date("2020-03-15"))  # Year-month -> 15th
  expect_equal(result[3], as.Date("2020-03-15"))  # Full date
})

test_that("parse_swedish_date handles special cases", {
  # Test "0000" and "00" replacements
  dates <- c("19990000", "199900")
  result <- parse_swedish_date(dates)
  
  expect_equal(result[1], as.Date("1999-07-01"))  # "0000" -> "0701"
  expect_equal(result[2], as.Date("1999-07-01"))  # "00" -> "01" after "0000" -> "0701"
})

test_that("parse_swedish_date handles NA values", {
  # Test various NA representations
  dates <- c("", "NA", "9999", "99999999", NA_character_)
  result <- parse_swedish_date(dates)
  
  expect_true(all(is.na(result)))
})

test_that("parse_swedish_date handles custom defaults", {
  # Test custom default values
  dates <- c("2020", "202003")
  result <- parse_swedish_date(dates, default_month_day = "0101", default_day = "01")
  
  expect_equal(result[1], as.Date("2020-01-01"))
  expect_equal(result[2], as.Date("2020-03-01"))
})

test_that("parse_swedish_date handles invalid dates", {
  # Test invalid date strings
  dates <- c("20200230", "20201301", "abcd", "12345")
  
  expect_warning(result <- parse_swedish_date(dates))
  expect_true(all(is.na(result)))
})

test_that("parse_swedish_date is vectorized", {
  # Test with mixed valid and invalid dates
  dates <- c("2020", "202003", "20200315", "", "9999", "invalid")
  result <- parse_swedish_date(dates)
  
  expect_equal(length(result), length(dates))
  expect_equal(result[1], as.Date("2020-07-01"))
  expect_equal(result[2], as.Date("2020-03-15"))
  expect_equal(result[3], as.Date("2020-03-15"))
  expect_true(is.na(result[4]))
  expect_true(is.na(result[5]))
  expect_true(is.na(result[6]))
})

test_that("parse_swedish_date handles numeric input", {
  # Test with numeric input (should convert to character)
  dates <- c(2020, 202003, 20200315)
  result <- parse_swedish_date(dates)
  
  expect_equal(result[1], as.Date("2020-07-01"))
  expect_equal(result[2], as.Date("2020-03-15"))
  expect_equal(result[3], as.Date("2020-03-15"))
})

test_that("parse_swedish_date handles whitespace", {
  # Test with whitespace
  dates <- c(" 2020 ", "  202003  ", "20200315")
  result <- parse_swedish_date(dates)
  
  expect_equal(result[1], as.Date("2020-07-01"))
  expect_equal(result[2], as.Date("2020-03-15"))
  expect_equal(result[3], as.Date("2020-03-15"))
})