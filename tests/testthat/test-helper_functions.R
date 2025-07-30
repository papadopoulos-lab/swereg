test_that("min_with_infinite_as_na works correctly", {
  expect_equal(min_with_infinite_as_na(c(1, 2, 4)), 1)
  expect_equal(min_with_infinite_as_na(c(Inf, -Inf)), NA_real_)
  expect_equal(min_with_infinite_as_na(c(NA, 1, 2)), 1)
})

test_that("max_with_infinite_as_na works correctly", {
  expect_equal(max_with_infinite_as_na(c(1, 2, 4)), 4)
  expect_equal(max_with_infinite_as_na(c(Inf, -Inf)), NA_real_)
  expect_equal(max_with_infinite_as_na(c(NA, 1, 2)), 2)
})

test_that("first_non_na works correctly", {
  expect_equal(as.numeric(first_non_na(c(NA, NA, 3, 4, 5))), 3)
  expect_equal(as.numeric(first_non_na(c(1, 2, 3))), 1)
  expect_true(is.na(as.numeric(first_non_na(c(NA, NA, NA)))))
})

test_that("last_non_na works correctly", {
  expect_equal(as.numeric(last_non_na(c(1, 2, 3, NA, NA))), 3)
  expect_equal(as.numeric(last_non_na(c(1, 2, 3))), 3)
  expect_true(is.na(as.numeric(last_non_na(c(NA, NA, NA)))))
})