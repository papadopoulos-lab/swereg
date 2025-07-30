test_that("make_rowind_first_occurrence works correctly", {
  # Create test data
  library(data.table)
  dt <- data.table(
    id = c(1, 1, 1, 2, 2, 2),
    time = c(1, 2, 3, 1, 2, 3),
    condition = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
    value = c(10, 20, 30, 100, 200, 300)
  )
  
  # Apply function
  result <- make_rowind_first_occurrence(
    dt, 
    condition = "condition == TRUE",
    value_var = "value",
    new_var = "first_value"
  )
  
  # Check that result is returned (invisibly)
  expect_identical(result, dt)
  
  # Check that new column exists
  expect_true("first_value" %in% names(dt))
  
  # Check values are correct
  expect_equal(dt[id == 1, unique(first_value)], 20)  # First TRUE at time 2
  expect_equal(dt[id == 2, unique(first_value)], 100) # First TRUE at time 1
  
  # Check that all rows for each ID have the same value
  expect_equal(dt[id == 1, length(unique(first_value))], 1)
  expect_equal(dt[id == 2, length(unique(first_value))], 1)
})

test_that("make_rowind_first_occurrence handles no matches", {
  library(data.table)
  dt <- data.table(
    id = c(1, 1, 1),
    condition = c(FALSE, FALSE, FALSE),
    value = c(10, 20, 30)
  )
  
  make_rowind_first_occurrence(
    dt,
    condition = "condition == TRUE",
    value_var = "value", 
    new_var = "first_value"
  )
  
  # Should be NA when no condition matches
  expect_true(all(is.na(dt$first_value)))
})