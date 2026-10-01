test_that("create_skeleton creates expected structure", {
  # Create a simple skeleton
  skeleton <- create_skeleton(
    ids = c(1, 2),
    isoyear_min = 2020,
    isoyearweek_max = "2020-05"
  )
  
  # Check that it's a data.table
  expect_s3_class(skeleton, "data.table")
  
  # Check required columns exist
  expected_cols <- c("id", "isoyear", "isoyearweek", "is_isoyear", 
                     "isoyearweeksun", "personyears")
  expect_true(all(expected_cols %in% names(skeleton)))
  
  # Check that we have the right number of IDs
  expect_equal(length(unique(skeleton$id)), 2)
  
  # Check that 2020 is included in the years
  expect_true(2020 %in% skeleton$isoyear)
})

test_that("create_skeleton handles single ID", {
  skeleton <- create_skeleton(
    ids = 100,
    isoyear_min = 2020, 
    isoyearweek_max = "2020-02"
  )
  
  expect_equal(unique(skeleton$id), 100)
  expect_true(nrow(skeleton) > 0)
})