# skeleton_eligible_no_observation_in_window_excluding_wk0() compares `var`
# with `value`. An NA week is a week with no observation, so it MUST NOT turn
# the eligibility of a later week into NA.

test_that("an NA week counts as no observation in the window helper", {
  dt <- data.table::data.table(
    id = 1L,
    isoyearweek = c("2020-01", "2020-02", "2020-03"),
    wk = c("b", NA, "b")
  )
  skeleton_eligible_no_observation_in_window_excluding_wk0(
    dt,
    "wk",
    value = "a",
    window = 2,
    col_name = "elig"
  )
  expect_identical(dt$elig, c(TRUE, TRUE, TRUE))
})
