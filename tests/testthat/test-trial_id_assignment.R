# Pin .assign_trial_ids(): every TTE pipeline output (CONSORT counts,
# matching tuples, IRR denominators) buckets person-weeks into
# sequential trials by `trial_id`. Mis-assigning trial_id silently
# misclassifies eligibility windows.
#
# Implementation (R/r6_tteenrollment.R):
#   trial_id := (week_index - 1) %/% period_width
# where week_index is the 1-indexed position in cstime::dates_by_isoyearweek$isoyearweek.
# Result: weeks 1..period_width -> trial_id 0; weeks (period_width+1)..2*period_width -> trial_id 1; ...

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

test_that(".assign_trial_ids: consecutive weeks within one period share a trial_id", {
  # Pull a contiguous slice of real cstime weeks so the index lookup is
  # exact (no boundary surprises).
  weeks <- cstime::dates_by_isoyearweek$isoyearweek[1:8]
  data <- data.table::data.table(
    id = 1L,
    isoyearweek = weeks
  )
  swereg:::.assign_trial_ids(data, period_width = 4L)
  # Weeks 1..4 -> trial 0; weeks 5..8 -> trial 1
  expect_equal(data$trial_id, c(rep(0L, 4L), rep(1L, 4L)))
})

test_that(".assign_trial_ids: period_width = 1 gives a unique trial per week", {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek[1:6]
  data <- data.table::data.table(id = 1L, isoyearweek = weeks)
  swereg:::.assign_trial_ids(data, period_width = 1L)
  expect_equal(data$trial_id, 0:5)
})

test_that(".assign_trial_ids: assignment is independent of person_id", {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek[1:8]
  data <- data.table::data.table(
    id = c(rep("A", 8L), rep("B", 8L)),
    isoyearweek = c(weeks, weeks)
  )
  swereg:::.assign_trial_ids(data, period_width = 4L)
  expected <- rep(c(rep(0L, 4L), rep(1L, 4L)), 2L)
  expect_equal(data$trial_id, expected,
               info = ".assign_trial_ids should map (week) -> trial_id, independent of id")
})

test_that(".assign_trial_ids: unknown isoyearweek strings get NA trial_id", {
  data <- data.table::data.table(
    id = 1L,
    isoyearweek = c("2020-01", "9999-99")
  )
  swereg:::.assign_trial_ids(data, period_width = 4L)
  # First should resolve, second is not a real cstime week and should
  # come back NA -- this is the same NA branch that `.s1_compute_attrition`
  # later filters with `[!is.na(trial_id)]` (see test-attrition_global_count.R).
  expect_false(is.na(data$trial_id[1L]))
  expect_true(is.na(data$trial_id[2L]))
})

test_that(".assign_trial_ids: person-disjoint subsets produce identical trial_ids on shared weeks", {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek[1:12]
  data <- data.table::data.table(
    id = c(rep("A", 12L), rep("B", 12L)),
    isoyearweek = c(weeks, weeks)
  )
  full <- data.table::copy(data)
  swereg:::.assign_trial_ids(full, period_width = 4L)
  half_a <- data.table::copy(data[id == "A"])
  half_b <- data.table::copy(data[id == "B"])
  swereg:::.assign_trial_ids(half_a, period_width = 4L)
  swereg:::.assign_trial_ids(half_b, period_width = 4L)
  # The trial_id for week W must be the same regardless of whether it
  # was assigned in the full table or in a per-person split.
  expect_equal(half_a$trial_id, full[id == "A", trial_id])
  expect_equal(half_b$trial_id, full[id == "B", trial_id])
})
