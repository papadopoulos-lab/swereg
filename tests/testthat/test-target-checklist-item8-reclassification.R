# =============================================================================
# TARGET checklist Item 8 and a person-trial that changes arm
# =============================================================================
# `.s1_compute_attrition()` re-derives the arm at every cumulative eligibility
# level. The arm is `any()` over the weeks that are still eligible. A criterion
# that makes weeks ineligible can therefore move a person-trial between arms,
# instead of out of the cohort. One arm then GROWS while the cohort shrinks,
# and the difference of two levels is negative for that arm.
#
# The producer is correct. Each level reports the arm at that level, and the
# reclassification is a real property of the data. Item 8 printed the
# difference, and Item 8 goes into a paper.
#
# This file drives the real producer. Every other Item 8 fixture writes its
# attrition table by hand, so it could only hard-code the negative it wants to
# see. `.item8r_skeleton()` produces it from person-weeks instead.
#
# The helpers below repeat `.item8_plan()` and the line readers from
# `test-target-checklist-item8-counts.R`. testthat evaluates each test file in
# its own environment, so one test file cannot call a function another one
# defines.

skip_if_not_installed("data.table")


#' Three person-trials over one trial and two weeks each.
#'
#' `eligible_washout` is the criterion that reclassifies:
#'   - A starts treated in week 1 and stops in week 2. Week 1 is ineligible,
#'     so at the last level `any()` runs over week 2 alone and A is a
#'     comparator. A stays in the cohort.
#'   - B is treated in both weeks and neither week is eligible, so B leaves
#'     the cohort. B was an intervention person-trial.
#'   - C is untreated in both eligible weeks, so C is a comparator throughout.
#'
#' The cohort therefore falls by one person-trial while the comparator arm
#' rises by one.
.item8r_skeleton <- function() {
  return(data.table::data.table(
    person_id = c("A", "A", "B", "B", "C", "C"),
    trial_id = rep(1L, 6L),
    eligible_year = rep(TRUE, 6L),
    eligible_washout = c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
    rd_intervention = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  ))
}


#' The attrition table the real producer computes from that skeleton.
.item8r_attrition <- function() {
  return(swereg:::.s1_compute_attrition(
    skeleton = .item8r_skeleton(),
    eligible_cols = c("eligible_year", "eligible_washout"),
    pid = "person_id"
  ))
}


#' The global rows, in application order. Item 8 reads these and no others.
.item8r_global <- function(att) {
  g <- att[is.na(att$trial_id)]
  return(g[match(
    c("before_exclusions", "eligible_year", "eligible_washout"),
    g$criterion
  )])
}


#' A TTEPlan carrying the produced attrition table for enrollment 01.
#'
#' `$print_target_checklist()` reads `plan$spec` above Item 8, so the spec is
#' complete enough to print.
.item8r_plan <- function() {
  spec <- list(
    study = list(
      title = "Item 8 reclassification",
      implementation = list(project_prefix = "test")
    ),
    inclusion_criteria = list(isoyears = c(2010, 2020)),
    exclusion_criteria = list(),
    confounders = list(),
    outcomes = list(
      list(name = "Event", implementation = list(variable = "event_a"))
    ),
    follow_up = list(list(label = "1 year", weeks = 52)),
    enrollments = list(
      list(
        id = "01",
        name = "Test 01",
        additional_inclusion = list(
          list(
            type = "age_range",
            min = 50,
            max = 60,
            implementation = list(variable = "rd_age")
          )
        ),
        treatment = list(
          arms = list(intervention = "A", comparator = "B"),
          implementation = list(
            variable = "rd_exp",
            intervention_value = "a",
            comparator_value = "b",
            comparator_to_intervention_ratio = 2,
            seed = 1
          )
        )
      )
    )
  )
  plan <- TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "/tmp/skel.qs2",
    global_max_isoyearweek = "2020-52"
  )
  plan$spec <- spec
  plan$add_one_ett(
    enrollment_id = "01",
    outcome_var = "event_a",
    outcome_name = "Event",
    follow_up = 52,
    confounder_vars = "rd_age",
    time_treatment_var = "rd_intervention",
    eligible_var = "eligible",
    argset = list(age_group = "50_60", age_min = 50, age_max = 60)
  )
  plan$enrollment_counts <- list("01" = list(attrition = .item8r_attrition()))
  return(plan)
}


#' Every number on one printed line, as numbers.
#'
#' The pattern allows a leading minus. A pattern that cannot match a negative
#' count reports one as absent instead of as wrong.
.item8r_line_nums <- function(x) {
  hits <- regmatches(x, gregexpr("-?[0-9][0-9,]*", x))[[1]]
  return(as.numeric(gsub(",", "", hits)))
}


#' The Item 8 participant-flow lines of `plan`, stripped and trimmed.
.item8r_flow_lines <- function(plan) {
  out <- gsub(
    "\033\\[[0-9;]*m",
    "",
    capture.output(plan$print_target_checklist())
  )
  start <- grep("participant flow:", out, fixed = TRUE)
  if (length(start) == 0L) {
    return(character(0))
  }
  ends <- grep("[FILL IN]", out, fixed = TRUE)
  ends <- ends[ends > start[1L]]
  return(trimws(out[seq(start[1L], ends[1L] - 1L)]))
}


test_that("the producer moves a person-trial between arms, and Item 8 must not difference that", {
  g <- .item8r_global(.item8r_attrition())

  expect_identical(g$n_person_trials, c(3L, 3L, 2L))
  expect_identical(g$n_intervention, c(2L, 2L, 0L))
  expect_identical(g$n_comparator, c(1L, 1L, 2L))

  # The cohort loses one person-trial at `eligible_washout` and the
  # comparator arm GAINS one. Differencing the two levels reports -1
  # comparator person-trials excluded, which no cohort can do.
  expect_identical(diff(g$n_person_trials), c(0L, -1L))
  expect_identical(diff(g$n_comparator), c(0L, 1L))
})


test_that("a step line of Item 8 carries one total and no arm count", {
  lines <- .item8r_flow_lines(.item8r_plan())

  # `eligible_year` excludes nobody and is not the last criterion, so it
  # prints both step lines. `eligible_washout` is the last criterion and
  # prints the exclusion alone: it IS the eligible cohort.
  expect_identical(
    lapply(grep("Excluding", lines, value = TRUE), .item8r_line_nums),
    list(0, 1)
  )
  expect_identical(
    lapply(grep("Remaining", lines, value = TRUE), .item8r_line_nums),
    list(3)
  )
  expect_false(
    any(grepl("(", grep("Excluding|Remaining", lines, value = TRUE), fixed = TRUE))
  )

  # The arm counts appear at the eligible cohort, as the LEVEL the producer
  # computed and not as a difference.
  expect_identical(
    .item8r_line_nums(lines[which(lines == "Eligible cohort:") + 1L]),
    c(2, 0, 2)
  )
})


test_that("Item 8 prints no negative count for a cohort that reclassifies an arm", {
  lines <- .item8r_flow_lines(.item8r_plan())
  counted <- grep("person-trials", lines, value = TRUE)

  # The positive control. An empty window makes every assertion below pass.
  expect_gte(length(counted), 5L)

  nums <- unlist(lapply(counted, .item8r_line_nums))
  expect_gte(length(nums), 5L)
  expect_identical(nums[nums < 0], numeric(0))
})
