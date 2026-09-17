# =============================================================================
# TARGET checklist Item 8 counts
# =============================================================================
# Item 8 prints participant flow into manuscript methods text, so a wrong
# number there is a wrong number in a paper.
#
# The attrition table holds two kinds of row for each criterion. One global
# row carries `trial_id = NA` and counts across every trial. The rest carry
# one row per trial. `.attrition_overall()` reads the global rows and nothing
# else. A sum over both kinds counts every person-trial of a criterion twice.
#
# These three tests separate the rules Item 8 follows:
#   - it reads the global rows, so the printed count is the global count;
#   - it prints its placeholder when one criterion carries no global row;
#   - it prints its placeholder for EVERY enrollment when one enrollment
#     carries no global row. A flow that omits one enrollment reads exactly
#     like a complete flow.
#
# The helpers below repeat `.make_plan_with_spec()` and `.strip_ansi()` from
# `test-tte_spec.R`. testthat evaluates each test file in its own environment,
# so one test file cannot call a function another one defines.

.item8_strip_ansi <- function(x) gsub("\033\\[[0-9;]*m", "", x)


#' The counts across every trial, which the global rows hold.
#'
#' `.item8_attrition()` builds the global rows from these, and the assertions
#' compare against them. No assertion holds a literal taken from running the
#' code.
.ITEM8_GLOBAL_PT <- c(before_exclusions = 1000, eligible_age = 760)
.ITEM8_GLOBAL_INT <- c(before_exclusions = 400, eligible_age = 300)
.ITEM8_GLOBAL_CMP <- c(before_exclusions = 600, eligible_age = 460)


#' One enrollment's attrition table, over two criteria and two trials.
#'
#' The per-trial rows cover two trials and do not sum to the global counts.
#' Three aggregation rules therefore print three different numbers for
#' `before_exclusions`:
#'   - 1000 from the global row alone, which is the rule Item 8 uses;
#'   - 900 from the per-trial rows alone;
#'   - 1900 from both sets together, which is the defect.
#'
#' `drop_global_for` names the criterion whose global row is removed. The
#' table then has the shape of one written before the global rows existed.
.item8_attrition <- function(drop_global_for = NULL) {
  att <- data.table::data.table(
    trial_id = c(NA_integer_, 0L, 1L, NA_integer_, 0L, 1L),
    criterion = rep(c("before_exclusions", "eligible_age"), each = 3L),
    n_persons = c(700, 400, 500, 520, 300, 360),
    n_person_trials = c(
      .ITEM8_GLOBAL_PT[["before_exclusions"]], 400, 500,
      .ITEM8_GLOBAL_PT[["eligible_age"]], 300, 400
    ),
    n_intervention = c(
      .ITEM8_GLOBAL_INT[["before_exclusions"]], 160, 200,
      .ITEM8_GLOBAL_INT[["eligible_age"]], 120, 160
    ),
    n_comparator = c(
      .ITEM8_GLOBAL_CMP[["before_exclusions"]], 240, 300,
      .ITEM8_GLOBAL_CMP[["eligible_age"]], 180, 240
    )
  )
  if (is.null(drop_global_for)) {
    return(att)
  }
  return(att[!(is.na(trial_id) & criterion == drop_global_for)])
}


#' The capture groups of the first match, as numbers.
#'
#' Item 8 writes a count with `big.mark = ","` and right-justifies it, so the
#' pattern MUST allow the comma and the padding. This strips both.
.item8_captures <- function(txt, pattern) {
  m <- regmatches(txt, regexec(pattern, txt, perl = TRUE))[[1]]
  if (length(m) < 2L) {
    return(numeric(0))
  }
  return(as.numeric(gsub(",", "", m[-1])))
}


#' A TTEPlan carrying one enrollment per identifier in `ids`.
#'
#' `$print_target_checklist()` reads `plan$spec` above Item 8, so the spec MUST
#' be complete enough to print. It reads `plan$enrollment_counts` for Item 8,
#' and each test sets that itself.
.item8_plan <- function(ids = "01") {
  spec <- list(
    study = list(
      title = "Item 8 counts",
      implementation = list(project_prefix = "test")
    ),
    inclusion_criteria = list(isoyears = c(2010, 2020)),
    exclusion_criteria = list(),
    confounders = list(),
    outcomes = list(
      list(name = "Event", implementation = list(variable = "event_a"))
    ),
    follow_up = list(list(label = "1 year", weeks = 52)),
    enrollments = lapply(ids, function(id) {
      list(
        id = id,
        name = paste("Test", id),
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
    })
  )
  plan <- TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "/tmp/skel.qs2",
    global_max_isoyearweek = "2020-52"
  )
  plan$spec <- spec
  for (id in ids) {
    plan$add_one_ett(
      enrollment_id = id,
      outcome_var = "event_a",
      outcome_name = "Event",
      follow_up = 52,
      confounder_vars = "rd_age",
      time_treatment_var = "rd_intervention",
      eligible_var = "eligible",
      argset = list(age_group = "50_60", age_min = 50, age_max = 60)
    )
  }
  return(plan)
}


#' The TARGET checklist of `plan`, with every ANSI colour code removed.
.item8_checklist <- function(plan) {
  return(.item8_strip_ansi(
    paste(capture.output(plan$print_target_checklist()), collapse = "\n")
  ))
}


test_that("Item 8 prints the global attrition count, not the global plus per-trial sum", {
  plan <- .item8_plan()
  plan$enrollment_counts <- list("01" = list(attrition = .item8_attrition()))
  full <- .item8_checklist(plan)

  expect_identical(
    .item8_captures(
      full,
      "Before exclusions:\\s+\u21b3\\s+([0-9,]+) person-trials"
    ),
    .ITEM8_GLOBAL_PT[["before_exclusions"]]
  )

  expect_identical(
    .item8_captures(
      full,
      paste0(
        "Excluding\\s+([0-9,]+) person-trials ",
        "\\(\\s*([0-9,]+) intervention person-trials, ",
        "\\s*([0-9,]+) comparator person-trials\\)"
      )
    ),
    c(
      .ITEM8_GLOBAL_PT[["before_exclusions"]] - .ITEM8_GLOBAL_PT[["eligible_age"]],
      .ITEM8_GLOBAL_INT[["before_exclusions"]] - .ITEM8_GLOBAL_INT[["eligible_age"]],
      .ITEM8_GLOBAL_CMP[["before_exclusions"]] - .ITEM8_GLOBAL_CMP[["eligible_age"]]
    )
  )

  expect_identical(
    .item8_captures(
      full,
      paste0(
        "Remaining\\s+([0-9,]+) person-trials ",
        "\\(\\s*([0-9,]+) intervention person-trials, ",
        "\\s*([0-9,]+) comparator person-trials\\)"
      )
    ),
    c(
      .ITEM8_GLOBAL_PT[["eligible_age"]],
      .ITEM8_GLOBAL_INT[["eligible_age"]],
      .ITEM8_GLOBAL_CMP[["eligible_age"]]
    )
  )
})


test_that("Item 8 prints the placeholder when one criterion carries no global row", {
  mat <- data.table::data.table(
    trial_id = c(0L, 1L),
    n_intervention_total = c(160, 200),
    n_comparator_total = c(240, 300),
    n_intervention_enrolled = c(100, 120),
    n_comparator_enrolled = c(200, 240)
  )

  plan <- .item8_plan()
  plan$enrollment_counts <- list(
    "01" = list(
      attrition = .item8_attrition(drop_global_for = "eligible_age"),
      matching = mat
    )
  )
  full <- .item8_checklist(plan)

  expect_true(grepl("Run.*s1_generate_enrollments_and_ipw.*first", full))

  # Item 8 prints no participant flow for this enrollment. A comparator-draw
  # line on its own would report the end of a cascade the reader cannot see.
  expect_false(grepl("participant flow:", full, fixed = TRUE))
  expect_false(grepl("Before exclusions:", full, fixed = TRUE))
  expect_false(grepl("After the comparator draw", full, fixed = TRUE))
  expect_false(grepl("1,000 person-trials", full, fixed = TRUE))
})


test_that("one incomplete enrollment suppresses the flow of a complete one", {
  # Positive control. Enrollment 01 alone prints its flow, so the placeholder
  # asserted below is caused by enrollment 02 and by nothing else.
  alone <- .item8_plan(ids = "01")
  alone$enrollment_counts <- list("01" = list(attrition = .item8_attrition()))
  full_alone <- .item8_checklist(alone)

  expect_true(
    grepl("Enrollment '01' participant flow:", full_alone, fixed = TRUE)
  )
  expect_true(grepl("1,000 person-trials", full_alone, fixed = TRUE))

  # Enrollment 01 is unchanged and complete. Enrollment 02 lacks one global
  # row. Item 8 is all or nothing, so neither flow prints.
  plan <- .item8_plan(ids = c("01", "02"))
  plan$enrollment_counts <- list(
    "01" = list(attrition = .item8_attrition()),
    "02" = list(attrition = .item8_attrition(drop_global_for = "eligible_age"))
  )
  full <- .item8_checklist(plan)

  expect_true(grepl("Run.*s1_generate_enrollments_and_ipw.*first", full))
  expect_false(grepl("participant flow:", full, fixed = TRUE))
  expect_false(grepl("Before exclusions:", full, fixed = TRUE))
  expect_false(grepl("1,000 person-trials", full, fixed = TRUE))
  expect_false(grepl("760 person-trials", full, fixed = TRUE))
})
