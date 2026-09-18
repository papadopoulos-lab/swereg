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
# These tests separate the rules Item 8 follows:
#   - it reads the global rows, so the printed count is the global count;
#   - a step line carries the total and no arm count, because the arm at a
#     step is a difference of two re-derived levels and can be negative;
#   - arm counts appear at a LEVEL: the eligible cohort, the comparator draw
#     and the analysis dataset;
#   - the analysis line is absent until `$s3_analyze()` stores a baseline
#     panel, and it drops to the total alone when the panel stored no arm;
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
#'
#' Three criteria, not two. The last one is the eligible cohort and prints no
#' `Remaining` line, so a two-criterion table holds no intermediate step and
#' pins nothing about one.
.ITEM8_CRITERIA <- c("before_exclusions", "eligible_age", "eligible_washout")
.ITEM8_GLOBAL_PT <- c(
  before_exclusions = 1000,
  eligible_age = 760,
  eligible_washout = 610
)
.ITEM8_GLOBAL_INT <- c(
  before_exclusions = 400,
  eligible_age = 300,
  eligible_washout = 210
)
.ITEM8_GLOBAL_CMP <- c(
  before_exclusions = 600,
  eligible_age = 460,
  eligible_washout = 400
)


#' One enrollment's attrition table, over three criteria and two trials.
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
    trial_id = rep(c(NA_integer_, 0L, 1L), times = 3L),
    criterion = rep(.ITEM8_CRITERIA, each = 3L),
    n_persons = c(700, 400, 500, 520, 300, 360, 430, 250, 290),
    n_person_trials = c(
      .ITEM8_GLOBAL_PT[["before_exclusions"]], 400, 500,
      .ITEM8_GLOBAL_PT[["eligible_age"]], 300, 400,
      .ITEM8_GLOBAL_PT[["eligible_washout"]], 250, 320
    ),
    n_intervention = c(
      .ITEM8_GLOBAL_INT[["before_exclusions"]], 160, 200,
      .ITEM8_GLOBAL_INT[["eligible_age"]], 120, 160,
      .ITEM8_GLOBAL_INT[["eligible_washout"]], 100, 130
    ),
    n_comparator = c(
      .ITEM8_GLOBAL_CMP[["before_exclusions"]], 240, 300,
      .ITEM8_GLOBAL_CMP[["eligible_age"]], 180, 240,
      .ITEM8_GLOBAL_CMP[["eligible_washout"]], 150, 190
    )
  )
  if (is.null(drop_global_for)) {
    return(att)
  }
  return(att[!(is.na(trial_id) & criterion == drop_global_for)])
}


#' Every number on one printed line, as numbers.
#'
#' Item 8 writes a count with `big.mark = ","` and right-justifies it, so the
#' pattern allows the comma and the padding. It also allows a leading minus:
#' a pattern that cannot match a negative count reports one as absent instead
#' of as wrong.
.item8_line_nums <- function(x) {
  hits <- regmatches(x, gregexpr("-?[0-9][0-9,]*", x))[[1]]
  return(as.numeric(gsub(",", "", hits)))
}


#' The Item 8 participant-flow lines of `plan`, stripped and trimmed.
#'
#' The window runs from the first `participant flow:` line to the `[FILL IN]`
#' prompt that closes Item 8. A scan of the whole checklist would also read
#' the page range of the reference, `JAMA. 2025;334(12):1084-1093.`
.item8_flow_lines <- function(plan) {
  out <- .item8_strip_ansi(capture.output(plan$print_target_checklist()))
  start <- grep("participant flow:", out, fixed = TRUE)
  if (length(start) == 0L) {
    return(character(0))
  }
  ends <- grep("[FILL IN]", out, fixed = TRUE)
  ends <- ends[ends > start[1L]]
  return(trimws(out[seq(start[1L], ends[1L] - 1L)]))
}


#' The line under `header`, which carries that level's counts.
.item8_under <- function(lines, header) {
  hit <- which(lines == header)
  if (length(hit) != 1L) {
    return(NA_character_)
  }
  return(lines[hit + 1L])
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
  lines <- .item8_flow_lines(plan)

  expect_identical(
    .item8_line_nums(.item8_under(lines, "Before exclusions:")),
    .ITEM8_GLOBAL_PT[["before_exclusions"]]
  )

  # One number per step line, and it is the TOTAL. The arm at a step is a
  # difference of two re-derived levels, and that difference can be negative.
  expect_identical(
    lapply(grep("Excluding", lines, value = TRUE), .item8_line_nums),
    list(
      .ITEM8_GLOBAL_PT[["before_exclusions"]] -
        .ITEM8_GLOBAL_PT[["eligible_age"]],
      .ITEM8_GLOBAL_PT[["eligible_age"]] -
        .ITEM8_GLOBAL_PT[["eligible_washout"]]
    )
  )

  # `eligible_washout` is the last criterion and prints no `Remaining` line:
  # it IS the eligible cohort, which the block below reports with its split.
  expect_identical(
    lapply(grep("Remaining", lines, value = TRUE), .item8_line_nums),
    list(.ITEM8_GLOBAL_PT[["eligible_age"]])
  )

  expect_identical(
    .item8_line_nums(.item8_under(lines, "Eligible cohort:")),
    c(
      .ITEM8_GLOBAL_PT[["eligible_washout"]],
      .ITEM8_GLOBAL_INT[["eligible_washout"]],
      .ITEM8_GLOBAL_CMP[["eligible_washout"]]
    )
  )
})


test_that("Item 8 prints no analysis line before the baseline panel exists", {
  # An s1-shaped plan: attrition stored, `$s3_analyze()` never run. The
  # analysis set does not exist yet, and its absence here is correct.
  plan <- .item8_plan()
  plan$enrollment_counts <- list("01" = list(attrition = .item8_attrition()))
  lines <- .item8_flow_lines(plan)

  # The positive control. An empty window satisfies every absence below.
  expect_true(any(grepl("Eligible cohort:", lines, fixed = TRUE)))
  expect_identical(nrow(plan$get_baselines()), 0L)
  expect_false(any(grepl("Analysis dataset", lines, fixed = TRUE)))
})


test_that("Item 8 prints the analysis set with its arm split", {
  plan <- .item8_plan()
  plan$enrollment_counts <- list("01" = list(attrition = .item8_attrition()))
  # The shape `$s3_analyze()` stores. `$get_baselines()` reads the three
  # counts from it, and `.baseline_count()` reports them one at a time.
  plan$results_enrollment <- list(
    "01" = list(
      n_baseline = 500,
      n_baseline_intervention = 180,
      n_baseline_comparator = 320
    )
  )
  lines <- .item8_flow_lines(plan)

  expect_identical(
    .item8_line_nums(.item8_under(lines, "Analysis dataset (per-protocol):")),
    c(500, 180, 320)
  )
})


test_that("Item 8 prints the analysis total alone when the arm counts are NA", {
  # A panel that stored the total and neither arm. The CONSORT analysis box
  # falls back to the total the same way, and Item 8 matches it.
  plan <- .item8_plan()
  plan$enrollment_counts <- list("01" = list(attrition = .item8_attrition()))
  plan$results_enrollment <- list("01" = list(n_baseline = 500))
  lines <- .item8_flow_lines(plan)

  analysis <- .item8_under(lines, "Analysis dataset (per-protocol):")
  expect_identical(.item8_line_nums(analysis), 500)
  expect_false(grepl("(", analysis, fixed = TRUE))
  # The arm counts are what is absent, and nothing else is.
  expect_true(is.na(swereg:::.baseline_count(
    plan$get_baselines(),
    "01",
    "n_baseline_intervention"
  )))
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
