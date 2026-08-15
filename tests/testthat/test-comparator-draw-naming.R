# swereg never calls its own comparator draw "matching".
#
# The draw is a seeded random sample of qualified comparators, stratified by
# the entry band and by nothing else. No covariate enters it. Confounding
# adjustment is by inverse probability weighting on the covariates read at the
# recruiting week.
#
# The generated protocol table, the generated TARGET methods text and the
# CONSORT node labels all reach a manuscript. A word there that names covariate
# matching asserts a balance property that nothing established. A reviewer then
# asks for balance diagnostics for a procedure that balanced nothing.
#
# One use of the word is correct and stays: the clause that says the draw is
# NOT covariate matching. `.cdn_strip_permitted()` removes exactly that clause
# before each assertion, so the assertion cannot pass on it.

skip_if_not_installed("data.table")

.CDN_PERMITTED <- "not covariate matching"

.cdn_strip_permitted <- function(x) {
  gsub(.CDN_PERMITTED, "", x, fixed = TRUE)
}

.cdn_offenders <- function(x) {
  x <- .cdn_strip_permitted(as.character(x))
  x <- x[!is.na(x)]
  grep("match", x, ignore.case = TRUE, value = TRUE)
}

.cdn_spec <- function() {
  list(
    study = list(
      title = "Comparator draw naming fixture",
      design = "Sequential target trial emulation",
      implementation = list(project_prefix = "cdn", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2010L, 2020L)),
    exclusion_criteria = list(
      list(
        name = "Prior outcome event",
        implementation = list(
          source_variable = "osd_x",
          source_variable_combined = "osd_x",
          window = 104,
          window_weeks = 104L,
          computed = TRUE
        )
      )
    ),
    confounders = list(
      list(
        name = "Age (continuous)",
        implementation = list(variable = "rd_age_continuous")
      )
    ),
    outcomes = list(
      list(
        name = "Outcome A",
        role = "primary",
        description = "The primary fixture outcome",
        implementation = list(variable = "osd_a", variable_combined = "osd_a")
      )
    ),
    follow_up = list(list(label = "5 years", weeks = 260)),
    enrollments = list(
      list(
        id = "01",
        name = "Arm A vs Arm B, age 50-54",
        observed_var = list(sentinel = "row_presence"),
        intervention_tolerance_weeks = 0L,
        comparator_tolerance_weeks = 0L,
        additional_inclusion = list(
          list(
            name = "Age 50-54",
            type = "age_range",
            min = 50,
            max = 54,
            implementation = list(variable = "rd_age_continuous")
          )
        ),
        treatment = list(
          description = "Initiation of Arm A compared with Arm B.",
          arms = list(intervention = "Arm A", comparator = "Arm B"),
          implementation = list(
            comparator_to_intervention_ratio = 2,
            variable = "rd_tx",
            intervention_value = "arm_a",
            comparator_value = "arm_b",
            seed = 7
          )
        )
      )
    )
  )
}

.cdn_counts <- function() {
  list(
    attrition = data.table::data.table(
      enrollment_id = "01",
      trial_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_age"),
      n_persons = c(1000, 800),
      n_person_trials = c(5000, 4000),
      n_intervention = c(1000, 800),
      n_comparator = c(4000, 3200)
    ),
    matching = data.table::data.table(
      trial_id = 1L,
      n_intervention_total = 800,
      n_comparator_total = 3200,
      n_intervention_enrolled = 700,
      n_comparator_enrolled = 1400
    )
  )
}

.cdn_plan <- function() {
  ett <- data.table::data.table(
    ett_id = "ETT00001",
    enrollment_id = "01",
    age_group = "50_54",
    age_min = 50L,
    age_max = 54L,
    follow_up = 260L,
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_description = "The primary fixture outcome",
    outcome_role = "primary",
    description = "ETT00001",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    comparator_to_intervention_ratio = 2L,
    seed = 7L
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "cdn",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$spec <- .cdn_spec()
  plan$enrollment_counts <- list(`01` = .cdn_counts())
  plan
}


test_that("the generated protocol table never calls the comparator draw matching", {
  plan <- .cdn_plan()
  ctx <- swereg:::.protocol_context(plan, "ETT00001")
  tab <- swereg:::.build_protocol_table(plan$spec, ctx)
  cells <- unlist(lapply(tab, as.character), use.names = FALSE)
  console <- utils::capture.output(plan$print_spec_summary())

  expect_gt(length(cells), 0L)
  expect_gt(length(console), 0L)
  # The ratio and the seed must still reach the output, so the assertions
  # below cannot pass on an empty table or on empty console text.
  expect_true(any(grepl("Comparator ratio: 1:2", cells, fixed = TRUE)))
  expect_true(any(grepl("Comparator draw seed: 7", cells, fixed = TRUE)))
  expect_true(any(grepl("Comparator ratio: 1:2", console, fixed = TRUE)))
  expect_identical(.cdn_offenders(cells), character(0))
  expect_identical(.cdn_offenders(console), character(0))
})


test_that("the generated TARGET methods text never calls the comparator draw matching", {
  plan <- .cdn_plan()
  txt <- utils::capture.output(plan$print_target_checklist())

  expect_gt(length(txt), 0L)
  # Items 6c and 7c are the two paragraphs that describe assignment. Both must
  # be present, or the assertion below passes on absent text.
  expect_gte(sum(grepl("seeded random draw", txt, fixed = TRUE)), 2L)
  expect_true(any(grepl(
    "Assignment (6c): Comparator individuals entered by a seeded random draw",
    txt,
    fixed = TRUE
  )))
  expect_true(any(grepl(.CDN_PERMITTED, txt, fixed = TRUE)))
  # Item 8 reports the counts after the draw.
  expect_true(any(grepl("After the comparator draw:", txt, fixed = TRUE)))
  expect_identical(.cdn_offenders(txt), character(0))
})


test_that("the CONSORT flow and node labels never call the comparator draw matching", {
  flow <- swereg:::.build_cohort_flow(
    .cdn_counts(),
    analysis_n = 2050,
    analysis_n_intervention = 690,
    analysis_n_comparator = 1360
  )
  dot <- swereg:::.build_consort_dot(
    flow = flow,
    eid = "01",
    label = "Arm A vs Arm B",
    intervention_label = "Arm A",
    comparator_label = "Arm B"
  )

  expect_true("selection" %in% flow$kind)
  expect_true(grepl("Enrolled after the comparator draw", dot, fixed = TRUE))
  expect_identical(
    .cdn_offenders(c(flow$step, flow$kind, flow$change_kind)),
    character(0)
  )
  expect_identical(.cdn_offenders(strsplit(dot, "\n")[[1]]), character(0))
})
