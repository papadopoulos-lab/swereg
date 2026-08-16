# The comparator-to-intervention ratio prints one direction in every generated
# artefact.
#
# `comparator_to_intervention_ratio` sizes the comparator side of the draw, so
# the spec's number belongs on the LEFT of the colon. A spec value of 3 prints
# `3:1` and never `1:3`.
#
# Four artefacts carry the number and a manuscript reader meets them together:
# the protocol table, the console spec summary, the Excel "Study
# Specification" sheet and TARGET item 6b. Two of them printed `1:2` while the
# vignettes printed `2:1`, so the same number read both ways.
#
# The fixture uses 3, not 2. Under a `1:` prefix the digit 2 sits next to a
# colon in both directions, so `1:2` and `2:1` share every character and a
# direction defect can hide.
#
# The label is asserted with the digits. `Comparator ratio: 1:3` is ambiguous
# even after the direction is fixed, because the label names neither side.

skip_if_not_installed("data.table")
skip_if_not_installed("openxlsx")

.crd_ratio <- 3L

.crd_spec <- function() {
  list(
    study = list(
      title = "Comparator ratio direction fixture",
      design = "Sequential target trial emulation",
      implementation = list(project_prefix = "crd", version = "v001")
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
            comparator_to_intervention_ratio = .crd_ratio,
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

.crd_counts <- function() {
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
      n_comparator_enrolled = 2100
    )
  )
}

.crd_plan <- function() {
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
    comparator_to_intervention_ratio = .crd_ratio,
    seed = 7L
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "crd",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$spec <- .crd_spec()
  plan$enrollment_counts <- list(`01` = .crd_counts())
  plan
}

# The Excel sheet, read back as one character vector of cells.
.crd_sheet_cells <- function(plan) {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_spec_summary(wb, plan)
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  d <- openxlsx::read.xlsx(f, sheet = "Study Specification", colNames = FALSE)
  unlist(lapply(d, as.character), use.names = FALSE)
}


test_that("every generated artefact prints the ratio as 3:1 and none prints 1:3", {
  plan <- .crd_plan()

  ctx <- swereg:::.protocol_context(plan, "ETT00001")
  protocol <- unlist(
    lapply(swereg:::.build_protocol_table(plan$spec, ctx), as.character),
    use.names = FALSE
  )
  console <- utils::capture.output(plan$print_spec_summary())
  target <- utils::capture.output(plan$print_target_checklist())
  sheet <- .crd_sheet_cells(plan)

  artefacts <- list(
    `protocol table` = protocol,
    `console spec summary` = console,
    `TARGET checklist` = target,
    `Excel spec sheet` = sheet
  )

  # Each artefact must carry text, or every assertion below passes on nothing.
  for (nm in names(artefacts)) {
    expect_gt(length(artefacts[[nm]]), 0L)
  }

  # The direction, per artefact.
  for (nm in names(artefacts)) {
    x <- artefacts[[nm]]
    expect_true(
      any(grepl("3:1", x, fixed = TRUE)),
      info = paste0(nm, ": no 3:1 anywhere")
    )
    expect_identical(
      grep("1:3", x, fixed = TRUE, value = TRUE),
      character(0),
      info = paste0(nm, ": prints the reversed 1:3")
    )
  }

  # The label names both sides wherever the digits sit beside it.
  expect_true(any(grepl(
    "Comparator-to-intervention ratio: 3:1",
    protocol,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Comparator-to-intervention ratio: 3:1",
    console,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "comparator-to-intervention ratio: 3:1)",
    target,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Comparator-to-intervention ratio:",
    sheet,
    fixed = TRUE
  )))

  # The bare label carries no digits on the Excel sheet, so the value cell is
  # what proves the direction there.
  expect_true(any(grepl("^3:1$", sheet)))

  # The retired ambiguous label is gone from every artefact.
  for (nm in names(artefacts)) {
    expect_identical(
      grep("Comparator ratio:", artefacts[[nm]], fixed = TRUE, value = TRUE),
      character(0),
      info = paste0(nm, ": still carries the ambiguous label")
    )
  }
})


test_that("no source file prints the ratio with a 1: prefix", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(root, "DESCRIPTION")) && dir.exists(file.path(root, "R")),
    "source tree not available"
  )

  files <- c(
    list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
    list.files(
      file.path(root, "vignettes"),
      pattern = "\\.Rmd$",
      full.names = TRUE
    )
  )
  files <- files[file.exists(files)]
  expect_gt(length(files), 20L)

  # `1:` immediately before the ratio, either as a literal prefix or as a
  # `sprintf` conversion. `1:5` in `fake_person_ids[1:5]` is a sequence, so the
  # patterns below require the ratio's own context.
  patterns <- c(
    "\"1:\"",
    "ratio: 1:",
    "1:%d",
    "1:%s",
    "ratio \\(e\\.g\\. 1:"
  )
  hits <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    for (p in patterns) {
      i <- grep(p, lines)
      if (length(i) > 0L) {
        hits <- c(hits, sprintf("%s:%d", basename(f), i))
      }
    }
  }
  expect_identical(hits, character())
})
