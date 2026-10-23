# `$get_protocol_table()` returns the target trial protocol table in memory,
# so a manuscript can build its protocol table from the spec.
#
# Two properties, pinned separately:
#
#   1. The accessor returns the table the "Target trial protocol" sheet
#      writes. The tests read the sheet back from a saved workbook.
#   2. The emulation column names each variable, outcome, exposure level and
#      criterion by the spec's `name` or arm label. Every code in the fixture
#      differs from its name, so a line that prints a code cannot match the
#      expected text. The expected lines are typed here, not computed.
#
# Two variables carry no name: the treatment variable, which the schema gives
# no `name` field, and one confounder. Both keep their column names.

skip_if_not_installed("openxlsx")
skip_if_not_installed("data.table")

# --- fixture ---------------------------------------------------------------

.pta_spec <- function() {
  return(list(
    study = list(
      title = "Protocol table accessor fixture",
      implementation = list(project_prefix = "pta", version = "v001")
    ),
    inclusion_criteria = list(
      isoyears = c(2010L, 2020L),
      criteria = list(
        list(
          name = "Registered in the drug register",
          type = "has_event",
          implementation = list(
            source_variable = "ri_in_drugreg",
            source_variable_combined = "ri_in_drugreg",
            window = "lifetime_before_baseline"
          )
        )
      )
    ),
    exclusion_criteria = list(
      list(
        name = "Prior event of interest",
        implementation = list(
          source_variable = "osd_event",
          source_variable_combined = "osd_event",
          window = 104,
          computed = TRUE
        )
      )
    ),
    confounders = list(
      list(
        name = "Age at baseline",
        implementation = list(variable = "rd_age_cont")
      ),
      list(
        name = "Highest education",
        implementation = list(variable = "rd_edu_level")
      ),
      # No `name`: the variable keeps its column name.
      list(implementation = list(variable = "rd_income_unnamed"))
    ),
    outcomes = list(
      list(
        name = "Event of interest",
        role = "primary",
        implementation = list(variable = "osd_event")
      ),
      list(
        name = "Second event",
        role = "secondary",
        implementation = list(variable = "osd_second")
      )
    ),
    follow_up = list(
      list(label = "1 year", weeks = 52),
      list(label = "5 years", weeks = 260)
    ),
    min_expected_events_per_arm_per_year = 0,
    enrollments = list(
      list(
        id = "01",
        name = "Drug A vs Drug B, age 50-54",
        additional_inclusion = list(
          list(
            name = "Age 50-54",
            type = "age_range",
            min = 50,
            max = 54,
            implementation = list(variable = "rd_age_cont")
          )
        ),
        additional_exclusion = list(
          list(
            name = "Earlier use of Drug A",
            implementation = list(
              type = "no_prior_value",
              source_variable = "rd_tx_code",
              source_variable_combined = "rd_tx_code",
              value = "tx_a",
              window = "lifetime_before_baseline",
              computed = TRUE
            )
          )
        ),
        treatment = list(
          description = "Start of Drug A compared with start of Drug B.",
          arms = list(intervention = "Drug A", comparator = "Drug B"),
          implementation = list(
            comparator_to_intervention_ratio = 2,
            variable = "rd_tx_code",
            intervention_value = "tx_a",
            comparator_value = "tx_b",
            seed = 7
          )
        )
      )
    ),
    target_trial = list(
      causal_contrast = list(specification = "Per-protocol effect (fixture)")
    )
  ))
}

# Every code above that the spec gives a name or an arm label.
.PTA_NAMED_CODES <- c(
  "ri_in_drugreg",
  "osd_event",
  "rd_age_cont",
  "rd_edu_level",
  "osd_second",
  "tx_a",
  "tx_b"
)

.pta_plan <- function() {
  ett <- data.table::data.table(
    ett_id = c("ETT00001", "ETT00002"),
    enrollment_id = "01",
    follow_up = c(52L, 260L),
    outcome_var = c("osd_event", "osd_second"),
    outcome_name = c("Event of interest", "Second event"),
    age_min = 50L,
    age_max = 54L,
    confounder_vars = "rd_age_cont, rd_edu_level, rd_income_unnamed",
    person_id_var = "lopnr",
    treatment_var = "rd_tx_code"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "pta",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$spec <- .pta_spec()
  return(plan)
}

# The protocol table as the sheet writer writes it, read back from disk.
.pta_sheet <- function(plan, ett_id) {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_protocol_table(wb, "Target trial protocol", plan, ett_id)
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  return(openxlsx::read.xlsx(
    f,
    sheet = "Target trial protocol",
    startRow = 3,
    sep.names = " "
  ))
}

# One emulation cell of the accessor's table, as lines.
.pta_lines <- function(tab, component) {
  i <- which(tab[["Protocol component"]] == component)
  expect_length(i, 1L)
  return(strsplit(tab[["Target trial emulation"]][i], "\n", fixed = TRUE)[[1]])
}

# --- 1. the accessor returns the sheet's table -----------------------------

test_that("get_protocol_table() returns the table the protocol sheet writes", {
  plan <- .pta_plan()
  for (id in list(NULL, "ETT00001", "ETT00002")) {
    tab <- plan$get_protocol_table(id)
    sheet <- .pta_sheet(plan, id)
    expect_s3_class(tab, "data.table")
    expect_identical(dim(tab), c(7L, 3L))
    expect_identical(as.data.frame(tab), sheet)
  }
  # The two trials differ in their outcome, so the tables differ.
  expect_false(identical(
    plan$get_protocol_table("ETT00001"),
    plan$get_protocol_table("ETT00002")
  ))
  expect_error(plan$get_protocol_table("ETT99999"), "unknown ett_id")

  # The public writer `$excel_spec_summary()` writes the same table. Its
  # Study Specification sheet stops on a confounder without a `name`, so this
  # plan names the third confounder.
  plan$spec$confounders[[3]]$name <- "Household income"
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(plan$excel_spec_summary(f, protocol_ett_id = "ETT00002"))
  expect_identical(
    as.data.frame(plan$get_protocol_table("ETT00002")),
    openxlsx::read.xlsx(
      f,
      sheet = "Target trial protocol",
      startRow = 3,
      sep.names = " "
    )
  )
})

# --- 2. the emulation column prints the spec's names -----------------------

test_that("the emulation column names variables, outcomes, exposure levels and criteria by the spec", {
  tab <- .pta_plan()$get_protocol_table("ETT00001")

  expect_identical(
    .pta_lines(tab, "Eligibility criteria"),
    c(
      "Require isoyear in 2010 to 2020",
      "Require Registered in the drug register (lifetime before baseline)",
      "Drop rows where Prior event of interest is TRUE (2 years before baseline)",
      "Require Age at baseline between 50 and 54",
      paste0(
        "Earlier use of Drug A: no prior rd_tx_code equal to Drug A ",
        "(lifetime before baseline)"
      )
    )
  )
  expect_identical(
    .pta_lines(tab, "Treatment strategies"),
    c(
      "Treatment variable: rd_tx_code",
      "Intervention value: Drug A",
      "Comparator value: Drug B"
    )
  )
  expect_true(
    "Arm values: Drug A versus Drug B" %in%
      .pta_lines(tab, "Assignment procedure")
  )
  expect_identical(
    .pta_lines(tab, "Outcome"),
    "Outcome variable: Event of interest"
  )
  expect_true(all(
    c(
      "Intervention value: Drug A",
      "Comparator value: Drug B",
      "Outcome variable: Event of interest"
    ) %in%
      .pta_lines(tab, "Causal contrast")
  ))
  plan_lines <- .pta_lines(tab, "Analysis plan")
  expect_identical(
    plan_lines[startsWith(plan_lines, "Treatment and censoring models")],
    c(
      "Treatment and censoring models adjust for Age at baseline",
      "Treatment and censoring models adjust for Highest education",
      "Treatment and censoring models adjust for rd_income_unnamed"
    )
  )

  # The second trial names its own outcome.
  tab2 <- .pta_plan()$get_protocol_table("ETT00002")
  expect_identical(.pta_lines(tab2, "Outcome"), "Outcome variable: Second event")
})

# --- 3. a variable without a name keeps its column name --------------------

test_that("a variable that the spec does not name keeps its column name", {
  tab <- .pta_plan()$get_protocol_table("ETT00001")
  # The confounder without a `name`.
  expect_true(
    "Treatment and censoring models adjust for rd_income_unnamed" %in%
      .pta_lines(tab, "Analysis plan")
  )
  # The treatment variable: the schema gives it no `name` field.
  for (component in c(
    "Treatment strategies",
    "Assignment procedure",
    "Causal contrast"
  )) {
    expect_true(
      "Treatment variable: rd_tx_code" %in% .pta_lines(tab, component),
      info = component
    )
  }
})

# --- 4. no emulation cell prints a code that the spec names ----------------

test_that("no emulation cell holds a column name or value that the spec names", {
  plan <- .pta_plan()
  for (id in plan$ett$ett_id) {
    cells <- plan$get_protocol_table(id)[["Target trial emulation"]]
    expect_length(cells, 7L)
    for (code in .PTA_NAMED_CODES) {
      hit <- grepl(paste0("\\b", code, "\\b"), cells, perl = TRUE)
      expect_false(any(hit), info = paste0(id, ": ", code))
    }
  }
})
