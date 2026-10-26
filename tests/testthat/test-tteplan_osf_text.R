# tteplan_osf_text() writes the OSF fields that a spec determines. Every spec
# entry MUST appear in the text, each fact MUST sit in one field, and the
# methods sentences MUST state what swereg fits.

skip_if_not_installed("yaml")
skip_if_not_installed("withr")

osf_spec_lines <- function() {
  return(c(
    "study:",
    "  title: Example study",
    "  implementation:",
    "    project_prefix: example",
    "    version: v001",
    "inclusion_criteria:",
    "  isoyears: [2010, 2020]",
    "exclusion_criteria:",
    "  - name: Prior oophorectomy",
    "    implementation:",
    "      source_variable: os_oophorectomy",
    "      window: lifetime_before_baseline",
    "  - name: Recent anticoagulant",
    "    implementation:",
    "      source_variable: os_anticoag",
    "      window: 104",
    "confounders:",
    "  - name: Age",
    "    implementation:",
    "      variable: rd_age_continuous",
    "  - name: Prior rare condition",
    "    implementation:",
    "      source_variable: os_rare",
    "      window: lifetime_before_baseline",
    "      computed: true",
    "rare_condition_rule:",
    "  min_n: 10",
    "  decided_by: ['01']",
    "  candidates: [rd_no_os_rare_everbefore]",
    "outcomes:",
    "  - name: Outcome",
    "    description: Outcome codes, recorded in specialist care.",
    "    implementation:",
    "      variable: os_outcome",
    "follow_up:",
    "  - {label: 1 year, weeks: 52}",
    "min_expected_events_per_arm_per_year: 0",
    "enrollments:",
    "  - id: '01'",
    "    name: A vs B",
    "    observed_var: {sentinel: row_presence}",
    "    intervention_tolerance_weeks: 0",
    "    comparator_tolerance_weeks: 0",
    "    additional_inclusion:",
    "      - {name: Age 50-59, type: age_range, min: 50, max: 59}",
    "      - {name: Entry 2012-2018, type: isoyear_range, min: 2012, max: 2018}",
    "    treatment:",
    "      arms: {intervention: Drug A, comparator: Drug B}",
    "      implementation:",
    "        variable: rd_tx",
    "        intervention_value: a",
    "        comparator_value: b",
    "        comparator_to_intervention_ratio: 2",
    "        seed: 1",
    "  - id: '02'",
    "    name: C vs D",
    "    observed_var: {sentinel: row_presence}",
    "    intervention_tolerance_weeks: 0",
    "    comparator_tolerance_weeks: 0",
    "    additional_inclusion:",
    "      - {name: Age 50-69, type: age_range, min: 50, max: 69}",
    "      - {name: Entry 2012-2018, type: isoyear_range, min: 2012, max: 2018}",
    "      - name: No prior Drug C",
    "        implementation:",
    "          type: no_prior_value",
    "          source_variable: rd_tx2",
    "          value: c",
    "          window: lifetime_before_baseline",
    "    treatment:",
    "      description: Drug C includes the depot form.",
    "      arms: {intervention: Drug C, comparator: Drug D}",
    "      implementation:",
    "        variable: rd_tx2",
    "        intervention_value: c",
    "        comparator_value: d",
    "        comparator_to_intervention_ratio: 2",
    "        seed: 1"
  ))
}

osf_write_spec <- function(lines = osf_spec_lines(), env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(lines, path)
  return(path)
}

osf_registry <- function(env = parent.frame()) {
  study <- swereg::RegistryStudy$new(data_rawbatch_dir = withr::local_tempdir(.local_envir = env))
  study$register_codes(
    codes = list(oophorectomy = c("LAE20", "LAF10"), anticoag = "B01AA", rare = "D50", outcome = "I26"),
    fn = swereg::add_diagnoses,
    groups = list(os = c("inpatient", "outpatient"))
  )
  return(study)
}

osf_text <- function(...) {
  return(suppressWarnings(swereg::tteplan_osf_text(...)))
}

test_that("the fields are the OSF headings, in order", {
  osf <- osf_text(osf_write_spec())
  expect_identical(names(osf), swereg:::.OSF_FIELDS)
  expect_true(all(nzchar(osf)))
})

test_that("every exclusion criterion appears, numbered, with its window", {
  x <- osf_text(osf_write_spec())[["Data inclusion and exclusion"]]
  expect_match(x, "1. Prior oophorectomy, at any time before baseline.", fixed = TRUE)
  expect_match(x, "2. Recent anticoagulant, in the 2 years before baseline.", fixed = TRUE)
})

test_that("a criterion shared by every comparison is stated once; an own one names its comparison", {
  x <- osf_text(osf_write_spec())[["Data inclusion and exclusion"]]
  expect_match(x, "2. Trial entry in ISO years 2012 to 2018.\n3. In comparison 1", fixed = TRUE)
  expect_match(x, "In comparison 1 (Drug A versus Drug B) only: Age 50 to 59 years.", fixed = TRUE)
  expect_match(x, "In comparison 2 (Drug C versus Drug D) only: Age 50 to 69 years.", fixed = TRUE)
  expect_identical(lengths(regmatches(x, gregexpr("ISO years 2012 to 2018", x))), 1L)
})

test_that("a criterion shared by some comparisons is stated once, naming each", {
  lines <- osf_spec_lines()
  first <- which(lines == "  - id: '02'")
  third <- lines[first:length(lines)]
  third[1L] <- "  - id: '03'"
  third <- sub("Drug D}", "Drug E}", third, fixed = TRUE)
  x <- osf_text(osf_write_spec(c(lines, third)))[["Data inclusion and exclusion"]]
  expect_match(x, "In comparison 2 and 3 only: No prior Drug C", fixed = TRUE)
  expect_identical(lengths(regmatches(x, gregexpr("No prior Drug C", x))), 1L)
  expect_match(x, "In comparison 1 (Drug A versus Drug B) only: Age 50 to 59 years.", fixed = TRUE)
})

test_that("with several comparisons, each enrollment period opens one trial per comparison", {
  x <- osf_text(osf_write_spec())[["Study design"]]
  expect_match(x, "each enrollment period opens one trial in each comparison.", fixed = TRUE)
  one <- osf_spec_lines()
  one <- one[seq_len(which(one == "  - id: '02'") - 1L)]
  y <- osf_text(osf_write_spec(one))[["Study design"]]
  expect_match(y, "each enrollment period opens one trial.", fixed = TRUE)
})

test_that("the minimum-events rule says the survival curve is still reported", {
  lines <- sub("min_expected_events_per_arm_per_year: 0", "min_expected_events_per_arm_per_year: 5", osf_spec_lines(), fixed = TRUE)
  x <- osf_text(osf_write_spec(lines))[["Inference criteria"]]
  expect_match(x, "Events, person-time, rates and the survival curve are still reported.", fixed = TRUE)
})

test_that("the rare-condition rule sits in the eligibility field, and the covariates point to it", {
  osf <- osf_text(osf_write_spec())
  expect_match(osf[["Data inclusion and exclusion"]], "at least 10 persons hold the condition at baseline in the smaller arm of comparison 1 (Drug A versus Drug B)", fixed = TRUE)
  expect_match(osf[["Data inclusion and exclusion"]], "- Prior rare condition", fixed = TRUE)
  expect_false(grepl("at least 10", osf[["Measured variables/Baseline covariates"]], fixed = TRUE))
  expect_match(osf[["Measured variables/Baseline covariates"]], "by the rule under 'Data inclusion and exclusion'", fixed = TRUE)
})

test_that("calendar time is in the censoring weights and not in the treatment weights", {
  x <- osf_text(osf_write_spec())[["Measured variables/Baseline covariates"]]
  expect_match(x, "Calendar time does not enter the treatment weights.", fixed = TRUE)
  expect_match(x, "The censoring weights use its value in each follow-up interval, together with the time since time zero and calendar time.", fixed = TRUE)
})

test_that("comparisons are numbered, never named by enrollment id, and carry the description", {
  x <- osf_text(osf_write_spec())[["Measured variables/Exposures"]]
  expect_match(x, "1. Drug A versus Drug B.\n2. Drug C versus Drug D. Drug C includes the depot form.", fixed = TRUE)
  expect_false(grepl("\\b0[12]\\b", x))
})

test_that("the administrative end names the last day of its ISO week", {
  x <- osf_text(osf_write_spec(), global_max_isoyearweek = "2025-01")[["Study design"]]
  expect_match(x, "at the end of ISO week 2025-01 (5 January 2025)", fixed = TRUE)
  expect_identical(swereg:::.osf_isoweek_sunday("2020-53"), "3 January 2021")
  expect_identical(swereg:::.osf_isoweek_sunday("2024-01"), "7 January 2024")
})

test_that("the code list resolves a computed covariate through its source variable", {
  x <- osf_text(osf_write_spec(), code_registry = osf_registry())[["Code list"]]
  expect_match(x, "- Prior rare condition (covariate). Codes: D50. Window: at any time before baseline.", fixed = TRUE)
  expect_match(x, "- Prior oophorectomy (exclusion). Codes: LAE20, LAF10. Window: at any time before baseline.", fixed = TRUE)
  expect_match(x, "- Outcome (outcome). Codes: I26.", fixed = TRUE)
})

test_that("the participant noun is used throughout", {
  osf <- osf_text(osf_write_spec(), people = c("woman", "women"))
  expect_match(osf[["Sample size"]], "every eligible woman in the intervention arm is enrolled", fixed = TRUE)
  expect_false(any(grepl("\\bpersons?\\b(?!-)", osf, perl = TRUE)))
  expect_error(swereg::tteplan_osf_text(osf_write_spec(), people = "woman"), "two strings")
})

test_that("an enrollment criterion that reads a variable has a code-list row naming its comparison", {
  x <- osf_text(osf_write_spec(), code_registry = osf_registry())[["Code list"]]
  expect_match(x, "- No prior Drug C (inclusion in comparison 2). Derived variable, no codes. Window: at any time before baseline.", fixed = TRUE)
  expect_false(grepl("- Age 50-59", x, fixed = TRUE))
  expect_false(grepl("|", x, fixed = TRUE))
})

test_that("the methods sentences state where each term and offset enters", {
  osf <- osf_text(osf_write_spec())
  expect_false(grepl("number of eligible persons in the data", osf[["Sample size"]], fixed = TRUE))
  expect_match(osf[["Transformations"]], "The treatment model and the time-zero censoring model are logistic regressions", fixed = TRUE)
  expect_false(grepl("Each interval enters the models with its person-time", osf[["Transformations"]], fixed = TRUE))
  expect_match(osf[["Transformations"]], "as a spline, as a factor, or not at all", fixed = TRUE)
  expect_match(osf[["Measured variables/Baseline covariates"]], "Every covariate that the rare-condition rule keeps enters both weight models.", fixed = TRUE)
  expect_false(grepl("counts as a deviation", osf[["Missing data"]], fixed = TRUE))
  expect_identical(lengths(regmatches(paste(osf, collapse = "\n"), gregexpr("analysed separately", paste(osf, collapse = "\n")))), 1L)
})

test_that("a spec without a rare-condition rule says every covariate enters", {
  lines <- osf_spec_lines()
  i <- which(lines == "rare_condition_rule:")
  lines <- lines[-(i:(i + 3L))]
  osf <- osf_text(osf_write_spec(lines))
  expect_match(osf[["Measured variables/Baseline covariates"]], "Every covariate enters both weight models.", fixed = TRUE)
  expect_false(grepl("Rare conditions", osf[["Data inclusion and exclusion"]], fixed = TRUE))
})

test_that("subgroups add their analysis and their missing-value rule", {
  lines <- c(osf_spec_lines(), "subgroups:", "  - name: Age group", "    implementation:", "      variable: rd_agegroup")
  osf <- osf_text(osf_write_spec(lines))
  expect_match(osf[["Other planned analysis"]], "within each level of Age group", fixed = TRUE)
  expect_match(osf[["Missing data"]], "A missing subgroup value removes the row from the analyses of that subgroup.", fixed = TRUE)
  expect_false(grepl("subgroup", osf_text(osf_write_spec())[["Missing data"]], fixed = TRUE))
})

test_that("the minimum-events rule sits in the Inference criteria field, after Transformations", {
  expect_identical(
    match(c("Transformations", "Inference criteria"), swereg:::.OSF_FIELDS),
    c(7L, 8L)
  )
  lines <- osf_spec_lines()
  lines[lines == "min_expected_events_per_arm_per_year: 0"] <- "min_expected_events_per_arm_per_year: 5"
  osf <- osf_text(osf_write_spec(lines))
  expect_match(
    osf[["Inference criteria"]],
    "requires at least 5 expected events per arm in each year of follow-up.",
    fixed = TRUE
  )
  # The pattern also matches a line break between the two words, so it is at
  # least as strict as the fixed phrase.
  expect_identical(sum(grepl("expected\\s+events", osf, perl = TRUE)), 1L)
  expect_match(
    osf_text(osf_write_spec())[["Inference criteria"]],
    "requires at least 0 expected events per arm in each year of follow-up, so it withholds no estimate.",
    fixed = TRUE
  )
})
