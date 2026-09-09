# The prevalent-user guard warns when no washout exclusion covers an
# enrollment's intervention level. An enrollment classifies a person-band as
# "intervention" from the weeks at that level, with no built-in initiation
# rule, so without a covering washout prevalent users enrol as intervention at
# every eligible band and discontinuers flip to comparator.
#
# `tteplan_validate_spec()` runs the guard, because it receives the first
# skeleton batch and can measure containment. `tteplan_read_spec()` reads no
# data and carries no guard.

skip_if_not_installed("data.table")

# A washout as an enrollment-level `additional_exclusion` block. `source`
# carries the pre-indented `source_variable` lines.
.newuser_washout_enrollment <- function(source, level) {
  paste0(
    "    additional_exclusion:\n",
    "      - name: \"Prior treatment\"\n",
    "        implementation:\n",
    "          type: \"no_prior_intervention\"\n",
    source,
    "          intervention_value: ",
    level,
    "\n",
    "          window: \"lifetime_before_baseline\"\n",
    "          computed: true\n"
  )
}

# The same washout as a global `exclusion_criteria` block.
.newuser_washout_global <- function(source, level) {
  paste0(
    "exclusion_criteria:\n",
    "  - name: \"Prior treatment (global)\"\n",
    "    implementation:\n",
    "      type: \"no_prior_intervention\"\n",
    source,
    "      intervention_value: ",
    level,
    "\n",
    "      window: \"lifetime_before_baseline\"\n",
    "      computed: true\n"
  )
}

.newuser_spec_yaml <- function(enrollment_washout = "", global_washout = "") {
  paste0(
    "study:\n",
    "  title: \"New-user warning test\"\n",
    "  implementation:\n",
    "    project_prefix: nu_test\n",
    "    version: v001\n",
    global_washout,
    "inclusion_criteria:\n",
    "  isoyears: [2010, 2015]\n",
    "enrollments:\n",
    "  - id: \"01\"\n",
    "    name: \"A vs B\"\n",
    "    observed_var:\n",
    "      sentinel: row_presence\n",
    "    intervention_tolerance_weeks: 0\n",
    "    comparator_tolerance_weeks: 0\n",
    "    additional_inclusion:\n",
    "      - name: \"Age 50-59\"\n",
    "        type: \"age_range\"\n",
    "        min: 50\n",
    "        max: 59\n",
    "        implementation:\n",
    "          variable: rd_age_continuous\n",
    enrollment_washout,
    "    treatment:\n",
    "      arms:\n",
    "        intervention: \"Arm A\"\n",
    "        comparator: \"Arm B\"\n",
    "      implementation:\n",
    "        comparator_to_intervention_ratio: 2\n",
    "        variable: rd_tx\n",
    "        intervention_value: a\n",
    "        comparator_value: b\n",
    "        seed: 1\n",
    "outcomes:\n",
    "  - name: \"Outcome X\"\n",
    "    implementation:\n",
    "      variable: os_x\n",
    "follow_up:\n",
    "  - weeks: 52\n"
  )
}

# Two persons, two weeks each. Person 1 is on arm "a" in both weeks.
# `rd_parent` holds "wide" on exactly those two weeks, so it is a parent of
# the treatment level. `rd_other` never holds the level a washout on it names.
# `rd_src1` and `rd_src2` each cover one of the two intervention weeks.
.newuser_skeleton <- function() {
  data.table::data.table(
    id = c(1L, 1L, 2L, 2L),
    isoyearweek = c("2010-01", "2010-02", "2010-01", "2010-02"),
    is_isoyear = FALSE,
    rd_age_continuous = c(52, 52, 53, 53),
    rd_tx = c("a", "a", "b", "b"),
    rd_parent = c("wide", "wide", "narrow", "narrow"),
    rd_other = "y",
    rd_src1 = c(TRUE, FALSE, FALSE, FALSE),
    rd_src2 = c(FALSE, TRUE, FALSE, FALSE),
    os_x = FALSE
  )
}

.newuser_validate <- function(yaml_txt, skel, batch = 1L) {
  f <- tempfile(fileext = ".yaml")
  on.exit(unlink(f), add = TRUE)
  writeLines(yaml_txt, f)
  suppressMessages({
    spec <- swereg::tteplan_read_spec(f)
    swereg::tteplan_validate_spec(spec, skel, skeleton_batch = batch)
  })
}

test_that("a spec with no washout exclusion warns", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  expect_warning(
    .newuser_validate(.newuser_spec_yaml(), .newuser_skeleton()),
    "prevalent users will enrol"
  )
})

test_that("a washout on the parent column covers the intervention level", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_parent\n",
      "wide"
    )
  )
  expect_no_warning(.newuser_validate(yaml_txt, .newuser_skeleton()))
})

test_that("a washout on an unrelated column warns and counts the weeks", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_other\n",
      "z"
    )
  )
  expect_warning(
    .newuser_validate(yaml_txt, .newuser_skeleton(), batch = 3L),
    paste0(
      "On skeleton batch 3, 2 of 2 weeks at rd_tx == \"a\" ",
      "are outside every washout"
    ),
    fixed = TRUE
  )
})

test_that("a washout on the treatment column at the wrong level warns", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_tx\n",
      "b"
    )
  )
  expect_warning(
    .newuser_validate(yaml_txt, .newuser_skeleton()),
    "2 of 2 weeks at rd_tx == \"a\"",
    fixed = TRUE
  )
})

test_that("a missing value in the washout column counts as uncovered", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  skel <- .newuser_skeleton()
  skel[2L, rd_parent := NA_character_]
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_parent\n",
      "wide"
    )
  )
  expect_warning(
    .newuser_validate(yaml_txt, skel),
    "1 of 2 weeks at rd_tx == \"a\"",
    fixed = TRUE
  )
})

test_that("two sources of one global washout cover the level jointly", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  # A multi-source washout covers through the union of its sources. Neither
  # source covers on its own, so the second block proves the union is what
  # silences the warning.
  both <- .newuser_spec_yaml(
    global_washout = .newuser_washout_global(
      paste0(
        "      source_variable:\n",
        "        - rd_src1\n",
        "        - rd_src2\n"
      ),
      "true"
    )
  )
  expect_no_warning(.newuser_validate(both, .newuser_skeleton()))

  one <- .newuser_spec_yaml(
    global_washout = .newuser_washout_global(
      "      source_variable:\n        - rd_src1\n",
      "true"
    )
  )
  expect_warning(
    .newuser_validate(one, .newuser_skeleton()),
    "1 of 2 weeks at rd_tx == \"a\"",
    fixed = TRUE
  )
})

test_that("two sources of one enrollment washout cover the level jointly", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  # The same union, routed through an enrollment's additional_exclusion
  # instead of the global exclusion_criteria. The enrollment-level column
  # check MUST be vectorised, or validation stops with "the condition has
  # length > 1" before the guard runs.
  both <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      paste0(
        "          source_variable:\n",
        "            - rd_src1\n",
        "            - rd_src2\n"
      ),
      "true"
    )
  )
  # It validates: no error, and the "passed" message names both columns.
  expect_true(.newuser_validate(both, .newuser_skeleton()))
  expect_no_warning(.newuser_validate(both, .newuser_skeleton()))

  one <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable:\n            - rd_src1\n",
      "true"
    )
  )
  expect_warning(
    .newuser_validate(one, .newuser_skeleton()),
    "1 of 2 weeks at rd_tx == \"a\"",
    fixed = TRUE
  )
})

test_that("a multi-source enrollment washout names every missing column", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      paste0(
        "          source_variable:\n",
        "            - rd_absent_one\n",
        "            - rd_absent_two\n"
      ),
      "true"
    )
  )
  expect_error(
    .newuser_validate(yaml_txt, .newuser_skeleton()),
    paste0(
      "additional_exclusion source_variable ",
      "'rd_absent_one', 'rd_absent_two' not found in skeleton"
    ),
    fixed = TRUE
  )
})

test_that("only weekly rows are measured", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  # The annual row sits at the intervention level and no washout covers it.
  # A person cannot initiate in a year, so the guard MUST NOT read it.
  skel <- rbind(
    .newuser_skeleton(),
    data.table::data.table(
      id = 1L,
      isoyearweek = NA_character_,
      is_isoyear = TRUE,
      rd_age_continuous = 52,
      rd_tx = "a",
      rd_parent = NA_character_,
      rd_other = "y",
      rd_src1 = FALSE,
      rd_src2 = FALSE,
      os_x = FALSE
    )
  )
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_parent\n",
      "wide"
    )
  )
  expect_no_warning(.newuser_validate(yaml_txt, skel))
})

test_that("the option silences the warning", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = FALSE)
  expect_no_warning(
    .newuser_validate(.newuser_spec_yaml(), .newuser_skeleton())
  )
})

test_that("tteplan_read_spec() carries no prevalent-user guard", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  f <- withr::local_tempfile(fileext = ".yaml")
  writeLines(.newuser_spec_yaml(), f)
  expect_no_warning(suppressMessages(swereg::tteplan_read_spec(f)))
})

test_that("the build says it skipped the check when no skeleton is loaded", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  root <- withr::local_tempdir()
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  for (d in c(dir_spec, dir_tteplan, dir_results)) {
    dir.create(d, recursive = TRUE)
  }
  writeLines(.newuser_spec_yaml(), file.path(dir_spec, "spec_v001.yaml"))

  msgs <- NULL
  expect_no_warning(
    msgs <- testthat::capture_messages(
      swereg::tteplan_from_spec_and_registrystudy(
        study = list(
          skeleton_files = file.path(dir_tteplan, "skeleton_00001.qs2")
        ),
        candidate_dir_spec = dir_spec,
        candidate_dir_tteplan = dir_tteplan,
        candidate_dir_results = dir_results,
        spec_version = "v001",
        global_max_isoyearweek = "2015-52"
      )
    )
  )
  expect_true(any(grepl(
    "prevalent-user check skipped: no skeleton loaded",
    msgs,
    fixed = TRUE
  )))
})
