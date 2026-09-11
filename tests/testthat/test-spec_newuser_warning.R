# The prevalent-user guard warns when no washout rule covers an enrollment's
# intervention level. An enrollment classifies a person-band as "intervention"
# from the weeks at that level, with no built-in initiation rule, so without a
# covering washout prevalent users enrol as intervention at every eligible band
# and discontinuers flip to comparator.
#
# A prevalent week is a week at the intervention level that follows an earlier
# week of the same person at that level. A washout covers the enrollment when
# it makes every prevalent week ineligible. The guard evaluates the eligibility
# expression the compiler builds, so a first initiation stays eligible and is
# not an uncovered week.
#
# `tteplan_validate_spec()` runs the guard, because it receives the first
# skeleton batch and can measure coverage. `tteplan_read_spec()` reads no data
# and carries no guard.

skip_if_not_installed("data.table")

# A washout as an enrollment-level `additional_exclusion` block. `source`
# carries the pre-indented `source_variable` lines.
.newuser_washout_enrollment <- function(source, level, type = "no_prior_value") {
  paste0(
    "    additional_exclusion:\n",
    "      - name: \"Prior treatment\"\n",
    "        implementation:\n",
    "          type: \"",
    type,
    "\"\n",
    source,
    "          value: ",
    level,
    "\n",
    "          window: \"lifetime_before_baseline\"\n",
    "          computed: true\n"
  )
}

# The same washout as a global `exclusion_criteria` block.
.newuser_washout_global <- function(source, level, type = "no_prior_value") {
  paste0(
    "exclusion_criteria:\n",
    "  - name: \"Prior treatment (global)\"\n",
    "    implementation:\n",
    "      type: \"",
    type,
    "\"\n",
    source,
    "      value: ",
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

# Three persons, two weeks each. Persons 1 and 3 are on arm "a" in both weeks,
# so each has exactly one prevalent week: their second week. Person 2 is on
# arm "b" and supplies the comparator level the validator checks for.
#
# `rd_parent` holds "wide" in every week of both intervention persons, so a
# washout on it excludes both prevalent weeks. `rd_other` never holds the level
# a washout on it names. `rd_src1` covers person 1 alone and `rd_src2` covers
# person 3 alone, so neither source covers on its own and the union covers
# both.
.newuser_skeleton <- function() {
  data.table::data.table(
    id = c(1L, 1L, 2L, 2L, 3L, 3L),
    isoyearweek = rep(c("2010-01", "2010-02"), 3),
    is_isoyear = FALSE,
    rd_age_continuous = rep(c(52, 53, 54), each = 2),
    rd_tx = c("a", "a", "b", "b", "a", "a"),
    rd_parent = c("wide", "wide", "narrow", "narrow", "wide", "wide"),
    rd_other = "y",
    rd_src1 = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    rd_src2 = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    os_x = FALSE
  )
}

# Person 1 initiates arm "a" in week 3, after two weeks on arm "b". Person 2
# never leaves arm "b". Every person at the intervention level is therefore a
# clean initiator, and the one prevalent week is person 1's week 4.
.newuser_naive_skeleton <- function() {
  data.table::data.table(
    id = rep(c(1L, 2L), each = 4),
    isoyearweek = rep(sprintf("2010-%02d", 1:4), 2),
    is_isoyear = FALSE,
    rd_age_continuous = rep(c(52, 53), each = 4),
    rd_tx = c("b", "b", "a", "a", "b", "b", "b", "b"),
    rd_parent = "narrow",
    rd_other = "y",
    rd_src1 = FALSE,
    rd_src2 = FALSE,
    os_x = FALSE
  )
}

# The uncovered prevalent weeks each washout of one enrollment leaves. It runs
# the guard's own three steps, so the number the test reports is the number the
# guard measured.
.newuser_uncovered <- function(spec, skel) {
  enr <- spec$enrollments[[1]]
  weekly <- which(skel[["is_isoyear"]] %in% FALSE)
  prevalent <- swereg:::.tte_prevalent_positions(
    skel,
    weekly,
    enr$treatment$implementation$variable,
    enr$treatment$implementation$intervention_value
  )
  washouts <- swereg:::.tte_washouts(spec, enr)
  return(vapply(
    washouts,
    function(w) {
      sum(!swereg:::.tte_washout_ineligible(skel, weekly, w)[prevalent])
    },
    numeric(1)
  ))
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
      "On skeleton batch 3, 2 prevalent weeks at rd_tx == \"a\". ",
      "Washout 'Prior treatment' leaves 2 uncovered."
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
    "2 prevalent weeks at rd_tx == \"a\"",
    fixed = TRUE
  )
})

test_that("a missing value in a prior week leaves the week uncovered", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  # `no_prior_value` reads the prior weeks alone, so the missing value has to
  # sit in one of them. Person 1 week 1 is the only week before her prevalent
  # week. Person 3 keeps both "wide" weeks and stays covered.
  skel <- .newuser_skeleton()
  skel[1L, rd_parent := NA_character_]
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_parent\n",
      "wide"
    )
  )
  expect_warning(
    .newuser_validate(yaml_txt, skel),
    "Washout 'Prior treatment' leaves 1 uncovered.",
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
    "Washout 'Prior treatment (global)' leaves 1 uncovered.",
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
    "Washout 'Prior treatment' leaves 1 uncovered.",
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
  # Person 2 gets two annual rows at the intervention level. Her weekly rows
  # never reach that level, and `rd_parent` never holds "wide" for her, so the
  # second annual row would be an uncovered prevalent week. A person cannot
  # initiate in a year, so the guard MUST NOT read either row.
  skel <- rbind(
    .newuser_skeleton(),
    data.table::data.table(
      id = 2L,
      isoyearweek = NA_character_,
      is_isoyear = TRUE,
      rd_age_continuous = 53,
      rd_tx = "a",
      rd_parent = "narrow",
      rd_other = "y",
      rd_src1 = FALSE,
      rd_src2 = FALSE,
      os_x = FALSE
    )[rep(1L, 2L)]
  )
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_parent\n",
      "wide"
    )
  )
  expect_no_warning(.newuser_validate(yaml_txt, skel))
})

test_that("an only_prior_value washout on the treatment column covers it", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  # Every prior week of the prevalent week holds arm "a", which is not "b", so
  # the rule excludes it. The initiation week itself has only "b" before it and
  # stays eligible.
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_tx\n",
      "b",
      type = "only_prior_value"
    )
  )
  expect_no_warning(
    .newuser_validate(yaml_txt, .newuser_naive_skeleton())
  )
})

test_that("removing the only_prior_value washout raises the warning", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  expect_warning(
    .newuser_validate(.newuser_spec_yaml(), .newuser_naive_skeleton()),
    "No washout applies to this enrollment.",
    fixed = TRUE
  )
})

test_that("a clean initiator leaves no uncovered week", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  # Every person at the intervention level initiated from arm "b". The guard
  # measures one prevalent week and the rule covers it, so the count is 0 and
  # no warning fires.
  yaml_txt <- .newuser_spec_yaml(
    enrollment_washout = .newuser_washout_enrollment(
      "          source_variable: rd_tx\n",
      "b",
      type = "only_prior_value"
    )
  )
  f <- withr::local_tempfile(fileext = ".yaml")
  writeLines(yaml_txt, f)
  spec <- suppressMessages(swereg::tteplan_read_spec(f))
  skel <- .newuser_naive_skeleton()
  expect_identical(.newuser_uncovered(spec, skel), 0)
  expect_no_warning(
    suppressMessages(swereg::tteplan_validate_spec(spec, skel))
  )
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
