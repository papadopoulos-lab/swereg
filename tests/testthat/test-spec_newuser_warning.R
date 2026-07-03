# tteplan_read_spec() must warn when an enrollment has no new-user/washout
# exclusion tied to its treatment variable: without one, prevalent users
# enrol as "intervention" at every eligible band (prevalent-user design) and
# discontinuers flip to comparator. The guard is spec-driven and used to be
# silent when omitted.

.newuser_spec_yaml <- function(with_newuser_exclusion) {
  extra <- if (with_newuser_exclusion) {
    paste0(
      "    additional_exclusion:\n",
      "      - name: \"Prior treatment\"\n",
      "        implementation:\n",
      "          type: \"no_prior_intervention\"\n",
      "          source_variable: rd_tx\n",
      "          intervention_value: a\n",
      "          window: \"lifetime_before_baseline\"\n",
      "          computed: true\n"
    )
  } else {
    ""
  }
  paste0(
    "study:\n",
    "  title: \"New-user warning test\"\n",
    "  implementation:\n",
    "    project_prefix: nu_test\n",
    "inclusion_criteria:\n",
    "  isoyears: [2010, 2015]\n",
    "enrollments:\n",
    "  - id: \"01\"\n",
    "    name: \"A vs B\"\n",
    extra,
    "    treatment:\n",
    "      arms:\n",
    "        intervention: \"Arm A\"\n",
    "        comparator: \"Arm B\"\n",
    "      implementation:\n",
    "        matching_ratio: 2\n",
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

test_that("spec without a new-user exclusion on the treatment variable warns", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  f <- withr::local_tempfile(fileext = ".yaml")
  writeLines(.newuser_spec_yaml(with_newuser_exclusion = FALSE), f)
  expect_warning(
    tteplan_read_spec(f),
    "prevalent users will enrol"
  )
})

test_that("spec with a no_prior_intervention exclusion does not warn", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  f <- withr::local_tempfile(fileext = ".yaml")
  writeLines(.newuser_spec_yaml(with_newuser_exclusion = TRUE), f)
  expect_no_warning(tteplan_read_spec(f))
})

test_that("a global exclusion referencing the treatment variable also counts", {
  skip_if_not_installed("yaml")
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  f <- withr::local_tempfile(fileext = ".yaml")
  yaml_txt <- sub(
    "enrollments:",
    paste0(
      "exclusion_criteria:\n",
      "  - name: \"Prior treatment (global)\"\n",
      "    implementation:\n",
      "      source_variable: rd_tx\n",
      "      window: \"lifetime_before_baseline\"\n",
      "      computed: true\n",
      "enrollments:"
    ),
    .newuser_spec_yaml(with_newuser_exclusion = FALSE),
    fixed = TRUE
  )
  writeLines(yaml_txt, f)
  expect_no_warning(tteplan_read_spec(f))
})
