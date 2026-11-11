# The schema is the only list of legal specification paths. These tests pin
# how it classifies a path, and they need nothing outside the package.
#
# swereg 27.1.2 removed the tests that read the study specifications. Their
# result depended on the machine, and they skipped in CI. The owner of the
# specifications runs `tteplan_check_spec()` over them instead.


# The key paths swereg retired. The schema declares none of them, so each is
# refused as an unknown key.
.SCHEMA_RETIRED_PATHS <- c(
  "$/enrollments[]/treatment/implementation/matching_ratio",
  "$/horizon_rule",
  "$/inclusion_criteria/additional_inclusion",
  "$/inclusion_criteria/implementation",
  "$/inclusion_criteria/name",
  "$/inclusion_criteria/rationale",
  "$/sparse_rule"
)


test_that("a retired key path is undeclared", {
  expect_true(all(is.na(.tte_spec_key_class(.SCHEMA_RETIRED_PATHS))))
  # `matching_ratio_default` is a different key at a different path.
  expect_identical(
    .tte_spec_key_class("$/standing_methods/matching_ratio_default"),
    "metadata"
  )
  expect_identical(
    .tte_spec_key_class("$/standing_methods/matching_ratio_default/handling"),
    "metadata"
  )
})


test_that("the finding for a retired key lists the key that replaced it", {
  # The finding lists every key its context accepts. For each rename the new
  # key sits in the same context, so the reader finds it in that list.
  replaced_by <- c(
    "$/enrollments[]/treatment/implementation/matching_ratio" =
      "comparator_to_intervention_ratio",
    "$/horizon_rule" = "min_expected_events_per_arm_per_year",
    "$/inclusion_criteria/additional_inclusion" = "criteria",
    "$/sparse_rule" = "rare_condition_rule"
  )
  for (p in names(replaced_by)) {
    finding <- .tte_spec_key_finding(p)
    expect_match(finding, "Unknown key", fixed = TRUE, label = p)
    expect_match(finding, replaced_by[[p]], fixed = TRUE, label = p)
  }
})


test_that("the schema leaves an undeclared key path unclassified", {
  expect_true(is.na(.tte_spec_key_class("$/study/implementation/not_a_key")))
  expect_true(is.na(.tte_spec_key_class("$/not_a_section")))
  expect_true(is.na(.tte_spec_key_class("$/not_a_section/child")))
})


test_that("the schema has two key classes and pins the metadata set", {
  # 10 paths are accepted without being read. 4 of them are measured across
  # the 34 parseable specifications of the fleet. The other 6 are the two
  # `standing_methods` blocks no code in `R/` reads. This test is the gate on
  # the classification.
  metadata <- c(
    "$/open_questions[]/resolution",
    "$/standing_methods/admin_censoring",
    "$/standing_methods/admin_censoring/handling",
    "$/standing_methods/admin_censoring/note",
    "$/standing_methods/comparator_to_intervention_ratio_default",
    "$/standing_methods/comparator_to_intervention_ratio_default/handling",
    "$/standing_methods/comparator_to_intervention_ratio_default/note",
    "$/standing_methods/matching_ratio_default",
    "$/standing_methods/matching_ratio_default/handling",
    "$/standing_methods/matching_ratio_default/note"
  )
  expect_identical(
    .tte_spec_key_class(metadata),
    rep("metadata", length(metadata))
  )
  expect_identical(.tte_spec_paths("metadata"), sort(metadata))
  expect_setequal(
    unique(.tte_spec_key_class(.tte_spec_paths())),
    c("consumed", "metadata")
  )
  for (context in names(.TTE_SPEC_SCHEMA)) {
    expect_true(
      all(names(.TTE_SPEC_SCHEMA[[context]]) %in% c("consumed", "metadata")),
      label = context
    )
  }

  # 37 mapping contexts are measured across the 34 parseable specifications.
  # The schema declares those plus the contexts no specification uses yet.
  expect_gte(length(.TTE_SPEC_SCHEMA), 37L)
})


test_that("the schema declares the keys swereg reads but no specification uses", {
  # These are the 14 paths the schema declares beyond the 124 the fleet uses,
  # the 4 paths of the rare_condition_rule block, and the 1 path of the
  # minimum-events key. 27.2.1 added the rare-condition block as
  # `sparse_rule`, and 27.2.15 renamed it.
  # `inclusion_criteria$criteria` is the replacement container. `subgroups`,
  # `observed_var$column` and `study$implementation$conf_level` are read by
  # swereg. No specification in the fleet carried any of them when the schema
  # landed, so this test is the only thing that pins them.
  declared <- c(
    "$/inclusion_criteria/criteria",
    "$/inclusion_criteria/criteria[]/name",
    "$/inclusion_criteria/criteria[]/rationale",
    "$/inclusion_criteria/criteria[]/type",
    "$/inclusion_criteria/criteria[]/implementation",
    "$/inclusion_criteria/criteria[]/implementation/computed",
    "$/inclusion_criteria/criteria[]/implementation/source_variable",
    "$/inclusion_criteria/criteria[]/implementation/window",
    "$/subgroups",
    "$/subgroups[]/name",
    "$/subgroups[]/implementation",
    "$/subgroups[]/implementation/variable",
    "$/enrollments[]/observed_var/column",
    "$/study/implementation/conf_level",
    "$/rare_condition_rule",
    "$/rare_condition_rule/min_n",
    "$/rare_condition_rule/decided_by",
    "$/rare_condition_rule/candidates",
    "$/min_expected_events_per_arm_per_year"
  )
  expect_identical(
    .tte_spec_key_class(declared),
    rep("consumed", length(declared))
  )
})


test_that("the minimum-events key is flat and declares no window width", {
  expect_null(.TTE_SPEC_SCHEMA[["$/min_expected_events_per_arm_per_year"]])
  expect_identical(
    .tte_spec_key_class("$/min_expected_events_per_arm_per_year/window_weeks"),
    NA_character_
  )
})
