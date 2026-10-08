# The schema is the only list of legal specification paths. These tests pin
# how it classifies a path, and they need nothing outside the package.
#
# swereg 27.1.2 removed the tests that read the study specifications. Their
# result depended on the machine, and they skipped in CI. The owner of the
# specifications runs `tteplan_check_spec()` over them instead.


test_that("the two matching_ratio keys carry different classes", {
  expect_identical(
    .tte_spec_key_class(
      "$/enrollments[]/treatment/implementation/matching_ratio"
    ),
    "legacy"
  )
  expect_identical(
    .tte_spec_key_class("$/standing_methods/matching_ratio_default"),
    "metadata"
  )
  expect_identical(
    .tte_spec_key_class("$/standing_methods/matching_ratio_default/handling"),
    "metadata"
  )
})


# TRUE when `path` has a migration message that contains `needle`. Returns
# FALSE rather than NA for a path that carries no message, so a broken schema
# fails the assertion instead of erroring inside it.
msg_names <- function(path, needle) {
  msg <- .tte_spec_legacy_message(path)
  if (length(msg) != 1L || is.na(msg)) {
    return(FALSE)
  }
  return(grepl(needle, msg, fixed = TRUE))
}


test_that("each legacy key carries a migration message naming its replacement", {
  expect_true(msg_names(
    "$/enrollments[]/treatment/implementation/matching_ratio",
    "comparator_to_intervention_ratio"
  ))
  for (p in .tte_spec_paths("legacy")) {
    if (startsWith(p, "$/inclusion_criteria")) {
      expect_true(msg_names(p, "inclusion_criteria$criteria"))
    }
  }
  # A consumed key has no migration message.
  expect_true(is.na(
    .tte_spec_legacy_message("$/study/implementation/project_prefix")
  ))
})


test_that("the schema leaves an undeclared key path unclassified", {
  expect_true(is.na(.tte_spec_key_class("$/study/implementation/not_a_key")))
  expect_true(is.na(.tte_spec_key_class("$/not_a_section")))
  expect_true(is.na(.tte_spec_key_class("$/not_a_section/child")))
})


test_that("the schema pins the measured legacy and metadata sets", {
  # 15 paths are refused and 10 are accepted without being read. The 15 and 4
  # of them are measured across the 34 parseable specifications of the fleet.
  # The other 6 are the two `standing_methods` blocks no code in `R/` reads.
  # This test is the gate on the classification.
  legacy <- c(
    "$/enrollments[]/treatment/implementation/matching_ratio",
    "$/inclusion_criteria/additional_inclusion",
    "$/inclusion_criteria/additional_inclusion[]/implementation",
    "$/inclusion_criteria/additional_inclusion[]/implementation/computed",
    "$/inclusion_criteria/additional_inclusion[]/implementation/source_variable",
    "$/inclusion_criteria/additional_inclusion[]/implementation/window",
    "$/inclusion_criteria/additional_inclusion[]/name",
    "$/inclusion_criteria/additional_inclusion[]/rationale",
    "$/inclusion_criteria/additional_inclusion[]/type",
    "$/inclusion_criteria/implementation",
    "$/inclusion_criteria/implementation/computed",
    "$/inclusion_criteria/implementation/source_variable",
    "$/inclusion_criteria/implementation/window",
    "$/inclusion_criteria/name",
    "$/inclusion_criteria/rationale"
  )
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

  expect_identical(.tte_spec_key_class(legacy), rep("legacy", length(legacy)))
  expect_identical(
    .tte_spec_key_class(metadata),
    rep("metadata", length(metadata))
  )
  expect_identical(.tte_spec_paths("legacy"), sort(legacy))
  expect_identical(.tte_spec_paths("metadata"), sort(metadata))

  # 37 mapping contexts are measured across the 34 parseable specifications.
  # The schema declares those plus the contexts no specification uses yet.
  expect_gte(length(.TTE_SPEC_SCHEMA), 37L)
})


test_that("the schema declares the keys swereg reads but no specification uses", {
  # These are the 14 paths the schema declares beyond the 124 the fleet uses.
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
    "$/study/implementation/conf_level"
  )
  expect_identical(
    .tte_spec_key_class(declared),
    rep("consumed", length(declared))
  )
})
