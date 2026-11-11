# swereg 27.2.15 renamed the two rule keys of a specification.
# `horizon_rule: {min_expected_events: k}` became the flat key
# `min_expected_events_per_arm_per_year: k`, and `sparse_rule` became
# `rare_condition_rule`. The old keys are retired: `tteplan_read_spec()` and
# `tteplan_check_spec()` refuse them as unknown keys, and the message lists the
# new key among the keys the root accepts. `tteplan_load()` refuses a plan that
# swereg 27.2.14 saved, through its check for the 27.3.10 design keys.

skip_if_not_installed("yaml")

# The fixture spec text, with the line of the minimum-events key replaced by
# `lines`. `extra` is appended at the end of the file.
mer_write <- function(lines, extra = character(0)) {
  txt <- readLines(testthat::test_path("fixtures", "spec_3x2x2.yaml"))
  i <- which(txt == "min_expected_events_per_arm_per_year: 0")
  stopifnot(length(i) == 1L)
  txt <- c(txt[seq_len(i - 1L)], lines, txt[-seq_len(i)], extra)
  dir <- tempfile("mer_spec_")
  dir.create(dir)
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(txt, path)
  return(path)
}

# A rare-condition block under `key`. The fixture has no computed confounder,
# so the refusal of the old key is the only finding the gate can make.
mer_rare_block <- function(key) {
  return(c(
    paste0(key, ":"),
    "  min_n: 100",
    "  decided_by: [\"01\"]",
    "  candidates: [rd_age_continuous]"
  ))
}

mer_read_error <- function(path) {
  return(tryCatch(
    {
      tteplan_read_spec(path)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  ))
}


test_that("a spec with horizon_rule is refused, naming min_expected_events_per_arm_per_year", {
  path <- mer_write(c("horizon_rule:", "  min_expected_events: 5"))

  msg <- mer_read_error(path)
  expect_match(msg, "carries 1 key swereg does not accept", fixed = TRUE)
  expect_match(msg, "$/horizon_rule\n", fixed = TRUE)
  expect_match(msg, "min_expected_events_per_arm_per_year", fixed = TRUE)

  r <- tteplan_check_spec(path)
  expect_identical(r$path, "$/horizon_rule")
  expect_identical(r$kind, "undeclared_key")
  expect_match(r$message, "min_expected_events_per_arm_per_year", fixed = TRUE)
})

test_that("a spec with sparse_rule is refused, naming rare_condition_rule", {
  path <- mer_write(
    "min_expected_events_per_arm_per_year: 5",
    mer_rare_block("sparse_rule")
  )

  msg <- mer_read_error(path)
  expect_match(msg, "carries 1 key swereg does not accept", fixed = TRUE)
  expect_match(msg, "$/sparse_rule\n", fixed = TRUE)
  expect_match(msg, "rare_condition_rule", fixed = TRUE)

  r <- tteplan_check_spec(path)
  expect_identical(r$path, "$/sparse_rule")
  expect_identical(r$kind, "undeclared_key")
  expect_match(r$message, "rare_condition_rule", fixed = TRUE)
})

test_that("a spec with the new keys reads, and the plan holds min_expected_events_per_arm_per_year 5", {
  path <- mer_write("min_expected_events_per_arm_per_year: 5")
  expect_identical(nrow(tteplan_check_spec(path)), 0L)
  spec <- tteplan_read_spec(path)
  expect_identical(spec$min_expected_events_per_arm_per_year, 5)

  # The plan reads the value through `$reload_spec()`, which copies it onto a
  # plan that holds another value.
  plan <- swereg::TTEPlan$new(
    project_prefix = "fixture-3x2x2",
    skeleton_files = "mer_skeleton.qs2",
    global_max_isoyearweek = "2020-52"
  )
  plan$spec <- tteplan_read_spec(
    mer_write("min_expected_events_per_arm_per_year: 0")
  )
  plan$reload_spec(spec_path = path, quiet = TRUE)
  expect_true(plan$spec$min_expected_events_per_arm_per_year == 5)
  expect_null(plan$spec_reload_skipped_diffs)
})

test_that("tteplan_load() refuses a plan saved with the 27.2.14 keys", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- spr_plan_no_s1()
  rule <- plan$spec$rare_condition_rule
  k <- plan$spec$min_expected_events_per_arm_per_year

  # The stored spec of a plan that swereg 27.2.14 saved. It predates the two
  # design keys too.
  old <- plan$spec
  old$rare_condition_rule <- NULL
  old$min_expected_events_per_arm_per_year <- NULL
  old$period_width <- NULL
  old$admin_censoring_isoyearweek <- NULL
  old$sparse_rule <- rule
  old$horizon_rule <- list(min_expected_events = k)
  plan$spec <- old

  path <- plan$save()
  expect_error(swereg::tteplan_load(path), "older than 27.3.10", fixed = TRUE)
  expect_error(swereg::tteplan_load(path), "rerun s0_init.R", fixed = TRUE)
})

test_that("tteplan_load() refuses a plan whose spec lacks the design keys", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- spr_plan_no_s1()
  path <- plan$save()
  expect_no_error(swereg::tteplan_load(path))

  plan$spec$period_width <- NULL
  path <- plan$save()
  expect_error(swereg::tteplan_load(path), "older than 27.3.10", fixed = TRUE)
})
