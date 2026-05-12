# Pin the TTEPlan save / load round-trip. A long pipeline run lasts
# 2-4 days; if `plan$save()` plus `tteplan_locate_and_load()` round-trip
# isn't bit-equal in the fields downstream stages care about, you've
# silently lost work. Specifically, this test pins:
#   - project_prefix and ett structure survive
#   - results_enrollment / results_ett populated entries survive
#   - schema-version migration check passes
#   - tteplan.qs2 is the on-disk filename

skip_if_not_installed("data.table")
skip_if_not_installed("withr")
skip_if_not_installed("qs2")

.fixture_plan_with_results <- function() {
  ett <- data.table::data.table(
    enrollment_id   = c("01", "01"),
    ett_id          = c("ETT00001", "ETT00002"),
    outcome_var     = "osd_a",
    outcome_name    = "Outcome A",
    follow_up       = c(52L, 156L),
    age_min         = 50L,
    age_max         = 59L,
    age_group       = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var   = "lopnr",
    treatment_var   = "rd_tx",
    file_imp        = "imp_01.qs2",
    file_raw        = "raw_01.qs2",
    file_analysis   = c("analysis_001.qs2", "analysis_002.qs2"),
    description     = c("ETT00001", "ETT00002")
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "roundtrip_test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$results_enrollment <- list(
    "01" = list(table1_unweighted = "ENR_01_TABLE1",
                n_baseline = 12345L,
                computed_at = Sys.time())
  )
  plan$results_ett <- list(
    "ETT00001" = list(
      enrollment_id = "01",
      description = "ETT00001",
      irr_pp_trunc = list(IRR = 1.5, IRR_lower = 0.9, IRR_upper = 2.5)
    )
  )
  plan
}

test_that("plan$save() writes tteplan.qs2 to the requested dir", {
  dir <- withr::local_tempdir()
  plan <- .fixture_plan_with_results()
  plan$save(dir = dir)
  expect_true(file.exists(file.path(dir, "tteplan.qs2")))
})

test_that("tteplan_locate_and_load: round-trip preserves project_prefix and ett", {
  dir <- withr::local_tempdir()
  plan <- .fixture_plan_with_results()
  plan$save(dir = dir)

  reloaded <- swereg::tteplan_locate_and_load(dir)
  expect_equal(reloaded$project_prefix, "roundtrip_test")
  expect_equal(nrow(reloaded$ett), 2L)
  expect_equal(sort(reloaded$ett$ett_id), c("ETT00001", "ETT00002"))
})

test_that("tteplan_locate_and_load: round-trip preserves populated results_*", {
  dir <- withr::local_tempdir()
  plan <- .fixture_plan_with_results()
  plan$save(dir = dir)

  reloaded <- swereg::tteplan_locate_and_load(dir)
  expect_equal(length(reloaded$results_enrollment), 1L)
  expect_equal(reloaded$results_enrollment[["01"]]$table1_unweighted,
               "ENR_01_TABLE1")
  expect_equal(reloaded$results_enrollment[["01"]]$n_baseline, 12345L)

  expect_equal(length(reloaded$results_ett), 1L)
  expect_equal(reloaded$results_ett[["ETT00001"]]$irr_pp_trunc$IRR, 1.5)
  expect_equal(reloaded$results_ett[["ETT00001"]]$irr_pp_trunc$IRR_lower, 0.9)
})

test_that("tteplan_locate_and_load: schema version check passes for fresh save", {
  dir <- withr::local_tempdir()
  plan <- .fixture_plan_with_results()
  plan$save(dir = dir)
  # If the on-disk schema version is older than the class's, load
  # would error inside check_version() with a clear migration message.
  # A round-trip on the same swereg version must not trigger that.
  expect_silent(swereg::tteplan_locate_and_load(dir))
})

test_that("tteplan_locate_and_load: errors clearly when dir has no tteplan.qs2", {
  dir <- withr::local_tempdir()
  # Empty dir -> no plan file
  expect_error(swereg::tteplan_locate_and_load(dir))
})
