# `$s3_analyze()` names every NA estimate and every warned fit on stdout.
#
# Both signals used to be invisible while a run was going. `.tte_fit_irr()`
# calls `warning()` inside a worker subprocess, and the batch pool discards that
# subprocess's stderr. An NA ratio reached the operator only as an empty cell in
# a results sheet opened days later. In one 30-batch run 1,232 of 1,710 stored
# ratios were NA and nothing in the Slurm log said so.
#
# The dispatcher is mocked, so this test drives the driver's own reporting on
# crafted worker returns. It is not a boundary test: `.tte_fit_irr()` itself is
# pinned in test-irr-zero-events.R.

skip_if_not_installed("data.table")

# Three ETTs under one enrollment. Same shape as the fixture in
# test-s3_item_contract.R, which pins the item builder on the same driver.
.s3rep_fixture_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = c("ETT00001", "ETT00002", "ETT00003"),
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = c(
      "analysis_001.qs2",
      "analysis_002.qs2",
      "analysis_003.qs2"
    ),
    description = c("ETT00001: A", "ETT00002: A", "ETT00003: A")
  )
  return(swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  ))
}

# One `$irr()` return value. `warn` is TRUE on the NA path too, which is why
# the driver reports a warned fit only when the ratio is finite.
.s3rep_irr_row <- function(irr, warn) {
  return(data.table::data.table(
    IRR = irr,
    IRR_lower = irr,
    IRR_upper = irr,
    IRR_pvalue = 0.5,
    warn = warn,
    events_intervention = if (is.na(irr)) 0 else 7,
    events_comparator = 5
  ))
}

# The dispatcher stand-in. The enrollment dispatch returns one empty slot per
# item. The ETT dispatch returns one crafted IRR row per ETT, under the
# `irr_itt` slot, and nothing for any other analysis call.
.s3rep_fake_batch_run <- function(rows) {
  return(function(target, items, n_workers, ...) {
    if (!identical(target$symbol, ".s3_ett_worker")) {
      return(stats::setNames(vector("list", length(items)), names(items)))
    }
    out <- lapply(items, function(it) {
      keep <- identical(it$method, "irr") &&
        identical(it$weight_col, "ipw_trunc")
      if (!keep) {
        return(list())
      }
      return(stats::setNames(list(rows[[it$ett_id]]), "irr_itt"))
    })
    names(out) <- names(items)
    return(out)
  })
}


test_that("s3_analyze names every NA estimate and every warned fit on stdout", {
  plan <- .s3rep_fixture_plan()
  testthat::local_mocked_bindings(
    .batch_run = .s3rep_fake_batch_run(list(
      ETT00001 = .s3rep_irr_row(NA_real_, TRUE),
      ETT00002 = .s3rep_irr_row(NA_real_, TRUE),
      ETT00003 = .s3rep_irr_row(1.2, TRUE)
    )),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()

  out <- utils::capture.output(
    plan$s3_analyze(output_dir = output_dir, n_workers = 1L)
  )

  na_line <- grep("estimates are NA", out, value = TRUE)
  expect_length(na_line, 1L)
  expect_match(na_line, "2 of", fixed = TRUE)
  expect_match(na_line, "ETT00001", fixed = TRUE)
  expect_match(na_line, "ETT00002", fixed = TRUE)

  warn_line <- grep("raised a warning", out, value = TRUE)
  expect_length(warn_line, 1L)
  expect_match(warn_line, "1 of", fixed = TRUE)
  expect_match(warn_line, "ETT00003", fixed = TRUE)

  # The warned line names the finite fit only. ETT00001 and ETT00002 also carry
  # warn = TRUE, and reporting them there would say the same ETT twice.
  expect_false(grepl("ETT00001", warn_line, fixed = TRUE))
})


test_that("s3_analyze reports the clean case on both lines", {
  plan <- .s3rep_fixture_plan()
  testthat::local_mocked_bindings(
    .batch_run = .s3rep_fake_batch_run(list(
      ETT00001 = .s3rep_irr_row(1.1, FALSE),
      ETT00002 = .s3rep_irr_row(0.9, FALSE),
      ETT00003 = .s3rep_irr_row(1.2, FALSE)
    )),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()

  out <- utils::capture.output(
    plan$s3_analyze(output_dir = output_dir, n_workers = 1L)
  )

  expect_length(grep("all 3 ETT estimates are finite", out), 1L)
  expect_length(grep("no ETT fit raised a warning", out), 1L)
  expect_length(grep("estimates are NA", out), 0L)
})
