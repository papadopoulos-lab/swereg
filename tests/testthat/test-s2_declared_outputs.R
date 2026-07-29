# s2 is the first TTE stage migrated onto batchit's declared-output commit
# engine. Two independent halves, both needed:
#   * the WORKER returns its analysis object and writes nothing -- a
#     captured-dispatch test never runs the worker, so it cannot see this;
#   * the DISPATCH goes through .batch_run_and_write(style = "return") with one
#     absolute declared output per item -- a direct worker test never touches
#     the call site.

skip_if_not_installed("data.table")

test_that(".s2_worker returns list(analysis=) and writes no file", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("qs2")

  dt <- tte_simulate(N = 2000, true_lor = -0.7, persist_coef = 8, seed = 11)
  long <- tte_build_long(dt)
  trial <- TTEEnrollment$new(long, tte_make_design(long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)

  work <- withr::local_tempdir()
  imp <- file.path(work, "imp.qs2")
  qs2::qs_save(trial, imp, nthreads = 1L)

  # The worker has no output-path formal at all.
  expect_false("file_analysis_path" %in% names(formals(swereg:::.s2_worker)))

  before <- list.files(work)
  res <- swereg:::.s2_worker(
    outcome = "event",
    follow_up = max(long$tstop),
    file_imp_path = imp,
    n_threads = 1L,
    sep_by_tx = TRUE,
    with_gam = TRUE,
    estimand = "itt"
  )

  expect_named(res, "analysis")
  expect_s3_class(res$analysis, "TTEEnrollment")
  expect_identical(res$analysis$estimand, "itt")
  expect_false("ipcw_pp" %in% names(res$analysis$data))
  # Nothing new on disk: the old code wrote its analysis file itself.
  expect_identical(sort(list.files(work)), sort(before))
})

test_that("s2 dispatch declares one absolute output per item via run_and_write", {
  ett <- data.table::data.table(
    enrollment_id = c("01", "02"),
    ett_id = c("ETT00001", "ETT00002"),
    outcome_var = c("osd_a", "osd_b"),
    outcome_name = c("Outcome A", "Outcome B"),
    follow_up = c(52L, 52L),
    age_min = c(50L, 60L),
    age_max = c(59L, 69L),
    age_group = c("50_59", "60_69"),
    confounder_vars = c("rd_age_continuous", "rd_age_continuous"),
    person_id_var = c("lopnr", "lopnr"),
    treatment_var = c("rd_tx", "rd_tx"),
    file_imp = c("imp_01.qs2", "imp_02.qs2"),
    file_raw = c("raw_01.qs2", "raw_02.qs2"),
    file_analysis = c("pfx_analysis_001.qs2", "pfx_analysis_002.qs2"),
    description = c("ETT00001", "ETT00002")
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "s2_declared",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )

  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .batch_run_and_write = function(target, ...) {
      cap$target <- target
      cap$args <- list(...)
      stop("__CAPTURED__")
    },
    .package = "swereg"
  )

  output_dir <- withr::local_tempdir()
  expect_error(
    plan$s2_generate_analysis_files_and_ipcw_pp(
      output_dir = output_dir,
      n_workers = 1L,
      swereg_dev_path = "/tmp/fake_dev_path"
    ),
    "__CAPTURED__"
  )

  expect_identical(cap$target$symbol, ".s2_worker")
  expect_identical(cap$target$package, "swereg")

  a <- cap$args
  expect_identical(a$style, "return")
  expect_identical(a$n_workers, 1L)
  expect_identical(a$dev_path, "/tmp/fake_dev_path")
  expect_true(is.function(a$p))

  items <- a$items
  outputs <- a$outputs
  expect_length(items, 2L * nrow(ett))
  expect_length(outputs, 2L * nrow(ett))
  expect_identical(names(outputs), names(items))
  expect_true(all(grepl("^s2_", names(items))))
  expect_identical(anyDuplicated(names(items)), 0L)

  # Item formals: complete for .s2_worker, and no output path among them.
  need <- c(
    "outcome",
    "follow_up",
    "file_imp_path",
    "n_threads",
    "sep_by_tx",
    "with_gam",
    "estimand"
  )
  for (it in items) {
    expect_setequal(names(it), need)
    expect_false("file_analysis_path" %in% names(it))
  }
  expect_identical(
    vapply(items, function(it) it$estimand, character(1)),
    setNames(rep(c("pp", "itt"), nrow(ett)), names(items))
  )

  # One declared output per item, named `analysis`, absolute.
  for (o in outputs) {
    expect_named(o, "analysis")
    expect_true(is.character(o))
    expect_match(o[["analysis"]], "^/")
    expect_identical(dirname(o[["analysis"]]), normalizePath(output_dir))
  }
  expect_identical(
    unname(vapply(outputs, function(o) basename(o[["analysis"]]), character(1))),
    c(
      "pfx_analysis_001.qs2",
      "pfx_analysis_itt_001.qs2",
      "pfx_analysis_002.qs2",
      "pfx_analysis_itt_002.qs2"
    )
  )
})
