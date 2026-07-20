# Pin the s3 ETT item builder's side of the batch contract (Phase 3).
#
# Two regressions this guards, both of the "worked under parallel_pool, breaks
# silently under .batch_run" shape:
#
# * n_threads: the builder used to put `n_threads = n_cores` in every ETT item
#   and RELY on parallel_pool overwriting it with the per-worker share. The
#   batch runner is thread-agnostic (deliberately: thread policy is the
#   consumer's), so carrying that item verbatim would have oversubscribed every
#   worker n_workers-fold. The builder must now compute the per-worker share
#   itself.
# * subgroup_var: an optional formal absent from the item is exactly the
#   arm_labels bug's shape, and .batch_run rejects it loudly. The builder must
#   name EVERY formal of .s3_ett_worker on every item, including the optional
#   one it mostly wants defaulted.
#
# The dispatcher is mocked to CAPTURE the items (this is a builder test, not a
# boundary test -- test-batch_s3_production.R drives the real boundary).

skip_if_not_installed("data.table")

.contract_fixture_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id   = c("01", "01"),
    ett_id          = c("ETT00001", "ETT00002"),
    outcome_var     = "osd_a",
    outcome_name    = "Outcome A",
    follow_up       = 52L,
    age_min         = 50L,
    age_max         = 59L,
    age_group       = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var   = "lopnr",
    treatment_var   = "rd_tx",
    file_imp        = "imp_01.qs2",
    file_raw        = "raw_01.qs2",
    file_analysis   = c("analysis_001.qs2", "analysis_002.qs2"),
    description     = c("ETT00001: A", "ETT00002: A")
  )
  swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
}

test_that("s3 ETT items carry per-worker n_threads and EVERY .s3_ett_worker formal", {
  plan <- .contract_fixture_plan()

  # Pin the core count: on a one-core host the buggy builder's
  # .safe_n_cores() and the fixed .threads_per_worker(2L) BOTH return 1, and
  # the thread assertion below would pass with the bug live. 8 cores makes
  # them 8 vs 4 -- always distinguishable.
  testthat::local_mocked_bindings(
    detectCores = function(...) 8L,
    .package = "parallel"
  )

  captured <- NULL
  testthat::local_mocked_bindings(
    .batch_run = function(target, items, n_workers, ...) {
      # s3 dispatches the enrollment loop first, then the ETT loop. Let the
      # enrollment dispatch return one result slot per item so s3 proceeds,
      # then capture the ETT dispatch -- the builder this test pins. (s3 no
      # longer skips "cached" enrollments, so the ETT loop is no longer the
      # first dispatch; select it by target rather than by ordering.)
      if (!identical(target$symbol, ".s3_ett_worker")) {
        return(stats::setNames(vector("list", length(items)), names(items)))
      }
      captured <<- list(target = target, items = items, n_workers = n_workers)
      stop("__SENTINEL_CAPTURE__")
    },
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  expect_error(
    plan$s3_analyze(output_dir = output_dir, n_workers = 2L),
    "__SENTINEL_CAPTURE__"
  )

  expect_identical(captured$target$symbol, ".s3_ett_worker")
  expect_gt(length(captured$items), 0L)

  # Every item names the target's COMPLETE formal set -- nothing extra, nothing
  # silently defaulted. This is the parent half of what .batch_run enforces;
  # asserting it on the builder pins the fix independent of the dispatcher.
  fml <- names(formals(swereg:::.s3_ett_worker))
  for (it in captured$items) {
    expect_true(setequal(names(it), fml),
      info = paste0("item formals mismatch: {", paste(names(it), collapse = ", "),
        "} vs {", paste(fml, collapse = ", "), "}"))
  }

  # Thread policy: the per-worker share, decided by the BUILDER (the runner
  # will not overwrite it). Under the reverted builder this is .safe_n_cores()
  # -- the full-machine count -- in every item.
  want <- swereg:::.threads_per_worker(2L)
  got <- vapply(captured$items, function(it) it[["n_threads"]], numeric(1))
  expect_true(all(got == want),
    info = sprintf("item n_threads %s != per-worker share %s",
      paste(unique(got), collapse = "/"), want))

  # Stable, unique, meaningful ids -- a failure must name the exact analysis.
  ids <- names(captured$items)
  expect_false(is.null(ids))
  expect_true(all(nzchar(ids)))
  expect_identical(anyDuplicated(ids), 0L)
  expect_true("ETT00001__summary_and_rates" %in% ids)
  expect_true("ETT00001__irr__analysis_weight_pp_trunc" %in% ids)
})
