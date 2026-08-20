# End-to-end parity: an incremental rebuild against a clean rebuild.
#
# Every other test in this suite covers one phase. This one covers the
# whole pipeline at once: framework -> trim -> codes -> randvars.
#
# The scenario deletes rows in a registered trim. It then changes one
# code entry and rebuilds twice. Run 2 runs once incrementally over run
# 1's skeleton directory, and once into an empty directory. The two
# skeletons MUST be identical.
#
# The witness below is what stops this test from passing for the wrong
# reason. An incremental run that rebuilt the base from scratch would
# also agree with the clean run, and would prove nothing. So the test
# also asserts which registered functions ran. In run 2 incremental the
# framework and the trim MUST NOT run, and the code entry and the
# randvar MUST run. That is the path the original defect took.

library(data.table)

# ---------------------------------------------------------------------------
# The witness
# ---------------------------------------------------------------------------

# Each registered function appends its own name to `log`. `run` names
# the run that is in progress. An environment gives reference semantics,
# so a function called deep inside the pipeline can record itself.
#
# The runs all happen in this process, because the scenario passes
# `n_workers = 1L`. Nothing here crosses a process boundary.
.parity_w <- new.env(parent = emptyenv())
.parity_w$run <- NA_character_
.parity_w$log <- character(0)

.parity_note <- function(what) {
  .parity_w$log <- c(.parity_w$log, paste0(.parity_w$run, "/", what))
  invisible(NULL)
}

.parity_called <- function(log, run, what) {
  paste0(run, "/", what) %in% log
}

# ---------------------------------------------------------------------------
# The four registered functions
# ---------------------------------------------------------------------------

# Phase 1. Four weekly rows for each of the 6 ids.
.parity_framework <- function(batch_data, config) {
  .parity_note("framework")
  d <- data.table::CJ(
    id = sort(batch_data[["persons"]]$lopnr),
    isoyearweek = c("2020-01", "2020-02", "2020-03", "2020-04")
  )
  d[, `:=`(isoyear = 2020L, is_isoyear = FALSE)]
  d[]
}

# Phase 1b. The trim, and the one declared place that deletes rows. It
# drops the earliest week, so it is not a pure predicate filter. A
# second run of it against its own output deletes another week. That is
# what makes this test able to detect a trim that runs outside the
# rebuild block.
.parity_trim <- function(skeleton, batch_data, config) {
  .parity_note("trim")
  skeleton[isoyearweek != min(isoyearweek)]
}

# Phase 2. A code entry whose output DEPENDS ON THE ROW SET. It flags
# the earliest week the trim left behind, so any change to the row set
# moves the answer.
.parity_code_fn <- function(skeleton, dataset, id_name, codes, ...) {
  .parity_note("code")
  for (nm in names(codes)) {
    hit_ids <- unique(dataset[atc %chin% codes[[nm]], get(id_name)])
    skeleton[, (nm) := isoyearweek == min(isoyearweek) & id %in% hit_ids]
  }
  invisible(skeleton)
}

# Phase 3. An additive randvar that READS the phase-2 column. It proves
# the replay reads the freshly applied code column, not a stale one.
.parity_randvar <- function(skeleton, batch_data, config) {
  .parity_note("randvar")
  skeleton[, rv_n_flagged := sum(dx_flag), by = id]
  skeleton[, rv_label := data.table::fifelse(rv_n_flagged > 0L, "hit", "miss")]
  invisible(skeleton)
}

# ---------------------------------------------------------------------------
# The scenario
# ---------------------------------------------------------------------------

# 3 of the 6 persons hold a prescription. Ids 1 and 2 hold N06A, id 3
# holds C07A. Run 1 registers N06A alone, run 2 registers both.
.parity_rx <- data.table::data.table(
  lopnr = c(1L, 2L, 3L),
  atc = c("N06A", "N06A", "C07A")
)

.parity_study <- function(dir, code_values) {
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("persons", "rx"),
    batch_size = 6L
  )
  study$set_ids(1:6)
  study$save_rawbatch("persons", data.table::data.table(lopnr = 1:6))
  study$save_rawbatch("rx", .parity_rx)
  study$register_framework(.parity_framework)
  study$register_trim(.parity_trim)
  study$register_codes(
    codes = list(dx_flag = code_values),
    fn = .parity_code_fn,
    groups = list("rx"),
    label = "rx_flag"
  )
  study$register_randvars("rv_additive", .parity_randvar)
  study
}

# Read batch 1 back from disk and key it, so the comparison does not
# depend on row order.
.parity_skeleton <- function(study) {
  d <- data.table::copy(study$load_skeleton(1L)$data)
  data.table::setkeyv(d, c("id", "isoyearweek"))
  d[]
}

.parity_scenario <- function() {
  .parity_w$log <- character(0)
  inc_dir <- withr::local_tempdir()
  clean_dir <- withr::local_tempdir()

  .parity_w$run <- "run1"
  s1 <- .parity_study(inc_dir, "N06A")
  s1$process_skeletons(n_workers = 1L)
  run1 <- .parity_skeleton(s1)

  # Run 2, incremental. Same directory, one changed code entry.
  .parity_w$run <- "incr"
  s2 <- .parity_study(inc_dir, c("N06A", "C07A"))
  s2$process_skeletons(n_workers = 1L)
  incr <- .parity_skeleton(s2)

  # Run 2, clean. An empty directory, and the same registration.
  .parity_w$run <- "clean"
  s3 <- .parity_study(clean_dir, c("N06A", "C07A"))
  s3$process_skeletons(n_workers = 1L)
  clean <- .parity_skeleton(s3)

  .parity_w$run <- NA_character_
  list(run1 = run1, incr = incr, clean = clean, log = .parity_w$log)
}

# The pipeline runs three times. Run it once here and read the results
# in the tests below.
.parity <- .parity_scenario()

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("run 1 pins the skeleton the trim and the first code entry produce", {
  expect_identical(nrow(.parity$run1), 18L)
  expect_identical(
    sort(unique(.parity$run1$isoyearweek)),
    c("2020-02", "2020-03", "2020-04")
  )
  expect_identical(sum(.parity$run1$dx_flag), 2L)
  expect_identical(sum(.parity$run1$rv_label == "hit"), 6L)
})

test_that("an incremental rebuild and a clean rebuild produce identical skeletons", {
  expect_identical(names(.parity$incr), names(.parity$clean))
  expect_identical(data.table::key(.parity$incr), data.table::key(.parity$clean))
  expect_true(isTRUE(all.equal(.parity$incr, .parity$clean)))
  expect_equal(.parity$incr, .parity$clean)
  # Column by column, on values and types. `as.list()` drops the
  # internal self-reference, which carries an address rather than data.
  expect_identical(as.list(.parity$incr), as.list(.parity$clean))
})

test_that("the changed code entry and its randvar reach both run 2 skeletons", {
  expect_identical(nrow(.parity$incr), 18L)
  expect_identical(nrow(.parity$clean), 18L)
  expect_identical(sum(.parity$incr$dx_flag), 3L)
  expect_identical(sum(.parity$clean$dx_flag), 3L)
  expect_identical(sum(.parity$incr$rv_label == "hit"), 9L)
  expect_identical(sum(.parity$clean$rv_label == "hit"), 9L)
})

test_that("the incremental run skipped the framework and the trim", {
  log <- .parity$log

  expect_true(.parity_called(log, "run1", "framework"))
  expect_true(.parity_called(log, "run1", "trim"))
  expect_true(.parity_called(log, "run1", "code"))
  expect_true(.parity_called(log, "run1", "randvar"))

  # The whole point. The base survived, so the trim MUST NOT run again.
  expect_false(.parity_called(log, "incr", "framework"))
  expect_false(.parity_called(log, "incr", "trim"))
  expect_true(.parity_called(log, "incr", "code"))
  expect_true(.parity_called(log, "incr", "randvar"))

  expect_true(.parity_called(log, "clean", "framework"))
  expect_true(.parity_called(log, "clean", "trim"))
  expect_true(.parity_called(log, "clean", "code"))
  expect_true(.parity_called(log, "clean", "randvar"))
})
