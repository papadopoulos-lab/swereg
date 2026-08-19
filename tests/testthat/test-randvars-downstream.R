# Every randvar replays when anything upstream of it changes.
#
# `RegistryStudy$randvars_hashes()` folds the framework hash and the whole
# code registry fingerprint set into every step's hash. So a change to
# either one diverges `Skeleton$sync_randvars()` at step 1 and replays the
# whole sequence.
#
# These tests drive `$process_skeletons()` end to end and read the stored
# `randvars_state` off the reloaded skeleton. A hash-level assertion alone
# would not show where the divergence point lands.

library(data.table)

# The generation counter. Each randvar names its output column after the
# current generation, so `randvars_state[[step]]$added_columns` records
# which run last ran that step. The counter lives outside every function
# body, so bumping it does NOT change any hash.
.rvd_gen <- new.env(parent = emptyenv())

.rvd_framework <- function(batch_data, config) {
  data.table::data.table(
    id = batch_data[["grp1"]]$lopnr,
    isoyear = 2020L,
    isoyearweek = "2020-01",
    is_isoyear = FALSE
  )
}

# Build one randvar step. The step name goes into the BODY via bquote(),
# so the three steps have three distinct `.hash_function()` values.
.rvd_randvar <- function(step) {
  fn <- function(skeleton, batch_data, config) NULL
  body(fn) <- bquote({
    gen <- .rvd_gen$n
    skeleton[, (paste0(.(step), "_gen", gen)) := gen]
    invisible(skeleton)
  })
  fn
}

.rvd_code_fn <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := TRUE]
  }
  invisible(skeleton)
}

.RVD_STEPS <- c("rv_a", "rv_b", "rv_c")

# A study over `dir`, with `framework_fn` as phase 1, `code_values` as the
# single code entry's codes, and the three randvars in fixed order.
# Called once per run so a re-run can change one input and nothing else.
.rvd_study <- function(dir, framework_fn = .rvd_framework, code_values = "X") {
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "grp1",
    batch_size = 6L
  )
  study$set_ids(1:6)
  study$register_framework(framework_fn)
  study$register_codes(
    codes = list(my_code = code_values),
    fn = .rvd_code_fn,
    groups = list("grp1"),
    label = "mycode"
  )
  for (step in .RVD_STEPS) {
    study$register_randvars(step, .rvd_randvar(step))
  }
  study
}

# Which generation last ran each step, read off the STORED state.
.rvd_generations <- function(sk) {
  vapply(
    .RVD_STEPS,
    function(step) sub("^.*_gen", "", sk$randvars_state[[step]]$added_columns),
    character(1)
  )
}

.rvd_first_run <- function(dir) {
  study <- .rvd_study(dir)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  .rvd_gen$n <- 1L
  study$process_skeletons(n_workers = 1L)
  study
}


test_that("first run records every step at generation 1", {
  dir <- withr::local_tempdir()
  study <- .rvd_first_run(dir)

  sk <- study$load_skeleton(1L)
  expect_identical(names(sk$randvars_state), .RVD_STEPS)
  expect_identical(.rvd_generations(sk), c(rv_a = "1", rv_b = "1", rv_c = "1"))
  expect_true(all(paste0(.RVD_STEPS, "_gen1") %in% names(sk$data)))
  expect_true("my_code" %in% names(sk$data))
})


test_that("a code entry change replays every randvar", {
  dir <- withr::local_tempdir()
  .rvd_first_run(dir)

  # Change ONLY the code entry's codes. Same framework, same randvars.
  study2 <- .rvd_study(dir, code_values = c("X", "Y"))
  .rvd_gen$n <- 2L
  study2$process_skeletons(n_workers = 1L)

  sk <- study2$load_skeleton(1L)
  expect_identical(.rvd_generations(sk), c(rv_a = "2", rv_b = "2", rv_c = "2"))
  # Rewind removed the generation-1 columns before the replay.
  expect_false(any(paste0(.RVD_STEPS, "_gen1") %in% names(sk$data)))
  expect_true(all(paste0(.RVD_STEPS, "_gen2") %in% names(sk$data)))
  # Phase 1 did not re-run: only the code entry changed.
  expect_identical(sk$framework_fn_hash, swereg:::.hash_function(.rvd_framework))
})


test_that("a framework change replays every randvar", {
  dir <- withr::local_tempdir()
  .rvd_first_run(dir)

  framework_v2 <- function(batch_data, config) {
    d <- data.table::data.table(
      id = batch_data[["grp1"]]$lopnr,
      isoyear = 2020L,
      isoyearweek = "2020-01",
      is_isoyear = FALSE
    )
    d[, framework_v2 := TRUE]
    d[]
  }

  study2 <- .rvd_study(dir, framework_fn = framework_v2)
  .rvd_gen$n <- 2L
  study2$process_skeletons(n_workers = 1L)

  sk <- study2$load_skeleton(1L)
  expect_identical(.rvd_generations(sk), c(rv_a = "2", rv_b = "2", rv_c = "2"))
  expect_false(any(paste0(.RVD_STEPS, "_gen1") %in% names(sk$data)))
  # A framework change rebuilds the base, so phase 1 and phase 2 re-ran too.
  expect_identical(sk$framework_fn_hash, swereg:::.hash_function(framework_v2))
  expect_true("framework_v2" %in% names(sk$data))
  expect_true("my_code" %in% names(sk$data))
})


test_that("changing nothing replays no randvar", {
  dir <- withr::local_tempdir()
  .rvd_first_run(dir)

  study2 <- .rvd_study(dir)
  # Bump the counter anyway. Any replay would write generation-2 columns.
  .rvd_gen$n <- 2L
  study2$process_skeletons(n_workers = 1L)

  sk <- study2$load_skeleton(1L)
  expect_identical(.rvd_generations(sk), c(rv_a = "1", rv_b = "1", rv_c = "1"))
  expect_true(all(paste0(.RVD_STEPS, "_gen1") %in% names(sk$data)))
  expect_false(any(paste0(.RVD_STEPS, "_gen2") %in% names(sk$data)))
})


test_that("randvars_hashes() folds in the framework and the code registry", {
  dir <- withr::local_tempdir()
  study <- .rvd_study(dir)

  base <- study$randvars_hashes()
  expect_identical(names(base), .RVD_STEPS)

  # Same registration, same hashes.
  expect_identical(.rvd_study(dir)$randvars_hashes(), base)

  # A code entry change moves every step's hash.
  changed_codes <- .rvd_study(dir, code_values = c("X", "Y"))$randvars_hashes()
  expect_false(any(changed_codes == base))

  # A framework change moves every step's hash.
  framework_v2 <- function(batch_data, config) {
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, extra = TRUE)
  }
  changed_fw <- .rvd_study(dir, framework_fn = framework_v2)$randvars_hashes()
  expect_false(any(changed_fw == base))
})


test_that("randvars_hashes() is character(0) with no step registered", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "grp1",
    batch_size = 6L
  )
  expect_identical(study$randvars_hashes(), character(0))

  # And with no framework registered, the framework component is NA rather
  # than an error.
  study$register_randvars("rv_a", .rvd_randvar("rv_a"))
  expect_type(study$randvars_hashes(), "character")
  expect_identical(names(study$randvars_hashes()), "rv_a")
})
