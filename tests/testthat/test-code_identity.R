# Direct unit tests for the plain functions in R/code_identity.R.
#
# `RegistryStudy$code_registry_fingerprints()`, `$randvars_hashes()` and
# `$pipeline_hash()` are one-call delegates to `.code_registry_fingerprints()`,
# `.randvars_hashes()` and `.pipeline_hash()`. A test here reaches the same
# code as the method.
#
# Three properties have no other home. The first is the `[[`-not-`$` guard
# inside `.fingerprint_entry()`. The second is that the three pipeline-hash
# surfaces agree on a freshly processed store. The third is that each method
# passes the study fields its plain function expects.

library(data.table)

# ---------------------------------------------------------------------------
# .fingerprint_entry reads fn with [[
# ---------------------------------------------------------------------------

test_that(".fingerprint_entry stops on an entry with fn_args but no fn", {
  # `fn_args` holds a FUNCTION here, and that is the design of the test. `$`
  # partial-matches on a list, so `reg$fn` resolves to `fn_args`,
  # `.hash_function()` succeeds on it, and the entry fingerprints the wrong
  # object without stopping. `reg[["fn"]]` is NULL, and `.hash_function(NULL)`
  # stops.
  entry <- list(
    codes = list(foo = "X"),
    label = "no_fn",
    groups = list("inpatient"),
    fn_args = function(skeleton, data, id_name, codes) invisible(NULL),
    combine_as = NULL
  )

  # Guard the guard. Without these two the test would run on an entry where
  # `$` and `[[` agree, and would say nothing about the accessor.
  expect_identical(entry$fn, entry$fn_args)
  expect_null(entry[["fn"]])

  expect_error(
    swereg:::.fingerprint_entry(entry),
    "is\\.function\\(fn\\) is not TRUE"
  )

  # The same entry WITH an fn fingerprints without error. So the stop above
  # comes from the missing `fn`, and not from the shape of the entry.
  entry$fn <- function(skeleton, data, id_name, codes) invisible(NULL)
  fp <- swereg:::.fingerprint_entry(entry)
  expect_type(fp, "character")
  expect_length(fp, 1L)
  expect_true(nzchar(fp))
})

# ---------------------------------------------------------------------------
# Fixture: 6 persons, batch size 3 -> 2 batches, one framework, one trim,
# two code entries, one randvars step.
# ---------------------------------------------------------------------------

.ci_framework <- function(batch_data, config) {
  data.table::data.table(
    id = batch_data[["ids"]]$lopnr,
    isoyear = 2020L,
    isoyearweek = "2020-01",
    is_isoyear = FALSE
  )
}

.ci_trim <- function(skeleton, batch_data, config) {
  invisible(skeleton)
}

.ci_code_fn <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := TRUE]
  }
  invisible(skeleton)
}

.ci_randvars_fn <- function(skeleton, batch_data, config) {
  skeleton[, rv_a := 1L]
  invisible(skeleton)
}

.ci_study <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("ids", "codes"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  study$save_rawbatch("ids", data.table::data.table(lopnr = 1:6, val = "a"))
  study$save_rawbatch("codes", data.table::data.table(lopnr = 1:6, code = "X"))
  study$register_framework(.ci_framework)
  study$register_trim(.ci_trim)
  study$register_codes(
    codes = list(ci_one = "X"),
    fn = .ci_code_fn,
    groups = list("codes"),
    label = "ci_one"
  )
  study$register_codes(
    codes = list(ci_two = "Y"),
    fn = .ci_code_fn,
    groups = list("codes"),
    label = "ci_two"
  )
  study$register_randvars("rv_a", .ci_randvars_fn)
  study
}

# An explicit batch list makes `full_run` FALSE, so
# `.commit_skeleton_manifest()` raises nothing and each assertion below is the
# thing that detects a defect. test-phase-order.R uses the same convention.
.ci_run <- function(study) {
  invisible(utils::capture.output(
    suppressMessages(study$process_skeletons(
      batches = seq_len(study$n_batches),
      n_workers = 1L
    )),
    type = "output"
  ))
}

# ---------------------------------------------------------------------------
# Three-way parity of the pipeline-hash surfaces
# ---------------------------------------------------------------------------

test_that("study, skeleton and sidecar pipeline hashes agree after a run", {
  study <- .ci_study()
  .ci_run(study)

  study_hash <- study$pipeline_hash()
  skeleton_hash <- study$load_skeleton(1L)$pipeline_hash()
  sidecar <- study$skeleton_pipeline_hashes()

  # Non-degenerate first. Two NA hashes compare equal, and an empty table has
  # no row to disagree with anything.
  expect_type(study_hash, "character")
  expect_length(study_hash, 1L)
  expect_true(nzchar(study_hash))
  expect_identical(nrow(sidecar), 2L)
  expect_false(anyNA(sidecar$pipeline_hash))

  expect_identical(skeleton_hash, study_hash)
  expect_identical(unique(sidecar$pipeline_hash), study_hash)
})

# ---------------------------------------------------------------------------
# The methods delegate to the plain functions
# ---------------------------------------------------------------------------

test_that("each RegistryStudy method passes the fields its function expects", {
  study <- .ci_study()

  fps <- swereg:::.code_registry_fingerprints(study$code_registry)
  rvh <- swereg:::.randvars_hashes(
    study$randvars_fns,
    study$framework_fn,
    study$trim_fn,
    fps
  )

  # Non-degenerate first. Two empty results also compare identical.
  expect_length(fps, 2L)
  expect_length(rvh, 1L)
  expect_true(all(nzchar(c(fps, rvh))))

  expect_identical(study$code_registry_fingerprints(), fps)
  expect_identical(study$randvars_hashes(), rvh)
  expect_identical(
    study$pipeline_hash(),
    swereg:::.pipeline_hash(
      study$framework_fn,
      study$trim_fn,
      rvh,
      fps
    )
  )
})

# ---------------------------------------------------------------------------
# The empty cases the plain functions own
# ---------------------------------------------------------------------------

test_that("an empty registry and an empty step list return character(0)", {
  expect_identical(swereg:::.code_registry_fingerprints(list()), character(0))
  expect_identical(
    swereg:::.randvars_hashes(list(), NULL, NULL, character(0)),
    character(0)
  )
})
