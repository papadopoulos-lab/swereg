# Direct unit tests for the plain functions in R/code_identity.R.
#
# `RegistryStudy$code_registry_fingerprints()`, `$randvars_hashes()` and
# `$pipeline_identity()` are one-call delegates to `.code_registry_fingerprints()`,
# `.randvars_hashes()` and `.pipeline_identity()`. A test here reaches the same
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

  study_identity <- study$pipeline_identity()
  study_hash <- swereg:::.pipeline_identity_hash(study_identity)
  skeleton_identity <- study$load_skeleton(1L)$pipeline_identity()
  sidecar <- study$skeleton_pipeline_hashes()

  # Non-degenerate first. Two NA hashes compare equal, and an empty table has
  # no row to disagree with anything.
  expect_type(study_identity, "list")
  expect_length(study_identity, 5L)
  expect_true(nzchar(study_hash))
  expect_identical(nrow(sidecar), 2L)
  expect_false(anyNA(sidecar$identity_hash))

  expect_identical(skeleton_identity, study_identity)
  expect_identical(unique(sidecar$identity_hash), study_hash)
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
    study$pipeline_identity(),
    swereg:::.pipeline_identity(
      framework_hash = swereg:::.hash_function(study$framework_fn),
      trim_hash = swereg:::.trim_hash(study$trim_fn),
      phase_order = swereg:::.PHASE_ORDER,
      randvars_hashes = rvh,
      fingerprints = fps
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

# ---------------------------------------------------------------------------
# Code fingerprints stay ORDERED. The 2026-09-21 regression is fixed at the
# source instead, in Skeleton$sync_with_registry().
# ---------------------------------------------------------------------------
#
# Slurm job 79 rebuilt all 2194 batches and was then rejected. Only one
# component differed, and only in ORDER: $sync_with_registry() re-applies a
# changed entry at the END, so `applied_registry` was in application order
# while the study was in registration order. The two digests over the same
# eight fingerprints were f768d51bb00f9556 and 059371e9306a7db5.
#
# Sorting here would have hidden it. Application order is semantic:
# .apply_code_entry_impl() hands a registered function the whole skeleton, so
# one entry may read a column another wrote. So the identity stays ordered and
# the STORED order is normalized after every sync. See test-r6_skeleton.R.

test_that("a permutation of the code fingerprints IS a difference", {
  fps <- c(
    "df3a02768e356758", "7589342b915ea31b", "80801271436d9362",
    "90951e7a958a92f4", "97adef825fcf2c69", "fccc9d230ae4eec3",
    "5638fbee3371c653", "9252af92692787ca"
  )
  # The application order job 79 wrote: add_operations re-applied last.
  applied <- c(fps[1:5], fps[7], fps[8], fps[6])
  expect_setequal(applied, fps)
  expect_false(identical(applied, fps))

  mk <- function(codes) {
    swereg:::.pipeline_identity(
      framework_hash = "fw", trim_hash = "tr",
      phase_order = swereg:::.PHASE_ORDER,
      randvars_hashes = c(step_a = "rv"), fingerprints = codes
    )
  }
  expect_identical(
    swereg:::.first_identity_difference(mk(applied), mk(fps)),
    "codes"
  )
})

test_that("the identity still moves when the code SET changes", {
  mk <- function(codes) {
    swereg:::.pipeline_identity(
      framework_hash = "fw", trim_hash = "tr",
      phase_order = swereg:::.PHASE_ORDER,
      randvars_hashes = c(step_a = "rv"), fingerprints = codes
    )
  }
  base <- mk(c("aaa", "bbb", "ccc"))
  expect_identical(
    swereg:::.first_identity_difference(mk(c("aaa", "bbb")), base), "codes"
  )
  expect_identical(
    swereg:::.first_identity_difference(mk(c("aaa", "bbb", "ddd")), base), "codes"
  )
  expect_null(
    swereg:::.first_identity_difference(mk(c("aaa", "bbb", "ccc")), base)
  )
})

test_that("randvars stay ORDERED -- a swapped step sequence is a difference", {
  mk <- function(rv) {
    swereg:::.pipeline_identity(
      framework_hash = "fw",
      trim_hash = "tr",
      phase_order = swereg:::.PHASE_ORDER,
      randvars_hashes = rv,
      fingerprints = c("aaa", "bbb")
    )
  }
  a <- mk(c(step_a = "h1", step_b = "h2"))
  b <- mk(c(step_b = "h2", step_a = "h1"))
  expect_identical(swereg:::.first_identity_difference(a, b), "randvars")
})

test_that(".first_identity_difference names the first differing component", {
  mk <- function(fw = "fw", tr = "tr") {
    swereg:::.pipeline_identity(
      framework_hash = fw,
      trim_hash = tr,
      phase_order = swereg:::.PHASE_ORDER,
      randvars_hashes = c(step_a = "rv"),
      fingerprints = c("aaa")
    )
  }
  base <- mk()
  expect_null(swereg:::.first_identity_difference(base, mk()))
  expect_identical(swereg:::.first_identity_difference(mk(fw = "x"), base), "framework")
  # framework outranks trim when both differ
  expect_identical(
    swereg:::.first_identity_difference(mk(fw = "x", tr = "y"), base),
    "framework"
  )
})


# ---------------------------------------------------------------------------
# `label` is presentation, not identity
# ---------------------------------------------------------------------------
#
# It is documented on $register_codes() as the label describe_codes() prints.
# It writes no column and changes no value, so it must not invalidate a
# skeleton: `sync_with_registry()` drops and re-applies by fingerprint, and on
# the MHT pipeline that is a full rebuild of phase 2 and phase 3 over 2194
# batches.

test_that("editing a code entry's label does not change its fingerprint", {
  mk <- function(lab) {
    list(
      kind = "primary",
      codes = list(foo = "X"),
      label = lab,
      groups = list(p = "grp_a"),
      fn_args = list(),
      combine_as = NULL,
      fn = function(skeleton, batch_data, ...) NULL
    )
  }
  expect_identical(
    swereg:::.fingerprint_entry(mk("swereg::add_diagnoses")),
    swereg:::.fingerprint_entry(mk("a friendlier name"))
  )
  # NULL vs a label is also the same entry.
  expect_identical(
    swereg:::.fingerprint_entry(mk(NULL)),
    swereg:::.fingerprint_entry(mk("anything"))
  )
})

test_that("every OTHER component of a code entry still moves the fingerprint", {
  base <- list(
    kind = "primary",
    codes = list(foo = "X"),
    label = "lab",
    groups = list(p = "grp_a"),
    fn_args = list(),
    combine_as = NULL,
    fn = function(skeleton, batch_data, ...) NULL
  )
  h0 <- swereg:::.fingerprint_entry(base)

  mut <- base
  mut$codes <- list(foo = "Y")
  expect_false(identical(swereg:::.fingerprint_entry(mut), h0))

  mut <- base
  mut$groups <- list(p = "grp_b")
  expect_false(identical(swereg:::.fingerprint_entry(mut), h0))

  mut <- base
  mut$fn_args <- list(source = "atc")
  expect_false(identical(swereg:::.fingerprint_entry(mut), h0))

  mut <- base
  mut$combine_as <- "os"
  expect_false(identical(swereg:::.fingerprint_entry(mut), h0))

  mut <- base
  mut$fn <- function(skeleton, batch_data, ...) TRUE
  expect_false(identical(swereg:::.fingerprint_entry(mut), h0))
})
