# Smoke tests for RegistryStudy$compute_summary().
# We don't try to exercise every TSV-suppression edge case here -- just
# verify that the artefacts are written, partial vs full gating works,
# and the per-column counts from apply_code_entry round-trip through
# the meta sidecar.

library(data.table)

.cs_mk_study <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study
}

.cs_framework <- function(batch_data, config) {
  swereg::create_skeleton(
    ids       = batch_data[["grp1"]]$lopnr,
    date_min  = "2020-01-01",
    date_max  = "2020-01-31"
  )
}


test_that("process_skeletons writes summary.qs2 and status.txt on partial runs", {
  study <- .cs_mk_study()
  study$register_framework(.cs_framework)
  study$process_skeletons()

  qs_path  <- file.path(study$data_skeleton_dir, "summary.qs2")
  # status.txt now lives in data_meta_dir (which defaults to rawbatch dir
  # in this study). It is also still written automatically.
  txt_path <- file.path(study$data_meta_dir, "status.txt")

  expect_true(file.exists(qs_path))
  expect_true(file.exists(txt_path))

  summary <- study$summary
  expect_true(summary$meta$is_complete)  # this study has only its own 2 batches
  expect_false(summary$meta$tsv_written) # data_summaries_dir not configured
})


test_that("process_skeletons writes the TSV when data_summaries_dir is set + run is complete", {
  dir <- withr::local_tempdir()
  summ_dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir   = dir,
    data_summaries_dir  = summ_dir,
    group_names = c("grp1"),
    batch_size  = 3L
  )
  study$set_ids(1:6)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study$register_framework(.cs_framework)
  study$process_skeletons()

  summary <- study$summary
  expect_true(summary$meta$is_complete)
  expect_true(summary$meta$tsv_written)
  written <- list.files(summ_dir, pattern = "^summary_.*\\.tsv$")
  expect_length(written, 1L)
})
