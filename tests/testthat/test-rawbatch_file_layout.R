# Pin the rawbatch file-layout contract that downstream
# `process_skeletons()` and `tte_*` workflows depend on:
#
#   1. File naming: `{BBB}_rawbatch_{group}.qs2` with BBB = zero-padded
#      3-digit batch number (001, 002, ...).
#   2. Per-batch partitioning is person-disjoint -- a person ID never
#      appears in more than one batch's rawbatch file for the same group.
#
# A change to either invariant silently breaks `load_rawbatch()` /
# `process_skeletons()`. test-registrystudy.R already covers basic
# round-trip; this file pins the layout + partitioning contracts
# explicitly.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")

test_that("save_rawbatch produces files named {BBB}_rawbatch_{group}.qs2", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("lmed", "other"),
    batch_size = 3L
  )
  study$set_ids(1:8)  # -> 3 batches: 1-3, 4-6, 7-8

  lmed <- data.table::data.table(
    lopnr = 1:8,
    atc   = rep("N05A", 8L)
  )
  study$save_rawbatch("lmed", lmed)

  files <- list.files(dir, pattern = "^\\d+_rawbatch_lmed\\.qs2$")
  expect_setequal(files, c("001_rawbatch_lmed.qs2",
                           "002_rawbatch_lmed.qs2",
                           "003_rawbatch_lmed.qs2"))
  # Specifically zero-padded to width 3
  expect_true(all(grepl("^\\d{3}_rawbatch_lmed\\.qs2$", files)))
})

test_that("save_rawbatch partitions IDs disjointly across batches", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("lmed"),
    batch_size = 3L
  )
  study$set_ids(1:8)

  lmed <- data.table::data.table(
    lopnr = rep(1:8, each = 2L),
    atc   = "N05A"
  )
  study$save_rawbatch("lmed", lmed)

  per_batch_ids <- list()
  for (b in seq_len(study$n_batches)) {
    batch <- study$load_rawbatch(b)
    per_batch_ids[[b]] <- unique(batch$lmed$lopnr)
  }
  # Disjoint: union over batches has no duplicates.
  flat <- unlist(per_batch_ids)
  expect_equal(length(flat), length(unique(flat)),
               info = "save_rawbatch should partition IDs disjointly across batches")
  # And union covers all original IDs.
  expect_setequal(flat, 1:8)
})

test_that("load_rawbatch returns a named list keyed by group_names", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("lmed", "other"),
    batch_size = 3L
  )
  study$set_ids(1:5)

  lmed <- data.table::data.table(lopnr = 1:5, atc = "N05A")
  other <- list(
    grunduppgifter = data.table::data.table(lopnr = 1:5, fodelsear = 1970L)
  )
  study$save_rawbatch("lmed", lmed)
  study$save_rawbatch("other", other)

  batch1 <- study$load_rawbatch(1)
  expect_true("lmed" %in% names(batch1))
  expect_true("grunduppgifter" %in% names(batch1))
})

test_that("load_rawbatch errors on out-of-range batch numbers", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("lmed"),
    batch_size = 3L
  )
  study$set_ids(1:5)
  lmed <- data.table::data.table(lopnr = 1:5, atc = "N05A")
  study$save_rawbatch("lmed", lmed)

  expect_error(study$load_rawbatch(0L), "batch_number")
  expect_error(study$load_rawbatch(study$n_batches + 1L), "batch_number")
})
