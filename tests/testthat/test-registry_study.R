# =============================================================================
# Tests for RegistryStudy R6 class
# =============================================================================

test_that("RegistryStudy can be created with defaults", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)

  expect_s3_class(study, "RegistryStudy")
  expect_equal(study$data_generic_dir, dir)
  expect_equal(study$skeleton_dir, dir)
  expect_null(study$data_raw_dir)
  expect_equal(study$group_names, c("lmed", "inpatient", "outpatient", "cancer", "dors", "other"))
  expect_equal(study$batch_sizes, c(1000L, 10000L))
  expect_equal(study$seed, 4L)
  expect_equal(study$id_col, "lopnr")
  expect_equal(study$n_ids, 0L)
  expect_equal(study$n_batches, 0L)
})

test_that("created_at is set on construction", {
  dir <- withr::local_tempdir()
  before <- Sys.time()
  study <- RegistryStudy$new(data_generic_dir = dir)
  after <- Sys.time()

  expect_s3_class(study$created_at, "POSIXct")
  expect_true(study$created_at >= before)
  expect_true(study$created_at <= after)
})

test_that("created_at survives save/load round-trip", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  original_time <- study$created_at

  study$save_meta()
  loaded <- .qs_read(study$meta_file)

  expect_s3_class(loaded$created_at, "POSIXct")
  expect_equal(as.numeric(loaded$created_at), as.numeric(original_time))
})

test_that("print shows created timestamp", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)

  expect_output(print(study), "Created:")
})

test_that("directory active bindings are read-only", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)

  expect_error(study$data_generic_dir <- "/tmp", "read-only")
  expect_error(study$skeleton_dir <- "/tmp", "read-only")
  expect_error(study$data_raw_dir <- "/tmp", "read-only")
})

test_that("portability: re-resolves when cached path becomes invalid", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()

  study <- RegistryStudy$new(data_generic_dir = c(dir1, dir2))
  expect_equal(study$data_generic_dir, dir1)

  # Simulate invalidation: remove the first directory
  unlink(dir1, recursive = TRUE)
  expect_false(dir.exists(dir1))

  # Should fall back to dir2
  expect_equal(study$data_generic_dir, dir2)
})

test_that("data_raw_dir is accessible when set, NULL when not", {
  dir <- withr::local_tempdir()
  raw_dir <- withr::local_tempdir()

  study_no_raw <- RegistryStudy$new(data_generic_dir = dir)
  expect_null(study_no_raw$data_raw_dir)

  study_with_raw <- RegistryStudy$new(
    data_generic_dir = dir,
    data_raw_dir = raw_dir
  )
  expect_equal(study_with_raw$data_raw_dir, raw_dir)
})

test_that("skeleton_dir defaults to same candidates as data_generic_dir", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  expect_equal(study$skeleton_dir, study$data_generic_dir)
})

test_that("set_ids splits IDs into batches correctly", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    batch_sizes = c(3L, 5L)
  )
  study$set_ids(1:18)

  expect_equal(study$n_ids, 18L)
  # First batch = 3, remaining 15 in batches of 5 = 3 more batches = 4 total

  expect_equal(study$n_batches, 4L)
  expect_equal(length(study$batch_id_list[[1]]), 3)
  expect_equal(length(study$batch_id_list[[2]]), 5)
  expect_equal(length(study$batch_id_list[[3]]), 5)
  expect_equal(length(study$batch_id_list[[4]]), 5)
})

test_that("set_ids handles small ID sets", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    batch_sizes = c(100L, 1000L)
  )
  study$set_ids(1:5)

  expect_equal(study$n_ids, 5L)
  expect_equal(study$n_batches, 1L)
  expect_equal(length(study$batch_id_list[[1]]), 5)
})

test_that("code registry starts empty", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)

  expect_equal(length(study$icd10_codes), 0)
  expect_equal(length(study$rx_atc_codes), 0)
  expect_equal(length(study$rx_produkt_codes), 0)
  expect_equal(length(study$operation_codes), 0)
  expect_equal(length(study$icdo3_codes), 0)
})

test_that("register_codes assigns code lists", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)

  icd <- list("stroke" = c("I60", "I61"))
  atc <- list("rx_n05a" = c("N05A"))

  study$register_codes(icd10_codes = icd)
  expect_equal(study$icd10_codes, icd)
  expect_equal(length(study$rx_atc_codes), 0)

  study$register_codes(rx_atc_codes = atc)
  expect_equal(study$rx_atc_codes, atc)
  expect_equal(study$icd10_codes, icd)  # unchanged
})

test_that("summary_table returns correct structure for empty codes", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  st <- study$summary_table()

  expect_s3_class(st, "data.table")
  expect_equal(nrow(st), 0)
  expect_true(all(c("name", "codes", "type", "generated_columns") %in% names(st)))
})

test_that("summary_table returns correct ICD10 entries with 5 columns each", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  study$icd10_codes <- list(
    "stroke_any" = c("I60", "I61", "I63"),
    "mi" = c("I21", "I22")
  )

  st <- study$summary_table()
  expect_equal(nrow(st), 2)
  expect_equal(st$type, c("icd10", "icd10"))

  # Check generated columns for stroke_any
  stroke_row <- st[name == "stroke_any"]
  expect_true(grepl("ov_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("sv_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("dors_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("can_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("osdc_stroke_any", stroke_row$generated_columns))
})

test_that("summary_table returns correct rx_atc entries with 1 column each", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  study$rx_atc_codes <- list("rx_n05a" = c("N05A"))

  st <- study$summary_table()
  expect_equal(nrow(st), 1)
  expect_equal(st$type, "rx_atc")
  expect_equal(st$generated_columns, "rx_n05a")
})

test_that("summary_table respects type filter", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  study$icd10_codes <- list("stroke_any" = c("I60", "I61", "I63"))
  study$rx_atc_codes <- list("rx_n05a" = c("N05A"))

  st_icd <- study$summary_table(type = "icd10")
  expect_equal(nrow(st_icd), 1)
  expect_equal(st_icd$type, "icd10")

  st_rx <- study$summary_table(type = "rx_atc")
  expect_equal(nrow(st_rx), 1)
  expect_equal(st_rx$type, "rx_atc")
})

test_that("meta_file returns expected path", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  expect_equal(study$meta_file, file.path(dir, "registry_study_meta.qs2"))
})

test_that("save_rawbatch rejects unknown group", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  study$set_ids(1:5)

  expect_error(
    study$save_rawbatch("nonexistent", data.table::data.table(lopnr = 1)),
    "not in group_names"
  )
})

test_that("save_rawbatch and load_rawbatch round-trip correctly", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    group_names = c("lmed", "other"),
    batch_sizes = c(2L, 3L)
  )
  study$set_ids(1:5)

  lmed <- data.table::data.table(
    lopnr = c(1L, 1L, 2L, 3L, 4L, 5L),
    atc = c("N05A", "C10", "N05A", "A10", "C02", "N05A")
  )
  other <- list(
    "grunduppgifter" = data.table::data.table(
      lopnr = 1:5,
      fodelsear = rep(1970L, 5)
    )
  )

  study$save_rawbatch("lmed", lmed)
  study$save_rawbatch("other", other)

  expect_true("lmed" %in% study$groups_saved)
  expect_true("other" %in% study$groups_saved)

  batch1 <- study$load_rawbatch(1)
  expect_true("lmed" %in% names(batch1))
  expect_true("grunduppgifter" %in% names(batch1))
  expect_s3_class(batch1[["lmed"]], "data.table")
})

test_that("process_skeletons runs sequentially", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    group_names = c("grp1"),
    batch_sizes = c(3L, 3L)
  )
  study$set_ids(1:6)

  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  result <- study$process_skeletons(
    function(batch_data, batch_number, config) {
      nrow(batch_data[["grp1"]])
    }
  )

  expect_equal(result$results[[1]], 3)
  expect_equal(result$results[[2]], 3)
})

test_that("process_skeletons saves skeleton when returned by process_fn", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    group_names = c("grp1"),
    batch_sizes = c(3L, 3L),
    ids_per_skeleton_file = 3L
  )
  study$set_ids(1:6)
  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  result <- study$process_skeletons(function(batch_data, batch_number, config) {
    skeleton <- data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
    profiling <- data.table::data.table(label = "test", time = Sys.time(), mem_mb = 0)
    list(skeleton = skeleton, profiling = profiling)
  })

  skel_files <- list.files(dir, pattern = "skeleton_.*\\.qs2$")
  expect_true(length(skel_files) >= 2)
  expect_true(length(result$study$skeleton_files) >= 2)
  expect_s3_class(result$results[[1]], "data.table")
  expect_true("label" %in% names(result$results[[1]]))
})

test_that("describe_codes prints without error", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  study$icd10_codes <- list("stroke_any" = c("I60", "I61", "I63"))
  study$rx_atc_codes <- list("rx_n05a" = c("N05A"))
  study$operation_codes <- list("op_hysterectomy" = c("LCD", "LCC"))

  expect_output(study$describe_codes(), "ICD-10")
  expect_output(study$describe_codes(type = "icd10"), "stroke_any")
  expect_output(study$describe_codes(type = "rx_atc"), "rx_n05a")
})

test_that("print method works", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)
  study$icd10_codes <- list("stroke_any" = c("I60", "I61", "I63"))
  study$rx_atc_codes <- list("rx_n05a" = c("N05A"))

  expect_output(print(study), "RegistryStudy")
  expect_output(print(study), "icd10")
  expect_output(print(study), "rx_atc")
})

test_that("print shows dir candidates with > marking resolved path", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = c(dir1, dir2),
    data_raw_dir = c("/nonexistent/path", dir2)
  )

  out <- capture.output(print(study))
  out_str <- paste(out, collapse = "\n")

  # Resolved data_generic (dir1) should be marked with >
  expect_true(grepl(paste0("> ", dir1), out_str, fixed = TRUE))
  # data_raw resolves to dir2 (first candidate doesn't exist), marked with >
  expect_true(grepl("Data raw:", out_str, fixed = TRUE))
  expect_true(grepl(paste0("> ", dir2), out_str, fixed = TRUE))
})

test_that("reset clears all state", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    group_names = c("grp1"),
    batch_sizes = c(3L)
  )
  study$set_ids(1:3)
  dt <- data.table::data.table(lopnr = 1:3, val = 1:3)
  study$save_rawbatch("grp1", dt)

  expect_true(length(list.files(dir, pattern = "rawbatch")) > 0)

  study$reset()
  expect_equal(study$groups_saved, character(0))
  expect_equal(length(study$skeleton_files), 0)
  expect_equal(length(list.files(dir, pattern = "rawbatch")), 0)
})

test_that("skeleton_files active binding is read-only", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_generic_dir = dir)

  expect_error(study$skeleton_files <- "x", "read-only")
})

test_that("skeleton_files active binding detects files on disk", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    group_names = c("grp1"),
    batch_sizes = c(3L, 3L),
    ids_per_skeleton_file = 3L
  )
  study$set_ids(1:6)
  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  # No skeleton files yet

  expect_equal(length(study$skeleton_files), 0)

  # Process skeletons to create files
  study$process_skeletons(function(batch_data, batch_number, config) {
    skeleton <- data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
    profiling <- data.table::data.table(label = "test", time = Sys.time(), mem_mb = 0)
    list(skeleton = skeleton, profiling = profiling)
  })

  # Active binding should detect them without explicit assignment
  expect_true(length(study$skeleton_files) >= 2)
})

test_that("expected_skeleton_file_count matches actual count", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_generic_dir = dir,
    group_names = c("grp1"),
    batch_sizes = c(3L, 3L),
    ids_per_skeleton_file = 3L
  )

  # Before set_ids: 0 expected
  expect_equal(study$expected_skeleton_file_count, 0L)

  study$set_ids(1:6)

  # 2 batches of 3 IDs, 3 IDs per skeleton file = 1 file per batch = 2 expected
  expect_equal(study$expected_skeleton_file_count, 2L)

  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  study$process_skeletons(function(batch_data, batch_number, config) {
    skeleton <- data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
    profiling <- data.table::data.table(label = "test", time = Sys.time(), mem_mb = 0)
    list(skeleton = skeleton, profiling = profiling)
  })

  expect_equal(length(study$skeleton_files), study$expected_skeleton_file_count)
})
