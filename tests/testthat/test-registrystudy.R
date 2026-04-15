# =============================================================================
# Tests for RegistryStudy R6 class
# =============================================================================

test_that("RegistryStudy can be created with defaults", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  expect_s3_class(study, "RegistryStudy")
  expect_equal(study$data_rawbatch_dir, dir)
  expect_equal(study$data_skeleton_dir, dir)
  expect_null(study$data_raw_dir)
  expect_equal(study$group_names, c("lmed", "inpatient", "outpatient", "cancer", "dors", "other"))
  expect_equal(study$batch_size, 1000L)
  expect_equal(study$seed, 4L)
  expect_equal(study$id_col, "lopnr")
  expect_equal(study$n_ids, 0L)
  expect_equal(study$n_batches, 0L)
})

test_that("created_at is set on construction", {
  dir <- withr::local_tempdir()
  before <- Sys.time()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  after <- Sys.time()

  expect_s3_class(study$created_at, "POSIXct")
  expect_true(study$created_at >= before)
  expect_true(study$created_at <= after)
})

test_that("created_at survives save/load round-trip", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  original_time <- study$created_at

  study$save_meta()
  loaded <- qs2_read(study$meta_file)

  expect_s3_class(loaded$created_at, "POSIXct")
  expect_equal(as.numeric(loaded$created_at), as.numeric(original_time))
})

test_that("print shows created timestamp", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  expect_output(print(study), "Created:")
})

test_that("directory active bindings are read-only", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  expect_error(study$data_rawbatch_dir <- "/tmp", "read-only")
  expect_error(study$data_skeleton_dir <- "/tmp", "read-only")
  expect_error(study$data_raw_dir <- "/tmp", "read-only")
})

test_that("portability: re-resolves when cached path becomes invalid", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()

  study <- RegistryStudy$new(data_rawbatch_dir = c(dir1, dir2))
  expect_equal(study$data_rawbatch_dir, dir1)

  # Simulate invalidation: remove the first directory entirely
  unlink(dir1, recursive = TRUE)
  expect_false(dir.exists(dir1))

  # Should fall back to dir2
  expect_equal(study$data_rawbatch_dir, dir2)
})

test_that("data_raw_dir is accessible when set, NULL when not", {
  dir <- withr::local_tempdir()
  raw_dir <- withr::local_tempdir()

  study_no_raw <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_null(study_no_raw$data_raw_dir)

  study_with_raw <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    data_raw_dir = raw_dir
  )
  expect_equal(study_with_raw$data_raw_dir, raw_dir)
})

test_that("data_skeleton_dir defaults to same as data_rawbatch_dir", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_equal(study$data_skeleton_dir, study$data_rawbatch_dir)
})

test_that("set_ids splits IDs into batches correctly", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    batch_size = 5L
  )
  study$set_ids(1:18)

  expect_equal(study$n_ids, 18L)
  # 18 IDs / 5 per batch = 4 batches (last has 3)
  expect_equal(study$n_batches, 4L)
  expect_equal(length(study$batch_id_list[[1]]), 5)
  expect_equal(length(study$batch_id_list[[2]]), 5)
  expect_equal(length(study$batch_id_list[[3]]), 5)
  expect_equal(length(study$batch_id_list[[4]]), 3)
})

test_that("set_ids handles small ID sets", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    batch_size = 100L
  )
  study$set_ids(1:5)

  expect_equal(study$n_ids, 5L)
  expect_equal(study$n_batches, 1L)
  expect_equal(length(study$batch_id_list[[1]]), 5)
})

test_that("code_registry starts empty", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  expect_equal(length(study$code_registry), 0)
})

test_that("register_codes appends to code_registry", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  icd <- list("stroke" = c("I60", "I61"))
  atc <- list("rx_n05a" = c("N05A"))

  study$register_codes(
    codes = icd,
    fn = add_diagnoses,
    groups = list(ov = "outpatient", sv = "inpatient"),
    combine_as = "osdc"
  )
  expect_equal(length(study$code_registry), 1)
  expect_equal(study$code_registry[[1]]$codes, icd)
  expect_equal(study$code_registry[[1]]$combine_as, "osdc")

  study$register_codes(
    codes = atc,
    fn = add_rx,
    fn_args = list(source = "atc"),
    groups = list("lmed")
  )
  expect_equal(length(study$code_registry), 2)
  expect_equal(study$code_registry[[2]]$codes, atc)
  expect_equal(study$code_registry[[1]]$codes, icd)  # unchanged
})

test_that("summary_table returns correct structure for empty codes", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  st <- study$summary_table()

  expect_s3_class(st, "data.table")
  expect_equal(nrow(st), 0)
  expect_true(all(c("name", "codes", "label", "generated_columns") %in% names(st)))
})

test_that("summary_table returns correct ICD10 entries with prefixed columns", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_codes(
    codes = list(
      "stroke_any" = c("I60", "I61", "I63"),
      "mi" = c("I21", "I22")
    ),
    fn = add_diagnoses,
    groups = list(ov = "outpatient", sv = "inpatient", dors = "dors", can = "cancer"),
    combine_as = "osdc"
  )

  st <- study$summary_table()
  expect_equal(nrow(st), 2)

  # Check generated columns for stroke_any
  stroke_row <- st[name == "stroke_any"]
  expect_true(grepl("ov_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("sv_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("dors_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("can_stroke_any", stroke_row$generated_columns))
  expect_true(grepl("osdc_stroke_any", stroke_row$generated_columns))
})

test_that("summary_table returns correct rx entries with no prefix", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_codes(
    codes = list("rx_n05a" = c("N05A")),
    fn = add_rx,
    fn_args = list(source = "atc"),
    groups = list("lmed")
  )

  st <- study$summary_table()
  expect_equal(nrow(st), 1)
  expect_equal(st$generated_columns, "rx_n05a")
})

test_that("data_skeleton_dir can be set independently", {
  rb_dir <- withr::local_tempdir()
  sk_dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = rb_dir,
    data_skeleton_dir = sk_dir
  )
  expect_equal(study$data_rawbatch_dir, rb_dir)
  expect_equal(study$data_skeleton_dir, sk_dir)
})

test_that("single non-existing candidate path is auto-created", {
  parent <- withr::local_tempdir()
  new_dir <- file.path(parent, "newsubdir")
  expect_false(dir.exists(new_dir))
  study <- RegistryStudy$new(data_rawbatch_dir = new_dir)
  expect_true(dir.exists(new_dir))
  expect_equal(study$data_rawbatch_dir, new_dir)
})

test_that("multi-candidate auto-creates first path whose parent exists", {
  parent <- withr::local_tempdir()
  rb_dir <- file.path(parent, "rawbatch")
  sk_dir <- file.path(parent, "skeleton")
  expect_false(dir.exists(rb_dir))
  expect_false(dir.exists(sk_dir))

  study <- RegistryStudy$new(
    data_rawbatch_dir = c("/nonexistent/path/rawbatch", rb_dir),
    data_skeleton_dir = c("/nonexistent/path/skeleton", sk_dir)
  )
  expect_true(dir.exists(rb_dir))
  expect_true(dir.exists(sk_dir))
  expect_equal(study$data_rawbatch_dir, rb_dir)
  expect_equal(study$data_skeleton_dir, sk_dir)
})

test_that("meta_file returns expected path", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_equal(study$meta_file, file.path(dir, "registrystudy.qs2"))
})

test_that("save_rawbatch rejects unknown group", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$set_ids(1:5)

  expect_error(
    study$save_rawbatch("nonexistent", data.table::data.table(lopnr = 1)),
    "not in group_names"
  )
})

test_that("save_rawbatch and load_rawbatch round-trip correctly", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("lmed", "other"),
    batch_size = 3L
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

test_that("process_skeletons runs sequentially and saves one skeleton per batch", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6)

  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  # New three-phase API: register a framework fn (phase 1) and let
  # phase 2 / phase 3 be empty. process_skeletons() walks batches,
  # calls framework_fn, wraps in a Skeleton, and persists.
  study$register_framework(function(batch_data, config) {
    data.table::data.table(
      id = batch_data[["grp1"]]$lopnr,
      week = 1L
    )
  })

  study$process_skeletons()

  skel_files <- list.files(dir, pattern = "^skeleton_\\d+\\.qs2$")
  expect_equal(length(skel_files), 2)

  # Each batch's skeleton round-trips as a Skeleton R6 with the 3 rows
  # the framework fn produced
  sk1 <- study$load_skeleton(1L)
  sk2 <- study$load_skeleton(2L)
  expect_s3_class(sk1, "Skeleton")
  expect_s3_class(sk2, "Skeleton")
  expect_equal(nrow(sk1$data), 3L)
  expect_equal(nrow(sk2$data), 3L)
})

test_that("process_skeletons runs a registered randvars step on every batch", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  study$register_framework(function(batch_data, config) {
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
  })
  study$register_randvars("add_flag", function(skeleton, batch_data, config) {
    skeleton[, flag := TRUE]
    invisible(skeleton)
  })

  study$process_skeletons()

  # Both the framework columns and the randvars column are present
  sk1 <- study$load_skeleton(1L)
  expect_true("flag" %in% names(sk1$data))
  expect_true(all(sk1$data$flag))

  # And the randvars provenance got recorded
  expect_equal(names(sk1$randvars_state), "add_flag")
})

test_that("describe_codes prints without error", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_codes(
    codes = list("stroke_any" = c("I60", "I61", "I63")),
    fn = add_diagnoses,
    groups = list(ov = "outpatient", sv = "inpatient", dors = "dors", can = "cancer"),
    combine_as = "osdc"
  )
  study$register_codes(
    codes = list("rx_n05a" = c("N05A")),
    fn = add_rx,
    fn_args = list(source = "atc"),
    groups = list("lmed")
  )
  study$register_codes(
    codes = list("op_hysterectomy" = c("LCD", "LCC")),
    fn = add_operations,
    groups = list(c("inpatient", "outpatient"))
  )

  expect_output(study$describe_codes(), "add_diagnoses")
  expect_output(study$describe_codes(), "stroke_any")
  expect_output(study$describe_codes(), "rx_n05a")
})

test_that("print method works", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_codes(
    codes = list("stroke_any" = c("I60", "I61", "I63")),
    fn = add_diagnoses,
    groups = list(ov = "outpatient", sv = "inpatient"),
    combine_as = "osdc"
  )
  study$register_codes(
    codes = list("rx_n05a" = c("N05A")),
    fn = add_rx,
    fn_args = list(source = "atc"),
    groups = list("lmed")
  )

  expect_output(print(study), "RegistryStudy")
  expect_output(print(study), "Code registry")
})

test_that("print shows dir candidates with > marking resolved path", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = c(dir1, dir2),
    data_raw_dir = c("/nonexistent/path", dir2)
  )

  out <- capture.output(print(study))
  out_str <- paste(out, collapse = "\n")

  # Resolved rawbatch parent (dir1) should be marked with >
  expect_true(grepl(paste0("> ", dir1), out_str, fixed = TRUE))
  # data_raw resolves to dir2 (first candidate doesn't exist), marked with >
  expect_true(grepl("Data raw:", out_str, fixed = TRUE))
  expect_true(grepl(paste0("> ", dir2), out_str, fixed = TRUE))
})

test_that("reset clears all state", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:3)
  dt <- data.table::data.table(lopnr = 1:3, val = 1:3)
  study$save_rawbatch("grp1", dt)

  expect_true(length(list.files(dir, pattern = "rawbatch")) > 0)

  study$delete_rawbatches()
  study$delete_skeletons()
  study$delete_meta_file()
  expect_equal(study$groups_saved, character(0))
  expect_equal(length(study$skeleton_files), 0)
  expect_equal(length(list.files(dir, pattern = "rawbatch")), 0)
})

test_that("skeleton_files active binding is read-only", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  expect_error(study$skeleton_files <- "x", "read-only")
})

test_that("skeleton_files active binding detects files on disk", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  # No skeleton files yet

  expect_equal(length(study$skeleton_files), 0)

  # Process skeletons to create files via the new three-phase API
  study$register_framework(function(batch_data, config) {
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
  })
  study$process_skeletons()

  # Active binding should detect them without explicit assignment
  expect_true(length(study$skeleton_files) >= 2)
})

test_that("expected_skeleton_file_count matches actual count", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )

  # Before set_ids: 0 expected
  expect_equal(study$expected_skeleton_file_count, 0L)

  study$set_ids(1:6)

  # 2 batches = 2 skeleton files expected
  expect_equal(study$expected_skeleton_file_count, 2L)

  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  study$register_framework(function(batch_data, config) {
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
  })
  study$process_skeletons()

  expect_equal(length(study$skeleton_files), study$expected_skeleton_file_count)
})

test_that("compute_population counts unique persons per year and group", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  # Create skeletons with both annual and weekly rows, sex, and age
  study$register_framework(function(batch_data, config) {
    ids <- batch_data[["grp1"]]$lopnr
    data.table::rbindlist(list(
      # Annual rows (old data)
      data.table::data.table(
        id = ids,
        isoyear = 2000L,
        isoyearweek = "2000-**",
        is_isoyear = TRUE,
        personyears = 1,
        saab = c("Male", "Female", "Male")[seq_along(ids)],
        age = c(25L, 30L, 25L)[seq_along(ids)]
      ),
      # Weekly rows (new data) -- multiple rows per person per year
      data.table::data.table(
        id = rep(ids, each = 3L),
        isoyear = 2020L,
        isoyearweek = rep(c("2020-01", "2020-02", "2020-03"), length(ids)),
        is_isoyear = FALSE,
        personyears = 1 / 52.25,
        saab = rep(c("Male", "Female", "Male")[seq_along(ids)], each = 3L),
        age = rep(c(45L, 50L, 45L)[seq_along(ids)], each = 3L)
      )
    ))
  })
  study$process_skeletons()

  pop <- study$compute_population(by = c("saab", "age"))

  # Check structure
  expect_s3_class(pop, "data.table")
  expect_true(all(c("isoyear", "saab", "age", "n") %in% names(pop)))

  # Weekly rows should NOT inflate counts -- uniqueN ensures 1 count per person
  pop_2020 <- pop[isoyear == 2020 & n > 0]
  expect_equal(sum(pop_2020$n), 6L) # 6 persons total

  # Annual rows: each person counted once
  pop_2000 <- pop[isoyear == 2000 & n > 0]
  expect_equal(sum(pop_2000$n), 6L)

  # CJ grid: missing combinations filled with 0
  expect_true(any(pop$n == 0L))
  # Grid should cover all observed combinations
  expect_equal(nrow(pop), length(unique(pop$isoyear)) *
    length(unique(pop$saab)) * length(unique(pop$age)))

  # File saved to disk
  expect_true(file.exists(file.path(dir, "population.qs2")))
})

test_that("compute_population errors when no skeleton files exist", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_error(study$compute_population(by = "saab"), "No skeleton files found")
})

test_that("compute_population batches parameter filters correctly", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  dt <- data.table::data.table(lopnr = 1:6, val = letters[1:6])
  study$save_rawbatch("grp1", dt)

  study$register_framework(function(batch_data, config) {
    ids <- batch_data[["grp1"]]$lopnr
    data.table::data.table(
      id = ids,
      isoyear = 2020L,
      isoyearweek = "2020-**",
      is_isoyear = TRUE,
      personyears = 1,
      saab = "Male"
    )
  })
  study$process_skeletons()

  # Only batch 1 (3 persons)
  pop <- study$compute_population(by = "saab", batches = 1L)
  expect_equal(pop[n > 0]$n, 3L)

  # Both batches (6 persons)
  pop <- study$compute_population(by = "saab", batches = 1:2)
  expect_equal(pop[n > 0]$n, 6L)
})

# =============================================================================
# CandidatePath integration
# =============================================================================

test_that("RegistryStudy holds CandidatePath fields", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  expect_s3_class(study$data_rawbatch_cp, "CandidatePath")
  expect_s3_class(study$data_skeleton_cp, "CandidatePath")
  expect_null(study$data_raw_cp)
})

test_that("save_meta invalidates caches before serialization", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  # Force a resolve so the cache is populated
  study$data_rawbatch_dir
  expect_true(study$data_rawbatch_cp$is_resolved())

  study$save_meta()

  # After save, the cache must be cleared (so the on-disk file carries no
  # host-specific resolved path).
  expect_false(study$data_rawbatch_cp$is_resolved())

  # The saved file exists and re-loads
  expect_true(file.exists(study$meta_file))
  loaded <- qs2::qs_read(study$meta_file)
  expect_s3_class(loaded, "RegistryStudy")
  expect_false(loaded$data_rawbatch_cp$is_resolved())

  # First access on the loaded object re-resolves
  expect_equal(loaded$data_rawbatch_dir, dir)
  expect_true(loaded$data_rawbatch_cp$is_resolved())
})

test_that("check_version errors on a too-old schema", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  # Forcibly downgrade the schema version on this instance to simulate a
  # study saved under an older format.
  assign(".schema_version", 0L, envir = study$.__enclos_env__$private)

  expect_error(
    study$check_version(),
    "schema version",
    class = NULL
  )
})

test_that("meta_file path uses the new stub-free filename", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_equal(basename(study$meta_file), "registrystudy.qs2")
})

# =============================================================================
# Phase registration (framework + randvars) and pipeline hashing
# =============================================================================

test_that("register_framework stores the function", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  f <- function(batch_data, config) data.table::data.table(id = 1:3)
  study$register_framework(f)

  expect_true(is.function(study$framework_fn))
  expect_identical(study$framework_fn, f)
})

test_that("register_framework rejects non-functions", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_error(study$register_framework("not a function"))
})

test_that("register_randvars appends to randvars_fns in order", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  study$register_randvars("step_a", function(skeleton, batch_data, config) NULL)
  study$register_randvars("step_b", function(skeleton, batch_data, config) NULL)
  study$register_randvars("step_c", function(skeleton, batch_data, config) NULL)

  expect_equal(names(study$randvars_fns), c("step_a", "step_b", "step_c"))
  expect_true(all(vapply(study$randvars_fns, is.function, logical(1))))
})

test_that("register_randvars errors on duplicate step name", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_randvars("step_a", function(skeleton, batch_data, config) NULL)
  expect_error(
    study$register_randvars("step_a", function(skeleton, batch_data, config) NULL),
    "already registered"
  )
})

test_that("register_randvars rejects empty or non-scalar names", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_error(study$register_randvars("", function(s, b, c) NULL))
  expect_error(study$register_randvars(c("a", "b"), function(s, b, c) NULL))
  expect_error(study$register_randvars(123, function(s, b, c) NULL))
})

test_that("code_registry_fingerprints returns one digest per registered entry", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_codes(
    codes = list(foo = "X"),
    fn = add_diagnoses,
    groups = list("inpatient")
  )
  study$register_codes(
    codes = list(bar = "Y"),
    fn = add_diagnoses,
    groups = list("outpatient")
  )
  fps <- study$code_registry_fingerprints()
  expect_length(fps, 2)
  expect_type(fps, "character")
  expect_true(all(nzchar(fps)))
  # Different entries -> different fingerprints
  expect_false(identical(fps[[1]], fps[[2]]))
})

test_that("code_registry_fingerprints is stable across calls on identical state", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_codes(
    codes = list(foo = "X"),
    fn = add_diagnoses,
    groups = list("inpatient")
  )
  fps1 <- study$code_registry_fingerprints()
  fps2 <- study$code_registry_fingerprints()
  expect_identical(fps1, fps2)
})

test_that("pipeline_hash returns a scalar and changes with each component", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  h0 <- study$pipeline_hash()
  expect_type(h0, "character")
  expect_length(h0, 1)

  # Registering a framework changes the hash
  study$register_framework(function(batch_data, config) data.table::data.table())
  h1 <- study$pipeline_hash()
  expect_false(identical(h0, h1))

  # Registering a randvars step changes the hash
  study$register_randvars("a", function(skeleton, batch_data, config) NULL)
  h2 <- study$pipeline_hash()
  expect_false(identical(h1, h2))

  # Registering a code entry changes the hash
  study$register_codes(
    codes = list(foo = "X"),
    fn = add_diagnoses,
    groups = list("inpatient")
  )
  h3 <- study$pipeline_hash()
  expect_false(identical(h2, h3))
})

test_that("pipeline_hash is stable across identical sessions", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  # Two studies with identical config in different directories should still
  # produce the same pipeline hash (it only depends on body/formals of
  # registered fns + code registry fingerprints, not on the directories).
  mk <- function(dir) {
    s <- RegistryStudy$new(data_rawbatch_dir = dir)
    s$register_framework(function(batch_data, config) data.table::data.table())
    s$register_randvars("step_a", function(skeleton, batch_data, config) NULL)
    s$register_codes(
      codes = list(foo = "X"),
      fn = add_diagnoses,
      groups = list("inpatient")
    )
    s
  }
  expect_identical(mk(dir1)$pipeline_hash(), mk(dir2)$pipeline_hash())
})

# =============================================================================
# adopt_runtime_state_from
# =============================================================================

test_that("adopt_runtime_state_from copies runtime but not config", {
  dir_a <- withr::local_tempdir()
  dir_b <- withr::local_tempdir()

  # `other` is a study with different config AND some runtime state
  other <- RegistryStudy$new(
    data_rawbatch_dir = dir_a,
    group_names = c("lmed", "inpatient", "outpatient")  # shorter than default
  )
  other$set_ids(seq_len(2500))
  expect_equal(other$n_ids, 2500L)
  expect_gt(other$n_batches, 0L)

  # `self` is a fresh study with DIFFERENT group_names (and therefore a
  # different config profile). It should adopt runtime from `other` without
  # picking up the stale group_names.
  self_study <- RegistryStudy$new(
    data_rawbatch_dir = dir_b,
    group_names = c("lmed", "inpatient", "outpatient", "cancer", "stroke")
  )
  expect_equal(self_study$n_ids, 0L)

  self_study$adopt_runtime_state_from(other)

  # Runtime fields were copied
  expect_equal(self_study$n_ids, 2500L)
  expect_equal(self_study$n_batches, other$n_batches)
  expect_equal(length(self_study$batch_id_list), length(other$batch_id_list))

  # Config was NOT overwritten
  expect_equal(
    self_study$group_names,
    c("lmed", "inpatient", "outpatient", "cancer", "stroke")
  )
})

test_that("adopt_runtime_state_from rejects non-RegistryStudy", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_error(study$adopt_runtime_state_from("not a study"))
  expect_error(study$adopt_runtime_state_from(list()))
})

# =============================================================================
# load_skeleton / save_skeleton
# =============================================================================

test_that("load_skeleton returns NULL for a missing batch", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_null(study$load_skeleton(1L))
})

test_that("save_skeleton + load_skeleton round-trip a Skeleton R6", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  dt <- data.table::data.table(id = 1:3, isoyear = 2020L, personyears = 1/52)
  sk <- Skeleton$new(data = dt, batch_number = 5L)
  sk$framework_fn_hash <- "fw_hash"

  path <- study$save_skeleton(sk)
  expect_true(file.exists(path))
  expect_equal(basename(path), "skeleton_005.qs2")

  loaded <- study$load_skeleton(5L)
  expect_s3_class(loaded, "Skeleton")
  expect_equal(loaded$batch_number, 5L)
  expect_equal(loaded$framework_fn_hash, "fw_hash")
  expect_identical(loaded$pipeline_hash(), sk$pipeline_hash())
})

test_that("load_skeleton errors loudly on a bare data.table file", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)

  # Bare data.table in place of a Skeleton R6 file
  stray_dt <- data.table::data.table(id = 1:3, x = 10:12)
  stray_path <- file.path(study$data_skeleton_dir, "skeleton_003.qs2")
  qs2::qs_save(stray_dt, stray_path)

  expect_error(
    study$load_skeleton(3L),
    "not a Skeleton R6 object"
  )
})

test_that("load_skeleton errors on a file of an unknown format", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  junk_path <- file.path(study$data_skeleton_dir, "skeleton_001.qs2")
  qs2::qs_save("not a data.table or Skeleton", junk_path)
  expect_error(study$load_skeleton(1L), "not a Skeleton R6 object")
})

# =============================================================================
# data_pipeline_snapshot_cp + write_pipeline_snapshot (unit, non-integration)
# =============================================================================

test_that("data_pipeline_snapshot_cp is NULL by default (feature disabled)", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir)
  expect_null(study$data_pipeline_snapshot_cp)
  expect_null(study$data_pipeline_snapshot_dir)
  # write_pipeline_snapshot is a no-op in this state
  expect_null(study$write_pipeline_snapshot())
})

test_that("data_pipeline_snapshot_cp is wired when constructor arg is given", {
  rb_dir <- withr::local_tempdir()
  snap_dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = rb_dir,
    data_pipeline_snapshot_dir = snap_dir
  )
  expect_s3_class(study$data_pipeline_snapshot_cp, "CandidatePath")
  expect_equal(study$data_pipeline_snapshot_dir, snap_dir)
})

test_that("data_pipeline_snapshot_dir is read-only", {
  rb_dir <- withr::local_tempdir()
  snap_dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = rb_dir,
    data_pipeline_snapshot_dir = snap_dir
  )
  expect_error(study$data_pipeline_snapshot_dir <- "/somewhere", "read-only")
})

# =============================================================================
# Helper: .format_batch_range
# =============================================================================

test_that(".format_batch_range collapses ranges", {
  expect_equal(swereg:::.format_batch_range(integer(0)), "(none)")
  expect_equal(swereg:::.format_batch_range(1L), "1")
  expect_equal(swereg:::.format_batch_range(c(1L, 2L, 3L)), "1-3")
  expect_equal(swereg:::.format_batch_range(c(1L, 2L, 3L, 5L, 6L, 7L)), "1-3, 5-7")
  expect_equal(swereg:::.format_batch_range(c(1L, 3L, 5L)), "1, 3, 5")
  expect_equal(swereg:::.format_batch_range(c(10L, 2L, 3L, 1L)), "1-3, 10")  # sorted
  expect_equal(swereg:::.format_batch_range(c(5L, 5L, 5L)), "5")  # deduped
})
