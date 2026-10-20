# RegistryStudy$set_batches(): a caller-defined batch composition, an id in
# more than one batch, and the guard that keeps a composition with the files it
# wrote.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")

# Id 3 is in batch 1 and in batch 3.
.sb_batches <- list(c(1L, 2L, 3L), c(4L, 5L), c(6L, 3L))

# Two outpatient rows for id 3, in ISO weeks 2020-02 and 2020-06, and one row
# for id 5 in week 2020-04.
.sb_outpatient <- function() {
  return(data.table::data.table(
    lopnr = c(3L, 5L, 3L),
    indatum = as.Date(c("2020-01-08", "2020-01-22", "2020-02-05")),
    hdia = c("E110", "E110", "E110")
  ))
}

.sb_framework <- function(batch_data, config) {
  return(swereg::create_skeleton(
    ids = batch_data[["persons"]]$lopnr,
    isoyear_min = 2020,
    isoyearweek_max = "2020-10"
  ))
}

.sb_study <- function(dir, rawbatch_workers) {
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("persons", "outpatient"),
    batch_size = 3L
  )
  study$set_batches(.sb_batches)
  invisible(utils::capture.output({
    study$save_rawbatch(
      "persons",
      data.table::data.table(lopnr = 1:6),
      n_workers = rawbatch_workers
    )
    study$save_rawbatch(
      "outpatient",
      .sb_outpatient(),
      n_workers = rawbatch_workers
    )
  }))
  study$register_framework(.sb_framework)
  study$register_codes(
    codes = list(e11 = "E11"),
    fn = swereg::add_diagnoses,
    groups = list(ov = "outpatient")
  )
  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))
  return(study)
}

# The ov_e11 values of id 3 in weeks 2020-02 and 2020-06 of one skeleton.
.sb_id3_weeks <- function(study, batch) {
  d <- study$load_skeleton(batch)$data
  out <- d[
    id == 3L & isoyearweek %in% c("2020-02", "2020-06") & !is_isoyear,
    ov_e11
  ]
  return(out)
}

.sb_expect_id3_in_batches_1_and_3 <- function(study) {
  expect_identical(.sb_id3_weeks(study, 1L), c(TRUE, TRUE), info = "skeleton_00001")
  expect_identical(.sb_id3_weeks(study, 3L), c(TRUE, TRUE), info = "skeleton_00003")
  # Batch 2 holds id 5, and id 5 has one diagnosis week.
  d2 <- study$load_skeleton(2L)$data
  expect_identical(sum(d2[id == 5L & !is_isoyear, ov_e11]), 1L)
  expect_false(3L %in% d2$id)
  return(invisible(NULL))
}

test_that("an id in batches 1 and 3 gets its diagnosis in skeleton 1 and skeleton 3", {
  study <- .sb_study(withr::local_tempdir(), rawbatch_workers = 1L)
  expect_identical(study$n_batches, 3L)
  expect_identical(study$n_ids, 6L)
  .sb_expect_id3_in_batches_1_and_3(study)
})

test_that("an id in batches 1 and 3 gets its diagnosis in both skeletons with a parallel save_rawbatch", {
  skip_if_not_installed("mirai")
  study <- .sb_study(withr::local_tempdir(), rawbatch_workers = 2L)
  .sb_expect_id3_in_batches_1_and_3(study)
})

test_that("set_batches stops on an id that repeats within one batch", {
  study <- RegistryStudy$new(
    data_rawbatch_dir = withr::local_tempdir(),
    group_names = "grp1"
  )
  expect_error(
    study$set_batches(list(c(1L, 2L), c(3L, 4L, 3L))),
    "batch 2 holds ID 3 more than once"
  )
  expect_error(study$set_batches(list()), "non-empty list")
  expect_error(study$set_batches(list(1:2, integer(0))), "batch 2 must be")
  # The failed calls set nothing.
  expect_identical(study$batch_id_list, list())
  expect_null(study$batch_composition_digest)
})

test_that("set_ids keeps its 27.2.6 batch_id_list, and set_batches names batches as split() does", {
  study <- RegistryStudy$new(
    data_rawbatch_dir = withr::local_tempdir(),
    batch_size = 1000L,
    seed = 4L
  )
  study$set_ids(1:5000)
  expect_identical(
    digest::digest(study$batch_id_list),
    "5f1e1096fd6f7a15b5cb495909d3ca3f"
  )
  expect_identical(names(study$batch_id_list), as.character(1:5))
  expect_identical(
    study$batch_composition_digest,
    "5f1e1096fd6f7a15b5cb495909d3ca3f"
  )

  # The same composition through set_batches() gives the same list, the same
  # digest, and no shuffle.
  other <- RegistryStudy$new(data_rawbatch_dir = withr::local_tempdir())
  other$set_batches(unname(study$batch_id_list))
  expect_identical(other$batch_id_list, study$batch_id_list)
  expect_identical(
    other$batch_composition_digest,
    "5f1e1096fd6f7a15b5cb495909d3ca3f"
  )

  other$set_batches(.sb_batches)
  expect_identical(names(other$batch_id_list), c("1", "2", "3"))
  expect_identical(unname(other$batch_id_list), .sb_batches)
  expect_identical(other$n_batches, 3L)
  expect_identical(other$n_ids, 6L)
})

test_that("a changed composition stops while rawbatch or skeleton files are on disk", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "persons",
    batch_size = 3L
  )
  study$set_ids(1:6)
  # No file on disk yet: a new composition is free.
  study$set_batches(list(1:3, 4:6))
  study$set_ids(1:6)
  invisible(utils::capture.output(
    study$save_rawbatch("persons", data.table::data.table(lopnr = 1:6))
  ))

  expect_error(
    study$set_batches(list(1:3, 4:6)),
    "set_batches\\(\\): the new batch composition differs .* 2 rawbatch and 0 skeleton files"
  )
  expect_error(
    study$set_ids(1:7),
    "set_ids\\(\\): the new batch composition differs"
  )
  # The same composition again does not stop.
  expect_no_error(study$set_ids(1:6))

  study$register_framework(function(batch_data, config) {
    return(swereg::create_skeleton(
      ids = batch_data[["persons"]]$lopnr,
      isoyear_min = 2020,
      isoyearweek_max = "2020-03"
    ))
  })
  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))
  invisible(utils::capture.output(study$delete_rawbatches()))
  expect_length(list.files(dir, pattern = "_rawbatch_"), 0L)

  # The skeletons alone still hold the old composition.
  expect_error(
    study$set_batches(list(1:3, 4:6)),
    "differs .* 0 rawbatch and 2 skeleton files"
  )

  # With the skeletons gone too, the new composition is accepted.
  invisible(utils::capture.output(study$delete_skeletons()))
  expect_no_error(study$set_batches(list(1:3, 4:6)))
  expect_identical(
    study$batch_composition_digest,
    digest::digest(study$batch_id_list)
  )
})

test_that("a disjoint set_batches writes rawbatch files byte-identical to set_ids", {
  ids <- 1:8
  persons <- data.table::data.table(lopnr = 1:8, val = letters[1:8])
  write_rawbatch <- function(dir, set) {
    study <- RegistryStudy$new(
      data_rawbatch_dir = dir,
      group_names = c("persons", "outpatient"),
      batch_size = 3L
    )
    set(study)
    invisible(utils::capture.output({
      study$save_rawbatch("persons", data.table::copy(persons))
      study$save_rawbatch(
        "outpatient",
        list(op = .sb_outpatient(), lookup = data.frame(code = "E11"))
      )
    }))
    return(study)
  }

  dir_ids <- withr::local_tempdir()
  by_ids <- write_rawbatch(dir_ids, function(s) s$set_ids(ids))
  dir_batches <- withr::local_tempdir()
  composition <- unname(by_ids$batch_id_list)
  by_batches <- write_rawbatch(dir_batches, function(s) {
    return(s$set_batches(composition))
  })

  files <- sort(list.files(dir_ids, pattern = "_rawbatch_"))
  expect_length(files, 6L)
  expect_identical(sort(list.files(dir_batches, pattern = "_rawbatch_")), files)
  expect_identical(
    unname(tools::md5sum(file.path(dir_batches, files))),
    unname(tools::md5sum(file.path(dir_ids, files)))
  )

  # The same composition writes the content of the 27.2.6 rawbatch files.
  fixture <- test_path("fixtures", "registrystudy_27_2_6")
  old <- qs2_read(file.path(fixture, "registrystudy.qs2"))
  dir_old <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir_old,
    group_names = "grp1",
    batch_size = 3L
  )
  study$set_batches(unname(old$batch_id_list))
  invisible(utils::capture.output(
    study$save_rawbatch(
      "grp1",
      data.table::data.table(lopnr = 1:6, val = letters[1:6])
    )
  ))
  for (f in c("00001_rawbatch_grp1.qs2", "00002_rawbatch_grp1.qs2")) {
    expect_identical(
      qs2_read(file.path(dir_old, f)),
      qs2_read(file.path(fixture, f)),
      info = f
    )
  }
})

# The function make_fixture.R registered in swereg 27.2.6. Its body MUST stay
# byte-identical, so that its hash is 8cd1f322b83f0af5 and the fixture's
# skeletons match the current pipeline.
.sb_fixture_framework <- function(batch_data, config) {
  data.table::data.table(
    id = batch_data[["grp1"]]$lopnr,
    isoyear = 2020L,
    isoyearweek = "2020-01",
    is_isoyear = FALSE
  )
}

test_that("a store written by swereg 27.2.6 runs with no stop and no rebuilt batch", {
  expect_identical(.hash_function(.sb_fixture_framework), "8cd1f322b83f0af5")
  fixture <- test_path("fixtures", "registrystudy_27_2_6")
  batch_files <- c(
    "skeleton_00001.qs2",
    "skeleton_00002.qs2",
    "meta_00001.qs2",
    "meta_00002.qs2",
    "00001_rawbatch_grp1.qs2",
    "00002_rawbatch_grp1.qs2"
  )
  copy_fixture <- function() {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
    file.copy(
      list.files(fixture, full.names = TRUE),
      dir,
      copy.date = TRUE
    )
    return(dir)
  }
  file_state <- function(dir) {
    paths <- file.path(dir, batch_files)
    return(list(
      md5 = unname(tools::md5sum(paths)),
      mtime = file.mtime(paths)
    ))
  }
  fresh_study <- function(dir) {
    study <- RegistryStudy$new(
      data_rawbatch_dir = dir,
      group_names = "grp1",
      batch_size = 3L
    )
    study$register_framework(.sb_fixture_framework)
    return(study)
  }

  # The fixture carries no digest, as every 27.2.6 study does.
  old <- qs2_read(file.path(fixture, "registrystudy.qs2"))
  expect_false("batch_composition_digest" %in% ls(old, all.names = TRUE))

  # Path 1: adopt the runtime state of the 27.2.6 study.
  dir <- copy_fixture()
  before <- file_state(dir)
  study <- fresh_study(dir)
  expect_no_error(study$adopt_runtime_state_from(
    qs2_read(file.path(dir, "registrystudy.qs2"))
  ))
  expect_identical(
    study$batch_composition_digest,
    digest::digest(study$batch_id_list)
  )
  expect_no_error(invisible(utils::capture.output(
    study$process_skeletons(n_workers = 1L)
  )))
  expect_identical(file_state(dir), before)
  expect_false(is.null(study$skeleton_manifest))

  # Path 2: no adopt. set_ids() rebuilds the composition with no recorded
  # digest, while the 27.2.6 files are on disk.
  dir <- copy_fixture()
  before <- file_state(dir)
  study <- fresh_study(dir)
  expect_no_error(study$set_ids(1:6))
  expect_identical(study$batch_id_list, old$batch_id_list)
  expect_identical(study$groups_saved, "grp1")
  expect_no_error(invisible(utils::capture.output(
    study$process_skeletons(n_workers = 1L)
  )))
  expect_identical(file_state(dir), before)
})
