# .clear_s1_work_dir() removes the s1 work directory before a run starts.
#
# The count, the size and the duration are the point. A run that inherits
# thousands of leftover chunk files spends minutes in unlink() before the first
# sub-step, and the log said nothing about it.

test_that(".clear_s1_work_dir() reports the file count and removes the directory", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  dir.create(file.path(work_dir, "sub"), recursive = TRUE)
  writeLines("a", file.path(work_dir, "s1a_pre_enr1.qs2"))
  writeLines("b", file.path(work_dir, "s1b_enrolled_ids_enr1.qs2"))
  writeLines("c", file.path(work_dir, "sub", "s1c_panel_enr1.qs2"))

  out <- utils::capture.output(n <- .clear_s1_work_dir(work_dir))

  # Three files across two levels. The subdirectory itself is not counted.
  expect_identical(n, 3L)
  expect_length(out, 1L)
  expect_match(
    out,
    "^Cleared s1 work directory: 3 files, .+ in [0-9]+\\.[0-9] s$"
  )
  expect_false(dir.exists(work_dir))
})

test_that(".clear_s1_work_dir() returns 0 and prints nothing when the directory is absent", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  expect_false(dir.exists(work_dir))

  out <- utils::capture.output(n <- .clear_s1_work_dir(work_dir))

  expect_identical(n, 0L)
  expect_identical(out, character(0))
})

test_that(".clear_s1_work_dir() prints the label it is given", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  dir.create(work_dir, recursive = TRUE)
  writeLines("a", file.path(work_dir, "s1a_pre_enr1.qs2"))

  out <- utils::capture.output(
    .clear_s1_work_dir(work_dir, label = "Removed s1 work directory")
  )

  expect_match(
    out,
    "^Removed s1 work directory: 1 files, .+ in [0-9]+\\.[0-9] s$"
  )
})


# A directory that survives the delete. The deleter is injected because
# unlink(force = TRUE) chmods the parents, so a read-only subdirectory does not
# survive it.
test_that(".clear_s1_work_dir() stops on residue, and warns when fatal = FALSE", {
  root <- withr::local_tempdir()
  writeLines("a", file.path(root, "s1a_pre_enr1.qs2"))
  no_delete <- function(...) 0L

  expect_error(
    .clear_s1_work_dir(root, .unlink = no_delete),
    "Could not clear"
  )
  expect_true(dir.exists(root))

  expect_warning(
    n <- .clear_s1_work_dir(root, fatal = FALSE, .unlink = no_delete),
    "Could not clear"
  )
  expect_identical(n, 1L)
  expect_true(dir.exists(root))
})


# The size is what says whether a slow delete moved anything.
test_that(".clear_s1_work_dir() reports the size it deleted", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  dir.create(work_dir, recursive = TRUE)
  writeBin(raw(1000), file.path(work_dir, "s1a_pre_enr1.qs2"))

  expect_output(.clear_s1_work_dir(work_dir), "1 files, 1000 bytes in")
})


# The REAL boundary. The success path is the one that ran for minutes with no
# line in the log, and only a completed s1 executes it. A unit test of the
# helper cannot see which call site the pipeline uses.
test_that("a completed s1 reports the work directory it removed", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )

  sk <- ttm_skeleton("A", n_persons = 400L, seed = 2026L)
  root <- withr::local_tempdir()
  dirs <- list(
    spec = file.path(root, "spec"),
    tteplan = file.path(root, "tteplan"),
    results = file.path(root, "results"),
    meta = file.path(root, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "ttms1clear",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )

  out <- utils::capture.output(
    plan$s1_generate_enrollments_and_ipw(
      n_workers = 1L,
      swereg_dev_path = ttm_dev_path(), check_skeletons = FALSE),
    type = "output"
  )

  hit <- grep("^Removed s1 work directory: ", out, value = TRUE)
  expect_length(hit, 1L)
  # The count, the size and the duration are the point, so assert all three.
  expect_match(
    hit,
    "^Removed s1 work directory: [0-9]+ files, .+ in [0-9]+\\.[0-9] s$"
  )
  # `hit[1]` is NA when the line is absent, so this reports a failure rather
  # than erroring inside expect_gt().
  n_files <- as.integer(sub("^.*directory: ([0-9]+) files.*$", "\\1", hit[1]))
  expect_gt(n_files, 0L)
  expect_false(dir.exists(.s1_work_dir(plan, ensure_exists = FALSE)))
})


# The REAL boundary for `work_root`. Only a completed s1 drives the sweep, the
# flat layout and the validation order through production wiring. A unit test
# of either helper cannot see which call site the pipeline uses.
test_that("a completed s1 works under work_root and sweeps that root first", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )

  sk <- ttm_skeleton("A", n_persons = 400L, seed = 2026L)
  base <- withr::local_tempdir()
  dirs <- list(
    spec = file.path(base, "spec"),
    tteplan = file.path(base, "tteplan"),
    results = file.path(base, "results"),
    meta = file.path(base, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "ttms1root",
    "rd_age_continuous"
  )

  # Two stale entries for the sweep, one of them a dotfile. Populate first:
  # writing into a directory resets its modification time.
  root <- withr::local_tempdir()
  dir.create(file.path(root, "stale"))
  writeBin(raw(1000), file.path(root, "stale", "chunk.qs2"))
  dir.create(file.path(root, ".dotstale"))
  writeBin(raw(1000), file.path(root, ".dotstale", "chunk.qs2"))
  Sys.setFileTime(file.path(root, "stale"), Sys.time() - 15 * 86400)
  Sys.setFileTime(file.path(root, ".dotstale"), Sys.time() - 15 * 86400)

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )

  out <- utils::capture.output(
    plan$s1_generate_enrollments_and_ipw(
      n_workers = 1L,
      swereg_dev_path = ttm_dev_path(),
      work_root = root, check_skeletons = FALSE),
    type = "output"
  )

  # The flat layout, spelled out rather than read back from the helper this
  # test also stands over.
  expect_true(any(out == paste0(
    "Work directory: ",
    file.path(root, "s1_work_ttms1root")
  )))
  expect_true(any(grepl("^Swept stale: ", out)))
  expect_true(any(grepl("^Swept \\.dotstale: ", out)))
  expect_false(dir.exists(file.path(root, "stale")))
  expect_false(dir.exists(file.path(root, ".dotstale")))
  # `work_root` decides the layout alone: the Argos default is never built.
  expect_false(dir.exists(file.path(dirs$meta, "s1_work")))
})


# Validation precedes every side effect, the output directory included. This
# stands in its own block so neither assertion depends on the run above.
test_that("s1 refuses a relative work_root and creates nothing", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )

  # A plan the method accepts. It has ETTs and a spec, so nothing before the
  # output directory can stop the call and hide what this block measures.
  sk <- ttm_skeleton("A", n_persons = 400L, seed = 2026L)
  base <- withr::local_tempdir()
  dirs <- list(
    spec = file.path(base, "spec"),
    tteplan = file.path(base, "tteplan"),
    results = file.path(base, "results"),
    meta = file.path(base, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "ttms1rel",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )

  out_dir <- tempfile()
  # tryCatch, never expect_error(regexp =). testthat re-raises an error whose
  # message does not match the pattern. A run that got past validation and
  # died downstream would then report as an error, and not as this assertion
  # failing. The message is captured as a value here, so every wrong message
  # is a failure of THIS assertion. NA_character_ stands for "the call
  # returned", which expect_match also fails.
  msg <- tryCatch(
    {
      plan$s1_generate_enrollments_and_ipw(
        output_dir = out_dir,
        work_root = "relative/x", check_skeletons = FALSE)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  expect_match(msg, "work_root.*absolute")
  expect_false(dir.exists(out_dir))
})


# The public-method boundary for the existence check. `.validate_scratch_root()`
# on its own cannot show that the s1 method reaches it before any side effect.
test_that("s1 refuses a work_root that does not exist and creates nothing", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("yaml")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )

  sk <- ttm_skeleton("A", n_persons = 400L, seed = 2026L)
  base <- withr::local_tempdir()
  dirs <- list(
    spec = file.path(base, "spec"),
    tteplan = file.path(base, "tteplan"),
    results = file.path(base, "results"),
    meta = file.path(base, "meta")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "ttms1absent",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )

  # A root whose PARENT exists, so only the root itself is missing. The
  # arguments match the completed-s1 block above, so a run that got past
  # validation would be the real pipeline and would build the root.
  absent <- file.path(withr::local_tempdir(), "nonexistent")
  out_dir <- tempfile()
  # tryCatch, never expect_error(regexp =). testthat re-raises an error whose
  # message does not match, which would end the block and hide the two
  # directory assertions below it.
  msg <- tryCatch(
    {
      plan$s1_generate_enrollments_and_ipw(
        output_dir = out_dir,
        n_workers = 1L,
        swereg_dev_path = ttm_dev_path(),
        work_root = absent, check_skeletons = FALSE)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  expect_match(msg, "work_root.*existing directory")
  expect_false(dir.exists(absent))
  expect_false(dir.exists(out_dir))
})
