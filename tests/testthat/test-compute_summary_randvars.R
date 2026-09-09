# $compute_summary() reports what phase 3 wrote and what the framework
# removed.
#
# Phase 3 columns never reached the summary. Only a code registry entry
# carried per-column counts, so a study whose exposure is a randvars column
# got a summary that said nothing about its exposure.
#
# Three properties are pinned here:
#   * every logical and character column a randvars step adds is counted,
#     one row per level for a character column
#   * a batch whose meta predates the counts is backfilled from the
#     skeleton on disk, with no phase replay
#   * the framework's own `framework_removals` attribute is summed over the
#     batches that report it
#
# The fixture is two batches of 10 persons. Each person holds 4 weekly rows
# and 1 annual row, so the batch holds 50 rows and the study holds 100.

library(data.table)

.csr_weeks <- c("2020-01", "2020-02", "2020-03", "2020-04")

# What the fixture framework reports per batch. Summed over the two
# batches: age removes 14 persons and 56 person-weeks, region removes 6
# and 24.
.csr_removals <- function() {
  data.table::data.table(
    step = c("age", "region"),
    persons_removed = c(7L, 3L),
    person_weeks_removed = c(28L, 12L),
    persons_after = c(10L, 10L),
    person_weeks_after = c(40L, 40L)
  )
}

.csr_framework <- function(batch_data, config) {
  ids <- sort(batch_data[["grp1"]]$lopnr)
  weekly <- data.table::CJ(id = ids, isoyearweek = .csr_weeks)
  weekly[, `:=`(isoyear = 2020L, is_isoyear = FALSE)]
  annual <- data.table::data.table(
    id = ids,
    isoyearweek = NA_character_,
    isoyear = 2020L,
    is_isoyear = TRUE
  )
  d <- data.table::rbindlist(list(weekly, annual), use.names = TRUE)
  data.table::setattr(d, "framework_removals", .csr_removals())
  d
}

# Rank each person within the batch, then key the derived columns off the
# rank. Every batch holds 10 persons, so the per-batch counts hold whichever
# ids $set_ids() shuffled into it.
#
# rd_x is "a" for ranks 1-6, "b" for ranks 7-9, and NA for rank 10.
# ri_y is TRUE for ranks 1-5, FALSE for ranks 6-8, and NA for ranks 9-10.
# ri_n is an integer, so it carries no presence to count.
.csr_randvars <- function(skeleton, batch_data, config) {
  r <- match(skeleton$id, sort(unique(skeleton$id)))
  skeleton[, rd_x := data.table::fcase(
    r <= 6L,
    "a",
    r <= 9L,
    "b",
    default = NA_character_
  )]
  skeleton[, ri_y := data.table::fcase(
    r <= 5L,
    TRUE,
    r <= 8L,
    FALSE,
    default = NA
  )]
  skeleton[, ri_n := r]
  invisible(skeleton)
}

# A two-batch study that writes the audit-track TSV. `summaries` is where
# the TSV lands; the caller reads it back.
.csr_study <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  summaries <- withr::local_tempdir(.local_envir = env)
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    data_summaries_dir = summaries,
    group_names = "grp1",
    batch_size = 10L
  )
  study$set_ids(1:20)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:20, val = letters[1:20])
  )
  study$register_framework(.csr_framework)
  study$register_randvars("exposure", .csr_randvars)
  study
}

# The newest audit-track TSV in the study's summaries directory.
.csr_tsv_path <- function(study) {
  dir <- study$data_summaries_cp$resolve()
  files <- list.files(dir, pattern = "^summary_.*\\.tsv$", full.names = TRUE)
  expect_gt(length(files), 0L)
  files[order(file.mtime(files), decreasing = TRUE)][[1L]]
}

# The TSV body, without the comment header block. A summary with no
# per-column rows writes the header block and stops, so the body can be one
# line or none. Read it in a way that survives that, or an empty body
# errors before the assertion that is meant to catch it.
.csr_tsv <- function(study) {
  lines <- readLines(.csr_tsv_path(study))
  body <- lines[!startsWith(lines, "#")]
  if (length(body) == 0L) {
    return(data.table::data.table(column_name = character(0)))
  }
  data.table::fread(
    text = paste(body, collapse = "\n"),
    header = TRUE,
    colClasses = "character"
  )
}

.csr_status_lines <- function(study) {
  readLines(file.path(study$data_meta_dir, "status.txt"))
}

# `[.data.table` resolves a bare name against the table's own columns
# first, so the key argument carries a name no column has.
.csr_row <- function(tsv, col_key) {
  tsv[tsv$column_name == col_key, ]
}


test_that("the summary counts every randvars column, one row per level", {
  study <- .csr_study()
  study$process_skeletons()

  tsv <- .csr_tsv(study)
  expect_true(all(c("rd_x=a", "rd_x=b", "ri_y") %in% tsv$column_name))

  # 6 persons per batch hold rd_x == "a", on 4 weekly rows and 1 annual
  # row each. Two batches: 12 persons, 48 person-weeks, 12 person-years.
  a <- .csr_row(tsv, "rd_x=a")
  expect_identical(as.character(a$n_persons_with), "12")
  expect_identical(as.character(a$n_person_weeks_with), "48")
  expect_identical(as.character(a$n_person_years_with), "12")
  expect_identical(a$entry_label, "exposure")

  b <- .csr_row(tsv, "rd_x=b")
  expect_identical(as.character(b$n_persons_with), "6")
  expect_identical(as.character(b$n_person_weeks_with), "24")
  expect_identical(as.character(b$n_person_years_with), "6")

  y <- .csr_row(tsv, "ri_y")
  expect_identical(as.character(y$n_persons_with), "10")
  expect_identical(as.character(y$n_person_weeks_with), "40")
  expect_identical(as.character(y$n_person_years_with), "10")

  # rd_x is NA for rank 10, and an NA carries no level.
  expect_false(any(grepl("^rd_x=NA$", tsv$column_name)))

  # An integer column is skipped, and the meta names it.
  meta <- study$load_skeleton_meta(1L)
  expect_identical(meta$randvars_counts_skipped, "ri_n")

  # The label describes what n_persons counts.
  txt <- .csr_status_lines(study)
  expect_true(any(grepl(
    "n_persons (any row, weekly or annual)",
    txt,
    fixed = TRUE
  )))
  expect_false(any(grepl("n_persons (any weekly row)", txt, fixed = TRUE)))
})


test_that("the summary sums the framework's removals over the batches", {
  study <- .csr_study()
  study$process_skeletons()

  header <- readLines(.csr_tsv_path(study))
  header <- header[startsWith(header, "#")]
  expect_true("# n_batches_with_framework_removals\t2" %in% header)
  expect_true("# framework_age_persons_removed\t14" %in% header)
  expect_true("# framework_age_person_weeks_removed\t56" %in% header)
  expect_true("# framework_region_persons_removed\t6" %in% header)
  expect_true("# framework_region_person_weeks_removed\t24" %in% header)

  txt <- .csr_status_lines(study)
  expect_true(any(grepl(
    "framework age: persons_removed 14, person_weeks_removed 56 (over 2 batches)",
    txt,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "framework region: persons_removed 6, person_weeks_removed 24 (over 2 batches)",
    txt,
    fixed = TRUE
  )))

  rw <- study$summary$registry_wide
  expect_identical(rw$n_batches_with_framework_removals, 2L)
  expect_identical(rw$framework_removals$step, c("age", "region"))
  expect_identical(rw$framework_removals$persons_removed, c(14L, 6L))
  expect_identical(rw$framework_removals$person_weeks_removed, c(56L, 24L))

  # The attribute stays out of the skeleton file: the worker strips it
  # before the skeleton is built.
  sk <- study$load_skeleton(1L)
  expect_null(attr(sk$data, "framework_removals", exact = TRUE))
})


test_that("a meta with no randvars_counts is backfilled without a replay", {
  study <- .csr_study()
  study$process_skeletons()

  meta_path <- file.path(study$data_skeleton_dir, "meta_00001.qs2")
  meta <- qs2::qs_read(meta_path)
  expect_false(is.null(meta$randvars_counts))
  before_state <- meta$randvars_state
  before_framework_hash <- meta$framework_fn_hash
  before_trim_hash <- meta$trim_fn_hash
  before_removals <- as.data.frame(meta$framework_removals)

  # A meta written before the counts existed. Everything else about the
  # batch is current.
  meta$randvars_counts <- NULL
  meta$randvars_counts_skipped <- NULL
  qs2::qs_save(meta, meta_path)

  sk_path <- file.path(study$data_skeleton_dir, "skeleton_00001.qs2")
  mtime_before <- file.mtime(sk_path)
  Sys.sleep(1.1) # ensure mtime resolution catches any rewrite

  study$process_skeletons()

  meta2 <- qs2::qs_read(meta_path)
  counts <- meta2$randvars_counts$exposure$counts
  expect_identical(counts[["rd_x=a"]]$n_persons_with, 6L)
  expect_identical(counts[["rd_x=a"]]$n_person_weeks_with, 24L)
  expect_identical(counts[["ri_y"]]$n_persons_with, 5L)
  expect_identical(meta2$randvars_counts_skipped, "ri_n")

  # No phase replayed: every stored hash is the one it was, and the
  # skeleton file was not rewritten.
  expect_identical(meta2$randvars_state, before_state)
  expect_identical(meta2$framework_fn_hash, before_framework_hash)
  expect_identical(meta2$trim_fn_hash, before_trim_hash)
  expect_equal(file.mtime(sk_path), mtime_before)

  # The framework did not run, so its removals are carried forward rather
  # than dropped.
  expect_identical(as.data.frame(meta2$framework_removals), before_removals)

  # And the summary still reports both sources.
  tsv <- .csr_tsv(study)
  expect_identical(as.character(.csr_row(tsv, "rd_x=a")$n_persons_with), "12")
  header <- readLines(.csr_tsv_path(study))
  expect_true("# framework_age_persons_removed\t14" %in% header)
})


test_that("a framework_removals attribute the summary cannot sum stops the batch", {
  study <- .csr_study()
  study$register_framework(function(batch_data, config) {
    d <- .csr_framework(batch_data, config)
    data.table::setattr(d, "framework_removals", list(step = "age"))
    d
  })
  expect_error(study$process_skeletons(), "framework_removals")
})


test_that("a column with more levels than the cap is named, not counted", {
  n <- swereg:::.RANDVARS_MAX_LEVELS + 1L
  d <- data.table::data.table(
    id = seq_len(n),
    isoyearweek = "2020-01",
    isoyear = 2020L,
    is_isoyear = FALSE,
    rd_many = paste0("v", seq_len(n)),
    rd_few = rep(c("a", "b"), length.out = n)
  )
  sk <- Skeleton$new(data = d, batch_number = 1L)
  sk$randvars_state <- list(
    exposure = list(
      fn_hash = "h",
      added_columns = c("rd_many", "rd_few")
    )
  )

  rc <- swereg:::.compute_randvars_counts(sk)
  expect_identical(rc$skipped, "rd_many")
  keys <- names(rc$counts$exposure$counts)
  expect_true("rd_few=a" %in% keys)
  expect_false(any(startsWith(keys, "rd_many=")))
})


test_that("a framework_removals table missing persons_after is rejected", {
  study <- .csr_study()
  study$register_framework(function(batch_data, config) {
    d <- .csr_framework(batch_data, config)
    fr <- .csr_removals()
    fr[, "persons_after" := NULL]
    data.table::setattr(d, "framework_removals", fr)
    d
  })
  expect_error(study$process_skeletons(), "framework_removals")
})


test_that("a factor is counted by its observed values, not its declared levels", {
  wide <- swereg:::.RANDVARS_MAX_LEVELS + 1L
  d <- data.table::data.table(
    id = 1:4,
    isoyearweek = "2020-01",
    isoyear = 2020L,
    is_isoyear = FALSE,
    rd_f = factor(c("a", "a", "b", "b"), levels = c("a", "b", "unused")),
    rd_wide = factor(
      rep(c("p", "q"), 2L),
      levels = c("p", "q", paste0("L", seq_len(wide)))
    )
  )
  sk <- Skeleton$new(data = d, batch_number = 1L)
  sk$randvars_state <- list(
    exposure = list(fn_hash = "h", added_columns = c("rd_f", "rd_wide"))
  )

  rc <- swereg:::.compute_randvars_counts(sk)
  keys <- names(rc$counts$exposure$counts)

  # A declared level that no row holds is not a count.
  expect_false("rd_f=unused" %in% keys)
  expect_identical(rc$counts$exposure$counts[["rd_f=a"]]$n_persons_with, 2L)

  # rd_wide declares more levels than the cap and holds two. The cap reads
  # the two, so the column is counted rather than skipped.
  expect_gt(wide, swereg:::.RANDVARS_MAX_LEVELS)
  expect_false("rd_wide" %in% rc$skipped)
  expect_true(all(c("rd_wide=p", "rd_wide=q") %in% keys))
})


test_that("a zero-row framework_removals table counts as no report at all", {
  study <- .csr_study()
  study$register_framework(function(batch_data, config) {
    d <- .csr_framework(batch_data, config)
    data.table::setattr(d, "framework_removals", .csr_removals()[0])
    d
  })
  study$process_skeletons()

  meta <- study$load_skeleton_meta(1L)
  expect_false("framework_removals" %in% names(meta))

  rw <- study$summary$registry_wide
  expect_null(rw$framework_removals)
  expect_identical(rw$n_batches_with_framework_removals, 0L)
  header <- readLines(.csr_tsv_path(study))
  expect_false(any(startsWith(header, "# n_batches_with_framework_removals")))
  expect_false(any(grepl("Framework step removals", .csr_status_lines(study))))

  # A meta written by an earlier swereg MAY hold a zero-row table. The
  # summary MUST leave that batch out too.
  meta_path <- file.path(study$data_skeleton_dir, "meta_00001.qs2")
  m <- qs2::qs_read(meta_path)
  m$framework_removals <- .csr_removals()[0]
  qs2::qs_save(m, meta_path)

  study$process_skeletons()

  rw <- study$summary$registry_wide
  expect_null(rw$framework_removals)
  expect_identical(rw$n_batches_with_framework_removals, 0L)
})


test_that("the persisted randvars_counts record uses the summary's schema", {
  study <- .csr_study()
  study$process_skeletons()

  meta <- study$load_skeleton_meta(1L)
  expect_gt(length(meta$randvars_counts), 0L)
  for (entry in meta$randvars_counts) {
    expect_identical(
      names(entry),
      c("entry_label", "entry_fingerprint", "counts")
    )
    expect_type(entry$entry_label, "character")
    expect_type(entry$entry_fingerprint, "character")
    expect_gt(length(entry$counts), 0L)
    for (cnt in entry$counts) {
      expect_identical(
        names(cnt),
        c("n_persons_with", "n_person_weeks_with", "n_person_years_with")
      )
    }
  }

  # The label is the step name, and the fingerprint is that step's own hash.
  expect_identical(meta$randvars_counts$exposure$entry_label, "exposure")
  expect_identical(
    meta$randvars_counts$exposure$entry_fingerprint,
    meta$randvars_state$exposure$fn_hash
  )

  # The code-entry counter emits the same triple. That is what lets
  # $compute_summary() read both sources through one accumulation.
  sk <- study$load_skeleton(1L)
  registry_counts <- swereg:::.compute_entry_column_counts(sk$data, "ri_y")
  expect_identical(
    names(registry_counts$ri_y),
    names(meta$randvars_counts$exposure$counts$ri_y)
  )
})
