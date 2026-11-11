# One copy per person in the TTE (R/tteplan_s1_copies.R, issue #68).
#
# A study built with `$set_matched_sets()` writes a person's whole history once
# per membership, under one skeleton ID each. A study built with
# `$set_batches()` can put one ID in two batches. The TTE MUST still enroll a
# person at most once per enrollment period, and cluster all of a person's
# enrollments on the person.
#
# The fixture holds 40 persons in two batches. Four of them get a second copy:
#
#   P (id 2)  a second copy in batch 1, under skeleton ID 1002
#   I (id 1)  an initiator, a second copy in batch 1, under skeleton ID 2001
#   Q (id 3)  a second copy in batch 2, under the same skeleton ID 3
#   R (id 4)  a second copy in batch 2, under skeleton ID 3004
#
# The reference study holds the same 40 persons once each. A study with copies
# MUST give the reference result exactly: the same attrition, the same draw,
# the same panels and the same estimates.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

.MC_P <- 2L
.MC_I <- 1L
.MC_Q <- 3L
.MC_R <- 4L
.MC_COPY_IDS <- c(1000L + .MC_P, 2000L + .MC_I, 3000L + .MC_R)

.mc_base <- function() {
  return(ttm_skeleton(
    scenario = "A",
    n_persons = 40L,
    date_min = "2018-01-01",
    date_max = "2019-06-30",
    n_init_periods = 8L,
    seed = 4242L
  ))
}

# A copy of person `p` of `base`, under skeleton ID `sid`.
.mc_copy <- function(base, p, sid) {
  x <- data.table::copy(base[base[["id"]] == p])
  x[, id := sid]
  return(x)
}

# The rosters of the matched-set fixture, one per batch.
.mc_rosters <- function() {
  return(list(
    data.table::data.table(
      lopnr = c(1:20, .MC_P, .MC_I),
      set_id = 1L,
      role = "member",
      skeleton_id = c(1:20, 1000L + .MC_P, 2000L + .MC_I)
    ),
    data.table::data.table(
      lopnr = c(21:40, .MC_Q, .MC_R),
      set_id = 2L,
      role = "member",
      skeleton_id = c(21:40, .MC_Q, 3000L + .MC_R)
    )
  ))
}

# The two skeleton batches of a fixture `kind`:
#   "reference"    40 persons, once each
#   "matched_set"  the four copies above, with a roster
#   "set_batches"  Q's ID in both batches, with a batch ID list
.mc_batches <- function(base, kind) {
  b1 <- base[base[["id"]] <= 20L]
  b2 <- base[base[["id"]] > 20L]
  if (kind == "matched_set") {
    b1 <- rbind(
      b1,
      .mc_copy(base, .MC_P, 1000L + .MC_P),
      .mc_copy(base, .MC_I, 2000L + .MC_I)
    )
    b2 <- rbind(
      b2,
      .mc_copy(base, .MC_Q, .MC_Q),
      .mc_copy(base, .MC_R, 3000L + .MC_R)
    )
  }
  if (kind == "set_batches") {
    b2 <- rbind(b2, .mc_copy(base, .MC_Q, .MC_Q))
  }
  data.table::setkeyv(b1, c("id", "isoyearweek"))
  data.table::setkeyv(b2, c("id", "isoyearweek"))
  return(list(b1, b2))
}

# The study a plan embeds, as a plain list. `load_skeleton()` is what s0 calls
# on a `skeleton_NNNNN.qs2` file.
.mc_study <- function(files, meta_dir, kind) {
  study <- list(
    skeleton_files = files,
    data_meta_dir = meta_dir,
    load_skeleton = function(b) {
      return(list(data = swereg::qs2_read(files[b]), created_at = NULL))
    }
  )
  if (kind == "matched_set") {
    study$matched_set_roster <- .mc_rosters()
    study$id_col <- "lopnr"
  } else if (kind == "set_batches") {
    study$batch_id_list <- list(1:20, c(21:40, .MC_Q))
  } else {
    study$batch_id_list <- list(1:20, 21:40)
  }
  return(study)
}

# Write a fixture to `root` and build its plan (s0). `edit` changes the
# batches before they are written.
.mc_plan <- function(root, kind, edit = NULL) {
  for (d in c("spec", "tteplan", "results", "meta")) {
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
  }
  base <- .mc_base()
  batches <- .mc_batches(base, kind)
  if (is.function(edit)) {
    batches <- edit(batches)
  }
  files <- file.path(
    root,
    "tteplan",
    c("skeleton_00001.qs2", "skeleton_00002.qs2")
  )
  qs2::qs_save(batches[[1]], files[1])
  qs2::qs_save(batches[[2]], files[2])
  ttm_write_spec(
    file.path(root, "spec", "spec_v001.yaml"),
    "mc",
    "rd_age_continuous",
    admin = base[, max(isoyearweek)]
  )
  invisible(utils::capture.output(suppressMessages(
    plan <- swereg::tteplan_from_spec_and_registrystudy(
      study = .mc_study(files, file.path(root, "meta"), kind),
      candidate_dir_spec = file.path(root, "spec"),
      candidate_dir_tteplan = file.path(root, "tteplan"),
      candidate_dir_results = file.path(root, "results"),
      spec_version = "v001",
      check_skeletons = FALSE
    )
  ), type = "output"))
  return(plan)
}

# Run s1, and with `s3 = TRUE` s2 and s3 too. Returns the plan, the raw panel
# of enrollment 01 and the s1 log.
.mc_run <- function(kind, s3 = FALSE) {
  root <- tempfile(paste0("mc_", kind, "_"))
  plan <- .mc_plan(root, kind)
  dev <- ttm_dev_path()
  log <- utils::capture.output(
    {
      suppressWarnings(plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev,
        check_skeletons = FALSE
      ))
      if (s3) {
        suppressMessages(suppressWarnings(
          plan$s2_generate_analysis_files_and_ipcw_pp(
            n_workers = 1L,
            swereg_dev_path = dev
          )
        ))
        suppressMessages(suppressWarnings(
          plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev)
        ))
      }
    },
    type = "output"
  )
  raw <- swereg::qs2_read(
    file.path(plan$dir_tteplan, plan$ett$file_raw[1]),
    nthreads = 1L
  )
  return(list(plan = plan, panel = raw$data, log = log))
}

# s1 is the slow part, so each fixture runs once per file.
.mc_cache <- new.env(parent = emptyenv())
.mc_get <- function(kind, s3 = FALSE) {
  key <- paste0(kind, if (s3) "_s3" else "")
  if (is.null(.mc_cache[[key]])) {
    .mc_cache[[key]] <- .mc_run(kind, s3 = s3)
  }
  return(.mc_cache[[key]])
}

# ---- which copies s1a drops ----------------------------------------------------

test_that("the first copy of each person by (batch, skeleton ID) is kept", {
  files <- c("a/skeleton_00001.qs2", "a/skeleton_00002.qs2")
  study <- list(matched_set_roster = .mc_rosters(), id_col = "lopnr")
  x <- swereg:::.tte_copies_to_drop(study, files)
  expect_identical(x$n_dropped, 4L)
  expect_identical(
    x$drop,
    list(
      skeleton_00001.qs2 = c(1000L + .MC_P, 2000L + .MC_I),
      skeleton_00002.qs2 = c(.MC_Q, 3000L + .MC_R)
    )
  )
})

test_that("a batch the run does not read holds no kept copy", {
  # Only batch 2 is read. Q and R then keep their batch-2 copy.
  study <- list(matched_set_roster = .mc_rosters(), id_col = "lopnr")
  x <- swereg:::.tte_copies_to_drop(study, "a/skeleton_00002.qs2")
  expect_identical(x$n_dropped, 0L)
  expect_identical(x$drop, list())
  # Only batch 1 is read, as `n_skeleton_files = 1` gives.
  x <- swereg:::.tte_copies_to_drop(study, "a/skeleton_00001.qs2")
  expect_identical(
    x$drop,
    list(skeleton_00001.qs2 = c(1000L + .MC_P, 2000L + .MC_I))
  )
})

test_that("an ID in two set_batches() batches keeps its first batch", {
  files <- c("a/skeleton_00001.qs2", "a/skeleton_00002.qs2")
  x <- swereg:::.tte_copies_to_drop(
    list(batch_id_list = list(1:20, c(21:40, .MC_Q))),
    files
  )
  expect_identical(x$drop, list(skeleton_00002.qs2 = .MC_Q))
})

test_that("a study with one copy per person drops nothing", {
  files <- c("a/skeleton_00001.qs2", "a/skeleton_00002.qs2")
  none <- list(drop = list(), n_dropped = 0L)
  expect_identical(
    swereg:::.tte_copies_to_drop(list(batch_id_list = list(1:20, 21:40)), files),
    none
  )
  # An identity roster.
  roster <- list(data.table::data.table(lopnr = 1:3, skeleton_id = 1:3))
  expect_identical(
    swereg:::.tte_copies_to_drop(
      list(matched_set_roster = roster, id_col = "lopnr"),
      files[1]
    ),
    none
  )
  # A study that holds neither, and a file name with no batch number.
  expect_identical(swereg:::.tte_copies_to_drop(list(), files), none)
  expect_identical(
    swereg:::.tte_copies_to_drop(
      list(matched_set_roster = .mc_rosters(), id_col = "lopnr"),
      c("a/skel_a.qs2", "a/skeleton_00002.qs2")
    ),
    none
  )
  # The drop of no IDs returns the very table it was given.
  d <- data.table::data.table(id = 1:2, isoyearweek = c("2020-01", "2020-01"))
  expect_identical(swereg:::.tte_drop_copies(d, NULL), d)
})

test_that("a RegistryStudy gives the copies that its list form gives", {
  study <- RegistryStudy$new(data_rawbatch_dir = withr::local_tempdir())
  study$matched_set_roster <- .mc_rosters()
  expect_identical(study$id_col, "lopnr")
  expect_identical(
    swereg:::.tte_copies(study, 1:2),
    swereg:::.tte_copies(
      list(matched_set_roster = .mc_rosters(), id_col = "lopnr"),
      1:2
    )
  )
  expect_identical(swereg:::.tte_study_n_persons(study), 40L)
})

# ---- s1 on a study with copies -------------------------------------------------

test_that("s1 on a matched-set study gives the result of one copy per person", {
  skip_on_cran()
  ms <- .mc_get("matched_set", s3 = TRUE)
  ref <- .mc_get("reference", s3 = TRUE)

  expect_true(any(grepl("copies of a person dropped before eligibility: 4", ms$log)))
  expect_true(any(grepl("copies of a person dropped before eligibility: 0", ref$log)))

  # The same persons, trials and follow-up rows, in the same order.
  expect_identical(ms$panel, ref$panel)
  expect_identical(ms$plan$get_attrition(), ref$plan$get_attrition())
  expect_identical(ms$plan$get_matching(), ref$plan$get_matching())
  expect_identical(ms$plan$expected_n_ids, 40L)

  # No copy's skeleton ID reaches a panel, so every ID is one person.
  expect_false(any(.MC_COPY_IDS %in% ms$panel$id))
  expect_true(all(ms$panel$id %in% 1:40))
  # One trajectory per person-trial: no follow-up interval twice.
  expect_identical(
    anyDuplicated(ms$panel[, c("enrollment_person_trial_id", "tstart")]),
    0L
  )
  # At most one enrollment per person per enrollment period.
  trials <- unique(ms$panel[, c("id", "enrollment_period_id", "baseline_intervention")])
  expect_identical(anyDuplicated(trials[, c("id", "enrollment_period_id")]), 0L)
  # P, a comparator, is enrolled in several periods under the one ID 2.
  expect_gt(trials[id == .MC_P, .N], 1L)
  # I, an initiator, is enrolled once as an intervention.
  expect_identical(trials[id == .MC_I & baseline_intervention == TRUE, .N], 1L)

  # The estimates and their intervals match, so the copies add no cluster.
  ett_id <- ms$plan$ett$ett_id[1]
  a <- ms$plan$results_ett[[ett_id]]
  b <- ref$plan$results_ett[[ett_id]]
  for (k in c("irr_pp_trunc", "irr_itt")) {
    expect_false(is.null(a[[k]]), info = k)
    expect_identical(a[[k]], b[[k]], info = k)
  }
})

test_that("s1 on a set_batches() study with an ID in two batches reads it once", {
  skip_on_cran()
  sb <- .mc_get("set_batches")
  ref <- .mc_get("reference", s3 = TRUE)
  expect_true(any(grepl("copies of a person dropped before eligibility: 1", sb$log)))
  expect_identical(sb$panel, ref$panel)
  expect_identical(sb$plan$get_attrition(), ref$plan$get_attrition())
  expect_identical(sb$plan$get_matching(), ref$plan$get_matching())
})

# ---- the s0 check --------------------------------------------------------------

test_that("s0 refuses copies that differ in a column the spec reads", {
  skip_on_cran()
  # P's second copy gets another age, which the spec reads.
  edit <- function(batches) {
    b1 <- batches[[1]]
    b1[id == 1000L + .MC_P, rd_age_continuous := rd_age_continuous + 1]
    batches[[1]] <- b1
    return(batches)
  }
  expect_error(
    .mc_plan(withr::local_tempdir(), "matched_set", edit),
    "reads column 'rd_age_continuous'.*person 2"
  )
})

test_that("s0 refuses copies that hold different weeks", {
  skip_on_cran()
  edit <- function(batches) {
    b1 <- batches[[1]]
    batches[[1]] <- b1[!(id == 1000L + .MC_P & isoyearweek == min(isoyearweek))]
    return(batches)
  }
  expect_error(
    .mc_plan(withr::local_tempdir(), "matched_set", edit),
    "copies of person 2 .* hold different weeks"
  )
})

test_that("s0 refuses copies in a file whose name gives no batch", {
  skip_on_cran()
  root <- withr::local_tempdir()
  plan <- .mc_plan(root, "matched_set")
  files <- plan$skeleton_files
  renamed <- file.path(dirname(files), c("first.qs2", "second.qs2"))
  file.rename(files, renamed)
  study <- .mc_study(renamed, file.path(root, "meta"), "matched_set")
  expect_error(
    swereg:::.tte_check_copies_agree(study, renamed, plan$spec, list()),
    "'first.qs2' is not named skeleton_NNNNN.qs2"
  )
  # A run that reads batch 2 only holds each person once there, so such a
  # name is fine, although the study gives persons copies in other batches.
  expect_null(
    swereg:::.tte_check_copies_agree(study, renamed[2], plan$spec, list())
  )
  # A study without copies is fine too.
  ref <- withr::local_tempdir()
  ref_plan <- .mc_plan(ref, "reference")
  ref_files <- ref_plan$skeleton_files
  ref_renamed <- file.path(dirname(ref_files), c("first.qs2", "second.qs2"))
  file.rename(ref_files, ref_renamed)
  study <- .mc_study(ref_renamed, file.path(ref, "meta"), "reference")
  expect_null(
    swereg:::.tte_check_copies_agree(study, ref_renamed, ref_plan$spec, list())
  )
})

test_that("s0 accepts copies that differ only in a column the spec does not read", {
  skip_on_cran()
  edit <- function(batches) {
    for (k in 1:2) {
      b <- batches[[k]]
      b[, membership_role := fifelse(id >= 1000L, "second", "first")]
      batches[[k]] <- b
    }
    return(batches)
  }
  plan <- .mc_plan(withr::local_tempdir(), "matched_set", edit)
  expect_s3_class(plan, "TTEPlan")
  expect_identical(plan$expected_n_ids, 40L)
})
