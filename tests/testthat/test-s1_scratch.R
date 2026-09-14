# swereg.s1_work_root moves the s1 work directory to local disk, and the sweep
# deletes what a killed run left behind. The sweep deletes by age alone, so the
# age threshold and the NA-mtime guard are the two things that must hold.

.scratch_plan <- function(meta_dir) {
  # `.s1_work_dir()` reaches for plan$registrystudy$data_meta_dir and
  # plan$project_prefix only, so a plain list is a faithful stand-in.
  return(list(
    registrystudy = list(data_meta_dir = meta_dir),
    project_prefix = "phase1prefix"
  ))
}

# --- 1. Layout ---------------------------------------------------------------

test_that(".s1_work_dir() honours the root, and the option beats the env var", {
  tmp <- withr::local_tempdir()
  meta <- file.path(tmp, "meta")
  dir.create(meta)
  plan <- .scratch_plan(meta)
  root_opt <- file.path(tmp, "root_opt")
  root_env <- file.path(tmp, "root_env")

  withr::local_envvar(c(SWEREG_S1_WORK_ROOT = NA))
  withr::with_options(
    list(swereg.s1_work_root = root_opt),
    expect_identical(
      .s1_work_dir(plan, ensure_exists = FALSE),
      file.path(root_opt, "s1_work_phase1prefix")
    )
  )

  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = root_env),
    expect_identical(
      .s1_work_dir(plan, ensure_exists = FALSE),
      file.path(root_env, "s1_work_phase1prefix")
    )
  )

  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = root_env),
    withr::with_options(
      list(swereg.s1_work_root = root_opt),
      expect_identical(
        .s1_work_dir(plan, ensure_exists = FALSE),
        file.path(root_opt, "s1_work_phase1prefix")
      )
    )
  )

  # Unset: the nested {data_meta_dir}/s1_work/{project_prefix} layout.
  dir <- .s1_work_dir(plan, ensure_exists = FALSE)
  expect_true(endsWith(dir, file.path("s1_work", "phase1prefix")))
})

# --- 2. Sweep ----------------------------------------------------------------

# Sys.setFileTime() on a directory is reset by writing into it, so populate
# first and set the times afterwards.
test_that(".sweep_scratch_root() removes only the entries older than the age", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "A"))
  writeBin(raw(1000), file.path(root, "A", "chunk.qs2"))
  writeBin(raw(1000), file.path(root, "B"))
  dir.create(file.path(root, "C"))
  writeBin(raw(1000), file.path(root, "C", "chunk.qs2"))
  Sys.setFileTime(file.path(root, "A"), Sys.time() - 15 * 86400)
  Sys.setFileTime(file.path(root, "B"), Sys.time() - 15 * 86400)
  Sys.setFileTime(file.path(root, "C"), Sys.time() - 13 * 86400)

  out <- utils::capture.output(k <- .sweep_scratch_root(root, 14))

  expect_false(dir.exists(file.path(root, "A")))
  expect_false(file.exists(file.path(root, "B")))
  expect_true(dir.exists(file.path(root, "C")))
  expect_identical(k, 2L)
  expect_true(any(grepl("^Swept A: 1 files, 1000 bytes, age 15 days$", out)))
  expect_true(any(grepl("^Swept B: 1 files, 1000 bytes, age 15 days$", out)))
  expect_true(any(grepl("^Scratch sweep: 2 entries removed under ", out)))
  expect_false(any(grepl("Swept C:", out)))
})

# --- 3. No root ---------------------------------------------------------------

test_that(".sweep_scratch_root() is silent with no root, and never reads the age", {
  out <- utils::capture.output(k <- .sweep_scratch_root(NULL, 14))
  expect_identical(k, 0L)
  expect_identical(out, character(0))

  absent <- file.path(withr::local_tempdir(), "no_such_root")
  out <- utils::capture.output(k <- .sweep_scratch_root(absent, 14))
  expect_identical(k, 0L)
  expect_identical(out, character(0))

  # `max_age_days` is a lazy default. A run with no root configured must not
  # fail on an age it will never use.
  withr::local_options(list(swereg.scratch_max_age_days = "abc"))
  expect_identical(.sweep_scratch_root(NULL), 0L)
})

# --- 4. Resolvers -------------------------------------------------------------

test_that(".scratch_max_age_days() and .s1_scratch_root() validate their sources", {
  withr::local_envvar(c(
    SWEREG_SCRATCH_MAX_AGE_DAYS = NA,
    SWEREG_S1_WORK_ROOT = NA
  ))
  withr::local_options(list(
    swereg.scratch_max_age_days = NULL,
    swereg.s1_work_root = NULL
  ))

  expect_identical(.scratch_max_age_days(), 14)

  withr::with_envvar(
    c(SWEREG_SCRATCH_MAX_AGE_DAYS = "30"),
    expect_identical(.scratch_max_age_days(), 30)
  )
  withr::with_envvar(
    c(SWEREG_SCRATCH_MAX_AGE_DAYS = "30"),
    withr::with_options(
      list(swereg.scratch_max_age_days = 7),
      expect_identical(.scratch_max_age_days(), 7)
    )
  )
  withr::with_envvar(
    c(SWEREG_SCRATCH_MAX_AGE_DAYS = "abc"),
    expect_error(.scratch_max_age_days(), "SWEREG_SCRATCH_MAX_AGE_DAYS")
  )
  withr::with_options(
    list(swereg.scratch_max_age_days = -1),
    expect_error(.scratch_max_age_days(), "swereg.scratch_max_age_days")
  )

  expect_null(.s1_scratch_root())
  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = "relative/dir"),
    expect_error(.s1_scratch_root(), "absolute")
  )
})

# --- 5. Broken symlink --------------------------------------------------------

# file.mtime() is NA for a broken symlink, and `if (NA < x)` is an error. The
# sweep must step over it and still delete the stale directory beside it.
test_that(".sweep_scratch_root() steps over an entry with no modification time", {
  root <- withr::local_tempdir()
  skip_if_not(
    file.symlink("/nonexistent/target", file.path(root, "dangling")),
    "symlinks not supported here"
  )
  dir.create(file.path(root, "A"))
  writeBin(raw(1000), file.path(root, "A", "chunk.qs2"))
  Sys.setFileTime(file.path(root, "A"), Sys.time() - 15 * 86400)

  expect_no_error(utils::capture.output(.sweep_scratch_root(root, 14)))
  expect_false(dir.exists(file.path(root, "A")))
})

# --- 6. Residue ---------------------------------------------------------------

# The deleter is injected because unlink(force = TRUE) chmods the parents, so a
# read-only directory does not survive it.
test_that(".sweep_scratch_root() warns on residue and does not count it", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "A"))
  writeBin(raw(1000), file.path(root, "A", "chunk.qs2"))
  Sys.setFileTime(file.path(root, "A"), Sys.time() - 15 * 86400)

  expect_warning(
    k <- .sweep_scratch_root(root, 14, .unlink = function(...) 0L),
    "Could not sweep"
  )
  expect_identical(k, 0L)
  expect_true(dir.exists(file.path(root, "A")))
})

# --- 7. Tilde expansion -------------------------------------------------------

# `~` is absolute to a user, and file.path() never expands it. Without the
# expansion the s1 work directory lands in a literal "~" directory beside the
# working directory. A tilde that names no user does not expand, so the
# absolute check MUST refuse it.
test_that(".s1_scratch_root() expands a tilde and still refuses a relative path", {
  skip_if(identical(path.expand("~"), "~"), "a tilde does not expand here")
  withr::local_options(list(swereg.s1_work_root = NULL))

  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = "~/scr"),
    expect_identical(.s1_scratch_root(), path.expand("~/scr"))
  )
  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = "~"),
    expect_identical(.s1_scratch_root(), path.expand("~"))
  )
  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = "relative/dir"),
    expect_error(.s1_scratch_root(), "absolute")
  )
  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = "~nosuchuser/x"),
    expect_error(.s1_scratch_root(), "absolute")
  )
})

# --- 8. An NA option reads as unset -------------------------------------------

# Both resolvers mirror .default_n_workers_impl() in R/default_n_workers.R, so
# a caller can clear either setting without removing it. The option source and
# the environment source get one block each. A mutation of one source then
# cannot hide the other behind an early error in a shared block.
test_that("an NA option reads as unset", {
  withr::local_envvar(c(
    SWEREG_S1_WORK_ROOT = NA,
    SWEREG_SCRATCH_MAX_AGE_DAYS = NA
  ))
  withr::local_options(list(
    swereg.s1_work_root = NULL,
    swereg.scratch_max_age_days = NULL
  ))

  withr::with_options(
    list(swereg.s1_work_root = NA),
    expect_null(.s1_scratch_root())
  )
  withr::with_options(
    list(swereg.scratch_max_age_days = NA),
    expect_identical(.scratch_max_age_days(), 14)
  )
})

# --- 9. An empty environment variable reads as unset --------------------------

# withr sets these variables to an empty string, and does not remove them.
# Measured here: Sys.getenv(unset = NA_character_) returns "" inside each
# with_envvar() body, and NA outside it. So neither check below is vacuous.
test_that("an empty environment variable reads as unset", {
  withr::local_envvar(c(
    SWEREG_S1_WORK_ROOT = NA,
    SWEREG_SCRATCH_MAX_AGE_DAYS = NA
  ))
  withr::local_options(list(
    swereg.s1_work_root = NULL,
    swereg.scratch_max_age_days = NULL
  ))

  withr::with_envvar(
    c(SWEREG_S1_WORK_ROOT = ""),
    expect_null(.s1_scratch_root())
  )
  withr::with_envvar(
    c(SWEREG_SCRATCH_MAX_AGE_DAYS = ""),
    expect_identical(.scratch_max_age_days(), 14)
  )
})
