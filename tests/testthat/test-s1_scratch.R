# The `work_root` argument of $s1_generate_enrollments_and_ipw() moves the s1
# work directory to local disk, and the sweep deletes what a killed run left
# behind. The sweep deletes by age alone, so the age threshold and the NA-mtime
# guard are the two things that must hold.

.scratch_plan <- function(meta_dir) {
  # `.s1_work_dir()` reaches for plan$registrystudy$data_meta_dir and
  # plan$project_prefix only, so a plain list is a faithful stand-in.
  return(list(
    registrystudy = list(data_meta_dir = meta_dir),
    project_prefix = "phase1prefix"
  ))
}

# --- 1. Layout ---------------------------------------------------------------

test_that(".s1_work_dir() puts the work directory under the root it is given", {
  tmp <- withr::local_tempdir()
  meta <- file.path(tmp, "meta")
  dir.create(meta)
  plan <- .scratch_plan(meta)
  r <- file.path(tmp, "root")

  expect_identical(
    .s1_work_dir(plan, ensure_exists = FALSE, root = r),
    file.path(r, "s1_work_phase1prefix")
  )

  # No root: the nested {data_meta_dir}/s1_work/{project_prefix} layout.
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
  # Age is the only rule, so a dotfile goes like any other entry.
  writeBin(raw(1000), file.path(root, ".dotfile"))
  Sys.setFileTime(file.path(root, "A"), Sys.time() - 15 * 86400)
  Sys.setFileTime(file.path(root, "B"), Sys.time() - 15 * 86400)
  Sys.setFileTime(file.path(root, "C"), Sys.time() - 13 * 86400)
  Sys.setFileTime(file.path(root, ".dotfile"), Sys.time() - 15 * 86400)

  out <- utils::capture.output(k <- .sweep_scratch_root(root, 14))

  expect_false(dir.exists(file.path(root, "A")))
  expect_false(file.exists(file.path(root, "B")))
  expect_false(file.exists(file.path(root, ".dotfile")))
  expect_true(dir.exists(file.path(root, "C")))
  expect_identical(k, 3L)
  expect_true(any(grepl("^Swept A: 1 files, 1000 bytes, age 15 days$", out)))
  expect_true(any(grepl("^Swept B: 1 files, 1000 bytes, age 15 days$", out)))
  expect_true(any(grepl(
    "^Swept \\.dotfile: 1 files, 1000 bytes, age 15 days$",
    out
  )))
  expect_true(any(grepl("^Scratch sweep: 3 entries removed under ", out)))
  expect_false(any(grepl("Swept C:", out)))
})

# --- 3. No root ---------------------------------------------------------------

test_that(".sweep_scratch_root() is silent with no root, and defaults to 14 days", {
  out <- utils::capture.output(k <- .sweep_scratch_root(NULL, 14))
  expect_identical(k, 0L)
  expect_identical(out, character(0))

  absent <- file.path(withr::local_tempdir(), "no_such_root")
  out <- utils::capture.output(k <- .sweep_scratch_root(absent, 14))
  expect_identical(k, 0L)
  expect_identical(out, character(0))

  # The age has one source: the default of the formal. Nothing reads an option
  # or an environment variable for it.
  expect_identical(formals(.sweep_scratch_root)$max_age_days, 14)
})

# --- 4. Root validation -------------------------------------------------------

# `~` is absolute to a user, and file.path() never expands it. Without the
# expansion the s1 work directory lands in a literal "~" directory beside the
# working directory.
#
# One assertion per block. A call that errors ends its block, so two equality
# assertions in one block would let the first one hide the second.
test_that(".validate_scratch_root() expands a tilde with a path after it", {
  skip_if(identical(path.expand("~"), "~"), "a tilde does not expand here")

  expect_identical(
    .validate_scratch_root("~/x", "work_root"),
    path.expand("~/x")
  )
})

test_that(".validate_scratch_root() expands a bare tilde", {
  skip_if(identical(path.expand("~"), "~"), "a tilde does not expand here")

  expect_identical(.validate_scratch_root("~", "work_root"), path.expand("~"))
})

# A tilde that names no user does not expand, so the absolute check MUST
# refuse it. `source` is the argument name, and both messages name it.
test_that(".validate_scratch_root() refuses a relative path and names work_root", {
  skip_if(identical(path.expand("~"), "~"), "a tilde does not expand here")

  expect_error(
    .validate_scratch_root("relative/x", "work_root"),
    "work_root.*absolute"
  )
  expect_error(
    .validate_scratch_root("~nosuchuser/x", "work_root"),
    "work_root.*absolute"
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
