# The dev-path probe must NOT mistake an installed package for a dev source tree.
#
# CI (R CMD check) caught what load_all() testing could not: the old probe asked
# `any(startsWith(system.file(), .libPaths()))`, and under R CMD check the
# package is loaded from a check library whose realpath-normalized form is not a
# string-prefix of system.file()'s recorded path -- so `in_library` came out
# FALSE and the INSTALLED .Rcheck/swereg dir was handed back as a dev tree.
#
# The dispatcher's worker script (inst/batch_worker.R) used to be the source-tree
# marker, but it now ships with batchit, not swereg (Phase 4). So .dev_source_root()
# discriminates on swereg's OWN source instead: a DESCRIPTION naming swereg, no
# Meta/package.rds, and an R/ directory holding .R sources (an installed package's
# R/ holds swereg.rdb/.rdx bytecode, no .R files). Tested here on real fixture
# layouts so no R CMD check is needed to prove it.

# Build a minimal swereg SOURCE tree fixture: DESCRIPTION naming swereg + an R/
# directory with a .R file, and (by default) no Meta/package.rds.
.mk_src_tree <- function() {
  root <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "swereg")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines("Package: swereg", file.path(root, "DESCRIPTION"))
  writeLines("f <- function() NULL", file.path(root, "R", "something.R"))
  root
}

test_that(".dev_source_root() returns NULL for an installed-package layout", {
  # Installed layout: Meta/package.rds present, and R/ holds compiled bytecode
  # (swereg.rdb/.rdx) rather than .R files. This is exactly what system.file()
  # resolves to under R CMD check, and what the old probe wrongly treated as a
  # dev tree.
  root <- file.path(withr::local_tempdir(), "swereg")
  dir.create(file.path(root, "Meta"), recursive = TRUE)
  saveRDS(list(), file.path(root, "Meta", "package.rds"))
  writeLines("Package: swereg", file.path(root, "DESCRIPTION"))
  dir.create(file.path(root, "R"))
  writeLines("bytecode", file.path(root, "R", "swereg.rdb"))
  writeLines("bytecode", file.path(root, "R", "swereg.rdx"))
  # Even a stray R/*.R must not override Meta/package.rds.
  writeLines("x <- 1", file.path(root, "R", "stray.R"))

  expect_null(swereg:::.dev_source_root(root))
})

test_that(".dev_source_root() returns the root for a real dev source tree", {
  # Source tree: DESCRIPTION naming swereg, R/ with .R source, and NO
  # Meta/package.rds.
  root <- .mk_src_tree()

  expect_identical(swereg:::.dev_source_root(root), root)
})

test_that(".dev_source_root() strips a trailing /inst before deciding", {
  # devtools::load_all() can report system.file() as the inst/ subdir; the probe
  # must climb to the package root before testing the markers.
  root <- .mk_src_tree()
  dir.create(file.path(root, "inst"))

  expect_identical(swereg:::.dev_source_root(file.path(root, "inst")), root)
})

test_that(".dev_source_root() returns NULL for a tree with no R/ source files", {
  # A DESCRIPTION-bearing tree whose R/ holds no .R sources has nothing useful to
  # dev-load, so the probe declines it (the installed package is used instead).
  root <- file.path(withr::local_tempdir(), "swereg")
  dir.create(root, recursive = TRUE)
  writeLines("Package: swereg", file.path(root, "DESCRIPTION"))

  expect_null(swereg:::.dev_source_root(root))
})

test_that(".dev_source_root() returns NULL for an empty path", {
  # system.file() returns "" when the package cannot be located at all.
  expect_null(swereg:::.dev_source_root(""))
})
