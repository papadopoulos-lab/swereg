# The dev-path probe must NOT mistake an installed package for a dev source tree.
#
# CI (R CMD check) caught what load_all() testing could not: the old probe asked
# `any(startsWith(system.file(), .libPaths()))`, and under R CMD check the
# package is loaded from a check library whose realpath-normalized form is not a
# string-prefix of system.file()'s recorded path -- so `in_library` came out
# FALSE and the INSTALLED .Rcheck/swereg dir was handed back as a dev tree. The
# dispatcher then sought inst/batch_worker.R (install promotes inst/* to the
# root, so it is absent) and load_all()-able source an installed layout cannot
# provide -- the four batch-production/loud-error failures in run 29637351394.
#
# .dev_source_root() replaces the heuristic with structural markers, tested here
# on real fixture layouts so no R CMD check is needed to prove it.

test_that(".dev_source_root() returns NULL for an installed-package layout", {
  # Installed layout: Meta/package.rds present, no inst/ subdir (install promoted
  # inst/* to the root). This is exactly what system.file() resolves to under
  # R CMD check, and what the old probe wrongly treated as a dev tree.
  root <- file.path(withr::local_tempdir(), "swereg")
  dir.create(file.path(root, "Meta"), recursive = TRUE)
  saveRDS(list(), file.path(root, "Meta", "package.rds"))
  writeLines("Package: swereg", file.path(root, "DESCRIPTION"))
  # Even if a stray inst/batch_worker.R existed, Meta/package.rds must win.
  dir.create(file.path(root, "inst"))
  writeLines("x", file.path(root, "inst", "batch_worker.R"))

  expect_null(swereg:::.dev_source_root(root))
})

test_that(".dev_source_root() returns the root for a real dev source tree", {
  # Source tree: the worker script the runner needs, and NO Meta/package.rds.
  root <- file.path(withr::local_tempdir(), "swereg")
  dir.create(file.path(root, "inst"), recursive = TRUE)
  writeLines("# worker", file.path(root, "inst", "batch_worker.R"))
  writeLines("Package: swereg", file.path(root, "DESCRIPTION"))

  expect_identical(swereg:::.dev_source_root(root), root)
})

test_that(".dev_source_root() strips a trailing /inst before deciding", {
  # devtools::load_all() can report system.file() as the inst/ subdir; the probe
  # must climb to the package root before testing the markers.
  root <- file.path(withr::local_tempdir(), "swereg")
  dir.create(file.path(root, "inst"), recursive = TRUE)
  writeLines("# worker", file.path(root, "inst", "batch_worker.R"))

  expect_identical(swereg:::.dev_source_root(file.path(root, "inst")), root)
})

test_that(".dev_source_root() returns NULL for a tree lacking the worker script", {
  # A source tree with no dispatcher worker script has nothing useful to
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
