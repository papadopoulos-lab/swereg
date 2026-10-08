# The validation evidence (vignettes/tte-validation-evidence.rds) records the
# md5 of the estimator source files in `$meta$estimator_hash`. This test is
# always on, under R CMD check too: evidence from other estimator source does
# not measure the shipped estimators. A version bump alone changes no
# estimator, so the evidence still stands. `$meta$swereg` records the version
# that generated it.
#
# Under R CMD check, val_package_root() resolves to the unpacked source in
# <pkg>.Rcheck/00_pkg_src/swereg. val_estimator_hash() stops when an
# estimator file is missing there, so the test errors and never skips.

test_that("the validation evidence was generated from the current estimator source", {
  root <- val_package_root()
  expect_false(is.null(root), label = "a package root holding DESCRIPTION")
  path <- file.path(root, "vignettes", "tte-validation-evidence.rds")
  expect_true(file.exists(path), label = path)
  ev <- readRDS(path)
  expect_identical(ev$meta$estimator_hash, val_estimator_hash(root))
})
