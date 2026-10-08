# Each validation claim of vignette("tte-methods") is a named predicate in
# vignettes/validation-claims.R. The vignette stops building when a predicate
# is FALSE. This test checks the same predicates on the shipped evidence, so a
# regenerated evidence file that contradicts the prose fails here too.
#
# The two files are found through val_package_root() (helper-tte_validation.R):
# the source tree under testthat::test_local() and devtools::test(), and
# <pkg>.Rcheck/00_pkg_src/swereg under R CMD check. .Rbuildignore keeps
# vignettes/validation-claims.R in the built tarball. A missing file fails the
# test; it never skips.

vc_files <- function() {
  root <- val_package_root()
  if (is.null(root)) {
    return(NULL)
  }
  list(
    claims = file.path(root, "vignettes", "validation-claims.R"),
    evidence = file.path(root, "vignettes", "tte-validation-evidence.rds"),
    vignette = file.path(root, "vignettes", "tte-methods.Rmd")
  )
}

test_that("every validation claim of tte-methods holds on the shipped evidence", {
  f <- vc_files()
  expect_false(is.null(f), label = "a package root holding DESCRIPTION")
  expect_true(file.exists(f$claims), label = f$claims)
  expect_true(file.exists(f$evidence), label = f$evidence)
  env <- new.env()
  sys.source(f$claims, envir = env)
  claims <- env$validation_claims
  expect_true(is.list(claims) && length(claims) > 0L)
  ev <- readRDS(f$evidence)
  for (nm in names(claims)) {
    expect_true(isTRUE(claims[[nm]](ev)), label = paste0("claim ", nm))
  }
})

test_that("the vignette checks every predicate, and names no other", {
  f <- vc_files()
  expect_false(is.null(f), label = "a package root holding DESCRIPTION")
  expect_true(file.exists(f$vignette), label = f$vignette)
  env <- new.env()
  sys.source(f$claims, envir = env)
  defined <- sort(names(env$validation_claims))
  rmd <- paste(readLines(f$vignette, warn = FALSE), collapse = "\n")
  hits <- regmatches(rmd, gregexpr('claim\\("[A-Za-z0-9_]+"\\)', rmd))[[1L]]
  used <- sort(unique(sub('^claim\\("([A-Za-z0-9_]+)"\\)$', "\\1", hits)))
  expect_identical(used, defined)
})
