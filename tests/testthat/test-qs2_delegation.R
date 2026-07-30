# swereg::qs2_write_atomic() holds no temp/rename logic of its own any more --
# the implementation moved to batchit::write_qs2_atomically(). swereg keeps the
# exported name and its documented contract; batchit keeps the engine.
#
# Duplicated atomic-write logic in two packages is the failure this guards
# against: one copy gets a fix, the other silently does not, and the two
# disagree about what "atomic" means. So the delegation itself is the thing
# under test, not merely the behaviour -- behaviour alone would keep passing
# against a re-inlined copy.

test_that("qs2_write_atomic() delegates -- its body IS the batchit call", {
  # removeSource() on BOTH sides: under pkgload::load_all() the body carries a
  # srcref and under an installed package it does not, and identical() compares
  # attributes. Stripping it compares code, not provenance.
  strip <- function(x) utils::removeSource(x)

  fn <- strip(swereg::qs2_write_atomic)
  expect_identical(names(formals(fn)), c("object", "path", "..."))
  # The whole body is one `{` block holding exactly the batchit call and
  # nothing else -- no temp file, no rename, no fallback.
  expect_identical(
    body(fn),
    strip(quote({
      batchit::write_qs2_atomically(object, path, ...)
    }))
  )
})

test_that("qs2_write_atomic() forwards its arguments once and returns invisibly", {
  seen <- list()
  n <- 0L
  sentinel <- "sentinel-9f3c2a"

  testthat::local_mocked_bindings(
    write_qs2_atomically = function(object, path, ...) {
      n <<- n + 1L
      seen <<- list(object = object, path = path, dots = list(...))
      invisible(sentinel)
    },
    .package = "batchit"
  )

  # A REAL, writable destination directory on purpose. Under the delegation the
  # mock intercepts and nothing is written; if the delegation were replaced by
  # an inlined temp+rename, that copy would SUCCEED here rather than erroring
  # on a missing directory -- so the assertions below (n == 1, the recorded
  # arguments, the sentinel) are what detects it, not an incidental crash.
  dir <- tempfile("delegation_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dest <- file.path(dir, "x.qs2")

  obj <- list(a = 1L, b = "two")
  out <- withVisible(
    swereg::qs2_write_atomic(obj, dest, nthreads = 3L, compress_level = 7L)
  )

  expect_identical(n, 1L)
  expect_identical(seen$object, obj)
  expect_identical(seen$path, dest)
  expect_identical(seen$dots, list(nthreads = 3L, compress_level = 7L))
  expect_identical(out$value, sentinel)
  expect_false(out$visible)
})

test_that("R/qs2.R really names batchit::write_qs2_atomically", {
  # NON-VACUITY ONLY. This proves the lockdown's primitive exemption has a real
  # call site to exempt -- it does NOT prove delegation. The two tests above are
  # what prove delegation; a file could name the symbol in dead code and still
  # fail them.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  # Self-contained walker: the lockdown's own walker lives in
  # test-batch_lockdown.R, and testthat gives each test file its own
  # environment, so it is not in scope here under a filtered run.
  mentions <- function(e, acc = character()) {
    if (is.call(e)) {
      f <- e[[1L]]
      if ((identical(f, quote(`::`)) || identical(f, quote(`:::`))) &&
          length(e) == 3L && identical(as.character(e[[2L]]), "batchit")) {
        acc <- c(acc, paste0("batchit::", as.character(e[[3L]])))
      }
    }
    if (is.recursive(e)) {
      for (i in seq_along(e)) {
        acc <- tryCatch(mentions(e[[i]], acc), error = function(err) acc)
      }
    }
    acc
  }

  hits <- unique(unlist(lapply(
    parse(file.path(r_dir, "qs2.R"), keep.source = FALSE), mentions
  )))
  expect_true("batchit::write_qs2_atomically" %in% hits)
})
