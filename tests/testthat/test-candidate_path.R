test_that("CandidatePath$new rejects empty input", {
  expect_error(
    CandidatePath$new(character(0), "lbl"),
    "non-empty"
  )
})

test_that("CandidatePath$resolve returns the first existing candidate", {
  d1 <- tempfile("cp_")
  d2 <- tempfile("cp_")
  dir.create(d1)
  dir.create(d2)
  on.exit(unlink(c(d1, d2), recursive = TRUE), add = TRUE)

  cp <- CandidatePath$new(c(d1, d2), "lbl")
  expect_equal(cp$resolve(), d1)

  cp2 <- CandidatePath$new(c(d2, d1), "lbl")
  expect_equal(cp2$resolve(), d2)
})

test_that("CandidatePath$resolve caches", {
  d1 <- tempfile("cp_")
  dir.create(d1)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)

  cp <- CandidatePath$new(c("/definitely/not/there", d1), "lbl")
  expect_false(cp$is_resolved())

  first <- cp$resolve()
  expect_true(cp$is_resolved())

  # Second call hits the cache -- even if we could manipulate the candidate
  # list (we don't here), the return value must still come from cache.
  second <- cp$resolve()
  expect_identical(first, second)
})

test_that("CandidatePath$invalidate clears the cache", {
  d1 <- tempfile("cp_")
  dir.create(d1)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)

  cp <- CandidatePath$new(d1, "lbl")
  cp$resolve()
  expect_true(cp$is_resolved())

  cp$invalidate()
  expect_false(cp$is_resolved())
})

test_that("CandidatePath$is_resolved returns FALSE when cached dir vanishes", {
  d1 <- tempfile("cp_")
  dir.create(d1)

  cp <- CandidatePath$new(d1, "lbl")
  cp$resolve()
  expect_true(cp$is_resolved())

  unlink(d1, recursive = TRUE)
  expect_false(cp$is_resolved())
})

test_that("CandidatePath$resolve re-walks when cache becomes stale", {
  d1 <- tempfile("cp_")
  d2 <- tempfile("cp_")
  dir.create(d1)
  dir.create(d2)
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)

  cp <- CandidatePath$new(c(d1, d2), "lbl")
  expect_equal(cp$resolve(), d1)

  # Simulate host transfer: the first candidate disappears
  unlink(d1, recursive = TRUE)

  # Without a cache-validity check inside resolve, we would return the stale
  # path. CandidatePath checks dir.exists() on the cache and re-walks.
  expect_equal(cp$resolve(), d2)
})

test_that("CandidatePath$resolve creates first creatable candidate", {
  parent <- tempfile("cp_parent_")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)

  creatable <- file.path(parent, "will_be_made")
  cp <- CandidatePath$new(
    c(file.path(tempdir(), "no-parent-zzz", "child"), creatable),
    "lbl"
  )
  expect_equal(cp$resolve(), creatable)
  expect_true(dir.exists(creatable))
})

test_that("CandidatePath$print marks the resolved candidate", {
  d1 <- tempfile("cp_")
  dir.create(d1)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)

  cp <- CandidatePath$new(c("/not/here", d1), "lbl")
  cp$resolve()

  output <- capture.output(print(cp))
  expect_match(output[1], "CandidatePath")
  expect_match(output[1], "lbl")
  # Resolved line should be prefixed with the ">" marker
  resolved_line <- output[grepl(d1, output, fixed = TRUE)]
  expect_match(resolved_line, "^  > ")
})

test_that("invalidate_candidate_paths clears a direct CandidatePath", {
  d1 <- tempfile("cp_")
  dir.create(d1)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)

  cp <- CandidatePath$new(d1, "lbl")
  cp$resolve()
  expect_true(cp$is_resolved())

  invalidate_candidate_paths(cp)
  expect_false(cp$is_resolved())
})

test_that("invalidate_candidate_paths recurses into embedded R6", {
  d1 <- tempfile("cp_")
  d2 <- tempfile("cp_")
  dir.create(d1)
  dir.create(d2)
  on.exit(unlink(c(d1, d2), recursive = TRUE), add = TRUE)

  cp_inner <- CandidatePath$new(d1, "inner")

  # Tiny R6 wrapper that holds a CandidatePath as a public field and exposes
  # it through a plain field (no active binding).
  Wrapper <- R6::R6Class(
    "Wrapper",
    public = list(
      inner = NULL,
      other_cp = NULL,
      plain_value = 42L,
      initialize = function(inner, other) {
        self$inner <- inner
        self$other_cp <- other
      }
    )
  )

  cp_other <- CandidatePath$new(d2, "other")
  w <- Wrapper$new(cp_inner, cp_other)

  cp_inner$resolve()
  cp_other$resolve()
  expect_true(cp_inner$is_resolved())
  expect_true(cp_other$is_resolved())

  invalidate_candidate_paths(w)
  expect_false(cp_inner$is_resolved())
  expect_false(cp_other$is_resolved())
})

test_that("invalidate_candidate_paths skips active bindings", {
  d1 <- tempfile("cp_")
  dir.create(d1)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)

  cp <- CandidatePath$new(d1, "lbl")

  # R6 class with both a plain CandidatePath field AND an active binding
  # that resolves it. If invalidate_candidate_paths visited the active
  # binding it would call $resolve() and re-populate the cache, defeating
  # the purpose. It must skip active bindings and reach the CandidatePath
  # via the backing field instead.
  Holder <- R6::R6Class(
    "Holder",
    public = list(
      dir_foo_cp = NULL,
      initialize = function(cp) self$dir_foo_cp <- cp
    ),
    active = list(
      dir_foo = function() self$dir_foo_cp$resolve()
    )
  )

  h <- Holder$new(cp)
  h$dir_foo   # force resolve
  expect_true(cp$is_resolved())

  invalidate_candidate_paths(h)
  expect_false(cp$is_resolved())
})
