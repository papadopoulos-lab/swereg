# Pins the mirai contract that save_rawbatch(n_workers > 1) depends on.
#
# `drain_one()` in RegistryStudy$save_rawbatch() decides whether a slice write
# failed by reading `call_mirai(h)$data` and asking `is_error_value()`. That is
# the FAILURE path: a happy-path production run -- however fast, however often
# -- never executes it. So it needs a test of its own, or a future mirai release
# could silently turn every failed slice write into a reported success.
#
# History worth keeping: an adversarial review asserted that `$value` (the
# original accessor) had been removed in mirai 0.2.0 and therefore returned
# NULL, making the guard dead. That was checked against mirai's *documentation*,
# which indeed only mentions `$data` -- and it was wrong. Both bindings exist and
# are identical. Absence from the docs is not removal from the API. This file is
# what that claim should have been checked against.

skip_if_not_installed("mirai")

test_that("a failed mirai task is detectable via $data + is_error_value()", {
  mirai::daemons(1L, dispatcher = FALSE)
  on.exit(mirai::daemons(0L), add = TRUE)

  m <- mirai::mirai({
    stop("simulated slice write failure")
  })
  v <- mirai::call_mirai(m)$data

  expect_true(mirai::is_error_value(v))
  expect_match(as.character(v), "simulated slice write failure")
})

test_that("a successful mirai task is NOT flagged as an error", {
  mirai::daemons(1L, dispatcher = FALSE)
  on.exit(mirai::daemons(0L), add = TRUE)

  m <- mirai::mirai({
    TRUE
  })
  v <- mirai::call_mirai(m)$data

  expect_false(mirai::is_error_value(v))
  expect_true(v)
})

# Every mirai::daemons()/mirai::mirai() call reachable from an expression.
# The AST, not a regex: the dispatching mirai() call spans many lines and its
# .compute argument is nowhere near its opening paren, so line-based matching
# would be wrong in both directions.
.mirai_dispatch_calls <- function(e, acc = list()) {
  if (is.call(e)) {
    f <- e[[1L]]
    if (
      is.call(f) && identical(f[[1L]], quote(`::`)) &&
        identical(f[[2L]], quote(mirai)) &&
        as.character(f[[3L]]) %in% c("daemons", "mirai")
    ) {
      acc[[length(acc) + 1L]] <- e
    }
  }
  if (is.recursive(e)) {
    for (i in seq_along(e)) {
      # e[[i]] can be the empty symbol (a missing argument); touching it errors.
      acc <- tryCatch(.mirai_dispatch_calls(e[[i]], acc), error = function(err) acc)
    }
  }
  acc
}

test_that("save_rawbatch() never dispatches on mirai's DEFAULT compute profile", {
  # THE guard for this defect, and it inspects the function that actually runs.
  #
  # An earlier version of this test set up a named profile by hand and showed it
  # did not disturb the default one. That proved a fact about *mirai*, not about
  # *swereg*: production could revert to a bare daemons(n) and the test would
  # still pass. It guarded nothing. (Caught in review -- against the standard
  # this project set for itself, which makes it worth the paragraph.)
  #
  # The defect: daemons(n) / daemons(0) on the default profile reset and then
  # destroy whatever daemon configuration the caller already had. Verified
  # before the fix: a caller holding 2 daemons was left holding 0. mirai's
  # guidance to package authors is to leave the default profile to users and
  # claim a unique one for dedicated internal resources.
  fn <- swereg::RegistryStudy$public_methods$save_rawbatch
  expect_false(is.null(fn))

  calls <- .mirai_dispatch_calls(body(fn))

  # Not vacuous: if the mirai path is ever restructured away, this test must
  # fail loudly rather than pass by finding nothing to check.
  expect_gt(length(calls), 0L)

  no_compute <- Filter(
    function(cl) !(".compute" %in% names(as.list(cl))),
    calls
  )
  expect_equal(
    vapply(no_compute, function(cl) paste(deparse(cl), collapse = " "), character(1)),
    character(0)
  )
})

test_that("a named compute profile really does leave the default profile alone", {
  # Backs the assumption the test above rests on: that .compute isolates at all.
  # If mirai ever changed this, the AST guard would still pass while the
  # behaviour it is protecting had evaporated.
  skip_on_cran()

  mirai::daemons(1L, dispatcher = FALSE)
  on.exit(mirai::daemons(0L), add = TRUE)
  expect_equal(mirai::status()$connections, 1L)

  # ... what save_rawbatch() does, on its own profile
  mirai::daemons(1L, .compute = "swereg_rawbatch", dispatcher = FALSE)
  mirai::daemons(0L, .compute = "swereg_rawbatch")

  # the caller's default profile must be untouched
  expect_equal(mirai::status()$connections, 1L)
})

test_that("the payload actually crosses into the daemon", {
  # save_rawbatch passes its slice by `.slice = payload_for_batch(b)`; if that
  # substitution silently stopped working the write would still 'succeed'.
  mirai::daemons(1L, dispatcher = FALSE)
  on.exit(mirai::daemons(0L), add = TRUE)

  m <- mirai::mirai(
    {
      sum(.x)
    },
    .x = c(1L, 2L, 3L)
  )
  expect_equal(mirai::call_mirai(m)$data, 6L)
})
