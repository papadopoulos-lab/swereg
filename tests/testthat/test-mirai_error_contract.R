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
  mirai::daemons(1L)
  on.exit(mirai::daemons(0L), add = TRUE)

  m <- mirai::mirai({
    stop("simulated slice write failure")
  })
  v <- mirai::call_mirai(m)$data

  expect_true(mirai::is_error_value(v))
  expect_match(as.character(v), "simulated slice write failure")
})

test_that("a successful mirai task is NOT flagged as an error", {
  mirai::daemons(1L)
  on.exit(mirai::daemons(0L), add = TRUE)

  m <- mirai::mirai({
    TRUE
  })
  v <- mirai::call_mirai(m)$data

  expect_false(mirai::is_error_value(v))
  expect_true(v)
})

test_that("the payload actually crosses into the daemon", {
  # save_rawbatch passes its slice by `.slice = payload_for_batch(b)`; if that
  # substitution silently stopped working the write would still 'succeed'.
  mirai::daemons(1L)
  on.exit(mirai::daemons(0L), add = TRUE)

  m <- mirai::mirai(
    {
      sum(.x)
    },
    .x = c(1L, 2L, 3L)
  )
  expect_equal(mirai::call_mirai(m)$data, 6L)
})
