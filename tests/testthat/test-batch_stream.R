# Shape B of the ONE contract (Phase 2): the parent is the producer, items are
# generated lazily under backpressure, and a bounded number are in flight at
# once. This is what save_rawbatch() needs (its items ARE the data slices, so
# materialising them all up front means the whole dataset on disk/in RAM twice).
# .batch_stream shares target resolution, validation, the result envelope and the
# failure semantics with .batch_run; only the transport differs (mirai in-memory
# vs processx qs2 files). Tested through a REAL mirai daemon, not a mock.

skip_if_not_installed("mirai")

dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
have_tree <- file.exists(file.path(dev_tree, "DESCRIPTION")) &&
  file.exists(file.path(dev_tree, "inst", "batch_worker.R"))

mk <- function(sym) swereg:::.batch_target("swereg", sym)

test_that(".batch_stream() runs a lazy producer through daemons, results in id order", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  seen <- character()
  producer <- function(id) {
    seen[[length(seen) + 1L]] <<- id
    list(x = id)
  }
  r <- swereg:::.batch_stream(
    mk(".batch_fixture_echo"),
    ids = c("a", "b", "c", "d"),
    producer = producer,
    n_workers = 2L, dev_path = dev_tree
  )
  # producer called exactly once per id, in id order
  expect_identical(seen, c("a", "b", "c", "d"))
  # results in id order, named by id
  expect_identical(r, list(a = "a", b = "b", c = "c", d = "d"))
})

test_that(".batch_stream() applies backpressure -- producer is gated by in-flight limit", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  # 1 worker => max_inflight = 2. The producer records its parent-side call time.
  # With backpressure, producer(3) cannot be called until a daemon drains id 1
  # (~1s of sleep), so call 3 lands well after call 1. Without the inflight guard
  # all four producers fire near-instantly. This is the raison d'etre of shape B.
  times <- numeric(0)
  producer <- function(id) {
    times[[length(times) + 1L]] <<- as.numeric(Sys.time())
    list(x = id, seconds = 1)
  }
  swereg:::.batch_stream(
    mk(".batch_fixture_slow_echo"),
    ids = as.character(1:4),
    producer = producer,
    n_workers = 1L, dev_path = dev_tree, collect = FALSE
  )
  expect_length(times, 4L)
  expect_gt(times[[3]] - times[[1]], 0.5)
})

test_that(".batch_stream() surfaces a target error rather than swallowing it", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  producer <- function(id) list(message = paste0("stream-boom-", id))
  expect_error(
    swereg:::.batch_stream(
      mk(".batch_fixture_boom"),
      ids = c("x", "y"), producer = producer,
      n_workers = 2L, dev_path = dev_tree
    ),
    "stream-boom"
  )
})

test_that(".batch_stream() preserves a NULL result in place (results[pos] <- list())", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  # Same [[<- deletion trap as .batch_run. .batch_stream drains strictly FIFO, so
  # results are assigned in position order and a MID-list NULL would be masked by
  # the next assignment extending the list back (exactly the benign order that
  # fooled the round-3 test). The corrupting case under FIFO is a NULL in the LAST
  # position: `results[[n]] <- NULL` deletes it and nothing follows to re-extend,
  # so the result comes back length n-1. The NULL id is therefore placed last.
  producer <- function(id) list(x = if (id == "n") NULL else id)
  r <- swereg:::.batch_stream(
    mk(".batch_fixture_echo"),
    ids = c("a", "z", "n"), producer = producer,
    n_workers = 2L, dev_path = dev_tree
  )
  expect_length(r, 3L)
  expect_identical(r[["a"]], "a")
  expect_identical(r[["z"]], "z")
  expect_null(r[["n"]])
})

test_that(".batch_stream() validates each produced item against the target", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  # A producer that yields an item missing the target's formal is rejected in the
  # parent, before it is ever dispatched to a daemon.
  producer <- function(id) list()  # .batch_fixture_echo needs `x`
  expect_error(
    swereg:::.batch_stream(
      mk(".batch_fixture_echo"),
      ids = "a", producer = producer,
      n_workers = 1L, dev_path = dev_tree
    ),
    "not supplied"
  )
})

test_that(".batch_stream() never dispatches on mirai's DEFAULT compute profile", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  # Defect #2's shape: daemons(n)/daemons(0) on the default profile reset and
  # destroy whatever the caller had. The caller here holds 1 daemon on the
  # default profile; .batch_stream must claim its own and leave the default alone.
  mirai::daemons(1L, dispatcher = FALSE)
  on.exit(mirai::daemons(0L), add = TRUE)
  expect_equal(mirai::status()$connections, 1L)

  swereg:::.batch_stream(
    mk(".batch_fixture_echo"),
    ids = c("a", "b"), producer = function(id) list(x = id),
    n_workers = 1L, dev_path = dev_tree
  )
  # the caller's default-profile daemon is untouched
  expect_equal(mirai::status()$connections, 1L)
})

test_that(".batch_stream() refuses to HIJACK a named profile the caller already owns", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  # The caller owns "test_owned_profile". .batch_stream told to use that same name
  # must refuse rather than reset+destroy it (the same ownership violation as the
  # default profile, on a colliding name).
  mirai::daemons(1L, .compute = "test_owned_profile", dispatcher = FALSE)
  on.exit(mirai::daemons(0L, .compute = "test_owned_profile"), add = TRUE)

  expect_error(
    swereg:::.batch_stream(
      mk(".batch_fixture_echo"),
      ids = c("a", "b"), producer = function(id) list(x = id),
      n_workers = 1L, dev_path = dev_tree, compute = "test_owned_profile"
    ),
    "already configured"
  )
  # the caller's profile survived the refusal
  expect_equal(mirai::status(.compute = "test_owned_profile")$connections, 1L)
})

test_that(".batch_stream() surfaces a wedged/slow task via its per-item timeout", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  t0 <- Sys.time()
  expect_error(
    swereg:::.batch_stream(
      mk(".batch_fixture_sleep"),
      ids = "slow", producer = function(id) list(seconds = 120),
      n_workers = 1L, dev_path = dev_tree, timeout = 3
    ),
    "daemon/timeout error"
  )
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 60)
})

test_that(".batch_stream() retention is metadata-only, arg values not persisted", {
  skip_on_cran()
  skip_if_not(have_tree, "package source tree not available")
  fdir <- withr::local_tempdir()
  tryCatch(
    swereg:::.batch_stream(
      mk(".batch_fixture_secret_fail"),
      ids = "s1", producer = function(id) list(payload = "STREAMSECRET-Q"),
      n_workers = 1L, dev_path = dev_tree, keep_failed_dir = fdir
    ),
    error = function(e) NULL
  )
  recs <- list.files(fdir, full.names = TRUE)
  expect_length(recs, 1L)
  rec <- swereg:::.batch_read_envelope(recs[[1]])
  expect_identical(rec$id, "s1")
  expect_identical(rec$arg_names, "payload")
  expect_false(any(grepl("STREAMSECRET", unlist(rec), fixed = TRUE)))
})

test_that(".batch_stream() validates ids, config and dev_path before doing any work", {
  # duplicate ids
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = c("a", "a"),
      producer = function(id) list(x = id), n_workers = 1L),
    "unique")
  # NA / empty id
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = c("a", NA),
      producer = function(id) list(x = id), n_workers = 1L),
    "non-empty, non-NA")
  # timeout / collect validated as scalars, not silently disabled
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = "a",
      producer = function(id) list(x = id), n_workers = 1L, timeout = c(1, 2)),
    "single positive")
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = "a",
      producer = function(id) list(x = id), n_workers = 1L, collect = NA),
    "TRUE or FALSE")
  # the DEFAULT mirai profile is refused outright -- including an ALIASED default
  # (a name on the vector must not smuggle "default" past the check)
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = "a",
      producer = function(id) list(x = id), n_workers = 1L, compute = "default"),
    "default profile")
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = "a",
      producer = function(id) list(x = id), n_workers = 1L,
      compute = c(alias = "default")),
    "default profile")
  # an NA profile name is rejected (nzchar(NA) is TRUE, so this needs its own check)
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = "a",
      producer = function(id) list(x = id), n_workers = 1L,
      compute = NA_character_),
    "non-NA")
  # given-but-wrong dev_path errors even with an empty workload
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = character(0),
      producer = function(id) list(x = id), n_workers = 1L,
      dev_path = "/no/such/tree"),
    "does not exist")
})

test_that(".batch_stream() FAILS CLOSED when profile ownership cannot be determined", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  # A daemons_set() error must NOT be treated as "profile is free" (fail-open
  # would reset an owned profile). Refuse instead.
  testthat::local_mocked_bindings(
    daemons_set = function(...) stop("cannot determine"),
    .package = "mirai"
  )
  # dev_path = NULL: this errors at the ownership check (before any load), and
  # NULL is always valid -- passing a real tree would fail dev_path validation
  # first under R CMD check, where the source tree is not available.
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = "a",
      producer = function(id) list(x = id), n_workers = 1L,
      dev_path = NULL, compute = "some_profile"),
    "refusing to claim")
})

test_that(".batch_stream() ownership uses daemons_set() (connection-independent)", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  # daemons_set() is TRUE for ANY configured profile regardless of live
  # connections -- so a configured-but-unconnected profile (which a connections>0
  # check would miss and hijack) is caught. Mock it to the "configured" answer.
  testthat::local_mocked_bindings(
    daemons_set = function(...) TRUE,
    .package = "mirai"
  )
  # dev_path = NULL: errors at the ownership check before any load (see above).
  expect_error(
    swereg:::.batch_stream(mk(".batch_fixture_echo"), ids = "a",
      producer = function(id) list(x = id), n_workers = 1L,
      dev_path = NULL, compute = "some_profile"),
    "already configured")
})
