# The one dispatcher (Phase 2), tested THROUGH the real process boundary.
#
# The carry-forward from Phase 1's sign-off: helper tests and dependency demos
# must not substitute for proving the actual parent -> dispatcher -> worker ->
# cleanup -> completion path. So the end-to-end tests here spawn REAL Rscript
# subprocesses driving the REAL inst/batch_worker.R over the REAL envelope codec.
# They cannot use dev_path = NULL: the batch dispatcher is new, so the *installed*
# swereg on this box has no .batch_* symbols -- the worker must load_all() the
# source tree, which is what production does too.

dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
have_tree <- file.exists(file.path(dev_tree, "DESCRIPTION")) &&
  file.exists(file.path(dev_tree, "inst", "batch_worker.R"))

mk <- function(sym) swereg:::.batch_target("swereg", sym)

# --- target descriptor -------------------------------------------------------

test_that(".batch_target() records the same identity hash as .hash_function()", {
  tgt <- mk(".batch_fixture_echo")
  fn <- swereg:::.batch_fixture_echo
  expect_identical(tgt$hash, swereg:::.hash_function(fn))
  expect_identical(tgt$formal_names, "x")
  expect_s3_class(tgt, "batch_target")
})

test_that(".batch_target() normalises a zero-argument target to character(0)", {
  # names(formals(fn)) is NULL for a no-arg function; if that NULL reaches the
  # descriptor a legitimate target looks malformed and .batch_run() rejects it.
  tgt <- mk(".batch_fixture_pid")
  expect_identical(tgt$formal_names, character(0))
})

test_that(".batch_target() rejects a `...` target, a non-function, and a missing symbol", {
  # `...` defeats reliable typo detection -- the whole point of the contract.
  expect_error(mk("paste"), "`\\.\\.\\.`|not defined")  # paste is base, not swereg
  expect_error(swereg:::.batch_target("swereg", ".BATCH_PROTOCOL"), "not a function")
  expect_error(mk(".no_such_symbol_here"), "not defined in package")
  expect_error(swereg:::.batch_target("no.such.pkg", "x"), "not available")
})

test_that("a real `...`-taking target is rejected by name", {
  # message() takes `...`; wrap it as a swereg symbol to hit the dots branch.
  expect_error(swereg:::.batch_target("base", "message"), "`\\.\\.\\.`")
})

# --- parent-side validation (every item, every formal) -----------------------

test_that(".batch_validate_item() enforces the full item contract", {
  tgt <- mk(".batch_fixture_echo")  # one formal: x
  expect_true(swereg:::.batch_validate_item(tgt, list(x = 1L)))

  # missing formal (the arm_labels shape): an OPTIONAL default is no excuse
  expect_error(swereg:::.batch_validate_item(tgt, list()), "not supplied")
  # extra name that is not a formal -- a typo must be caught, not ignored
  expect_error(swereg:::.batch_validate_item(tgt, list(x = 1L, y = 2L)),
    "not formals")
  # positional (unnamed) argument
  expect_error(swereg:::.batch_validate_item(tgt, list(1L)), "must be named")
  # duplicate name
  bad <- list(1L, 2L); names(bad) <- c("x", "x")
  expect_error(swereg:::.batch_validate_item(tgt, bad), "duplicate")
  # not a list at all
  expect_error(swereg:::.batch_validate_item(tgt, 1L), "must be a list")
})

test_that(".batch_run() validates EVERY item, not just the first", {
  skip_if_not(have_tree, "package source tree not available")
  tgt <- mk(".batch_fixture_echo")
  # first item is valid, second is missing its formal: must be rejected before
  # any subprocess is launched (heterogeneous schemas hide behind a good first).
  expect_error(
    swereg:::.batch_run(tgt, items = list(list(x = 1L), list()),
      n_workers = 1L, dev_path = dev_tree),
    "not supplied"
  )
})

# --- private codec -----------------------------------------------------------

test_that("the envelope codec round-trips and does NOT run qs2_read's R6 hook", {
  # .batch_read_envelope must be plain qs2 transport: qs2_read()'s check_version
  # duck-type is swereg persistence policy, not generic IPC, and an envelope is
  # always a plain list. Prove the hook does not fire by planting a check_version
  # member that would error if invoked.
  tmp <- withr::local_tempfile(fileext = ".qs2")
  poison <- list2env(list(
    check_version = function() stop("check_version must NOT run in the codec")
  ))
  swereg:::.batch_write_envelope(list(a = 1L, env = poison), tmp)
  expect_silent(got <- swereg:::.batch_read_envelope(tmp))
  expect_identical(got$a, 1L)
})

# --- end-to-end through a real subprocess ------------------------------------

test_that(".batch_run() drives the real worker end-to-end and preserves order", {
  skip_if_not(have_tree, "package source tree not available")
  r <- swereg:::.batch_run(
    mk(".batch_fixture_echo"),
    items = list(list(x = "a"), list(x = 2L), list(x = c(3, 4, 5))),
    n_workers = 2L, dev_path = dev_tree
  )
  expect_identical(r, list("a", 2L, c(3, 4, 5)))
})

test_that("a NULL result is PRESERVED in place, not dropped (results[idx] <- list())", {
  # THE regression from Phase 1 round 3, re-armed for the new runner -- and armed
  # with the CORRUPTING completion order, not the benign one. `results[[idx]] <-
  # NULL` deletes the element; but with in-order (single-worker) completion the
  # very next item extends the list back and MASKS the bug. It only manifests when
  # the NULL item finishes AFTER a higher-indexed item has filled its slot. So:
  # item 2 (returns NULL) sleeps; items 1 and 3 are instant; two workers -> item 3
  # completes and fills slot 3 while item 2 is still sleeping, then item 2's NULL
  # is assigned last. Under the bug that deletes slot 2 and the result is length 2.
  skip_if_not(have_tree, "package source tree not available")
  r <- swereg:::.batch_run(
    mk(".batch_fixture_slow_echo"),
    items = list(
      list(x = 1L,   seconds = 0),
      list(x = NULL, seconds = 8),
      list(x = 3L,   seconds = 0)
    ),
    n_workers = 2L, dev_path = dev_tree
  )
  expect_length(r, 3L)
  expect_identical(r[[1]], 1L)
  expect_null(r[[2]])
  expect_identical(r[[3]], 3L)
})

test_that("a target error returns a STRUCTURED error envelope (exit 0), surfaced by message", {
  skip_if_not(have_tree, "package source tree not available")
  expect_error(
    swereg:::.batch_run(mk(".batch_fixture_boom"),
      items = list(list(message = "kaboom-XYZ")),
      n_workers = 1L, dev_path = dev_tree),
    "kaboom-XYZ"
  )
})

test_that("a worker that dies WITHOUT an envelope is caught by the exit-code channel", {
  skip_if_not(have_tree, "package source tree not available")
  # .batch_fixture_crash quit(status=3) mid-execute -- no error condition, no
  # result envelope. The structured-error channel cannot see this; the exit-code
  # channel must.
  expect_error(
    swereg:::.batch_run(mk(".batch_fixture_crash"),
      items = list(list()), n_workers = 1L, dev_path = dev_tree),
    "exited 3 before writing a result"
  )
})

test_that("the child refuses a target whose code differs from what the parent hashed", {
  skip_if_not(have_tree, "package source tree not available")
  bad <- mk(".batch_fixture_echo")
  bad$hash <- "deadbeefdeadbeef"  # a hash the child cannot reproduce
  expect_error(
    swereg:::.batch_run(bad, items = list(list(x = 1L)),
      n_workers = 1L, dev_path = dev_tree),
    "DIFFERENT code version"
  )
})

test_that("each item runs in a FRESH process (the memory strategy, not an accident)", {
  skip_if_not(have_tree, "package source tree not available")
  pids <- swereg:::.batch_run(mk(".batch_fixture_pid"),
    items = list(list(), list(), list()), n_workers = 1L, dev_path = dev_tree)
  expect_length(unique(unlist(pids)), 3L)
})

test_that("a per-item timeout kills a runaway worker and reports it", {
  skip_if_not(have_tree, "package source tree not available")
  t0 <- Sys.time()
  expect_error(
    swereg:::.batch_run(mk(".batch_fixture_sleep"),
      items = list(list(seconds = 60)),
      n_workers = 1L, dev_path = dev_tree, timeout = 2),
    "timeout"
  )
  # it must not actually wait 60s
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 30)
})

test_that("collect = FALSE reports status but returns no values", {
  skip_if_not(have_tree, "package source tree not available")
  # success path: no error, invisible(NULL)
  out <- swereg:::.batch_run(mk(".batch_fixture_echo"),
    items = list(list(x = 1L)), n_workers = 1L, dev_path = dev_tree,
    collect = FALSE)
  expect_null(out)
  # and a failure is STILL surfaced even though no value is collected
  expect_error(
    swereg:::.batch_run(mk(".batch_fixture_boom"),
      items = list(list(message = "still-loud")),
      n_workers = 1L, dev_path = dev_tree, collect = FALSE),
    "still-loud"
  )
})

test_that("keep_failed_dir retains METADATA only -- arg VALUES are never persisted", {
  skip_if_not(have_tree, "package source tree not available")
  # The target fails with a FIXED message that does NOT echo its argument, so the
  # only way the value "SECRETVALUE-XYZ" could appear in the record is if the
  # runner retained argument VALUES. The record is DESERIALIZED and searched
  # field by field -- not grepped as compressed qs2 bytes, which would hide the
  # plaintext and pass for the wrong reason (the round-2 lesson).
  fdir <- withr::local_tempdir()
  tryCatch(
    swereg:::.batch_run(mk(".batch_fixture_secret_fail"),
      items = list(item_one = list(payload = "SECRETVALUE-XYZ")),
      n_workers = 1L, dev_path = dev_tree, keep_failed_dir = fdir),
    error = function(e) NULL
  )
  recs <- list.files(fdir, full.names = TRUE)
  expect_length(recs, 1L)
  rec <- swereg:::.batch_read_envelope(recs[[1]])
  expect_identical(rec$id, "item_one")         # the stable id, not "1"
  expect_identical(rec$arg_names, "payload")   # the NAME is kept
  expect_false(is.null(rec$error))
  expect_false(grepl("SECRETVALUE", rec$error, fixed = TRUE))  # error didn't echo it
  # the value appears in NO field of the deserialized record
  expect_false(any(grepl("SECRETVALUE", unlist(rec), fixed = TRUE)))
  # a caller-supplied secure dir is hardened, and the record file is 0600
  expect_identical(file.mode(fdir), as.octmode("700"))
  expect_identical(file.mode(recs[[1]]), as.octmode("600"))
})

test_that(".batch_run() leaves no temp input/output/log files behind", {
  skip_if_not(have_tree, "package source tree not available")
  before <- list.files(tempdir(), pattern = "^batch_(in|out|log)_")
  swereg:::.batch_run(mk(".batch_fixture_echo"),
    items = list(list(x = 1L), list(x = 2L)), n_workers = 2L, dev_path = dev_tree)
  after <- list.files(tempdir(), pattern = "^batch_(in|out|log)_")
  expect_identical(sort(after), sort(before))
})

test_that("a FAILING run also leaves no temp files behind", {
  skip_if_not(have_tree, "package source tree not available")
  before <- list.files(tempdir(), pattern = "^batch_(in|out|log)_")
  expect_error(
    swereg:::.batch_run(mk(".batch_fixture_boom"),
      items = list(list(message = "x")), n_workers = 1L, dev_path = dev_tree),
    "returned an error")
  after <- list.files(tempdir(), pattern = "^batch_(in|out|log)_")
  expect_identical(sort(after), sort(before))
})

# --- envelope + result validation (now load-bearing, not decorative) ---------

test_that(".batch_check_envelope() rejects malformed input envelopes", {
  good <- list(protocol = 1L, meta = list(package = "swereg", symbol = "s",
    hash = "h", id = "1", collect = TRUE), args = list())
  expect_true(swereg:::.batch_check_envelope(good))
  expect_error(swereg:::.batch_check_envelope(42), "not a list")
  expect_error(swereg:::.batch_check_envelope(within(good, protocol <- 99L)),
    "protocol mismatch")
  expect_error(swereg:::.batch_check_envelope(within(good, meta <- NULL)), "no meta")
  bad_pkg <- good; bad_pkg$meta$package <- NA_character_
  expect_error(swereg:::.batch_check_envelope(bad_pkg), "package")
  bad_col <- good; bad_col$meta$collect <- "yes"
  expect_error(swereg:::.batch_check_envelope(bad_col), "collect")
})

test_that(".batch_execute() is TOTAL: a malformed envelope yields an error envelope, not a throw", {
  res <- swereg:::.batch_execute(list(protocol = 1L, meta = NULL, args = list()))
  expect_identical(res$status, "error")
  expect_false(is.null(res$error$message))
  # even a non-list env must not throw
  expect_identical(swereg:::.batch_execute(42)$status, "error")
})

test_that(".batch_inspect_result() makes protocol, id and FULL target identity load-bearing, and is total", {
  tgt <- list(package = "swereg", symbol = "s", hash = "H")
  ok <- list(protocol = 1L, id = "7", status = "ok", value = 99L,
    warnings = character(),
    target = list(package = "swereg", symbol = "s", hash = "H"))
  expect_true(swereg:::.batch_inspect_result(ok, "7", tgt)$ok)
  # TOTAL: a non-list result is a failure REASON, not a throw (so it flows through
  # the caller's uniform failure path instead of crashing the inspector).
  nl <- swereg:::.batch_inspect_result("garbage", "7", tgt)
  expect_false(nl$ok)
  expect_match(nl$reason, "not a list")
  # wrong protocol
  expect_false(swereg:::.batch_inspect_result(within(ok, protocol <- 2L), "7", tgt)$ok)
  # error status carries the message
  err <- list(protocol = 1L, id = "7", status = "error",
    error = list(message = "boom"), warnings = character())
  expect_match(swereg:::.batch_inspect_result(err, "7", tgt)$reason, "boom")
  # id mismatch -- a valid-looking result for the WRONG item is rejected
  expect_false(swereg:::.batch_inspect_result(ok, "8", tgt)$ok)
  # identity is package + symbol + hash, not hash alone: each mismatch rejected
  expect_false(swereg:::.batch_inspect_result(ok, "7",
    list(package = "swereg", symbol = "s", hash = "DIFFERENT"))$ok)
  sym <- swereg:::.batch_inspect_result(ok, "7",
    list(package = "swereg", symbol = "OTHER", hash = "H"))
  expect_false(sym$ok)
  expect_match(sym$reason, "different target")
  expect_false(swereg:::.batch_inspect_result(ok, "7",
    list(package = "other", symbol = "s", hash = "H"))$ok)
  # a success envelope with NO value field is rejected; a legitimate NULL is not
  novalue <- ok
  novalue$value <- NULL
  expect_false(swereg:::.batch_inspect_result(novalue, "7", tgt)$ok)
  null_ok <- list(protocol = 1L, id = "7", status = "ok", value = NULL,
    warnings = character(),
    target = list(package = "swereg", symbol = "s", hash = "H"))
  expect_true(swereg:::.batch_inspect_result(null_ok, "7", tgt)$ok)
})

test_that(".batch_run() validates timeout and collect as scalars, even for empty work", {
  ie <- list(list(x = 1L))
  expect_error(swereg:::.batch_run(mk(".batch_fixture_echo"), ie, 1L,
    dev_path = NULL, timeout = c(1, 2)), "single positive")
  expect_error(swereg:::.batch_run(mk(".batch_fixture_echo"), ie, 1L,
    dev_path = NULL, timeout = NA_real_), "single positive")
  expect_error(swereg:::.batch_run(mk(".batch_fixture_echo"), ie, 1L,
    dev_path = NULL, timeout = -5), "single positive")
  expect_error(swereg:::.batch_run(mk(".batch_fixture_echo"), ie, 1L,
    dev_path = NULL, collect = 1), "TRUE or FALSE")
  # validated even when there is NO work (no early-return bypass)
  expect_error(swereg:::.batch_run(mk(".batch_fixture_echo"), list(), 1L,
    dev_path = NULL, timeout = -1), "single positive")
})

test_that(".batch_inspect_result() stays total on malformed list fields and is strict on id", {
  tgt <- list(package = "swereg", symbol = "s", hash = "H")
  ok <- list(protocol = 1L, id = "7", status = "ok", value = 1L,
    warnings = character(),
    target = list(package = "swereg", symbol = "s", hash = "H"))
  # a bare-string `error` (not a list) must not throw while extracting the message
  es <- list(protocol = 1L, id = "7", status = "error", error = "boom",
    warnings = character())
  expect_false(swereg:::.batch_inspect_result(es, "7", tgt)$ok)
  # a NUMERIC id is rejected (strict identical, no as.character coercion)
  num_id <- ok; num_id$id <- 7L
  expect_false(swereg:::.batch_inspect_result(num_id, "7", tgt)$ok)
  # a non-character `warnings` (e.g. a closure) is DROPPED, not coerced (which
  # would throw), keeping the inspector total
  weird <- ok; weird$warnings <- mean
  rw <- swereg:::.batch_inspect_result(weird, "7", tgt)
  expect_true(rw$ok)
  expect_identical(rw$warnings, character())
  # DUPLICATE critical field names cannot smuggle a bad value behind a good one:
  # `$protocol` would return the first (1L); the name check rejects the envelope.
  dup <- list(protocol = 1L, protocol = 99L, id = "7", status = "ok", value = 1L,
    warnings = character(),
    target = list(package = "swereg", symbol = "s", hash = "H"))
  d <- swereg:::.batch_inspect_result(dup, "7", tgt)
  expect_false(d$ok)
  expect_match(d$reason, "duplicate field names")
})

test_that(".batch_inspect_result() is total even against a hostile classed object", {
  tgt <- list(package = "swereg", symbol = "s", hash = "H")
  # The inspector reads fields with EXACT `[[`, so the realistic hostile object is
  # one whose `[[` method throws (a `$` method would never be invoked). It must
  # yield a failure REASON, not crash the pool -- the whole inspection is wrapped.
  # The method is REGISTERED because that is the only way `[[` dispatch inside the
  # swereg namespace finds it, and the only way this threat is real (an object
  # deserialized to a class with a loaded, registered `[[` method).
  registerS3method("[[", "batchHostile", function(x, i, ...) stop("hostile [[ access"))
  hostile <- structure(
    list(protocol = 1L, id = "7", status = "ok"), class = "batchHostile")
  r <- swereg:::.batch_inspect_result(hostile, "7", tgt)
  expect_false(r$ok)
  expect_match(r$reason, "malformed result envelope")

  # even a condition whose OWN conditionMessage() method throws must not escape
  # (the error handler renders the message safely). No throw here == the guarantee.
  registerS3method("[[", "batchHostile2",
    function(x, i, ...) stop(structure(
      class = c("hostCond", "error", "condition"), list(message = "m", call = NULL))))
  registerS3method("conditionMessage", "hostCond",
    function(c) stop("conditionMessage itself throws"))
  hostile2 <- structure(
    list(protocol = 1L, id = "7", status = "ok"), class = "batchHostile2")
  r2 <- swereg:::.batch_inspect_result(hostile2, "7", tgt)
  expect_false(r2$ok)
  expect_match(r2$reason, "malformed result envelope")
})

test_that(".batch_inspect_result() rejects DUPLICATE names in the nested target too", {
  tgt <- list(package = "swereg", symbol = "s", hash = "H")
  ok <- list(protocol = 1L, id = "7", status = "ok", value = 1L,
    warnings = character(),
    target = list(package = "swereg", symbol = "s", hash = "H"))
  # `target = list(package="swereg", package="evil", ...)` must not resolve via the
  # first `$package` and leave the executed identity ambiguous.
  dup_tgt <- ok
  dup_tgt$target <- list(package = "swereg", package = "evil", symbol = "s", hash = "H")
  expect_false(swereg:::.batch_inspect_result(dup_tgt, "7", tgt)$ok)
})

test_that(".batch_check_envelope() rejects duplicate outer and meta field names", {
  gm <- list(package = "swereg", symbol = "s", hash = "h", id = "1", collect = TRUE)
  expect_error(
    swereg:::.batch_check_envelope(list(protocol = 1L, protocol = 1L,
      meta = gm, args = list())),
    "duplicate field names")
  expect_error(
    swereg:::.batch_check_envelope(list(protocol = 1L,
      meta = c(gm, list(package = "evil")), args = list())),
    "meta has duplicate")
})

test_that("the REAL worker validates envelope structure BEFORE loading any code", {
  # The production wiring, not just the helper: a malformed envelope must be
  # rejected by the standalone worker BEFORE it acts on meta to load code. The
  # envelope has DUPLICATE meta$package AND a dev_path that would fail to load if
  # reached -- so if the worker validated first the error is "duplicate field
  # names", and if it loaded first the error would mention the bad dev_path. This
  # is the actual caller -> worker boundary through a real subprocess.
  skip_if_not(have_tree, "package source tree not available")
  worker <- file.path(dev_tree, "inst", "batch_worker.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  meta <- list(package = "swereg", package = "swereg",
    symbol = ".batch_fixture_echo", hash = "x", id = "1", collect = TRUE,
    runner_package = "swereg", dev_path = "/nonexistent/would/fail/to/load")
  env <- list(protocol = 1L, meta = meta, args = list(x = 1L))
  inp <- withr::local_tempfile(fileext = ".qs2")
  outp <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(env, inp)

  p <- processx::process$new(rscript, c("--vanilla", worker, inp, outp),
    env = c("current", R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    stdout = "|", stderr = "|")
  p$wait(timeout = 30000)

  expect_true(file.exists(outp))
  res <- swereg:::.batch_read_envelope(outp)
  expect_identical(res$status, "error")
  expect_match(res$error$message, "duplicate field names")
  # proof it did NOT load first: the error is structural, not about the bad path
  expect_no_match(res$error$message, "nonexistent|load_all|devtools")
})

test_that("the REAL worker uses EXACT field extraction (no `$` partial-match steering loading)", {
  # `$` partial-matches in R: `meta$dev_path` would resolve a field named
  # `dev_path_payload` when no exact `dev_path` exists. The worker uses exact `[[`,
  # so a noncanonical field cannot steer which code is loaded. Envelope carries
  # `dev_path_payload = "/attacker/tree..."` and NO exact `dev_path`; under `$` the
  # worker would `load_all()` that path -- under `[[` it never consults it.
  skip_if_not(have_tree, "package source tree not available")
  worker <- file.path(dev_tree, "inst", "batch_worker.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  meta <- list(dev_path_payload = "/attacker/tree/would/be/loaded",
    package = "swereg", symbol = ".batch_fixture_echo", hash = "x", id = "1",
    collect = TRUE, runner_package = "swereg")
  env <- list(protocol = 1L, meta = meta, args = list(x = 1L))
  inp <- withr::local_tempfile(fileext = ".qs2")
  outp <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(env, inp)

  p <- processx::process$new(rscript, c("--vanilla", worker, inp, outp),
    env = c("current", R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    stdout = "|", stderr = "|")
  p$wait(timeout = 30000)

  expect_true(file.exists(outp))
  res <- swereg:::.batch_read_envelope(outp)
  # whatever the outcome, the attacker path must appear NOWHERE (under `$` it
  # would be load_all()ed and surface in the error) -- proving exact extraction.
  expect_no_match(paste(unlist(res), collapse = " "), "attacker/tree", fixed = TRUE)
})

test_that(".batch_run() rejects a non-list `items` container, even when empty", {
  expect_error(swereg:::.batch_run(mk(".batch_fixture_echo"), character(0), 1L,
    dev_path = NULL), "must be a list")
  expect_error(swereg:::.batch_run(mk(".batch_fixture_echo"), 1:3, 1L,
    dev_path = NULL), "must be a list")
})

test_that(".batch_retain_failure() FAILS CLOSED when the directory cannot be secured", {
  # If Sys.chmod cannot secure the dir to 0700 (e.g. a mount that ignores it), no
  # record is written -- rather than dropping a target-authored message somewhere
  # world-readable and reporting success.
  fdir <- withr::local_tempdir()
  testthat::local_mocked_bindings(Sys.chmod = function(...) FALSE, .package = "base")
  suppressWarnings(
    r <- swereg:::.batch_retain_failure(fdir, "id1", 1L,
      list(package = "p", symbol = "s", hash = "h"), "argn", "msg")
  )
  expect_null(r)
  expect_length(list.files(fdir), 0L)
})

# --- warnings, memory, ids, empty-workload validation ------------------------

test_that("a successful target's warnings are surfaced in the parent, tagged by id", {
  skip_if_not(have_tree, "package source tree not available")
  expect_warning(
    swereg:::.batch_run(mk(".batch_fixture_warn"),
      items = list(only = list(x = "WOT")), n_workers = 1L, dev_path = dev_tree),
    "\\[batch item 'only'\\].*WOT"
  )
})

test_that("collect = FALSE drops the target value before it enters the envelope", {
  # The shape-A memory guarantee, tested at the executor: a large return must not
  # be carried back when collect = FALSE.
  env <- list(protocol = 1L, meta = list(package = "swereg",
    symbol = ".batch_fixture_big", version = "0",
    hash = mk(".batch_fixture_big")$hash, dev_path = NULL,
    runner_package = "swereg", id = "1", collect = FALSE),
    args = list(n = 1e6))
  res <- swereg:::.batch_execute(env)
  expect_identical(res$status, "ok")
  expect_null(res$value)
  # with collect = TRUE the same call DOES carry the value (control)
  env$meta$collect <- TRUE
  expect_length(swereg:::.batch_execute(env)$value, 1e6)
})

test_that("a given-but-wrong dev_path errors EVEN for an empty workload", {
  # Phase-1 shape: an early return must not skip input validation.
  expect_error(
    swereg:::.batch_run(mk(".batch_fixture_echo"), items = list(),
      n_workers = 1L, dev_path = "/no/such/tree"),
    "does not exist")
})

test_that("duplicate item ids are rejected", {
  bad <- list(list(x = 1L), list(x = 2L))
  names(bad) <- c("dup", "dup")
  expect_error(
    swereg:::.batch_run(mk(".batch_fixture_echo"), items = bad,
      n_workers = 1L, dev_path = dev_tree),
    "not unique")
})
