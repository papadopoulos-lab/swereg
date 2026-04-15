library(data.table)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.sk_dt <- function() {
  data.table::data.table(
    id          = c(1L, 2L, 3L),
    isoyear     = 2020L,
    isoyearweek = c("2020-01", "2020-01", "2020-01"),
    personyears = 1 / 52.25
  )
}

# A tiny registry entry using a fake `fn` that just adds one column per code
# with a known value. This lets the test check BOTH .entry_columns()
# predictions AND the apply/drop mechanics in isolation from the real
# add_diagnoses / add_rx / etc. (which have their own parity tests).
.fake_fn <- function(skeleton, dataset, id_name, codes, ...) {
  for (col_name in names(codes)) {
    skeleton[, (col_name) := TRUE]
  }
  invisible(skeleton)
}

.fake_entry <- function(codes, groups, combine_as = NULL, label = "fake") {
  list(
    codes      = codes,
    fn         = .fake_fn,
    fn_args    = list(),
    groups     = groups,
    combine_as = combine_as,
    label      = label
  )
}

.fake_batch_data <- function() {
  list(
    grp_a = data.table::data.table(id = 1:3, dummy = 1:3),
    grp_b = data.table::data.table(id = 1:3, dummy = 4:6)
  )
}

# ---------------------------------------------------------------------------
# Construction + basic fields
# ---------------------------------------------------------------------------

test_that("Skeleton$new constructs with expected initial state", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 5L)

  expect_s3_class(sk, "Skeleton")
  expect_s3_class(sk$data, "data.table")
  expect_equal(sk$batch_number, 5L)
  expect_null(sk$framework_fn_hash)
  expect_equal(sk$applied_registry, list())
  expect_equal(sk$randvars_state, list())
  expect_s3_class(sk$created_at, "POSIXct")
})

test_that("Skeleton$new rejects non-data.table input", {
  expect_error(
    Skeleton$new(data = data.frame(id = 1L), batch_number = 1L),
    "must be a data.table"
  )
})

test_that("Skeleton$print does not crash and includes batch number", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 42L)
  out <- capture.output(print(sk))
  expect_match(out[[1]], "batch 42")
  expect_true(any(grepl("pipeline_hash", out)))
})

# ---------------------------------------------------------------------------
# apply_code_entry / drop_code_entry
# ---------------------------------------------------------------------------

test_that("apply_code_entry adds columns and records the descriptor", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  entry <- .fake_entry(
    codes = list(foo = "X", bar = "Y"),
    groups = list(p = "grp_a")
  )

  sk$apply_code_entry(entry, .fake_batch_data(), id_col = "id", fingerprint = "fp1")

  expect_true(all(c("p_foo", "p_bar") %in% names(sk$data)))
  expect_true("fp1" %in% names(sk$applied_registry))
  stored <- sk$applied_registry[["fp1"]]
  expect_equal(stored[["codes"]], list(foo = "X", bar = "Y"))
  expect_equal(stored[["groups"]], list(p = "grp_a"))
  expect_null(stored[["combine_as"]])
  expect_equal(stored[["fn_args"]], list())
  expect_equal(stored[["label"]], "fake")
  # fn is intentionally NOT stored. Use [["fn"]] (exact match) rather than
  # $fn which partial-matches against $fn_args.
  expect_false("fn" %in% names(stored))
})

test_that("apply_code_entry respects combine_as", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  entry <- .fake_entry(
    codes      = list(foo = "X"),
    groups     = list(a = "grp_a", b = "grp_b"),
    combine_as = "ab"
  )

  sk$apply_code_entry(entry, .fake_batch_data(), id_col = "id", fingerprint = "fp1")

  expect_true("a_foo"  %in% names(sk$data))
  expect_true("b_foo"  %in% names(sk$data))
  expect_true("ab_foo" %in% names(sk$data))
})

test_that("drop_code_entry removes exactly the columns .entry_columns() predicts", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  entry <- .fake_entry(
    codes      = list(foo = "X", bar = "Y"),
    groups     = list(a = "grp_a", b = "grp_b"),
    combine_as = "ab"
  )

  sk$apply_code_entry(entry, .fake_batch_data(), id_col = "id", fingerprint = "fp1")
  applied_cols <- intersect(
    c("a_foo", "a_bar", "b_foo", "b_bar", "ab_foo", "ab_bar"),
    names(sk$data)
  )
  expect_length(applied_cols, 6L)

  sk$drop_code_entry("fp1")

  # Every predicted col removed; descriptor cleared
  expect_false(any(c("a_foo", "a_bar", "b_foo", "b_bar", "ab_foo", "ab_bar") %in% names(sk$data)))
  expect_false("fp1" %in% names(sk$applied_registry))
  # Original columns preserved
  expect_true(all(c("id", "isoyear", "isoyearweek") %in% names(sk$data)))
})

test_that("drop_code_entry on unknown fingerprint is a safe no-op", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  before_names <- copy(names(sk$data))
  expect_silent(sk$drop_code_entry("nonexistent"))
  expect_equal(names(sk$data), before_names)
})

test_that("drop_code_entry tolerates missing columns (partial-state robustness)", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  entry <- .fake_entry(
    codes  = list(foo = "X"),
    groups = list(p = "grp_a")
  )
  sk$apply_code_entry(entry, .fake_batch_data(), id_col = "id", fingerprint = "fp1")

  # Simulate partial state: someone externally deleted the column
  sk$data[, p_foo := NULL]

  expect_silent(sk$drop_code_entry("fp1"))
  expect_false("fp1" %in% names(sk$applied_registry))
})

# ---------------------------------------------------------------------------
# sync_with_registry
# ---------------------------------------------------------------------------

test_that("sync_with_registry is a no-op when fingerprints match", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  entry <- .fake_entry(codes = list(foo = "X"), groups = list(p = "grp_a"))
  sk$apply_code_entry(entry, .fake_batch_data(), id_col = "id", fingerprint = "fp1")
  snapshot_before <- copy(names(sk$data))

  loader_calls <- 0L
  sk$sync_with_registry(
    current_fps       = "fp1",
    registry          = list(entry),
    batch_data_loader = function() { loader_calls <<- loader_calls + 1L; .fake_batch_data() },
    id_col            = "id"
  )

  expect_equal(names(sk$data), snapshot_before)
  expect_equal(loader_calls, 0L)  # lazy: loader not called on no-op
})

test_that("sync_with_registry drops removed entries", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  entry_a <- .fake_entry(codes = list(foo = "X"), groups = list(p = "grp_a"))
  entry_b <- .fake_entry(codes = list(bar = "Y"), groups = list(p = "grp_a"))
  sk$apply_code_entry(entry_a, .fake_batch_data(), id_col = "id", fingerprint = "fp_a")
  sk$apply_code_entry(entry_b, .fake_batch_data(), id_col = "id", fingerprint = "fp_b")
  expect_true(all(c("p_foo", "p_bar") %in% names(sk$data)))

  # Remove entry_b: sync with only fp_a in current
  sk$sync_with_registry(
    current_fps       = "fp_a",
    registry          = list(entry_a),
    batch_data_loader = function() .fake_batch_data(),
    id_col            = "id"
  )

  expect_true("p_foo" %in% names(sk$data))
  expect_false("p_bar" %in% names(sk$data))
  expect_equal(names(sk$applied_registry), "fp_a")
})

test_that("sync_with_registry applies new entries lazily", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  entry <- .fake_entry(codes = list(foo = "X"), groups = list(p = "grp_a"))

  loader_calls <- 0L
  sk$sync_with_registry(
    current_fps       = "fp1",
    registry          = list(entry),
    batch_data_loader = function() { loader_calls <<- loader_calls + 1L; .fake_batch_data() },
    id_col            = "id"
  )

  expect_true("p_foo" %in% names(sk$data))
  expect_equal(loader_calls, 1L)  # loaded once, when needed
})

test_that("sync_with_registry handles a changed entry (drop + add)", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  old_entry <- .fake_entry(codes = list(foo = "X"), groups = list(p = "grp_a"))
  sk$apply_code_entry(old_entry, .fake_batch_data(), id_col = "id", fingerprint = "fp_old")
  expect_true("p_foo" %in% names(sk$data))

  # Same label, different codes -> different fingerprint
  new_entry <- .fake_entry(codes = list(bar = "Y"), groups = list(p = "grp_a"))
  sk$sync_with_registry(
    current_fps       = "fp_new",
    registry          = list(new_entry),
    batch_data_loader = function() .fake_batch_data(),
    id_col            = "id"
  )

  expect_false("p_foo" %in% names(sk$data))  # old columns gone
  expect_true("p_bar"  %in% names(sk$data))  # new columns present
  expect_equal(names(sk$applied_registry), "fp_new")
})

# ---------------------------------------------------------------------------
# sync_randvars (divergence-point rewind and replay)
# ---------------------------------------------------------------------------

.mk_randvars_fn <- function(col_name, col_value = 1L) {
  force(col_name); force(col_value)
  function(skeleton, batch_data, config) {
    skeleton[, (col_name) := col_value]
    invisible(skeleton)
  }
}

test_that("sync_randvars applies all steps on a fresh skeleton", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)

  fns <- list(
    step_a = .mk_randvars_fn("rv_a"),
    step_b = .mk_randvars_fn("rv_b")
  )
  hashes <- c(step_a = "hash_a", step_b = "hash_b")

  sk$sync_randvars(fns, hashes, function() list(), config = NULL)

  expect_true(all(c("rv_a", "rv_b") %in% names(sk$data)))
  expect_equal(names(sk$randvars_state), c("step_a", "step_b"))
  expect_equal(sk$randvars_state$step_a$fn_hash, "hash_a")
  expect_equal(sk$randvars_state$step_a$added_columns, "rv_a")
})

test_that("sync_randvars is a no-op when nothing diverges", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)

  fns <- list(step_a = .mk_randvars_fn("rv_a"))
  hashes <- c(step_a = "hash_a")
  sk$sync_randvars(fns, hashes, function() list(), config = NULL)

  snapshot_before <- copy(names(sk$data))
  loader_calls <- 0L
  sk$sync_randvars(
    fns, hashes,
    function() { loader_calls <<- loader_calls + 1L; list() },
    config = NULL
  )

  expect_equal(names(sk$data), snapshot_before)
  expect_equal(loader_calls, 0L)
})

test_that("sync_randvars replays from divergence point on hash change", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)

  fns_v1 <- list(
    step_a = .mk_randvars_fn("rv_a", 1L),
    step_b = .mk_randvars_fn("rv_b", 1L),
    step_c = .mk_randvars_fn("rv_c", 1L)
  )
  sk$sync_randvars(
    fns_v1,
    c(step_a = "h_a", step_b = "h_b", step_c = "h_c"),
    function() list(), config = NULL
  )
  expect_equal(sk$data$rv_b[[1]], 1L)

  # Change step_b: new fn produces rv_b = 2L. step_c downstream should
  # cascade (divergence at step_b -> replay step_b and step_c).
  fns_v2 <- list(
    step_a = fns_v1$step_a,
    step_b = .mk_randvars_fn("rv_b", 2L),
    step_c = fns_v1$step_c
  )
  sk$sync_randvars(
    fns_v2,
    c(step_a = "h_a", step_b = "h_b_new", step_c = "h_c"),
    function() list(), config = NULL
  )

  expect_equal(sk$data$rv_b[[1]], 2L)
  # Step c should have been replayed too (cascade)
  expect_equal(sk$randvars_state$step_c$fn_hash, "h_c")
})

test_that("sync_randvars handles step removal (trailing)", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)

  fns <- list(
    step_a = .mk_randvars_fn("rv_a"),
    step_b = .mk_randvars_fn("rv_b")
  )
  sk$sync_randvars(fns, c(step_a = "h_a", step_b = "h_b"), function() list(), config = NULL)
  expect_true("rv_b" %in% names(sk$data))

  # Remove step_b
  sk$sync_randvars(
    list(step_a = fns$step_a),
    c(step_a = "h_a"),
    function() list(), config = NULL
  )

  expect_true("rv_a" %in% names(sk$data))
  expect_false("rv_b" %in% names(sk$data))
  expect_equal(names(sk$randvars_state), "step_a")
})

test_that("sync_randvars handles step removal (in the middle)", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)

  fns <- list(
    step_a = .mk_randvars_fn("rv_a"),
    step_b = .mk_randvars_fn("rv_b"),
    step_c = .mk_randvars_fn("rv_c")
  )
  sk$sync_randvars(
    fns,
    c(step_a = "h_a", step_b = "h_b", step_c = "h_c"),
    function() list(), config = NULL
  )

  # Remove step_b -> divergence at position 2. Drop old step_b and step_c
  # columns; replay new step at position 2 which is step_c.
  sk$sync_randvars(
    list(step_a = fns$step_a, step_c = fns$step_c),
    c(step_a = "h_a", step_c = "h_c"),
    function() list(), config = NULL
  )

  expect_true(all(c("rv_a", "rv_c") %in% names(sk$data)))
  expect_false("rv_b" %in% names(sk$data))
  expect_equal(names(sk$randvars_state), c("step_a", "step_c"))
})

test_that("sync_randvars captures filter-and-return patterns via the return value", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)

  # Mirrors the real-world pattern in skeleton_randvars_lisa where the
  # user does `skeleton <- skeleton[cond]` to drop rows that fail a
  # data-quality check. The filter rebinds the LOCAL `skeleton` inside
  # the fn, so `self$data` would stay pointing at the unfiltered version
  # unless sync_randvars captures the return value.
  filter_fn <- function(skeleton, batch_data, config) {
    skeleton <- skeleton[id >= 2L]
    skeleton[, flag := TRUE]
    invisible(skeleton)
  }

  sk$sync_randvars(
    list(step_filter = filter_fn),
    c(step_filter = "h_filter"),
    function() list(),
    config = NULL
  )

  # Filter dropped id == 1
  expect_equal(nrow(sk$data), 2L)
  expect_equal(sk$data$id, c(2L, 3L))
  # Column was added
  expect_true("flag" %in% names(sk$data))
  # And provenance recorded it
  expect_equal(sk$randvars_state$step_filter$added_columns, "flag")
})

test_that("sync_randvars handles step insertion", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)

  fns_v1 <- list(step_a = .mk_randvars_fn("rv_a"))
  sk$sync_randvars(fns_v1, c(step_a = "h_a"), function() list(), config = NULL)

  # Insert step_b between nothing and... well, at the end for simplicity
  fns_v2 <- list(
    step_a = fns_v1$step_a,
    step_b = .mk_randvars_fn("rv_b")
  )
  sk$sync_randvars(
    fns_v2, c(step_a = "h_a", step_b = "h_b"),
    function() list(), config = NULL
  )

  expect_true(all(c("rv_a", "rv_b") %in% names(sk$data)))
  expect_equal(names(sk$randvars_state), c("step_a", "step_b"))
})

# ---------------------------------------------------------------------------
# pipeline_hash semantics
# ---------------------------------------------------------------------------

test_that("pipeline_hash is stable across successive calls on the same state", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  sk$framework_fn_hash <- "fw_hash"
  h1 <- sk$pipeline_hash()
  h2 <- sk$pipeline_hash()
  expect_identical(h1, h2)
})

test_that("pipeline_hash changes when framework_fn_hash changes", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  sk$framework_fn_hash <- "fw_v1"
  h1 <- sk$pipeline_hash()
  sk$framework_fn_hash <- "fw_v2"
  h2 <- sk$pipeline_hash()
  expect_false(identical(h1, h2))
})

test_that("pipeline_hash changes when applied_registry changes", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  sk$framework_fn_hash <- "fw_hash"
  h1 <- sk$pipeline_hash()

  entry <- .fake_entry(codes = list(foo = "X"), groups = list(p = "grp_a"))
  sk$apply_code_entry(entry, .fake_batch_data(), id_col = "id", fingerprint = "fp1")
  h2 <- sk$pipeline_hash()
  expect_false(identical(h1, h2))
})

test_that("pipeline_hash changes when randvars_state changes", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  sk$framework_fn_hash <- "fw_hash"
  h1 <- sk$pipeline_hash()

  sk$sync_randvars(
    list(step_a = .mk_randvars_fn("rv_a")),
    c(step_a = "h_a"),
    function() list(), config = NULL
  )
  h2 <- sk$pipeline_hash()
  expect_false(identical(h1, h2))
})

# ---------------------------------------------------------------------------
# save / load round-trip via qs2
# ---------------------------------------------------------------------------

test_that("save + qs2::qs_read round-trips all phase provenance", {
  dir <- withr::local_tempdir()
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 7L)
  sk$framework_fn_hash <- "fw_hash"
  entry <- .fake_entry(codes = list(foo = "X"), groups = list(p = "grp_a"))
  sk$apply_code_entry(entry, .fake_batch_data(), id_col = "id", fingerprint = "fp1")
  sk$sync_randvars(
    list(step_a = .mk_randvars_fn("rv_a")),
    c(step_a = "h_a"),
    function() list(), config = NULL
  )

  path <- sk$save(dir)
  expect_true(file.exists(path))
  expect_equal(basename(path), "skeleton_007.qs2")

  loaded <- qs2::qs_read(path)
  expect_s3_class(loaded, "Skeleton")
  expect_equal(loaded$batch_number, 7L)
  expect_equal(loaded$framework_fn_hash, "fw_hash")
  expect_equal(names(loaded$applied_registry), "fp1")
  expect_equal(names(loaded$randvars_state), "step_a")
  expect_equal(loaded$randvars_state$step_a$added_columns, "rv_a")
  expect_identical(loaded$pipeline_hash(), sk$pipeline_hash())
})

# ---------------------------------------------------------------------------
# check_version
# ---------------------------------------------------------------------------

test_that("check_version succeeds on a current-schema skeleton", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  expect_true(sk$check_version())
})

test_that("check_version errors on a too-old schema", {
  sk <- Skeleton$new(data = .sk_dt(), batch_number = 1L)
  # Forcibly downgrade the schema version on this instance
  assign(".schema_version", 0L, envir = sk$.__enclos_env__$private)
  expect_error(sk$check_version(), "schema version")
})
