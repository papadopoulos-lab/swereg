# Row deletion belongs to the registered trim, and nowhere else.
#
# Phase 1b is the trim registered with `RegistryStudy$register_trim()`. It runs
# on a fresh base, after the framework and before the code registry. It is the
# one declared place in the pipeline that may delete skeleton rows.
#
# This file pins that exclusivity. A randvars step that changes the row count
# stops. A code entry that changes the row count stops. Each error names the
# step the user has to edit and points at `$register_trim()`.
#
# The two checks sit in different places, and each placement is the only one
# that holds the offending step's name:
#   - randvars: inside `Skeleton$sync_randvars()`, around each replayed
#     function (R/r6_skeleton.R). `new_nm` is in scope there.
#   - code entries: `validate_skeleton_after_add()` (R/validation_helpers.R),
#     called by `.apply_code_entry_impl()` on both code-application paths.
#     `reg$label` is in scope there.

library(data.table)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# 3 rows, one week, three persons. `skeleton_snapshot()` needs the four
# structural columns, so the fixture carries them.
.nrd_dt <- function() {
  data.table::data.table(
    id = c(1L, 2L, 3L),
    isoyear = 2020L,
    isoyearweek = "2020-01",
    is_isoyear = FALSE,
    personyears = 1 / 52.25
  )
}

# Drive one or more randvars steps through the real sync API.
.nrd_sync <- function(sk, fns) {
  sk$sync_randvars(
    randvars_fns = fns,
    randvars_hashes = stats::setNames(
      paste0("h_", names(fns)),
      names(fns)
    ),
    batch_data_loader = function() list(),
    config = NULL
  )
}

# Return the condition object rather than the message, so each assertion below
# reads one field of one captured error.
.nrd_catch <- function(expr) {
  tryCatch(expr, error = function(e) e)
}

# The message of a captured condition, or "" when nothing was raised. The
# empty string keeps every text assertion below reporting as its own failure
# when the guard is removed. Without it one assertion errors on a
# non-condition and hides the rest.
.nrd_msg <- function(err) {
  if (inherits(err, "condition")) conditionMessage(err) else ""
}

# A code function that marks its codes and then deletes rows from the CALLER's
# skeleton.
#
# data.table has no by-reference row delete, so `skeleton[<predicate>]` always
# builds a new table and leaves the caller's binding alone.
# `.apply_code_entry_impl()` discards what `fn` returns, so a returning filter
# never reaches the caller either. That case is pinned on its own below. The
# `assign()` here is the shape that does change the caller's row count, and it
# is what the production check has to catch.
.nrd_code_fn_delete <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := TRUE]
  }
  assign("skeleton", skeleton[id >= 2L], envir = parent.frame())
  invisible(skeleton)
}

# The same marking, with the filter returned instead of assigned.
.nrd_code_fn_return <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := TRUE]
  }
  skeleton[id >= 2L]
}

# The entry shape `$register_codes()` builds.
.nrd_entry <- function(fn, label = "mycode") {
  list(
    codes = list(my_code = "X"),
    fn = fn,
    fn_args = list(),
    groups = list(grp = "grp1"),
    combine_as = NULL,
    label = label
  )
}

.nrd_batch_data <- function() {
  list(grp1 = data.table::data.table(lopnr = 1:3, val = letters[1:3]))
}

# ---------------------------------------------------------------------------
# The randvars side
# ---------------------------------------------------------------------------

test_that("a randvars step that deletes rows stops and names the step", {
  sk <- Skeleton$new(data = .nrd_dt(), batch_number = 1L)

  # The shape both consumer projects use: rebind `skeleton` to a filtered
  # table and return it. `sync_randvars()` adopts the return value, so the
  # deletion reaches `self$data`.
  delete_fn <- function(skeleton, batch_data, config) {
    skeleton <- skeleton[id >= 2L]
    invisible(skeleton)
  }

  err <- .nrd_catch(.nrd_sync(sk, list(rv_delete = delete_fn)))
  expect_s3_class(err, "error")
  msg <- .nrd_msg(err)

  # The step, by name.
  expect_match(msg, '$register_randvars("rv_delete")', fixed = TRUE)
  # The counts, both sides.
  expect_match(msg, "before = 3, after = 2", fixed = TRUE)
  # The rule.
  expect_match(msg, "MUST NOT change the row count", fixed = TRUE)
  # The migration route, by function name.
  expect_match(msg, "study$register_trim(fn)", fixed = TRUE)
  expect_match(msg, "may delete", fixed = TRUE)

  # The message keys to no predicate literal. Two consumer projects spell the
  # same filter differently, so a quoted literal would cover one of them.
  expect_false(grepl("to_keep", msg, fixed = TRUE))

  # A step that did not complete records no provenance.
  expect_length(sk$randvars_state, 0L)
})

test_that("a randvars step that adds rows stops as well", {
  sk <- Skeleton$new(data = .nrd_dt(), batch_number = 1L)

  # The invariant is an unchanged row count, in both directions.
  add_fn <- function(skeleton, batch_data, config) {
    rbind(skeleton, skeleton[1L])
  }

  err <- .nrd_catch(.nrd_sync(sk, list(rv_add = add_fn)))
  expect_s3_class(err, "error")
  msg <- .nrd_msg(err)
  expect_match(msg, '$register_randvars("rv_add")', fixed = TRUE)
  expect_match(msg, "before = 3, after = 4", fixed = TRUE)
  expect_length(sk$randvars_state, 0L)
})

test_that("a randvars step that only adds a column still runs", {
  sk <- Skeleton$new(data = .nrd_dt(), batch_number = 1L)

  additive_fn <- function(skeleton, batch_data, config) {
    skeleton[, rv_flag := TRUE]
    invisible(skeleton)
  }

  expect_silent(.nrd_sync(sk, list(rv_additive = additive_fn)))
  expect_identical(nrow(sk$data), 3L)
  expect_true("rv_flag" %in% names(sk$data))
  expect_identical(
    sk$randvars_state$rv_additive$added_columns,
    "rv_flag"
  )
})

# ---------------------------------------------------------------------------
# The code-entry side
# ---------------------------------------------------------------------------

test_that("a code entry that deletes rows stops and names the entry", {
  sk <- Skeleton$new(data = .nrd_dt(), batch_number = 1L)

  err <- .nrd_catch(
    sk$apply_code_entry(
      .nrd_entry(.nrd_code_fn_delete, label = "deleting_entry"),
      .nrd_batch_data(),
      "lopnr",
      "fp1"
    )
  )
  expect_s3_class(err, "error")
  msg <- .nrd_msg(err)

  # Assert the row-deletion text. `expect_error()` on the call alone would
  # also pass on the "did not add the expected columns" error further down
  # `validate_skeleton_after_add()`. That would prove nothing about rows.
  expect_match(msg, "$register_codes(deleting_entry)", fixed = TRUE)
  expect_match(msg, "changed skeleton row count", fixed = TRUE)
  expect_match(msg, "before = 3, after = 2", fixed = TRUE)
  expect_match(msg, "study$register_trim(fn)", fixed = TRUE)
  expect_false(grepl("to_keep", msg, fixed = TRUE))
})

test_that("the public apply_codes_to_skeleton() path stops too", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "grp1"
  )
  study$register_codes(
    codes = list(my_code = "X"),
    fn = .nrd_code_fn_delete,
    groups = list(grp = "grp1"),
    label = "public_entry"
  )

  dt <- .nrd_dt()
  err <- .nrd_catch(study$apply_codes_to_skeleton(dt, .nrd_batch_data()))
  expect_s3_class(err, "error")
  msg <- .nrd_msg(err)
  expect_match(msg, "$register_codes(public_entry)", fixed = TRUE)
  expect_match(msg, "changed skeleton row count", fixed = TRUE)
  expect_match(msg, "study$register_trim(fn)", fixed = TRUE)
})

test_that("a code entry that RETURNS a filtered table is ignored, not adopted", {
  # A known gap, pinned so it stays on the record. The code path discards
  # what `fn` returns, so this filter never reaches the caller's skeleton and
  # the row-count check has nothing to see. The randvars path adopts a
  # returned table and therefore does catch the same shape.
  sk <- Skeleton$new(data = .nrd_dt(), batch_number = 1L)

  expect_silent(
    sk$apply_code_entry(
      .nrd_entry(.nrd_code_fn_return, label = "returning_entry"),
      .nrd_batch_data(),
      "lopnr",
      "fp1"
    )
  )
  expect_identical(nrow(sk$data), 3L)
  # The `grp` group prefix makes the written column `grp_my_code`.
  expect_true("grp_my_code" %in% names(sk$data))
})

# ---------------------------------------------------------------------------
# The trim keeps its licence to delete
# ---------------------------------------------------------------------------

test_that("a trim that deletes rows runs without stopping", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "grp1",
    batch_size = 3L
  )
  study$set_ids(1:3)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:3, val = letters[1:3])
  )

  # 3 persons x 4 weeks = 12 rows.
  study$register_framework(function(batch_data, config) {
    ids <- batch_data[["grp1"]]$lopnr
    d <- data.table::CJ(
      id = ids,
      isoyearweek = c("2020-01", "2020-02", "2020-03", "2020-04")
    )
    d[, `:=`(isoyear = 2020L, is_isoyear = FALSE)]
    d[]
  })

  # Deletes one week: 3 rows of 12, leaving 9.
  study$register_trim(function(skeleton, batch_data, config) {
    skeleton[isoyearweek != "2020-04"]
  })

  study$register_randvars(
    "rv_additive",
    function(skeleton, batch_data, config) {
      skeleton[, rv_flag := TRUE]
      invisible(skeleton)
    }
  )

  study$process_skeletons()

  sk <- study$load_skeleton(1L)
  # The trim really did delete rows, so the pass above is not vacuous.
  expect_identical(nrow(sk$data), 9L)
  expect_false("2020-04" %in% sk$data$isoyearweek)
  expect_true(all(sk$data$rv_flag))
})

# ---------------------------------------------------------------------------
# Delete-then-add
# ---------------------------------------------------------------------------

test_that("a delete and an add that net to zero inside ONE step are not caught", {
  # The check compares the row COUNT before and after each step. A step that
  # deletes one row and appends another leaves the count unchanged, so it
  # passes. Catching it would need row identity, not a count.
  sk <- Skeleton$new(data = .nrd_dt(), batch_number = 1L)

  net_zero_fn <- function(skeleton, batch_data, config) {
    kept <- skeleton[id >= 2L]
    rbind(kept, kept[.N])
  }

  expect_silent(.nrd_sync(sk, list(rv_net_zero = net_zero_fn)))
  expect_identical(nrow(sk$data), 3L)
  # id 1 is gone and id 3 is duplicated. The row count hid both.
  expect_identical(sk$data$id, c(2L, 3L, 3L))
  expect_identical(names(sk$randvars_state), "rv_net_zero")
})

test_that("a delete in one step and an add in a later step stops at the delete", {
  # The check is per step, so a net-zero pair SPLIT across two steps is
  # caught. The second step never runs.
  sk <- Skeleton$new(data = .nrd_dt(), batch_number = 1L)
  second_step_ran <- FALSE

  delete_fn <- function(skeleton, batch_data, config) {
    skeleton[id >= 2L]
  }
  add_fn <- function(skeleton, batch_data, config) {
    second_step_ran <<- TRUE
    rbind(skeleton, skeleton[1L])
  }

  err <- .nrd_catch(
    .nrd_sync(sk, list(rv_delete = delete_fn, rv_add = add_fn))
  )
  expect_s3_class(err, "error")
  expect_match(
    .nrd_msg(err),
    '$register_randvars("rv_delete")',
    fixed = TRUE
  )
  expect_false(second_step_ran)
  expect_length(sk$randvars_state, 0L)
})
