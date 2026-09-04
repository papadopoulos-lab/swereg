# A data.table gains a column by reference only while its column-pointer
# vector has a free slot. Past the last free slot data.table allocates a
# longer vector, which is a NEW R object. The old object never gets the
# column, and data.table 1.18.4 reports nothing.
#
# The code registry drives every `add_*` function through `do.call()` with a
# VALUE, so `substitute()` inside the function yields the table itself. There
# is no binding to write a grown table back to. The return value is therefore
# the only route the new columns have, and `Skeleton$apply_code_entry()` MUST
# assign it.
#
# Every test here gives the skeleton 5 free column slots and asks one
# registry entry for 6 columns. It then drives the real path:
# `Skeleton$new(...)$apply_code_entry(...)`.
#
# The shape that made this visible: a skeleton of ten columns, a registry that
# writes more columns than the column-pointer vector has slots, and the slots
# running out part way through the registry.

skip_if_not_installed("data.table")

library(data.table)

# A data.table with exactly `spare` free column slots. Serialization drops
# data.table's over-allocation, which is what a qs2 file on disk does to a
# table, and `setalloccol()` then sets the exact number back. `setalloccol()`
# never shrinks, so it cannot produce this on its own.
.dar_headroom <- function(dt, spare) {
  dt <- unserialize(serialize(dt, NULL))
  return(data.table::setalloccol(dt, spare))
}

# Three people over one quarter, with `spare` free column slots.
.dar_skeleton <- function(spare = 5L) {
  return(.dar_headroom(
    swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
    spare
  ))
}

# Six column names, one more than the five free slots every skeleton here
# starts with.
.dar_cols <- function(n = 6L) {
  return(sprintf("c%02d", seq_len(n)))
}

.dar_codes <- function(pattern, n = 6L) {
  return(stats::setNames(rep(list(pattern), n), .dar_cols(n)))
}

# Apply one registry entry through the real R6 path and return the Skeleton.
.dar_apply <- function(entry, batch_data, spare = 5L, skeleton = NULL) {
  dt <- if (is.null(skeleton)) .dar_skeleton(spare) else skeleton
  testthat::expect_identical(data.table::truelength(dt) - ncol(dt), spare)
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)
  sk$apply_code_entry(entry, batch_data, "lopnr", "fp_test")
  return(sk)
}

# --- rawbatch stand-ins -----------------------------------------------------

.dar_npr <- function() {
  return(list(
    npr = data.table::data.table(
      lopnr = 1:3,
      indatum = as.Date(rep("2021-03-01", 3)),
      hdia = c("F320", "F320", "I600"),
      op1 = c("HAC10", "HAC10", "ZZZ99"),
      icdo10 = c("C509", "C509", "D000")
    )
  ))
}

.dar_dors <- function() {
  return(list(
    dors = data.table::data.table(
      lopnr = 1:3,
      dodsdat = as.Date(rep("2021-03-01", 3)),
      ulorsak = c("I219", "I219", "J189")
    )
  ))
}

.dar_lmed <- function() {
  return(list(
    lmed = data.table::data.table(
      lopnr = 1:3,
      edatum = as.Date(rep("2021-02-01", 3)),
      atc = c("C10AA01", "C10AA01", "N06AB03"),
      fddd = rep(30, 3)
    )
  ))
}

.dar_stroke <- function() {
  return(list(
    stroke = data.table::data.table(
      lopnr = 1:3,
      eventdate = as.Date(rep("2021-03-01", 3))
    )
  ))
}

.dar_batch_data <- function() {
  return(c(.dar_npr(), .dar_dors(), .dar_lmed(), .dar_stroke()))
}

# --- the six built-ins the code registry can dispatch -----------------------
#
# `.apply_code_entry_impl()` calls `fn(skeleton, data, id_name =, codes =)`
# plus `fn_args`. Six of the eight exported `add_*` functions take `codes`,
# so those six are the set. `add_onetime()` and `add_annual()` take no
# `codes` argument, and one test below holds that claim.
#
# Each entry carries its own group prefix, so the six write 36 distinct
# columns and can run as one registry.

.dar_builtin_entries <- function() {
  return(list(
    add_diagnoses = list(
      prefix = "dx",
      entry = list(
        codes = .dar_codes("F32"),
        fn = swereg::add_diagnoses,
        fn_args = list(),
        groups = list(dx = "npr"),
        combine_as = NULL,
        label = "add_diagnoses"
      ),
      hit = 1L,
      miss = 3L
    ),
    add_operations = list(
      prefix = "op",
      entry = list(
        codes = .dar_codes("HAC10"),
        fn = swereg::add_operations,
        fn_args = list(),
        groups = list(op = "npr"),
        combine_as = NULL,
        label = "add_operations"
      ),
      hit = 1L,
      miss = 3L
    ),
    add_cancer_without_morphology = list(
      prefix = "can",
      entry = list(
        codes = .dar_codes("C50"),
        fn = swereg::add_cancer_without_morphology,
        fn_args = list(),
        groups = list(can = "npr"),
        combine_as = NULL,
        label = "add_cancer_without_morphology"
      ),
      hit = 1L,
      miss = 3L
    ),
    add_cods = list(
      prefix = "dorsu",
      entry = list(
        codes = .dar_codes("I21"),
        fn = swereg::add_cods,
        fn_args = list(cod_type = "underlying"),
        groups = list(dorsu = "dors"),
        combine_as = NULL,
        label = "add_cods"
      ),
      hit = 1L,
      miss = 3L
    ),
    add_rx = list(
      prefix = "rx",
      entry = list(
        codes = .dar_codes("C10A"),
        fn = swereg::add_rx,
        fn_args = list(source = "atc"),
        groups = list(rx = "lmed"),
        combine_as = NULL,
        label = "add_rx"
      ),
      hit = 1L,
      miss = 3L
    ),
    add_quality_registry = list(
      prefix = "qr",
      entry = list(
        codes = stats::setNames(rep(list(TRUE), 6L), .dar_cols()),
        fn = swereg::add_quality_registry,
        fn_args = list(date_col = "eventdate"),
        groups = list(qr = "stroke"),
        combine_as = NULL,
        label = "add_quality_registry"
      ),
      hit = 1L,
      miss = NA_integer_
    )
  ))
}

test_that("every registry-dispatchable add_* lands its columns out of slots", {
  cases <- .dar_builtin_entries()
  batch_data <- .dar_batch_data()
  for (nm in names(cases)) {
    case <- cases[[nm]]
    want <- paste0(case$prefix, "_", .dar_cols())
    dt <- .dar_skeleton(5L)
    nrow_before <- nrow(dt)

    sk <- .dar_apply(case$entry, batch_data, skeleton = dt)

    expect_true(all(want %in% names(sk$data)), info = nm)
    expect_identical(nrow(sk$data), nrow_before, info = nm)
    last <- want[[6L]]
    expect_true(any(sk$data[id == case$hit][[last]]), info = nm)
    if (!is.na(case$miss)) {
      expect_false(any(sk$data[id == case$miss][[last]]), info = nm)
    }
  }
})

test_that("a two-group entry with combine_as lands every column", {
  # 6 code names x (2 groups + 1 combine) = 18 columns from 5 free slots.
  entry <- list(
    codes = .dar_codes("F32"),
    fn = swereg::add_diagnoses,
    fn_args = list(),
    groups = list(ov = "npr", sv = "npr2"),
    combine_as = "os",
    label = "add_diagnoses"
  )
  batch_data <- .dar_npr()
  batch_data$npr2 <- data.table::copy(batch_data$npr)

  sk <- .dar_apply(entry, batch_data)

  expect_true(all(paste0("ov_", .dar_cols()) %in% names(sk$data)))
  expect_true(all(paste0("sv_", .dar_cols()) %in% names(sk$data)))
  expect_true(all(paste0("os_", .dar_cols()) %in% names(sk$data)))
  expect_true(any(sk$data[id == 1L]$os_c06))
  expect_false(any(sk$data[id == 3L]$os_c06))
})

test_that("a custom fn that writes with := lands its columns out of slots", {
  # The fn takes the swereg `add_*` shape and mutates by reference. It
  # returns NULL, so the applier's up-front growth is what saves it.
  fn <- function(skeleton, dataset, id_name, codes) {
    for (nm in names(codes)) {
      skeleton[, (nm) := id %in% dataset[[id_name]]]
    }
    return(invisible(NULL))
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list("custom"),
    combine_as = NULL,
    label = "custom_fn"
  )
  batch_data <- list(custom = data.table::data.table(lopnr = c(1L, 2L)))

  sk <- .dar_apply(entry, batch_data)

  expect_true(all(.dar_cols() %in% names(sk$data)))
  expect_true(all(sk$data[id == 1L]$c06))
  expect_false(any(sk$data[id == 3L]$c06))
})

test_that("a custom fn that returns the skeleton lands its columns", {
  fn <- function(skeleton, dataset, id_name, codes) {
    for (nm in names(codes)) {
      skeleton[, (nm) := TRUE]
    }
    return(invisible(skeleton))
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list("custom"),
    combine_as = NULL,
    label = "custom_fn_returning"
  )

  sk <- .dar_apply(entry, list(custom = data.table::data.table(lopnr = 1L)))

  expect_true(all(.dar_cols() %in% names(sk$data)))
  expect_true(all(sk$data$c06))
})

test_that("a derived entry lands its columns out of slots", {
  # Before 26.10.14 this fixture kept the columns the five free slots held
  # and lost the sixth, and it returned normally either way.
  # `applied_registry` then recorded the entry as applied, so nothing later
  # in the pipeline re-ran it. A derived entry on a table with NO free slot
  # produced zero columns.
  dt <- swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31")
  for (nm in .dar_cols()) {
    dt[, (paste0("os_", nm)) := id == 1L]
    dt[, (paste0("dorsu_", nm)) := id == 2L]
  }
  entry <- list(
    kind = "derived",
    codes = stats::setNames(as.list(rep("ignored", 6L)), .dar_cols()),
    from = c("os", "dorsu"),
    as = "osd",
    label = "derived: osd_* = os_* | dorsu_*"
  )

  sk <- .dar_apply(entry, list(), skeleton = .dar_headroom(dt, 5L))

  expect_true(all(paste0("osd_", .dar_cols()) %in% names(sk$data)))
  expect_true(all(sk$data[id == 1L]$osd_c06))
  expect_true(all(sk$data[id == 2L]$osd_c06))
  expect_false(any(sk$data[id == 3L]$osd_c06))
})

test_that("an entry may recompute a column it declares", {
  # A second application of the same entry writes a fresh vector into the
  # entry's own output column. The column-address check MUST allow that,
  # and MUST still refuse a new vector in any other pre-existing column.
  dt <- swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31")
  dt[, os_c01 := id == 1L]
  dt[, prior := TRUE]
  entry <- list(
    kind = "derived",
    codes = list(c01 = "ignored"),
    from = "os",
    as = "osd",
    label = "osd_from_os"
  )
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  sk$apply_code_entry(entry, list(), "lopnr", "fp_first")
  expect_true("osd_c01" %in% names(sk$data))
  first_addr <- data.table::address(sk$data$osd_c01)

  expect_silent(sk$apply_code_entry(entry, list(), "lopnr", "fp_second"))
  expect_false(identical(data.table::address(sk$data$osd_c01), first_addr))
  expect_identical(sk$data$osd_c01, sk$data$id == 1L)
  expect_true(all(sk$data$prior))
})

test_that("a derived entry still names its missing source columns", {
  dt <- swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31")
  dt[, os_c01 := TRUE]
  entry <- list(
    kind = "derived",
    codes = list(c01 = "ignored"),
    from = c("os", "dorsu"),
    as = "osd",
    label = "osd_missing_source"
  )
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  expect_error(
    sk$apply_code_entry(entry, list(), "lopnr", "fp_missing"),
    "source columns missing"
  )
})

test_that("the applier returns the grown table, and the R6 field holds it", {
  entry <- .dar_builtin_entries()$add_diagnoses$entry
  batch_data <- .dar_npr()
  dt <- .dar_skeleton(5L)
  before <- data.table::address(dt)

  got <- swereg:::.apply_code_entry_impl(dt, batch_data, entry, "lopnr")

  # The growth produced a new object, and the columns are on that one.
  expect_false(identical(data.table::address(got), before))
  expect_true(all(paste0("dx_", .dar_cols()) %in% names(got)))
  expect_false("dx_c06" %in% names(dt))

  # The R6 holder assigns it, so `$data` is the object that carries them.
  sk <- swereg:::Skeleton$new(data = .dar_skeleton(5L), batch_number = 1L)
  sk$apply_code_entry(entry, batch_data, "lopnr", "fp_test")
  expect_true(all(paste0("dx_", .dar_cols()) %in% names(sk$data)))
})

test_that("a skeleton with the slots is written in place", {
  # The growth is not the normal path. With slots to spare the applier must
  # leave the caller's object alone, which is what keeps the pipeline cheap.
  entry <- .dar_builtin_entries()$add_diagnoses$entry
  dt <- .dar_skeleton(200L)
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  sk$apply_code_entry(entry, .dar_npr(), "lopnr", "fp_test")

  expect_identical(data.table::address(sk$data), data.table::address(dt))
  expect_true(all(paste0("dx_", .dar_cols()) %in% names(dt)))
})

test_that("a whole registry applies to a skeleton with 5 free slots", {
  # Seven entries in sequence. The slots run out inside the first entry, so
  # every later entry works on a table the first one grew.
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = tempfile("rawbatch_"),
    group_names = c("npr", "dors", "lmed", "stroke")
  )
  cases <- .dar_builtin_entries()
  for (case in cases) {
    study$code_registry[[length(study$code_registry) + 1L]] <- case$entry
  }
  study$register_derived_codes(
    codes = stats::setNames(as.list(rep("ignored", 6L)), .dar_cols()),
    from = c("dx", "op"),
    as = "dxop"
  )
  batch_data <- .dar_batch_data()

  sk <- swereg:::Skeleton$new(data = .dar_skeleton(5L), batch_number = 1L)
  fps <- study$code_registry_fingerprints()
  for (i in seq_along(study$code_registry)) {
    sk$apply_code_entry(study$code_registry[[i]], batch_data, "lopnr", fps[[i]])
  }

  for (case in cases) {
    expect_true(
      all(paste0(case$prefix, "_", .dar_cols()) %in% names(sk$data)),
      info = case$prefix
    )
  }
  expect_true(all(paste0("dxop_", .dar_cols()) %in% names(sk$data)))
  expect_length(sk$applied_registry, 7L)
})

test_that("apply_codes_to_skeleton lands every column out of slots", {
  # The public "apply everything at once" path. It reserves the slots the
  # whole registry needs and writes the grown table back to `dt`, which is a
  # name. The return carries the same table.
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = tempfile("rawbatch_"),
    group_names = c("npr", "dors", "lmed", "stroke")
  )
  cases <- .dar_builtin_entries()
  for (case in cases) {
    study$code_registry[[length(study$code_registry) + 1L]] <- case$entry
  }

  dt <- .dar_skeleton(5L)
  expect_identical(data.table::truelength(dt) - ncol(dt), 5L)

  got <- study$apply_codes_to_skeleton(dt, .dar_batch_data())

  for (case in cases) {
    want <- paste0(case$prefix, "_", .dar_cols())
    expect_true(all(want %in% names(got)), info = case$prefix)
    expect_true(all(want %in% names(dt)), info = case$prefix)
  }
})

test_that("add_onetime and add_annual take no codes argument", {
  # The claim the built-in list above rests on. The code registry always
  # passes `codes =`, so a function without that formal cannot be registered.
  expect_false("codes" %in% names(formals(swereg::add_onetime)))
  expect_false("codes" %in% names(formals(swereg::add_annual)))
  for (fn in list(
    swereg::add_diagnoses,
    swereg::add_operations,
    swereg::add_cods,
    swereg::add_cancer_without_morphology,
    swereg::add_rx,
    swereg::add_quality_registry
  )) {
    expect_true("codes" %in% names(formals(fn)))
  }
})

# --- the entry contract: in place, whole, and complete -----------------------

test_that("a group with no rawbatch row still lands its columns", {
  # `Filter()` drops a zero-row table, so `fn` never runs for that group.
  # Before 26.10.14 the entry was recorded as applied with its columns
  # absent, which is silent data loss on any batch a source has no record
  # for.
  # The applier passes the zero-row table to `fn`, so the value on the
  # skeleton is the one `fn` writes. It does not invent a `FALSE`.
  fn <- function(skeleton, dataset, id_name, codes) {
    # The zero-row table the group really holds, with its columns. A skipped
    # group would hand `fn` a table with no columns at all.
    stopifnot(
      data.table::is.data.table(dataset),
      nrow(dataset) == 0L,
      identical(names(dataset), c("lopnr", "extra"))
    )
    for (nm in names(codes)) skeleton[, (nm) := NA_integer_]
    return(invisible(NULL))
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list(grp = "g"),
    combine_as = NULL,
    label = "empty_group"
  )
  want <- paste0("grp_", .dar_cols())

  sk <- .dar_apply(entry, list(
    g = data.table::data.table(lopnr = integer(), extra = character())
  ))

  expect_true(all(want %in% names(sk$data)))
  expect_true(is.integer(sk$data[[want[[1L]]]]))
  expect_true(all(is.na(sk$data[[want[[1L]]]])))
  expect_length(sk$applied_registry, 1L)
})

test_that("a group absent from batch_data stops and names the group", {
  # `save_rawbatch()` writes a file for every group and every batch, and a
  # batch with no matching person gets a zero-row slice. `load_rawbatch()`
  # stops on a missing file. An absent NAME is therefore a group the study
  # never declared, and skipping it would record the entry as applied over
  # data it never read.
  fn <- function(skeleton, dataset, id_name, codes) {
    for (nm in names(codes)) skeleton[, (nm) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list(grp = "absent"),
    combine_as = NULL,
    label = "missing_group"
  )
  sk <- swereg:::Skeleton$new(data = .dar_skeleton(5L), batch_number = 1L)

  expect_error(
    sk$apply_code_entry(entry, list(), "lopnr", "fp_absent"),
    "names a rawbatch group that is not in this batch: absent"
  )
  expect_length(sk$applied_registry, 0L)
})

test_that("an empty combine_as group still lands its combined columns", {
  fn <- function(skeleton, dataset, id_name, codes) {
    stopifnot(nrow(dataset) == 0L, identical(names(dataset), "lopnr"))
    for (nm in names(codes)) skeleton[, (nm) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list(grp = "g"),
    combine_as = "os",
    label = "empty_combine"
  )

  sk <- .dar_apply(entry, list(g = data.table::data.table(lopnr = integer())))

  expect_true(all(paste0("grp_", .dar_cols()) %in% names(sk$data)))
  expect_true(all(paste0("os_", .dar_cols()) %in% names(sk$data)))
})

test_that("an entry that finishes without a declared column stops", {
  # The last gate, driven directly. Every branch above it now writes or
  # initialises its columns, so no registry entry reaches this error through
  # the public path today. It stays because `Skeleton$apply_code_entry()`
  # records the entry straight after the applier returns: a missing column
  # here becomes an entry recorded as applied and never re-run.
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = identity,
    fn_args = list(),
    groups = list(grp = "g"),
    combine_as = NULL,
    label = "declared_but_absent"
  )
  dt <- .dar_skeleton(200L)

  expect_error(
    swereg:::.assert_entry_columns_present(dt, entry),
    "finished without the columns it declares"
  )
  expect_error(
    swereg:::.assert_entry_columns_present(dt, entry),
    "grp_c01"
  )

  # With the columns present it returns and raises nothing.
  dt[, (paste0("grp_", .dar_cols())) := FALSE]
  expect_silent(swereg:::.assert_entry_columns_present(dt, entry))
})

test_that("a returned projection is ignored, and the entry then fails", {
  # A `.()` projection builds new column vectors, so it is not the skeleton
  # and the applier ignores it. The function wrote its column onto the
  # projection alone, so the skeleton does not have it.
  fn <- function(skeleton, dataset, id_name, codes) {
    out <- skeleton[, .(id, isoyear, isoyearweek, is_isoyear)]
    out[, (names(codes)) := TRUE]
    return(out)
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "projection"
  )
  dt <- .dar_skeleton(200L)
  dt[, prior := TRUE]
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  expect_error(
    sk$apply_code_entry(
      entry,
      list(g = data.table::data.table(lopnr = 1:3)),
      "lopnr",
      "fp_proj"
    ),
    "did not add the expected columns"
  )
  expect_true("prior" %in% names(sk$data))
  expect_false(any(.dar_cols() %in% names(sk$data)))
  expect_length(sk$applied_registry, 0L)
})

test_that("a returned row swap is ignored, and the skeleton is intact", {
  # The function writes its columns to the skeleton, then returns a table
  # whose ids are 2, 3, 3. `rbind()` builds new vectors, so the applier
  # ignores the return. The entry succeeds on what the function actually
  # wrote, which is what swereg did before 26.10.14.
  fn <- function(skeleton, dataset, id_name, codes) {
    skeleton[, (names(codes)) := TRUE]
    return(rbind(skeleton[id != 1L], skeleton[id == 3L]))
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "rowswap"
  )
  dt <- .dar_skeleton(200L)
  nrow_before <- nrow(dt)
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  sk$apply_code_entry(
    entry,
    list(g = data.table::data.table(lopnr = 1:3)),
    "lopnr",
    "fp_swap"
  )

  expect_identical(sort(unique(sk$data$id)), 1:3)
  expect_identical(nrow(sk$data), nrow_before)
  expect_true(all(.dar_cols() %in% names(sk$data)))
  expect_length(sk$applied_registry, 1L)
})

test_that("a returned foreign table with the right names is ignored", {
  # The names match, the vectors do not. The applier ignores it, and the
  # expected-column check reports what the skeleton is missing.
  fn <- function(skeleton, dataset, id_name, codes) {
    out <- data.table::copy(skeleton)
    out[, (names(codes)) := TRUE]
    return(out)
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "foreign"
  )
  sk <- swereg:::Skeleton$new(data = .dar_skeleton(200L), batch_number = 1L)

  expect_error(
    sk$apply_code_entry(
      entry,
      list(g = data.table::data.table(lopnr = 1:3)),
      "lopnr",
      "fp_foreign"
    ),
    "did not add the expected columns"
  )
  expect_false(any(.dar_cols() %in% names(sk$data)))
})

test_that("an incidental data.table return is ignored", {
  # An `add_*` function often ends on a statement that returns something
  # incidental. `dataset[, scratch := NULL]` returns `dataset`, and the
  # batched pipeline permits that mutation. swereg ignored such a return
  # before 26.10.14, and it does again.
  fn <- function(skeleton, dataset, id_name, codes) {
    skeleton[, (names(codes)) := FALSE]
    dataset[, scratch := NULL]
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "incidental"
  )
  sk <- swereg:::Skeleton$new(data = .dar_skeleton(200L), batch_number = 1L)

  sk$apply_code_entry(
    entry,
    list(g = data.table::data.table(lopnr = 1:3, scratch = 1:3)),
    "lopnr",
    "fp_incidental"
  )

  expect_true(all(.dar_cols() %in% names(sk$data)))
  expect_false(any(sk$data[[.dar_cols()[[1L]]]]))
  expect_length(sk$applied_registry, 1L)
})

test_that("row identity is checked, not only the row count", {
  # Drive the validator directly with a table whose row count is unchanged
  # and whose rows are not. The applier refuses a replacement before the
  # validator sees one, so this is the only route to that branch.
  before <- .dar_skeleton(200L)
  snap <- swereg:::skeleton_snapshot(before)
  after <- rbind(before[id != 1L], before[id == 3L])

  expect_identical(nrow(after), nrow(before))
  expect_error(
    swereg:::validate_skeleton_after_add(after, snap, context = "$test()"),
    "changed the skeleton's `id` column"
  )
})

test_that("losing a pre-existing column is checked", {
  before <- .dar_skeleton(200L)
  before[, prior := TRUE]
  snap <- swereg:::skeleton_snapshot(before)
  after <- data.table::copy(before)
  after[, prior := NULL]

  expect_error(
    swereg:::validate_skeleton_after_add(after, snap, context = "$test()"),
    "dropped columns that were already on the skeleton"
  )
})

test_that("the applier passes the skeleton to fn as a NAME", {
  # `substitute(skeleton)` inside a built-in feeds `.ensure_dt_alloc()`. A
  # name is a binding it can write a grown table back to; a value is not.
  # `do.call()` with the table itself gives it the value, so the built-in
  # would return a grown table the applier could not tell from a
  # replacement.
  seen <- NULL
  fn <- function(skeleton, dataset, id_name, codes) {
    seen <<- substitute(skeleton)
    for (nm in names(codes)) skeleton[, (nm) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = .dar_codes("ignored"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "name_probe"
  )

  .dar_apply(entry, list(g = data.table::data.table(lopnr = 1:3)))

  expect_true(is.name(seen))
  expect_identical(as.character(seen), "skeleton")
})

test_that("a built-in writes in place on a skeleton with no free slot", {
  # The reservation plus `.DT_ALLOC_ENTRY_MARGIN` closes the gap before the
  # built-in runs, so it never reaches its own allocator and never warns.
  entry <- .dar_builtin_entries()$add_diagnoses$entry
  dt <- .dar_headroom(
    swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
    0L
  )
  expect_identical(data.table::truelength(dt) - ncol(dt), 0L)
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  expect_silent(sk$apply_code_entry(entry, .dar_npr(), "lopnr", "fp_grow"))
  expect_true(all(paste0("dx_", .dar_cols()) %in% names(sk$data)))
  expect_gte(
    data.table::truelength(sk$data) - ncol(sk$data),
    swereg:::.DT_ALLOC_ENTRY_MARGIN
  )
})

test_that("an assign-only replacement stops, and its values do not land", {
  # The function copies the skeleton, changes a column it does not own, adds
  # its own, rebinds the applier's binding, and returns NULL. The header
  # address moves exactly as a legitimate growth moves it. Only the COLUMN
  # addresses tell the two apart.
  fn <- function(skeleton, dataset, id_name, codes) {
    out <- data.table::copy(skeleton)
    out[, prior := 999L]
    out[, (names(codes)) := TRUE]
    assign("skeleton", out, envir = parent.frame())
    return(invisible(NULL))
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "assign_replace"
  )
  dt <- .dar_skeleton(200L)
  dt[, prior := 1L]
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  expect_error(
    sk$apply_code_entry(
      entry,
      list(g = data.table::data.table(lopnr = 1:3)),
      "lopnr",
      "fp_assign"
    ),
    "replaced the skeleton instead of adding to it"
  )
  expect_identical(unique(sk$data$prior), 1L)
  expect_false("flag" %in% names(sk$data))
  expect_length(sk$applied_registry, 0L)
})

test_that("a fn that outgrows the reservation is accepted, not refused", {
  # 65 scratch columns past a 65-slot table. data.table reallocates inside
  # `fn`, so the header address moves while every column vector stays. That
  # is a growth, and it MUST be accepted whether it arrives by rebind or by
  # return. The margin is a performance choice, not a correctness threshold.
  fn <- function(skeleton, dataset, id_name, codes) {
    for (i in 1:65) skeleton[, (sprintf("aux%02d", i)) := i]
    skeleton[, (names(codes)) := TRUE]
    return(invisible(skeleton))
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "overflow"
  )
  dt <- .dar_headroom(
    swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
    65L
  )
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  sk$apply_code_entry(
    entry,
    list(g = data.table::data.table(lopnr = 1:3)),
    "lopnr",
    "fp_overflow"
  )

  expect_true("flag" %in% names(sk$data))
  expect_true(all(sk$data$flag))
  expect_true("aux65" %in% names(sk$data))
  expect_length(sk$applied_registry, 1L)
})

test_that("a fn that outgrows the reservation and returns NULL stops", {
  # The same overflow, with the grown table dropped on the floor. The
  # applier's binding keeps the old header, which does not carry the
  # expected column, and the per-group check names it.
  fn <- function(skeleton, dataset, id_name, codes) {
    for (i in 1:65) skeleton[, (sprintf("aux%02d", i)) := i]
    skeleton[, (names(codes)) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "overflow_null"
  )
  dt <- .dar_headroom(
    swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
    65L
  )
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  expect_error(
    sk$apply_code_entry(
      entry,
      list(g = data.table::data.table(lopnr = 1:3)),
      "lopnr",
      "fp_overflow_null"
    ),
    "did not add the expected columns"
  )
})

test_that("a subset write into isoyear stops", {
  # A `:=` that keeps the column type writes into the existing vector. The
  # row count does not move and the column address does not move, so only a
  # value comparison sees it.
  # `isoyear - 1L`, not a literal: `create_skeleton()` puts the earliest
  # annual row first, and its `isoyear` is already 1900.
  fn <- function(skeleton, dataset, id_name, codes) {
    skeleton[1L, isoyear := isoyear - 1L]
    skeleton[, (names(codes)) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "subset_isoyear"
  )
  sk <- swereg:::Skeleton$new(data = .dar_skeleton(200L), batch_number = 1L)

  expect_error(
    sk$apply_code_entry(
      entry,
      list(g = data.table::data.table(lopnr = 1:3)),
      "lopnr",
      "fp_isoyear"
    ),
    "changed the skeleton's `isoyear` column"
  )
  expect_length(sk$applied_registry, 0L)
})

test_that("a write into is_isoyear stops", {
  fn <- function(skeleton, dataset, id_name, codes) {
    skeleton[, is_isoyear := !is_isoyear]
    skeleton[, (names(codes)) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "flip_is_isoyear"
  )
  sk <- swereg:::Skeleton$new(data = .dar_skeleton(200L), batch_number = 1L)

  expect_error(
    sk$apply_code_entry(
      entry,
      list(g = data.table::data.table(lopnr = 1:3)),
      "lopnr",
      "fp_isisoyear"
    ),
    "changed the skeleton's `is_isoyear` column"
  )
})

test_that("every dispatchable built-in handles a zero-row dataset", {
  # `save_rawbatch()` writes a zero-row slice for a batch with no matching
  # person, so this is the shape a built-in meets in production. Each one
  # MUST write every requested column, with its own no-match value.
  cols <- .dar_cols(3L)
  cases <- list(
    add_diagnoses = list(
      fn = swereg::add_diagnoses,
      data = data.table::data.table(
        lopnr = integer(), indatum = as.Date(character()), hdia = character()
      ),
      codes = stats::setNames(rep(list("F32"), 3L), cols), args = list()
    ),
    add_operations = list(
      fn = swereg::add_operations,
      data = data.table::data.table(
        lopnr = integer(), indatum = as.Date(character()), op1 = character()
      ),
      codes = stats::setNames(rep(list("HAC10"), 3L), cols), args = list()
    ),
    add_cods = list(
      fn = swereg::add_cods,
      data = data.table::data.table(
        lopnr = integer(), dodsdat = as.Date(character()),
        ulorsak = character()
      ),
      codes = stats::setNames(rep(list("I21"), 3L), cols),
      args = list(cod_type = "underlying")
    ),
    add_cancer_without_morphology = list(
      fn = swereg::add_cancer_without_morphology,
      data = data.table::data.table(
        lopnr = integer(), indatum = as.Date(character()),
        icdo10 = character()
      ),
      codes = stats::setNames(rep(list("C50"), 3L), cols), args = list()
    ),
    add_rx = list(
      fn = swereg::add_rx,
      data = data.table::data.table(
        lopnr = integer(), edatum = as.Date(character()),
        atc = character(), fddd = numeric()
      ),
      codes = stats::setNames(rep(list("C10A"), 3L), cols),
      args = list(source = "atc")
    ),
    add_quality_registry = list(
      fn = swereg::add_quality_registry,
      data = data.table::data.table(
        lopnr = integer(), eventdate = as.Date(character())
      ),
      codes = stats::setNames(rep(list(TRUE), 3L), cols),
      args = list(date_col = "eventdate")
    )
  )

  for (nm in names(cases)) {
    cs <- cases[[nm]]
    entry <- list(
      codes = cs$codes,
      fn = cs$fn,
      fn_args = cs$args,
      groups = list("g"),
      combine_as = NULL,
      label = nm
    )
    sk <- swereg:::Skeleton$new(
      data = .dar_skeleton(200L),
      batch_number = 1L
    )
    # Captured, not suppressed. Every built-in warns on a zero-row dataset,
    # and the warnings say the dataset is empty rather than that anything
    # went wrong. A blanket `suppressWarnings()` would hide a new one.
    seen <- character()
    withCallingHandlers(
      sk$apply_code_entry(entry, list(g = cs$data), "lopnr", "fp_zero"),
      warning = function(w) {
        seen <<- c(seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_true(
      all(grepl(
        paste0(
          "has 0 rows|contains only NA values|no non-missing arguments",
          "|No matching IDs|out of 3 skeleton IDs"
        ),
        seen
      )),
      info = paste(nm, paste(seen, collapse = " | "))
    )
    # Every requested column, not only the first.
    for (col in cols) {
      expect_true(col %in% names(sk$data), info = paste(nm, col))
      expect_true(is.logical(sk$data[[col]]), info = paste(nm, col))
      expect_false(any(sk$data[[col]]), info = paste(nm, col))
      expect_identical(length(sk$data[[col]]), nrow(sk$data), info = nm)
    }
  }
})

test_that("a key and a secondary index survive a growing entry", {
  dt <- .dar_headroom(
    swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
    0L
  )
  dt[, prior := TRUE]
  data.table::setkey(dt, id, isoyearweek)
  invisible(dt[prior == TRUE]) # builds a secondary index on `prior`
  expect_identical(data.table::key(dt), c("id", "isoyearweek"))
  expect_identical(data.table::indices(dt), "prior")

  entry <- .dar_builtin_entries()$add_diagnoses$entry
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)
  sk$apply_code_entry(entry, .dar_npr(), "lopnr", "fp_key")

  expect_true(all(paste0("dx_", .dar_cols()) %in% names(sk$data)))
  expect_identical(data.table::key(sk$data), c("id", "isoyearweek"))
  # MEASURED: the `prior` index survives, and `add_diagnoses()` adds one of
  # its own on `is_isoyear`, which it filters on.
  expect_true("prior" %in% data.table::indices(sk$data))
  expect_setequal(data.table::indices(sk$data), c("prior", "is_isoyear"))
})

# --- what the applier does NOT guarantee ------------------------------------

test_that("DOCUMENTED GAP: a shallow header sharing the vectors passes", {
  # This test pins a LIMIT, not a feature. A registered function receives the
  # skeleton by reference. `setDT()` on a list of the original columns builds
  # a new header over the SAME vectors, and a same-type `set()` writes into
  # one of them. Every address the applier compares is unchanged, so the
  # write lands and nothing reports it.
  #
  # A function could do this before 26.10.14 as well. Catching it would need
  # a value copy of every column, which is a copy of the whole table.
  fn <- function(skeleton, dataset, id_name, codes) {
    out <- lapply(skeleton, identity)
    data.table::setDT(out)
    data.table::set(out, i = 1L, j = "prior", value = 999L)
    out[, (names(codes)) := TRUE]
    return(out)
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "shallow"
  )
  dt <- .dar_skeleton(200L)
  dt[, prior := 1L]
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  expect_silent(
    sk$apply_code_entry(
      entry,
      list(g = data.table::data.table(lopnr = 1:3)),
      "lopnr",
      "fp_shallow"
    )
  )
  expect_identical(sk$data$prior[[1L]], 999L)
  expect_true(all(sk$data$flag))
  expect_length(sk$applied_registry, 1L)
})

test_that("DOCUMENTED GAP: an in-place write to a prior column passes", {
  # The same limit, without any replacement. `set()` writes into the existing
  # vector, so neither the address nor the row count moves.
  fn <- function(skeleton, dataset, id_name, codes) {
    data.table::set(skeleton, i = 1L, j = "prior", value = 999L)
    skeleton[, (names(codes)) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "inplace_set"
  )
  dt <- .dar_skeleton(200L)
  dt[, prior := 1L]
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

  sk$apply_code_entry(
    entry,
    list(g = data.table::data.table(lopnr = 1:3)),
    "lopnr",
    "fp_inplace"
  )

  expect_identical(sk$data$prior[[1L]], 999L)
})

test_that("a failed entry records nothing, at every free-slot count", {
  # Two runs, 200 free slots and 0. What holds in BOTH is the documented
  # state: nothing is recorded, and the run stops. Whether a column the
  # entry added survives on `$data` depends on the slot count, so neither
  # run asserts it.
  fn <- function(skeleton, dataset, id_name, codes) {
    skeleton[, scratch := TRUE]
    skeleton[1L, isoyear := isoyear - 1L]
    skeleton[, (names(codes)) := TRUE]
    return(invisible(NULL))
  }
  entry <- list(
    codes = list(flag = "X"),
    fn = fn,
    fn_args = list(),
    groups = list("g"),
    combine_as = NULL,
    label = "partial"
  )

  seen <- logical()
  for (spare in c(200L, 0L)) {
    dt <- .dar_headroom(
      swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
      spare
    )
    sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)

    expect_error(
      sk$apply_code_entry(
        entry,
        list(g = data.table::data.table(lopnr = 1:3)),
        "lopnr",
        "fp_partial"
      ),
      "changed the skeleton's `isoyear` column",
      info = paste("spare =", spare)
    )
    expect_length(sk$applied_registry, 0L)
    seen <- c(seen, "scratch" %in% names(sk$data))
  }

  # MEASURED: the added column survives with slots to spare and is lost
  # without them. That is why the documentation promises neither.
  expect_identical(seen, c(TRUE, FALSE))
})

test_that("a failed batch is never written to disk", {
  # The reason the partial in-memory state above is tolerable.
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "g",
    batch_size = 3L
  )
  study$set_ids(1:3)
  study$save_rawbatch("g", data.table::data.table(lopnr = 1:3, v = "a"))
  study$register_framework(function(batch_data, config) {
    d <- data.table::CJ(
      id = batch_data[["g"]]$lopnr,
      isoyearweek = c("2020-01", "2020-02")
    )
    d[, `:=`(isoyear = 2020L, is_isoyear = FALSE)]
    d[]
  })
  study$register_codes(
    codes = list(flag = "X"),
    fn = function(skeleton, dataset, id_name, codes, ...) {
      skeleton[1L, isoyear := isoyear - 1L]
      skeleton[, (names(codes)) := TRUE]
      invisible(NULL)
    },
    groups = list("g"),
    label = "corrupting"
  )

  expect_error(
    suppressMessages(utils::capture.output(
      study$process_skeletons(batches = 1L),
      type = "output"
    )),
    "changed the skeleton's `isoyear` column"
  )

  expect_length(list.files(dir, pattern = "^skeleton_.*[.]qs2$"), 0L)
  expect_null(study$load_skeleton(1L))
})

# --- registration refuses a conflicting entry -------------------------------

test_that("registering a second owner of a column stops", {
  # `$drop_code_entry()` removes exactly the columns an entry declares, so
  # two owners of one column make a drop of either one delete the other's
  # output.
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "g"
  )
  fn <- function(skeleton, dataset, id_name, codes) {
    skeleton[, (names(codes)) := TRUE]
    return(invisible(NULL))
  }
  study$register_codes(
    codes = list(shared = "A"),
    fn = fn,
    groups = list("g"),
    label = "A"
  )

  expect_error(
    study$register_codes(
      codes = list(shared = "B"),
      fn = fn,
      groups = list("g"),
      label = "B"
    ),
    "generates a column that .* already generates: shared"
  )
  expect_length(study$code_registry, 1L)
})

test_that("registering a derived entry over a primary column stops", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "g"
  )
  study$register_codes(
    codes = list(e11 = "E11"),
    fn = function(skeleton, dataset, id_name, codes) {
      skeleton[, (names(codes)) := TRUE]
      return(invisible(NULL))
    },
    groups = list(osd = "g"),
    label = "primary"
  )

  expect_error(
    study$register_derived_codes(
      codes = list(e11 = "E11"),
      from = "os",
      as = "osd"
    ),
    "already generates: osd_e11"
  )
  expect_length(study$code_registry, 1L)
})

test_that("a prefix keeps two entries with the same code names apart", {
  # The ownership rule is about generated COLUMNS, not code names. Two
  # entries may share a code name when their group prefixes differ.
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("g1", "g2")
  )
  fn <- function(skeleton, dataset, id_name, codes) {
    skeleton[, (names(codes)) := TRUE]
    return(invisible(NULL))
  }
  study$register_codes(
    codes = list(shared = "A"), fn = fn, groups = list(a = "g1"), label = "A"
  )
  study$register_codes(
    codes = list(shared = "B"), fn = fn, groups = list(b = "g2"), label = "B"
  )

  expect_length(study$code_registry, 2L)
  expect_setequal(
    unlist(lapply(study$code_registry, swereg:::.entry_columns)),
    c("a_shared", "b_shared")
  )
})

test_that("registering a group outside group_names stops", {
  # `$load_rawbatch()` reads one file per name in `group_names`, so a name
  # outside that set never arrives. Registration is where it has to fail.
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "delivered"
  )

  expect_error(
    study$register_codes(
      codes = list(flag = "X"),
      fn = function(skeleton, dataset, id_name, codes) {
        skeleton[, (names(codes)) := TRUE]
        return(invisible(NULL))
      },
      groups = list(optional = "not_delivered"),
      label = "optional"
    ),
    "names a group the study does not hold: not_delivered"
  )
  expect_length(study$code_registry, 0L)
})

test_that("load_rawbatch stops on a group file the study never wrote", {
  # The second gate. A group that IS in `group_names` but has no file on disk
  # stops before phase 2 ever runs.
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("written", "missing"),
    batch_size = 3L
  )
  study$set_ids(1:3)
  study$save_rawbatch("written", data.table::data.table(lopnr = 1:3, v = "a"))

  expect_error(study$load_rawbatch(1L), "Rawbatch file missing")
})

# --- the TTE exclusion and confounder builders ------------------------------

test_that("the TTE exclusion builder hands its skeleton back", {
  # `.tte_build_exclusion_specs()` writes `eligible_isoyears` and
  # `eligible_age` to the skeleton. Before 26.10.14 it returned only the
  # specification lists, so a caller holding a table with no free slot lost
  # both columns and `tteplan_apply_exclusions()` stopped on them.
  sk <- swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31")
  sk[, rd_age := 30]
  sk <- .dar_headroom(sk, 0L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  spec <- list(
    inclusion_criteria = list(isoyears = c(2021, 2021)),
    outcomes = list(),
    enrollments = list(list(
      id = "e1",
      additional_inclusion = list(list(
        type = "age_range", min = 20, max = 40,
        implementation = list(variable = "rd_age")
      ))
    ))
  )
  enr <- list(enrollment_id = "e1")

  built <- swereg:::.tte_build_exclusion_specs(sk, spec, enr)

  expect_true(data.table::is.data.table(built$skeleton))
  # The exact set, not a subset. `all(character(0) %in% x)` is TRUE.
  expect_identical(
    built$eligible_cols,
    c("eligible_isoyears", "eligible_age")
  )
  expect_true(all(built$eligible_cols %in% names(built$skeleton)))
  expect_false(any(built$eligible_cols %in% names(sk)))

  out <- swereg::tteplan_apply_exclusions(sk, spec, enr)
  expect_true("eligible" %in% names(out))
  expect_true(all(built$eligible_cols %in% names(out)))
})

test_that("the TTE confounder builder hands its skeleton back", {
  sk <- swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31")
  sk[, src_a := id == 1L]
  sk[, src_b := id == 2L]
  sk <- .dar_headroom(sk, 0L)

  spec <- list(confounders = list(list(
    implementation = list(
      computed = TRUE,
      variable = "conf_ab",
      source_variable = c("src_a", "src_b"),
      source_variable_combined = "src_ab",
      window_weeks = 4
    )
  )))

  built <- swereg:::.tte_build_confounder_specs(sk, spec)

  expect_true("src_ab" %in% names(built$skeleton))
  expect_false("src_ab" %in% names(sk))

  out <- swereg::tteplan_apply_derived_confounders(sk, spec)
  expect_true("conf_ab" %in% names(out))
})

# --- direct calls, outside the registry -------------------------------------

test_that("every add_* reaches a name-bound skeleton out of slots", {
  # The rebind path. `sk` is a name, so the grown table is written back to it
  # and the caller reads the new columns off `sk` itself.
  cases <- .dar_builtin_entries()
  batch_data <- .dar_batch_data()
  for (nm in names(cases)) {
    case <- cases[[nm]]
    sk <- .dar_skeleton(5L)
    dataset <- batch_data[[case$entry$groups[[1L]]]]
    # Build `fn(sk, dataset, ...)` so `substitute()` inside the function
    # yields the NAME `sk`. `do.call()` with the table itself would give it
    # the value, which is the other path and is covered below.
    cl <- as.call(c(
      list(case$entry$fn, quote(sk), quote(dataset)),
      list(id_name = "lopnr", codes = case$entry$codes),
      case$entry$fn_args
    ))

    got <- eval(cl)

    expect_true(all(.dar_cols() %in% names(sk)), info = nm)
    expect_true(all(.dar_cols() %in% names(got)), info = nm)
  }
})

test_that("add_onetime reaches a name-bound skeleton out of slots", {
  sk <- .dar_skeleton(5L)
  wide <- data.table::data.table(lopnr = 1:3)
  for (nm in .dar_cols()) {
    data.table::set(wide, j = nm, value = seq_len(3L))
  }

  got <- swereg::add_onetime(sk, wide, "lopnr")

  expect_true(all(.dar_cols() %in% names(sk)))
  expect_true(all(.dar_cols() %in% names(got)))
})

test_that("add_annual reaches a name-bound skeleton out of slots", {
  sk <- .dar_skeleton(5L)
  wide <- data.table::data.table(lopnr = 1:3)
  for (nm in .dar_cols()) {
    data.table::set(wide, j = nm, value = seq_len(3L))
  }

  got <- swereg::add_annual(sk, wide, "lopnr", isoyear = 2021)

  expect_true(all(.dar_cols() %in% names(sk)))
  expect_true(all(.dar_cols() %in% names(got)))
})

test_that("a value passed through do.call carries columns on the return", {
  # `do.call()` with a value gives `substitute()` the table itself. There is
  # no binding to write to, so the function warns and the return is the only
  # route. This is exactly what the code registry does.
  sk <- .dar_skeleton(5L)

  expect_warning(
    got <- do.call(
      swereg::add_diagnoses,
      list(sk, .dar_npr()$npr, id_name = "lopnr", codes = .dar_codes("F32"))
    ),
    "Use the table this call returns"
  )

  expect_true(data.table::is.data.table(got))
  expect_true(all(.dar_cols() %in% names(got)))
  expect_false("c06" %in% names(sk))
})

# --- the figure qs2_read restores -------------------------------------------

test_that("qs2_read restores the documented number of free column slots", {
  path <- withr::local_tempfile(fileext = ".qs2")
  sk <- swereg:::Skeleton$new(
    data = swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
    batch_number = 1L
  )
  qs2::qs_save(sk, path)

  loaded <- swereg::qs2_read(path)

  expect_identical(swereg:::.DT_ALLOC_SPARE_SLOTS, 4096L)
  expect_identical(
    data.table::truelength(loaded$data) - ncol(loaded$data),
    4096L
  )
})

test_that("qs2_read gives a nested table 1024 slots, not 4096", {
  # A plan can hold thousands of small result tables, and one free slot costs
  # 16 bytes whether it is used or not. Only a skeleton gets 4096.
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(
    list(panels = list(
      one = data.table::data.table(a = 1:5),
      two = data.table::data.table(b = 1:5)
    )),
    path
  )

  loaded <- swereg::qs2_read(path)

  expect_identical(swereg:::.DT_ALLOC_NESTED_SLOTS, 1024L)
  for (nm in c("one", "two")) {
    d <- loaded$panels[[nm]]
    expect_identical(data.table::truelength(d) - ncol(d), 1024L, info = nm)
  }
})

test_that("qs2_read gives a non-Skeleton R6 field 1024 slots", {
  # The table sits DIRECTLY in the field, which is the branch that has to
  # tell a `Skeleton$data` from every other field. A table one level deeper,
  # inside a list, takes the list branch instead.
  path <- withr::local_tempfile(fileext = ".qs2")
  holder <- swereg::RegistryStudy$new(
    data_rawbatch_dir = tempfile("rawbatch_"),
    group_names = "g"
  )
  holder$batch_id_list <- data.table::data.table(a = 1:5)
  qs2::qs_save(holder, path)

  loaded <- swereg::qs2_read(path)
  d <- loaded$batch_id_list

  expect_true(data.table::is.data.table(d))
  expect_identical(data.table::truelength(d) - ncol(d), 1024L)
})

test_that("qs2_read gives a Skeleton data field 4096 slots", {
  # The other side of the same branch. A `Skeleton` is the one object whose
  # table a code registry writes to, so its `$data` gets the larger figure.
  path <- withr::local_tempfile(fileext = ".qs2")
  sk <- swereg:::Skeleton$new(
    data = data.table::data.table(a = 1:5),
    batch_number = 1L
  )
  qs2::qs_save(sk, path)

  loaded <- swereg::qs2_read(path)

  expect_identical(
    data.table::truelength(loaded$data) - ncol(loaded$data),
    4096L
  )
})

test_that("an alias taken before a growing call is stale", {
  # Documented in ?Skeleton and in every add_* help page. R cannot grow a
  # list in place, so the caller's binding holds a NEW object afterwards and
  # any other name still points at the old one.
  dt <- .dar_headroom(
    swereg::create_skeleton(1:3, "2021-01-01", "2021-03-31"),
    0L
  )
  sk <- swereg:::Skeleton$new(data = dt, batch_number = 1L)
  held <- sk$data
  entry <- .dar_builtin_entries()$add_diagnoses$entry

  sk$apply_code_entry(entry, .dar_npr(), "lopnr", "fp_alias")

  expect_true("dx_c06" %in% names(sk$data))
  expect_false("dx_c06" %in% names(held))
})

test_that("qs2_read ignores datatable.alloccol", {
  # data.table sets that option to 1024 when it loads, which is fewer slots
  # than a production code registry writes columns. The reader used to read
  # it, so it delivered 1024 while `?qs2_read` was read as promising 4096.
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(data.table::data.table(a = 1:5), path)

  withr::with_options(list(datatable.alloccol = 8L), {
    loaded <- swereg::qs2_read(path)
    expect_identical(data.table::truelength(loaded) - ncol(loaded), 4096L)
  })
})
