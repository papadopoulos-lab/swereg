# A data.table read back from qs2 loses its over-allocated column slots. The
# first `:=` on it inside a function then writes to a shallow copy. That copy
# is local to the function, and the caller never receives the column. These
# tests drive that path through every shape swereg loads from disk.

# Adds a column by reference and returns nothing. The caller MUST see the
# column afterwards.
add_col_by_reference <- function(dt) {
  dt[, added_by_reference := 1L]
  return(invisible(NULL))
}

test_that("qs2_read over-allocates a bare data.table", {
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(data.table::data.table(a = 1:5, b = letters[1:5]), path)

  d <- qs2_read(path)
  expect_gt(data.table::truelength(d), ncol(d))
  add_col_by_reference(d)
  expect_true("added_by_reference" %in% names(d))
})

test_that("qs2_read over-allocates a data.table nested in a list", {
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(
    list(
      label = "panels",
      inner = list(one = data.table::data.table(a = 1:5), two = NULL)
    ),
    path
  )

  obj <- qs2_read(path)
  expect_named(obj$inner, c("one", "two"))
  expect_gt(data.table::truelength(obj$inner$one), ncol(obj$inner$one))
  add_col_by_reference(obj$inner$one)
  expect_true("added_by_reference" %in% names(obj$inner$one))
})

test_that("qs2_read reaches a data.table nested 150 lists deep", {
  # The walker carries no depth limit. 150 is past the 100-level cap an
  # earlier draft carried, so restoring a cap of that size fails this block.
  # A cap returns the table unrepaired and reports nothing, which is the
  # failure mode this file exists to remove.
  depth <- 150L
  path <- withr::local_tempfile(fileext = ".qs2")
  nested <- data.table::data.table(a = 1:5)
  for (i in seq_len(depth)) {
    nested <- list(nested)
  }
  qs2::qs_save(nested, path)

  loaded <- qs2_read(path)
  deep <- loaded
  for (i in seq_len(depth)) {
    deep <- deep[[1L]]
  }
  expect_true(data.table::is.data.table(deep))
  expect_gt(data.table::truelength(deep), ncol(deep))
  add_col_by_reference(deep)
  expect_true("added_by_reference" %in% names(deep))
})

test_that("qs2_read over-allocates a data.table in an R6 field", {
  path <- withr::local_tempfile(fileext = ".qs2")
  sk <- Skeleton$new(
    data = data.table::data.table(a = 1:5),
    batch_number = 1L
  )
  qs2::qs_save(sk, path)

  loaded <- qs2_read(path)
  expect_gt(data.table::truelength(loaded$data), ncol(loaded$data))
  add_col_by_reference(loaded$data)
  expect_true("added_by_reference" %in% names(loaded$data))
})

test_that("qs2_read over-allocates a data.table in a list in an R6 field", {
  path <- withr::local_tempfile(fileext = ".qs2")
  sk <- Skeleton$new(
    data = data.table::data.table(a = 1:5),
    batch_number = 1L
  )
  sk$randvars_state <- list(step = list(panel = data.table::data.table(b = 1:5)))
  qs2::qs_save(sk, path)

  loaded <- qs2_read(path)
  panel <- loaded$randvars_state$step$panel
  expect_gt(data.table::truelength(panel), ncol(panel))
  add_col_by_reference(panel)
  expect_true("added_by_reference" %in% names(loaded$randvars_state$step$panel))
})

test_that("add_annual reaches the caller after a Skeleton disk round-trip", {
  dir <- withr::local_tempdir()
  sk <- Skeleton$new(
    data = create_skeleton(c(1, 2, 3), "2021-01-01", "2021-12-31"),
    batch_number = 7L
  )
  path <- sk$save(dir)

  loaded <- qs2_read(path)
  annual <- data.table::data.table(
    lopnr = c(1, 2, 3),
    dispink04 = c(10, 20, 30)
  )
  add_annual(loaded$data, annual, id_name = "lopnr", isoyear = 2021)

  expect_true("dispink04" %in% names(loaded$data))
  got <- loaded$data[isoyear == 2021, .(v = unique(dispink04)), keyby = id]
  expect_equal(got$v, c(10, 20, 30))
  expect_true(all(is.na(loaded$data[isoyear != 2021]$dispink04)))
})

test_that("qs2_read leaves an object that holds no data.table unchanged", {
  path <- withr::local_tempfile(fileext = ".qs2")
  obj <- list(a = 1:3, b = list(c = "x", d = NULL), e = data.frame(f = 1:2))
  qs2::qs_save(obj, path)
  expect_identical(qs2_read(path), obj)
})

test_that("RegistryStudy$load_skeleton hands add_annual a skeleton the caller keeps", {
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
  study$register_framework(function(batch_data, config) {
    create_skeleton(batch_data[["grp1"]]$lopnr, "2021-01-01", "2021-12-31")
  })
  study$process_skeletons()

  sk <- study$load_skeleton(1L)
  expect_gt(data.table::truelength(sk$data), ncol(sk$data))

  annual <- data.table::data.table(lopnr = 1:3, dispink04 = c(10, 20, 30))
  add_annual(sk$data, annual, id_name = "lopnr", isoyear = 2021)

  expect_true("dispink04" %in% names(sk$data))
  got <- sk$data[isoyear == 2021, .(v = unique(dispink04)), keyby = id]
  expect_equal(got$v, c(10, 20, 30))
})

test_that(".s1_load_skeleton returns an over-allocated skeleton", {
  dir <- withr::local_tempdir()
  sk <- Skeleton$new(
    data = create_skeleton(1:3, "2021-01-01", "2021-12-31"),
    batch_number = 1L
  )
  path <- sk$save(dir)

  d <- swereg:::.s1_load_skeleton(path, 1L)
  expect_gt(data.table::truelength(d), ncol(d))
  add_col_by_reference(d)
  expect_true("added_by_reference" %in% names(d))
})

test_that("qs2_read does not force an R6 active binding", {
  # `TTEPlan$dir_tteplan` and `RegistryStudy$data_rawbatch_dir` are active
  # bindings. Reading one runs user code, and both stop() in some states, so
  # the walker MUST leave every active binding alone.
  #
  # `expect_no_error()` carries the load. A regression then fails on an
  # expectation, rather than aborting the block before any expectation runs.
  path <- withr::local_tempfile(fileext = ".qs2")
  probe <- R6::R6Class(
    "SweregActiveBindingProbe",
    public = list(
      data = NULL,
      initialize = function(d) {
        self$data <- d
        return(invisible(self))
      }
    ),
    active = list(
      boom = function(value) stop("active binding was forced", call. = FALSE)
    )
  )
  qs2::qs_save(probe$new(data.table::data.table(a = 1:5)), path)

  loaded <- NULL
  expect_no_error(loaded <- qs2_read(path))
  expect_s3_class(loaded, "SweregActiveBindingProbe")
  expect_gt(data.table::truelength(loaded$data), 0L)
  expect_error(loaded$boom, "active binding was forced")
})

test_that("qs2_read leaves a data.table inside a classed list alone", {
  # `[[<-` dispatches on a classed list, so the walker refuses to enter one.
  # A `data.frame` is the classed list that reaches swereg, and a column of
  # one can hold data.tables.
  path <- withr::local_tempfile(fileext = ".qs2")
  d <- data.frame(grp = 1:2)
  d[["panels"]] <- list(
    data.table::data.table(a = 1:5),
    data.table::data.table(b = 1:5)
  )
  qs2::qs_save(d, path)

  loaded <- qs2_read(path)
  expect_s3_class(loaded, "data.frame", exact = TRUE)
  expect_true(data.table::is.data.table(loaded[["panels"]][[1L]]))
  expect_identical(data.table::truelength(loaded[["panels"]][[1L]]), 0L)
  expect_identical(data.table::truelength(loaded[["panels"]][[2L]]), 0L)
})

test_that("qs2_read does not enter an environment that is not an R6 object", {
  # A binding in a plain environment can hold a package namespace. A
  # namespace is locked, so `assign()` into it fails, and walking one forces
  # every lazy-loaded object in it. The fixture carries the `stats`
  # namespace, which the walker reaches if the guard goes away.
  path <- withr::local_tempfile(fileext = ".qs2")
  probe <- R6::R6Class(
    "SweregPlainEnvProbe",
    public = list(
      env = NULL,
      initialize = function(e) {
        self$env <- e
        return(invisible(self))
      }
    )
  )
  holder <- new.env(parent = emptyenv())
  holder$dt <- data.table::data.table(a = 1:5)
  holder$ns <- asNamespace("stats")
  qs2::qs_save(probe$new(holder), path)

  loaded <- NULL
  expect_no_error(loaded <- qs2_read(path))
  expect_true(isNamespace(loaded$env$ns))
  expect_true(data.table::is.data.table(loaded$env$dt))
  expect_identical(data.table::truelength(loaded$env$dt), 0L)
})

test_that("qs2_read visits a self-referential R6 object once", {
  # An R6 object is an environment, so an object graph can hold a cycle. The
  # visited set in `.restore_dt_alloc_r6()` is what ends it.
  #
  # `expressions` is lowered for this block. A regression then raises an R
  # error within milliseconds, instead of recursing to the default limit, and
  # `expect_no_error()` turns that error into a failed expectation. Nothing
  # here can loop forever: R counts nested evaluations and stops.
  path <- withr::local_tempfile(fileext = ".qs2")
  probe <- R6::R6Class(
    "SweregCycleProbe",
    public = list(
      data = NULL,
      other = NULL,
      initialize = function(d) {
        self$data <- d
        return(invisible(self))
      }
    )
  )
  obj <- probe$new(data.table::data.table(a = 1:5))
  obj$other <- obj
  qs2::qs_save(obj, path)

  withr::local_options(expressions = 500L)
  loaded <- NULL
  expect_no_error(loaded <- qs2_read(path))
  expect_s3_class(loaded, "SweregCycleProbe")
  expect_identical(loaded$other, loaded)
  expect_gt(data.table::truelength(loaded$data), 0L)
})

test_that("qs2_read over-allocates a data.table in an R6 private field", {
  # No swereg class stores a data.table in private today. The walker covers
  # private anyway, and this pins that it does.
  path <- withr::local_tempfile(fileext = ".qs2")
  holder <- R6::R6Class(
    "SweregPrivateFieldProbe",
    public = list(
      initialize = function(d) {
        private$.dt <- d
        return(invisible(self))
      },
      dt = function() {
        return(private$.dt)
      }
    ),
    private = list(.dt = NULL)
  )
  qs2::qs_save(holder$new(data.table::data.table(a = 1:5)), path)

  loaded <- qs2_read(path)
  expect_gt(data.table::truelength(loaded$dt()), ncol(loaded$dt()))
  add_col_by_reference(loaded$dt())
  expect_true("added_by_reference" %in% names(loaded$dt()))
})


test_that("add_annual writes isoyear into a rawbatch table the caller keeps", {
  # `add_annual()` runs `data[, isoyear := isoyear]` on its `data` argument.
  # A rawbatch table read straight off disk carried no over-allocation. That
  # write went to a shallow copy, and the caller kept a table without the
  # column. `RegistryStudy$load_rawbatch()` goes through `qs2_read()`, so the
  # write now reaches the caller.
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "annual",
    batch_size = 3L
  )
  study$set_ids(1:3)
  study$save_rawbatch(
    "annual",
    data.table::data.table(lopnr = 1:3, dispink04 = c(10, 20, 30))
  )

  annual <- study$load_rawbatch(1L)[["annual"]]
  expect_false("isoyear" %in% names(annual))

  skeleton <- create_skeleton(1:3, "2021-01-01", "2021-12-31")
  add_annual(skeleton, annual, id_name = "lopnr", isoyear = 2021)

  expect_true("isoyear" %in% names(annual))
  expect_identical(unique(annual$isoyear), 2021)
})
