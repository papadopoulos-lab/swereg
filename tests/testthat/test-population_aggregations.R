# Tests for the population_by_specs declarative aggregation cache:
#  * meta sidecars carry per-spec aggregations after process_skeletons()
#  * adding a new spec triggers a meta-only refresh (skeleton mtime
#    unchanged; meta mtime touched) -- skeleton not rebuilt
#  * unregistered by-specs error from study$population()

library(data.table)

.pa_framework <- function(batch_data, config) {
  ids <- batch_data[["grp1"]]$lopnr
  data.table::rbindlist(list(
    data.table::data.table(
      id = ids,
      isoyear = 2020L,
      isoyearweek = "2020-**",
      is_isoyear = TRUE,
      personyears = 1,
      saab = c("Male", "Female", "Male")[seq_along(ids)],
      age = c(25L, 30L, 25L)[seq_along(ids)]
    ),
    data.table::data.table(
      id = rep(ids, each = 2L),
      isoyear = 2021L,
      isoyearweek = rep(c("2021-01", "2021-02"), length(ids)),
      is_isoyear = FALSE,
      personyears = 1 / 52.25,
      saab = rep(c("Male", "Female", "Male")[seq_along(ids)], each = 2L),
      age = rep(c(26L, 31L, 26L)[seq_along(ids)], each = 2L)
    )
  ))
}

.pa_mk_study <- function(specs = list()) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  study <- RegistryStudy$new(
    data_rawbatch_dir   = dir,
    group_names         = c("grp1"),
    batch_size          = 3L,
    population_by_specs = specs
  )
  study$set_ids(1:6)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study$register_framework(.pa_framework)
  study
}


test_that("meta sidecar carries population_aggregations for each declared spec", {
  study <- .pa_mk_study(specs = list("saab", c("saab", "age")))
  study$process_skeletons()

  meta <- study$load_skeleton_meta(1L)
  expect_true(!is.null(meta$population_aggregations))
  expect_setequal(
    names(meta$population_aggregations),
    c("saab", "age+saab")
  )

  # Each entry is a data.table with isoyear + spec + n
  agg_saab <- meta$population_aggregations[["saab"]]
  expect_s3_class(agg_saab, "data.table")
  expect_setequal(names(agg_saab), c("isoyear", "saab", "n"))
})


test_that("study$population reads cached aggregations matching old uniqueN(id) semantics", {
  study <- .pa_mk_study(specs = list(c("saab", "age")))
  study$process_skeletons()

  pop <- study$population(by = c("saab", "age"))
  # Per-isoyear sum across all combos == 6 unique persons
  expect_equal(pop[isoyear == 2020, sum(n)], 6L)
  expect_equal(pop[isoyear == 2021, sum(n)], 6L)
})


test_that("adding a new spec triggers a meta-only refresh -- skeleton untouched", {
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir   = dir,
    group_names         = c("grp1"),
    batch_size          = 3L,
    population_by_specs = list("saab")
  )
  study$set_ids(1:6)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study$register_framework(.pa_framework)
  study$process_skeletons()

  sk_path   <- file.path(dir, "skeleton_00001.qs2")
  meta_path <- file.path(dir, "meta_00001.qs2")
  sk_mtime_before   <- file.mtime(sk_path)
  meta_mtime_before <- file.mtime(meta_path)

  # Sleep one second so mtime resolution can distinguish before/after
  Sys.sleep(1.1)

  # Now register a new spec on a fresh study object pointing at the
  # same directory and re-run. The skeleton on disk is still valid;
  # only the meta should be rewritten.
  study2 <- RegistryStudy$new(
    data_rawbatch_dir   = dir,
    group_names         = c("grp1"),
    batch_size          = 3L,
    population_by_specs = list("saab", c("saab", "age"))
  )
  study2$set_ids(1:6)
  study2$register_framework(.pa_framework)
  study2$process_skeletons()

  expect_equal(file.mtime(sk_path), sk_mtime_before,
               tolerance = 0.1, info = "skeleton must not have been rewritten")
  expect_gt(as.numeric(file.mtime(meta_path) - meta_mtime_before), 0)

  meta <- study2$load_skeleton_meta(1L)
  expect_setequal(
    names(meta$population_aggregations),
    c("saab", "age+saab")
  )
})


test_that("study$population errors clearly for an unregistered by-spec", {
  study <- .pa_mk_study(specs = list("saab"))
  study$process_skeletons()

  expect_error(
    study$population(by = "age"),
    "population_by_specs"
  )
})


test_that("population_by_specs validation rejects bad shapes", {
  dir <- withr::local_tempdir()
  expect_error(
    RegistryStudy$new(
      data_rawbatch_dir = dir,
      population_by_specs = "not_a_list"
    ),
    "list of character vectors"
  )
  expect_error(
    RegistryStudy$new(
      data_rawbatch_dir = dir,
      population_by_specs = list(character(0))
    ),
    "non-empty character vector"
  )
  expect_error(
    RegistryStudy$new(
      data_rawbatch_dir = dir,
      population_by_specs = list(c("ok", NA_character_))
    ),
    "non-empty character vector"
  )
})
