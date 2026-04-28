# Pin add_onetime() behaviour around id mismatches and column
# preservation. add_onetime is the workhorse for baseline data
# (demographics, register tags, etc.); a silent no-op or unhelpful
# error from the wrong id_name has historically been a debugging
# black hole.

skip_if_not_installed("data.table")

.tiny_skeleton <- function() {
  swereg::create_skeleton(
    ids = c(1L, 2L, 3L),
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
}

test_that("add_onetime: extra ids in data are silently ignored, no error", {
  skel <- .tiny_skeleton()
  data <- data.table::data.table(
    lopnr  = c(1L, 2L, 3L, 999L),  # 999 is not in skeleton
    sex    = c("F", "F", "M", "F"),
    region = c("stockholm", "skane", "skane", "other")
  )
  pre_n <- nrow(skel)
  swereg::add_onetime(skel, data, id_name = "lopnr") |>
    suppressWarnings()
  expect_equal(nrow(skel), pre_n,
               info = "add_onetime should not add or drop skeleton rows")
  expect_true("sex" %in% names(skel))
  expect_true("region" %in% names(skel))
})

test_that("add_onetime: missing ids in data leave skeleton rows with NA", {
  skel <- .tiny_skeleton()
  data <- data.table::data.table(
    lopnr = c(1L, 2L),                # id=3 missing from data
    sex   = c("F", "F")
  )
  swereg::add_onetime(skel, data, id_name = "lopnr") |>
    suppressWarnings()
  # Person 3 had no data row -> sex is NA on every row of theirs.
  expect_true(all(is.na(skel[id == 3L, sex])))
  # Persons 1 and 2 are populated.
  expect_true(all(skel[id == 1L, sex] == "F"))
})

test_that("add_onetime: clear error when id_name column is missing from data", {
  skel <- .tiny_skeleton()
  data <- data.table::data.table(
    not_lopnr = c(1L, 2L, 3L),
    sex       = c("F", "F", "M")
  )
  expect_error(
    swereg::add_onetime(skel, data, id_name = "lopnr")
  )
})

test_that("add_onetime: works when id_name column exists with different name in skeleton", {
  # Skeleton's id column is always "id", but add_onetime's `id_name`
  # parameter refers to the column name in `data`. The function
  # internally aligns "id" (skeleton) to id_name (data).
  skel <- .tiny_skeleton()
  data <- data.table::data.table(
    lopnr    = c(1L, 2L, 3L),
    register = c("case", "control", "case")
  )
  swereg::add_onetime(skel, data, id_name = "lopnr")
  expect_true(all(skel[id == 1L, register] == "case"))
  expect_true(all(skel[id == 2L, register] == "control"))
})

test_that("add_onetime: per-week values stay constant within id (one-time semantic)", {
  skel <- .tiny_skeleton()
  data <- data.table::data.table(
    lopnr     = c(1L, 2L, 3L),
    birthyear = c(1965L, 1972L, 1958L)
  )
  swereg::add_onetime(skel, data, id_name = "lopnr") |>
    suppressWarnings()
  # The whole point of `add_onetime` is that the value broadcasts to
  # every row of that id -- a single value per person. Verify that.
  per_id <- skel[, .(uniq = data.table::uniqueN(birthyear)), by = id]
  expect_true(all(per_id$uniq == 1L),
              info = "add_onetime should broadcast one value per id across all weeks")
  expect_setequal(unique(skel[id == 1L, birthyear]), 1965L)
  expect_setequal(unique(skel[id == 2L, birthyear]), 1972L)
})
