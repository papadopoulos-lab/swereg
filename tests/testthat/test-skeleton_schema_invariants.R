# Pins the structural invariants the skeleton must satisfy at every
# stage of the create_skeleton -> add_*() chain. Drift here (rows lost,
# id column renamed, sort broken, duplicate keys created) silently
# corrupts every downstream rate / IRR.

skip_if_not_installed("data.table")

.has_no_duplicate_id_week <- function(skel) {
  dups <- skel[, .N, by = .(id, isoyearweek)][N > 1L]
  nrow(dups) == 0L
}

test_that("create_skeleton: (id, isoyearweek) is unique", {
  skel <- create_skeleton(
    ids = c(101L, 102L, 103L),
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
  expect_true(.has_no_duplicate_id_week(skel))
})

test_that("create_skeleton: schema includes the documented columns", {
  skel <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
  required <- c("id", "isoyear", "isoyearweek", "is_isoyear",
                "isoyearweeksun", "personyears")
  expect_true(all(required %in% names(skel)))
})

test_that("create_skeleton: sorted by (id, isoyearweek)", {
  skel <- create_skeleton(
    ids = c("Z", "A", "M"),
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
  ord_id <- order(skel$id, skel$isoyearweek)
  expect_equal(seq_len(nrow(skel)), ord_id)
})

test_that("create_skeleton: weekly row count is identical across ids", {
  skel <- create_skeleton(
    ids = c(1L, 2L, 3L, 4L),
    date_min = as.Date("2021-01-04"),
    date_max = as.Date("2021-12-26")
  )
  per_id <- skel[is_isoyear == FALSE, .N, by = id]
  expect_equal(length(unique(per_id$N)), 1L,
               info = "weekly row count differs per id -- skeleton is not balanced")
})

test_that("create_skeleton: every weekly row's isoyear matches its isoyearweek", {
  skip_if_not_installed("cstime")
  skel <- create_skeleton(
    ids = 1L,
    date_min = as.Date("2019-12-30"),  # ISO 2020 W01
    date_max = as.Date("2021-01-03")   # ISO 2020 W53
  )
  weekly <- skel[is_isoyear == FALSE]
  expect_equal(
    weekly$isoyear,
    cstime::isoyearweek_to_isoyear_n(weekly$isoyearweek)
  )
})

test_that("add_*() functions preserve row count and existing columns", {
  data("fake_person_ids", package = "swereg")
  data("fake_demographics", package = "swereg")
  data("fake_diagnoses", package = "swereg")
  data("fake_prescriptions", package = "swereg")

  skel <- create_skeleton(
    ids = fake_person_ids[1:50],
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
  pre_n <- nrow(skel)
  pre_cols <- names(skel)

  # add_onetime
  demo <- data.table::copy(fake_demographics)
  swereg::make_lowercase_names(demo, date_columns = c("fodelseman", "doddatum"))
  swereg::add_onetime(skel, demo, id_name = "lopnr")
  expect_equal(nrow(skel), pre_n,
               info = "add_onetime should not change row count")
  expect_true(all(pre_cols %in% names(skel)),
              info = "add_onetime dropped a pre-existing skeleton column")
  expect_true(.has_no_duplicate_id_week(skel),
              info = "add_onetime introduced duplicate (id, isoyearweek) rows")

  # add_diagnoses
  dx <- data.table::copy(fake_diagnoses)
  swereg::make_lowercase_names(dx, date_columns = "indatum")
  pre_n_2 <- nrow(skel)
  pre_cols_2 <- names(skel)
  swereg::add_diagnoses(
    skel, dx, id_name = "lopnr",
    codes = list("dx_f32" = "^F32")
  )
  expect_equal(nrow(skel), pre_n_2)
  expect_true(all(pre_cols_2 %in% names(skel)))
  expect_true(.has_no_duplicate_id_week(skel))
  expect_true("dx_f32" %in% names(skel),
              info = "add_diagnoses did not add the requested code column")
  expect_type(skel$dx_f32, "logical")

  # add_rx (rename the LMED-style id col to `lopnr`, matching existing
  # tests' convention)
  rx <- data.table::copy(fake_prescriptions)
  swereg::make_lowercase_names(rx, date_columns = "edatum")
  if ("p444_lopnr_personnr" %in% names(rx) && !"lopnr" %in% names(rx)) {
    data.table::setnames(rx, "p444_lopnr_personnr", "lopnr")
  }
  pre_n_3 <- nrow(skel)
  pre_cols_3 <- names(skel)
  swereg::add_rx(
    skel, rx, id_name = "lopnr",
    codes = list("rx_n06a" = "^N06A")
  )
  expect_equal(nrow(skel), pre_n_3)
  expect_true(all(pre_cols_3 %in% names(skel)))
  expect_true(.has_no_duplicate_id_week(skel))
  expect_type(skel$rx_n06a, "logical")
})
