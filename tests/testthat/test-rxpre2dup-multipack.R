# add_rxpre2dup() reads the pack count of a dispensing from the column antal
# of lmed. LMED's fddd is the DDD of the whole dispensing, pack DDD x antal.

# One design row: a pack of 30 DDDs that lasts 20 to 30 days, 30 days usual.
# dur_max = 30 means that a duration of 60 days needs ratio = 2: with
# ratio = 1 the port caps the duration of the dispensing at 30 days.
mp_design <- function() {
  path <- tempfile(fileext = ".xlsx", tmpdir = tempdir())
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "packages")
  openxlsx::writeData(
    wb,
    "packages",
    data.frame(
      atc = "C10AA01",
      produkt = "Simvastatin X",
      lnmn = "Simvastatin X 20 mg",
      pack_ddd = 30,
      form = "tablet",
      dur_min = 20,
      dur_usual = 30,
      dur_max = 30,
      status = "approved",
      source = "FASS",
      note = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  return(rxpre2dup_read_design(path))
}

# 2020-03-02 is the Monday of ISO week 2020-10.
mp_day <- as.Date("2020-03-02")

mp_lmed <- function(antal, fddd, edatum = mp_day) {
  return(data.table::data.table(
    lopnr = 1L,
    edatum = edatum,
    atc = "C10AA01",
    produkt = "Simvastatin X",
    lnmn = "Simvastatin X 20 mg",
    antal = antal,
    fddd = fddd
  ))
}

mp_codes <- list(rxpre2dup_statin = "C10AA")

# Runs add_rxpre2dup() on a weekly skeleton of 2020-01 to 2020-52. Returns
# the skeleton and the purchases table that .pre2dup_periods() received.
mp_run <- function(lmed, design = mp_design()) {
  real <- .pre2dup_periods
  seen <- NULL
  testthat::local_mocked_bindings(
    .pre2dup_periods = function(purchases, ...) {
      seen <<- purchases
      return(real(purchases, ...))
    },
    .package = "swereg"
  )
  sk <- create_skeleton(1L, 2020, "2020-52")
  suppressMessages(add_rxpre2dup(sk, lmed, codes = mp_codes, design = design))
  return(list(skeleton = sk, purchases = seen))
}

mp_true_weeks <- function(sk) {
  return(sk[is_isoyear == FALSE & rxpre2dup_statin == TRUE, isoyearweek])
}

test_that("1. a dispensing of 2 packs matches the 1-pack design row and lasts 2 x 30 days", {
  out <- mp_run(mp_lmed(antal = 2L, fddd = 60))

  # The port receives ratio = antal and ddd = fddd, with the design row.
  expect_identical(out$purchases$package_id, 1L)
  expect_identical(as.numeric(out$purchases$ratio), 2)
  expect_identical(out$purchases$ddd, 60)
  expect_identical(
    attr(out$skeleton, "package_coverage")$share_without_package,
    0
  )
  # The period is 2020-03-02 to 2020-05-01, that is 60 days, or 2 x the
  # dur_usual of 30 days. 2020-05-01 is in ISO week 2020-18.
  expect_identical(mp_true_weeks(out$skeleton), sprintf("2020-%02d", 10:18))
})

test_that("2. a dispensing of 1 pack with fddd = 60 does not match the 30-DDD row", {
  out <- mp_run(mp_lmed(antal = 1L, fddd = 60))

  expect_identical(out$purchases$package_id, NA_integer_)
  expect_identical(as.numeric(out$purchases$ratio), 1)
  expect_identical(out$purchases$ddd, 60)
  expect_identical(
    attr(out$skeleton, "package_coverage")$share_without_package,
    1
  )
  # The ATC-level rule of C10AA: 60 DDD / 0.8 DDD a day = 75 days, so the
  # period ends 2020-05-16, in ISO week 2020-20.
  expect_identical(mp_true_weeks(out$skeleton), sprintf("2020-%02d", 10:20))
})

test_that("3. a return with a negative antal is dropped and counted in the message", {
  design <- mp_design()
  lmed <- rbind(
    mp_lmed(antal = 1L, fddd = 30),
    mp_lmed(antal = -1L, fddd = 30, edatum = mp_day + 10)
  )
  sk <- create_skeleton(1L, 2020, "2020-52")
  expect_message(
    add_rxpre2dup(sk, lmed, codes = mp_codes, design = design),
    "add_rxpre2dup(): dropped 1 of 2 rows of lmed: antal negative 1.",
    fixed = TRUE
  )

  out <- mp_run(lmed, design)
  expect_identical(out$purchases$date, mp_day)
  expect_identical(attr(out$skeleton, "package_coverage")$n_purchases, 1L)
  # One pack of 30 days: 2020-03-02 to 2020-04-01, in ISO week 2020-14.
  expect_identical(mp_true_weeks(out$skeleton), sprintf("2020-%02d", 10:14))
})

test_that("4. an lmed without antal stops, and the error names the column", {
  lmed <- mp_lmed(antal = 2L, fddd = 60)
  lmed[, antal := NULL]
  sk <- create_skeleton(1L, 2020, "2020-52")
  expect_error(
    add_rxpre2dup(sk, lmed, codes = mp_codes, design = mp_design()),
    "lmed is missing these columns: antal",
    fixed = TRUE
  )
  expect_false("rxpre2dup_statin" %in% names(sk))
})
