# add_rxpre2dup() and rxpre2dup_read_design(). No test here needs PRE2DUPR:
# the expected periods come from swereg's own port, .pre2dup_periods().

# One design row per element of the arguments, in the column order of the
# design file format.
rxp_design_rows <- function(
  atc = "C10AA01",
  produkt = "Simvastatin X",
  lnmn = "Simvastatin X 20 mg",
  pack_ddd = 30,
  form = "tablet",
  dur_min = 20,
  dur_usual = 30,
  dur_max = 60,
  status = "approved",
  source = "FASS",
  note = NA_character_
) {
  return(data.frame(
    atc = atc,
    produkt = produkt,
    lnmn = lnmn,
    pack_ddd = pack_ddd,
    form = form,
    dur_min = dur_min,
    dur_usual = dur_usual,
    dur_max = dur_max,
    status = status,
    source = source,
    note = note,
    stringsAsFactors = FALSE
  ))
}

rxp_write_design <- function(rows) {
  path <- tempfile(fileext = ".xlsx", tmpdir = tempdir())
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "packages")
  openxlsx::writeData(wb, "packages", rows)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  return(path)
}

# One person, 10 dispensings 150 days apart of the one designed package.
rxp_dates <- as.Date("2018-06-01") + 150 * (0:9)

rxp_lmed <- function(lnmn = "Simvastatin X 20 mg", fddd = 30) {
  return(data.table::data.table(
    lopnr = 1L,
    edatum = rxp_dates,
    atc = "C10AA01",
    produkt = "Simvastatin X",
    lnmn = lnmn,
    fddd = fddd
  ))
}

# Weekly rows 2020-01 to 2021-52, and annual rows for 1900 to 2019.
rxp_skeleton <- function() {
  return(create_skeleton(1L, 2020, "2021-52"))
}

# The skeleton rows that overlap the closed interval [start, end] of a
# period, found day by day: each day of a period marks its ISO week, or the
# annual row of its ISO year before the weekly rows start in 2020.
rxp_expected_rows <- function(skeleton, per) {
  days <- as.Date(unlist(Map(seq, as.Date(per$start), as.Date(per$end), by = 1L)))
  week <- cstime::date_to_isoyearweek_c(days)
  year <- as.integer(substr(week, 1L, 4L))
  key <- ifelse(year < 2020L, paste0(year, "-**"), week)
  return(skeleton$isoyearweek %in% key)
}

rxp_codes <- list(rxpre2dup_statin = "C10AA")

test_that("1. a design file round trip keeps every value", {
  rows <- rbind(
    rxp_design_rows(),
    rxp_design_rows(
      atc = "C10AA05",
      produkt = "Atorvastatin Y",
      lnmn = "Atorvastatin Y 40 mg",
      pack_ddd = 98,
      dur_min = 49,
      dur_usual = 98,
      dur_max = 196,
      status = "auto",
      source = NA_character_,
      note = "checked"
    )
  )
  design <- rxpre2dup_read_design(rxp_write_design(rows))

  expect_s3_class(design, "data.table")
  expect_identical(
    names(design),
    c(
      "package_id",
      "atc",
      "produkt",
      "lnmn",
      "pack_ddd",
      "form",
      "dur_min",
      "dur_usual",
      "dur_max",
      "status",
      "source",
      "note",
      "ddd_usual",
      "ddd_low"
    )
  )
  expect_identical(design$package_id, 1:2)
  for (col in names(rows)) {
    expect_identical(design[[col]], rows[[col]], label = col)
  }
  expect_identical(design$ddd_usual, c(30 / 30, 98 / 98))
  expect_identical(design$ddd_low, c(30 / 60, 98 / 196))
})

test_that("2. rxpre2dup_read_design() refuses each broken file", {
  # A missing column.
  rows <- rxp_design_rows()
  rows$form <- NULL
  expect_error(
    rxpre2dup_read_design(rxp_write_design(rows)),
    "is missing these columns: form"
  )

  # A status other than auto or approved, on data row 2 (sheet row 3).
  rows <- rbind(
    rxp_design_rows(),
    rxp_design_rows(lnmn = "Simvastatin X 40 mg", status = "proposed")
  )
  expect_error(
    rxpre2dup_read_design(rxp_write_design(rows)),
    "sheet row 3: status MUST be 'auto' or 'approved'",
    fixed = TRUE
  )

  # dur_min > dur_max.
  rows <- rxp_design_rows(dur_min = 70, dur_usual = 30, dur_max = 60)
  expect_error(
    rxpre2dup_read_design(rxp_write_design(rows)),
    "sheet row 2: dur_min <= dur_usual <= dur_max MUST hold",
    fixed = TRUE
  )

  # A duplicate key: the same atc and produkt, lnmn equal after
  # tolower(trimws()), and pack_ddd closer than 0.01.
  rows <- rbind(
    rxp_design_rows(),
    rxp_design_rows(lnmn = " SIMVASTATIN X 20 MG ", pack_ddd = 30.005)
  )
  expect_error(
    rxpre2dup_read_design(rxp_write_design(rows)),
    "sheet row 2, 3: two rows with the same atc, produkt and lnmn",
    fixed = TRUE
  )

  # The control: the same rows with pack_ddd 0.01 apart are one valid file.
  rows$pack_ddd[2L] <- 30.01
  expect_identical(nrow(rxpre2dup_read_design(rxp_write_design(rows))), 2L)
})

test_that("3. the column is TRUE exactly on the rows that overlap a period", {
  design <- rxpre2dup_read_design(rxp_write_design(rxp_design_rows()))
  lmed <- rxp_lmed()
  sk <- rxp_skeleton()
  expect_message(
    add_rxpre2dup(sk, lmed, codes = rxp_codes, design = design),
    "share of purchases without a design row"
  )

  per <- .pre2dup_periods(
    purchases = data.table::data.table(
      id = 1L,
      atc = "C10AA01",
      package_id = 1L,
      date = rxp_dates,
      ratio = 1,
      ddd = 30
    ),
    packages = design[,
      c(
        "package_id",
        "atc",
        "dur_min",
        "dur_usual",
        "dur_max",
        "ddd_low",
        "ddd_usual"
      ),
      with = FALSE
    ],
    atc_parameters = pre2dup_atc_parameters
  )
  expected <- rxp_expected_rows(sk, per)

  # The fixture exercises what the test claims: 10 periods, some of them in
  # the annual rows, every one longer than one week, and weeks between them.
  expect_identical(nrow(per), 10L)
  expect_true(all(as.integer(per$end - per$start) >= 7L))
  expect_true(any(expected[sk$is_isoyear]))
  expect_true(any(expected[!sk$is_isoyear]))
  expect_true(any(!expected[!sk$is_isoyear]))

  expect_type(sk$rxpre2dup_statin, "logical")
  expect_identical(sk$rxpre2dup_statin, expected)
})

test_that("4. a codes name without the rxpre2dup_ prefix stops", {
  design <- rxpre2dup_read_design(rxp_write_design(rxp_design_rows()))
  sk <- rxp_skeleton()
  expect_error(
    add_rxpre2dup(
      sk,
      rxp_lmed(),
      codes = list(rxpre2dup_statin = "C10AA", statin = "C10AA"),
      design = design
    ),
    "Every name in codes MUST start with 'rxpre2dup_'. These do not: statin",
    fixed = TRUE
  )
  expect_false("rxpre2dup_statin" %in% names(sk))
})

test_that("5. a dispensing whose lnmn differs only in case matches the design row", {
  design <- rxpre2dup_read_design(rxp_write_design(rxp_design_rows()))

  sk_exact <- rxp_skeleton()
  suppressMessages(
    add_rxpre2dup(sk_exact, rxp_lmed(), codes = rxp_codes, design = design)
  )
  sk_case <- rxp_skeleton()
  suppressMessages(
    add_rxpre2dup(
      sk_case,
      rxp_lmed(lnmn = "SIMVASTATIN x 20 MG"),
      codes = rxp_codes,
      design = design
    )
  )
  expect_identical(
    attr(sk_case, "package_coverage")$share_without_package,
    0
  )
  expect_identical(sk_case$rxpre2dup_statin, sk_exact$rxpre2dup_statin)

  # The control: another strength matches no design row.
  sk_other <- rxp_skeleton()
  suppressMessages(
    add_rxpre2dup(
      sk_other,
      rxp_lmed(lnmn = "Simvastatin X 40 mg"),
      codes = rxp_codes,
      design = design
    )
  )
  expect_identical(
    attr(sk_other, "package_coverage")$share_without_package,
    1
  )
})

test_that("6. add_rxpre2dup() leaves every pre-existing column and the row count unchanged", {
  design <- rxpre2dup_read_design(rxp_write_design(rxp_design_rows()))
  sk <- rxp_skeleton()
  sk[, rx_other := isoyear > 2020L]
  before <- data.table::copy(sk)
  suppressMessages(
    add_rxpre2dup(sk, rxp_lmed(), codes = rxp_codes, design = design)
  )
  expect_identical(nrow(sk), nrow(before))
  expect_identical(names(sk), c(names(before), "rxpre2dup_statin"))
  for (col in names(before)) {
    expect_identical(sk[[col]], before[[col]], label = col)
  }
})

test_that("7. hospital stays reach PRE2DUP, and a stay with a missing date or utdatum <= indatum is dropped", {
  design <- rxpre2dup_read_design(rxp_write_design(rxp_design_rows()))
  stay <- data.table::data.table(
    lopnr = 1L,
    indatum = as.Date("2020-01-25"),
    utdatum = as.Date("2020-02-14")
  )
  hosp <- rbind(
    stay,
    data.table::data.table(
      lopnr = 1L,
      indatum = as.Date(c("2021-01-10", "2021-02-10")),
      utdatum = as.Date(c("2021-01-10", "2021-02-01"))
    )
  )
  # The port stops on a stay with utdatum <= indatum, so the call MUST
  # succeed only if add_rxpre2dup() drops those stays first.
  sk_hosp <- rxp_skeleton()
  expect_no_error(suppressMessages(
    add_rxpre2dup(sk_hosp, rxp_lmed(), hosp, codes = rxp_codes, design = design)
  ))
  sk_none <- rxp_skeleton()
  suppressMessages(
    add_rxpre2dup(sk_none, rxp_lmed(), codes = rxp_codes, design = design)
  )

  # One more stay, with a missing utdatum. The call MUST succeed, and the
  # result MUST equal the run without that stay.
  hosp_na <- rbind(
    hosp,
    data.table::data.table(
      lopnr = 1L,
      indatum = as.Date("2020-06-25"),
      utdatum = as.Date(NA)
    )
  )
  sk_na <- rxp_skeleton()
  expect_no_error(suppressMessages(
    add_rxpre2dup(sk_na, rxp_lmed(), hosp_na, codes = rxp_codes, design = design)
  ))
  expect_identical(sk_na$rxpre2dup_statin, sk_hosp$rxpre2dup_statin)

  per <- .pre2dup_periods(
    purchases = data.table::data.table(
      id = 1L,
      atc = "C10AA01",
      package_id = 1L,
      date = rxp_dates,
      ratio = 1,
      ddd = 30
    ),
    packages = design[,
      c(
        "package_id",
        "atc",
        "dur_min",
        "dur_usual",
        "dur_max",
        "ddd_low",
        "ddd_usual"
      ),
      with = FALSE
    ],
    atc_parameters = pre2dup_atc_parameters,
    hosp = data.table::data.table(
      id = 1L,
      admission = stay$indatum,
      discharge = stay$utdatum
    )
  )
  expect_identical(sk_hosp$rxpre2dup_statin, rxp_expected_rows(sk_hosp, per))
  # The stay changes the periods, so the comparison above is not vacuous.
  expect_false(identical(sk_hosp$rxpre2dup_statin, sk_none$rxpre2dup_statin))
})

test_that("8. fddd that is NA, not finite or negative is dropped, and fddd = 0 is kept", {
  design <- rxpre2dup_read_design(rxp_write_design(rxp_design_rows()))
  sk_clean <- rxp_skeleton()
  suppressMessages(
    add_rxpre2dup(sk_clean, rxp_lmed(), codes = rxp_codes, design = design)
  )

  bad <- data.table::data.table(
    lopnr = 1L,
    edatum = as.Date(c("2020-04-01", "2020-05-01", "2020-06-01")),
    atc = "C10AA01",
    produkt = "Simvastatin X",
    lnmn = "Simvastatin X 20 mg",
    fddd = c(NA_real_, Inf, -30)
  )
  sk_bad <- rxp_skeleton()
  suppressMessages(
    add_rxpre2dup(
      sk_bad,
      rbind(rxp_lmed(), bad),
      codes = rxp_codes,
      design = design
    )
  )
  expect_identical(sk_bad$rxpre2dup_statin, sk_clean$rxpre2dup_statin)
  expect_identical(attr(sk_bad, "package_coverage")$n_purchases, 10L)

  zero <- bad[1L]
  zero[, fddd := 0]
  sk_zero <- rxp_skeleton()
  expect_message(
    add_rxpre2dup(
      sk_zero,
      rbind(rxp_lmed(), zero),
      codes = rxp_codes,
      design = design
    ),
    "rxpre2dup_statin C10AA01 9.1%.",
    fixed = TRUE
  )
  cov <- attr(sk_zero, "package_coverage")
  expect_identical(cov$n_purchases, 11L)
  expect_equal(cov$share_without_package, 1 / 11)
})

test_that("9. a dispensing with fddd = 0 matches no design row, even one with pack_ddd < 0.005", {
  design <- rxpre2dup_read_design(rxp_write_design(rxp_design_rows(
    pack_ddd = 0.001
  )))
  sk <- rxp_skeleton()
  suppressMessages(
    add_rxpre2dup(sk, rxp_lmed(fddd = 0), codes = rxp_codes, design = design)
  )
  cov <- attr(sk, "package_coverage")
  expect_identical(cov$n_purchases, 10L)
  expect_identical(cov$share_without_package, 1)
})

test_that("10. a numeric design column with text cells names each text cell's sheet row", {
  rows <- rbind(
    rxp_design_rows(),
    rxp_design_rows(lnmn = "Simvastatin X 40 mg"),
    rxp_design_rows(lnmn = "Simvastatin X 80 mg")
  )
  path <- tempfile(fileext = ".xlsx", tmpdir = tempdir())
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "packages")
  openxlsx::writeData(wb, "packages", rows)
  # pack_ddd is column 4. Sheet row 2 keeps its number cell 30. Sheet rows 3
  # and 4 get text cells, and openxlsx then reads the column as text.
  openxlsx::writeData(wb, "packages", "12a", startCol = 4L, startRow = 3L)
  openxlsx::writeData(wb, "packages", "12", startCol = 4L, startRow = 4L)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  expect_type(openxlsx::read.xlsx(path, sheet = "packages")$pack_ddd, "character")

  # The message is captured, so a wrong row list is an expectation failure.
  # expect_error() re-throws an error whose message does not match.
  msg <- tryCatch(
    {
      rxpre2dup_read_design(path)
      "no error"
    },
    error = conditionMessage
  )
  expect_match(
    msg,
    "breaks a rule on sheet row 3, 4: column pack_ddd MUST hold numbers.",
    fixed = TRUE
  )
})
