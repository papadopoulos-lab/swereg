# Pin the `"!"` row-level veto in add_rx() (introduced in 26.4.28),
# parallel to the same syntax in add_diagnoses().
#
# Final rule per named code:
#   row_matches <- (any positive pattern matches) AND
#                  (no "!" pattern matches)
#
# These tests cover:
#   - basic veto carves out a sub-prefix
#   - multiple positives + multiple negatives compose correctly
#   - veto is independent per named code (no leak across list entries)
#   - all-negative pattern set produces an empty column
#   - all-positive (no `!`) preserves prior behaviour (regression)
#   - works for source = "atc" (prefix) AND source = "produkt" (exact)
#   - veto interacts correctly with the per-week aggregation
#     (same person, same week, mix of vetoed + non-vetoed Rxes)

skip_if_not_installed("data.table")

.rx_skel <- function(ids = c(1L, 2L)) {
  swereg::create_skeleton(
    ids = ids,
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
}

.rx_dt <- function(rows) {
  dt <- data.table::as.data.table(rows)
  data.table::setnames(dt, names(dt), tolower(names(dt)))
  if (!"lopnr" %in% names(dt) && "id" %in% names(dt)) {
    data.table::setnames(dt, "id", "lopnr")
  }
  dt
}

# ---------------------------------------------------------------------------
# atc source
# ---------------------------------------------------------------------------

test_that("add_rx atc: bare positive only -> all matches kept (regression)", {
  skel <- .rx_skel(1L)
  rx <- .rx_dt(list(
    lopnr  = c(1L,        1L),
    edatum = as.Date(c("2020-03-02", "2020-04-02")),
    atc    = c("C10AA01", "C10AX09"),  # statin + non-statin
    fddd   = c(30, 30)
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(rx_c10a = "C10A"),
                 source = "atc")
  # Both prescriptions are within C10A -> both weeks must be TRUE.
  in_mar <- skel[id == 1L & is_isoyear == FALSE &
                 isoyearweeksun >= as.Date("2020-03-01") &
                 isoyearweeksun <= as.Date("2020-03-15"), rx_c10a]
  in_apr <- skel[id == 1L & is_isoyear == FALSE &
                 isoyearweeksun >= as.Date("2020-04-01") &
                 isoyearweeksun <= as.Date("2020-04-15"), rx_c10a]
  expect_true(any(in_mar))
  expect_true(any(in_apr))
})

test_that("add_rx atc: ! carves out a sub-prefix (non-statins only)", {
  skel <- .rx_skel(1L)
  rx <- .rx_dt(list(
    lopnr  = c(1L,        1L),
    edatum = as.Date(c("2020-03-02", "2020-04-02")),
    atc    = c("C10AA01", "C10AX09"),  # statin + non-statin
    fddd   = c(30, 30)
  ))
  # "any lipid-modifying agent except the C10AA / C10AB / C10AC sub-classes."
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(rx_c10_nonstatin = c("C10A", "!C10AA")),
                 source = "atc")
  # March (C10AA01, vetoed) -> FALSE during that interval.
  in_mar <- skel[id == 1L & is_isoyear == FALSE &
                 isoyearweeksun >= as.Date("2020-03-01") &
                 isoyearweeksun <= as.Date("2020-03-15"), rx_c10_nonstatin]
  expect_false(any(in_mar),
    info = "C10AA01 should be vetoed by !C10AA")
  # April (C10AX09, non-statin) -> TRUE during that interval.
  in_apr <- skel[id == 1L & is_isoyear == FALSE &
                 isoyearweeksun >= as.Date("2020-04-01") &
                 isoyearweeksun <= as.Date("2020-04-15"), rx_c10_nonstatin]
  expect_true(any(in_apr))
})

test_that("add_rx atc: multiple positives + multiple negatives compose correctly", {
  skel <- .rx_skel(1L)
  # 14-day fddd keeps each Rx's coverage window inside its month, so
  # the assertions below don't accidentally cross between Rxes.
  rx <- .rx_dt(list(
    lopnr  = c(1L, 1L, 1L, 1L),
    edatum = as.Date(c("2020-02-03", "2020-03-02", "2020-04-06", "2020-05-04")),
    atc    = c("C10AA01",  # statin       -> keep C10A but veto !C10AA = FALSE
               "C10AB02",  # fibrate      -> veto !C10AB              = FALSE
               "C10AX06",  # other C10A                               = TRUE
               "A10BA02"), # antidiabetic (not in the C10A union)     = FALSE
    fddd   = rep(14, 4)
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(rx_c10_nonstatin = c("C10A", "!C10AA", "!C10AB")),
                 source = "atc")

  feb <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-02-01") &
              isoyearweeksun <= as.Date("2020-02-15"), rx_c10_nonstatin]
  mar <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-03-01") &
              isoyearweeksun <= as.Date("2020-03-15"), rx_c10_nonstatin]
  apr <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-04-01") &
              isoyearweeksun <= as.Date("2020-04-15"), rx_c10_nonstatin]
  may <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-05-01") &
              isoyearweeksun <= as.Date("2020-05-15"), rx_c10_nonstatin]
  expect_false(any(feb), info = "C10AA01 vetoed by !C10AA")
  expect_false(any(mar), info = "C10AB02 vetoed by !C10AB")
  expect_true(any(apr),  info = "C10AX06 should match C10A and not be vetoed")
  expect_false(any(may), info = "A10BA02 (antidiabetic) is outside the C10A union")
})

test_that("add_rx atc: only negative patterns (no positive) -> empty column", {
  skel <- .rx_skel(1L)
  rx <- .rx_dt(list(
    lopnr  = 1L,
    edatum = as.Date("2020-03-02"),
    atc    = "C10AA01",
    fddd   = 30
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(rx_only_neg = c("!C10AA")),
                 source = "atc")
  expect_type(skel$rx_only_neg, "logical")
  expect_false(any(skel$rx_only_neg),
               info = "all-negative pattern set must produce an empty column (no positive matcher)")
})

test_that("add_rx atc: vetoes are independent per named code (no leak)", {
  skel <- .rx_skel(1L)
  rx <- .rx_dt(list(
    lopnr  = c(1L,        1L),
    edatum = as.Date(c("2020-03-02", "2020-04-02")),
    atc    = c("C10AA01", "C10AX06"),
    fddd   = c(30, 30)
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(
                   rx_c10_all       = "C10A",              # both kept
                   rx_c10_nonstatin = c("C10A", "!C10AA")  # only C10AX
                 ),
                 source = "atc")
  # The veto in rx_c10_nonstatin must not leak into rx_c10_all.
  mar_all  <- skel[id == 1L & is_isoyear == FALSE &
                   isoyearweeksun >= as.Date("2020-03-01") &
                   isoyearweeksun <= as.Date("2020-03-15"), rx_c10_all]
  mar_atyp <- skel[id == 1L & is_isoyear == FALSE &
                   isoyearweeksun >= as.Date("2020-03-01") &
                   isoyearweeksun <= as.Date("2020-03-15"), rx_c10_nonstatin]
  expect_true(any(mar_all),
              info = "rx_c10_all must include C10AA01 (no veto on this entry)")
  expect_false(any(mar_atyp),
               info = "rx_c10_nonstatin must veto C10AA01")
})

test_that("add_rx atc: per-(id, isoyearweek) aggregation respects the veto", {
  # Same person has TWO prescriptions whose coverage windows overlap in
  # the same week: one vetoed, one positive. The positive must win for
  # the affected week (the vetoed Rx is removed before aggregation, so
  # only the positive Rx contributes).
  skel <- .rx_skel(1L)
  rx <- .rx_dt(list(
    lopnr  = c(1L, 1L),
    edatum = as.Date(c("2020-03-02", "2020-03-02")),
    atc    = c("C10AA01", "C10AX06"),  # vetoed + kept, same start day
    fddd   = c(30, 30)
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(rx_c10_nonstatin = c("C10A", "!C10AA")),
                 source = "atc")
  mar <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-03-01") &
              isoyearweeksun <= as.Date("2020-03-15"), rx_c10_nonstatin]
  expect_true(any(mar),
    info = paste0(
      "When a vetoed Rx and a non-vetoed Rx overlap in the same week, ",
      "the non-vetoed Rx must still drive TRUE for that week"))
})

# ---------------------------------------------------------------------------
# produkt source
# ---------------------------------------------------------------------------

test_that("add_rx produkt: ! exact-match veto excludes named brands", {
  skel <- .rx_skel(1L)
  rx <- .rx_dt(list(
    lopnr   = c(1L,            1L,          1L),
    edatum  = as.Date(c("2020-03-02", "2020-04-02", "2020-05-04")),
    atc     = c("A10BA02",     "A10BA02",   "A10BA02"),
    fddd    = c(30, 30, 30),
    produkt = c("Sertralin Sandoz", "Sertralin Krka", "Zoloft")
  ))
  # "Any sertralin product except Sandoz / Krka generics."
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(rx_branded = c("Sertralin Sandoz", "Sertralin Krka",
                                             "Zoloft",
                                             "!Sertralin Sandoz", "!Sertralin Krka")),
                 source = "produkt")
  may <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-05-01") &
              isoyearweeksun <= as.Date("2020-05-15"), rx_branded]
  expect_true(any(may), info = "Zoloft must remain TRUE")

  mar <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-03-01") &
              isoyearweeksun <= as.Date("2020-03-15"), rx_branded]
  apr <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-04-01") &
              isoyearweeksun <= as.Date("2020-04-15"), rx_branded]
  expect_false(any(mar), info = "Sertralin Sandoz must be vetoed")
  expect_false(any(apr), info = "Sertralin Krka must be vetoed")
})

test_that("add_rx produkt: ! is exact-match (does NOT prefix-veto)", {
  # "produkt" semantics are exact-match, not prefix. A "!Sertralin"
  # veto must NOT mask "Sertralin Sandoz" (different exact value).
  skel <- .rx_skel(1L)
  rx <- .rx_dt(list(
    lopnr   = c(1L,                  1L),
    edatum  = as.Date(c("2020-03-02", "2020-04-02")),
    atc     = c("A10BA02",           "A10BA02"),
    fddd    = c(30,                  30),
    produkt = c("Sertralin",         "Sertralin Sandoz")
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list(rx_brand = c("Sertralin", "Sertralin Sandoz",
                                           "!Sertralin")),
                 source = "produkt")
  apr <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-04-01") &
              isoyearweeksun <= as.Date("2020-04-15"), rx_brand]
  expect_true(any(apr),
    info = "produkt veto is EXACT match -- 'Sertralin Sandoz' must survive a '!Sertralin' veto")
  mar <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-03-01") &
              isoyearweeksun <= as.Date("2020-03-15"), rx_brand]
  expect_false(any(mar),
    info = "produkt veto matches 'Sertralin' exactly")
})

test_that("add_rx: ! syntax produces a logical column with no NA / no crash", {
  # Robustness: even when no row matches in the lmed table at all, the
  # column is still allocated and typed correctly.
  skel <- .rx_skel(c(1L, 2L))
  rx <- .rx_dt(list(
    lopnr  = 99L,  # not in skeleton
    edatum = as.Date("2020-03-02"),
    atc    = "C10AA01",
    fddd   = 30
  ))
  expect_warning(
    swereg::add_rx(skel, rx, id_name = "lopnr",
                   codes = list(rx_c10_nonstatin = c("C10A", "!C10AA")))
  )
  expect_type(skel$rx_c10_nonstatin, "logical")
  expect_false(any(skel$rx_c10_nonstatin))
  expect_false(any(is.na(skel$rx_c10_nonstatin)))
})
