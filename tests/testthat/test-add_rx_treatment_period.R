# Pin treatment-period semantics for add_rx().
#
# add_rx() converts each prescription row (edatum, fddd) into a
# coverage interval [edatum, edatum + round(fddd)] and OR-s that into
# a logical column over the matching weeks.
#
# Matching:
#   - source = "atc": startsWith(lmed$atc, pattern); bare codes,
#     no anchoring needed -- prefix match.
#   - source = "produkt": exact match against `produkt` column via
#     %chin%.

skip_if_not_installed("data.table")

.rx_skeleton <- function(ids = c(1L, 2L)) {
  swereg::create_skeleton(
    ids = ids,
    date_min = as.Date("2020-01-06"),
    date_max = as.Date("2020-12-27")
  )
}

.rx_table <- function(rows) {
  dt <- data.table::as.data.table(rows)
  data.table::setnames(dt, names(dt), tolower(names(dt)))
  if (!"lopnr" %in% names(dt) && "id" %in% names(dt)) {
    data.table::setnames(dt, "id", "lopnr")
  }
  dt
}

test_that("add_rx: prescription on date D with duration N covers the interval", {
  skel <- .rx_skeleton(1L)
  rx <- .rx_table(list(
    lopnr   = 1L,
    edatum  = as.Date("2020-03-02"),
    atc     = "N06AB10",
    fddd    = 30
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list("rx_n06a" = "N06A"))

  # Week containing 2020-03-02 (start) should be TRUE.
  in_period <- skel[id == 1L & is_isoyear == FALSE &
                    isoyearweeksun >= as.Date("2020-03-01") &
                    isoyearweeksun <= as.Date("2020-03-15"), rx_n06a]
  expect_true(any(in_period))

  # Far before and far after must be FALSE.
  before <- skel[id == 1L & is_isoyear == FALSE &
                 isoyearweeksun < as.Date("2020-02-15"), rx_n06a]
  after <- skel[id == 1L & is_isoyear == FALSE &
                isoyearweeksun > as.Date("2020-06-01"), rx_n06a]
  expect_false(any(before))
  expect_false(any(after))
})

test_that("add_rx: NA fddd warns and drops the inverted interval, no crash", {
  skel <- .rx_skeleton(1L)
  rx <- .rx_table(list(
    lopnr   = 1L,
    edatum  = as.Date("2020-03-02"),
    atc     = "N06AB10",
    fddd    = NA_real_
  ))
  expect_warning(
    swereg::add_rx(skel, rx, id_name = "lopnr",
                   codes = list("rx_n06a" = "N06A")),
    regexp = "dropped|NA"
  )
  expect_type(skel$rx_n06a, "logical")
  expect_false(any(skel$rx_n06a))
})

test_that("add_rx: overlapping prescriptions OR together (still TRUE in union)", {
  skel <- .rx_skeleton(1L)
  rx <- .rx_table(list(
    lopnr   = c(1L, 1L),
    edatum  = as.Date(c("2020-03-02", "2020-03-15")),
    atc     = c("N06AB10", "N06AB10"),
    fddd    = c(30, 30)
  ))
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list("rx_n06a" = "N06A"))
  # The middle of the overlap (mid-March) must be TRUE.
  mid <- skel[id == 1L & is_isoyear == FALSE &
              isoyearweeksun >= as.Date("2020-03-15") &
              isoyearweeksun <= as.Date("2020-03-22"), rx_n06a]
  expect_true(all(mid))
})

test_that("add_rx: ATC source matches via startsWith on the `atc` column", {
  skel <- .rx_skeleton(1L)
  rx <- .rx_table(list(
    lopnr   = 1L,
    edatum  = as.Date("2020-03-02"),
    atc     = "N06AB10",   # SSRI subcategory
    fddd    = 30
  ))
  # Bare "N06A" prefix-matches "N06AB10".
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list("rx_n06a" = "N06A"),
                 source = "atc")
  expect_true(any(skel$rx_n06a))
})

test_that("add_rx: produkt source matches exactly on the `produkt` column", {
  skel <- .rx_skeleton(1L)
  rx <- .rx_table(list(
    lopnr   = 1L,
    edatum  = as.Date("2020-03-02"),
    atc     = "N06AB10",
    fddd    = 30,
    produkt = "Sertralin Sandoz 50 mg"
  ))
  # Exact-match must hit
  swereg::add_rx(skel, rx, id_name = "lopnr",
                 codes = list("rx_sertralin" = "Sertralin Sandoz 50 mg"),
                 source = "produkt")
  expect_true(any(skel$rx_sertralin))

  # Non-matching produkt must not fire (exact, not prefix)
  skel2 <- .rx_skeleton(1L)
  swereg::add_rx(skel2, rx, id_name = "lopnr",
                 codes = list("rx_other" = "Some Other Brand"),
                 source = "produkt")
  expect_false(any(skel2$rx_other))
})

test_that("add_rx: zero matching rows still produces an all-FALSE column", {
  skel <- .rx_skeleton(c(1L, 2L))
  rx <- .rx_table(list(
    lopnr   = 99L,  # not in skeleton
    edatum  = as.Date("2020-03-02"),
    atc     = "X99X99",
    fddd    = 30
  ))
  expect_warning(
    swereg::add_rx(skel, rx, id_name = "lopnr",
                   codes = list("rx_n06a" = "N06A"))
  )
  expect_type(skel$rx_n06a, "logical")
  expect_false(any(skel$rx_n06a))
})
