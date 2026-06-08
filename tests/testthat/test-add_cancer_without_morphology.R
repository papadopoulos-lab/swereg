# add_cancer_without_morphology(): cancer topography matching across BOTH
# ICD-O columns (icdo10 = ICD-O/2, complete; icdo3 = ICD-O/3, partial).
# These columns carry ICD-10 neoplasm SITE codes (C50 = breast, ...), which is
# why ICD-10 patterns match them.

skip_if_not_installed("data.table")

test_that("matches topography in BOTH icdo10 and icdo3", {
  skeleton <- swereg::create_skeleton(1:4, "2010-01-01", "2010-12-31")
  d <- data.table::data.table(
    lopnr = 1:4,
    indatum = as.Date(rep("2010-03-01", 4)),
    icdo10 = c("C509", "C541", NA, "C500"), # breast, endometrium, (none), in-situ breast
    icdo3 = c("C509", NA, "C569", "C500") # ovary recorded only in icdo3 for id 3
  )
  swereg::add_cancer_without_morphology(
    skeleton,
    d,
    "lopnr",
    codes = list(
      breast = c("C50"),
      endometrial = c("C54", "C55"),
      ovary = c("C56")
    )
  )
  flagged <- function(i, col) any(skeleton[id == i][[col]], na.rm = TRUE)
  expect_true(flagged(1, "breast")) # present in both columns
  expect_true(flagged(2, "endometrial")) # icdo10 ONLY -> proves icdo10 is searched
  expect_true(flagged(3, "ovary")) # icdo3 ONLY  -> proves icdo3 is searched
  expect_false(flagged(3, "breast")) # ovary is not breast
})

test_that("in-situ breast lives under C50 topography, not D05", {
  skeleton <- swereg::create_skeleton(1L, "2010-01-01", "2010-12-31")
  d <- data.table::data.table(
    lopnr = 1L,
    indatum = as.Date("2010-03-01"),
    icdo10 = "C500"
  )
  swereg::add_cancer_without_morphology(
    skeleton,
    d,
    "lopnr",
    codes = list(via_c50 = c("C50"), via_d05 = c("D05"))
  )
  expect_true(any(skeleton$via_c50, na.rm = TRUE)) # C50 catches in-situ breast
  expect_false(any(skeleton$via_d05, na.rm = TRUE)) # D05 does not (register has no D05)
})

test_that("errors when neither icdo10 nor icdo3 column is present", {
  skeleton <- swereg::create_skeleton(1L, "2010-01-01", "2010-12-31")
  d <- data.table::data.table(
    lopnr = 1L,
    indatum = as.Date("2010-03-01"),
    hdia = "C509"
  )
  expect_error(
    swereg::add_cancer_without_morphology(
      skeleton,
      d,
      "lopnr",
      codes = list(breast = "C50")
    ),
    "icdo10"
  )
})
