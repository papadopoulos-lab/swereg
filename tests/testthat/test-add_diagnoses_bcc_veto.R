# Pin how a "cancer excluding basal cell carcinoma" code list behaves.
#
# The patient register and the cause-of-death register carry ICD-10-SE codes
# without a dot, where the fifth character subdivides C44 (C449B). A list of
# C00-C97 plus "!" vetoes on C44[0-9][B-E] then:
#   - masks a record that carries a vetoed code in ANY scanned column;
#   - masks the whole record, so another cancer code on that record is lost;
#   - keeps a C44 code with no fifth character (C443) or another letter
#     (C443S), because the veto only matches the exact five-character prefix.
# The cancer register carries four-character ICD-O topography (C445), so a
# veto on C44[0-9][B-E] can never match there. Leaving C44 out of the list is
# the only topography-level way to keep BCC out.

skip_if_not_installed("data.table")

.bcc_codes <- list(
  cancer_excl_bcc = c(
    sprintf("C%02d", 0:97),
    paste0("!C44", rep(0:9, each = 4L), rep(LETTERS[2:5], times = 10L))
  )
)

.bcc_flag <- function(skel, i) any(skel[id == i, cancer_excl_bcc])

test_that("BCC veto list: C44 is vetoed for exactly B to E at every subsite", {
  vetoed <- sub("^!", "", grep("^!", .bcc_codes$cancer_excl_bcc, value = TRUE))
  expect_length(vetoed, 40L)
  expect_setequal(
    vetoed,
    as.vector(outer(paste0("C44", 0:9), c("B", "C", "D", "E"), paste0))
  )
})

test_that("add_diagnoses: BCC veto masks BCC and keeps other cancer", {
  skel <- swereg::create_skeleton(1:8, 2020, "2020-52")
  dx <- data.table::data.table(
    lopnr = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 8L),
    indatum = as.Date(c(
      "2020-03-02", "2020-03-02", "2020-03-02", "2020-03-02",
      "2020-03-02", "2020-03-02", "2020-03-02", "2020-03-02", "2020-03-04"
    )),
    hdia = c("C449B", "C509", "C509", "C443", "C443S", "C449B", "C440E", "C449E", "C509"),
    dia1 = c(NA, "C445E", NA, NA, NA, "C509", NA, NA, NA)
  )
  swereg::add_diagnoses(skel, dx, id_name = "lopnr", codes = .bcc_codes)

  expect_false(.bcc_flag(skel, 1L)) # BCC in hdia
  expect_false(.bcc_flag(skel, 2L)) # BCC in dia1 vetoes the C509 in hdia: dia1 is scanned
  expect_true(.bcc_flag(skel, 3L)) # breast cancer
  expect_true(.bcc_flag(skel, 4L)) # C44 with no fifth character counts
  expect_true(.bcc_flag(skel, 5L)) # C44 with a letter outside B to E counts
  expect_false(.bcc_flag(skel, 6L)) # the veto masks the whole record
  expect_false(.bcc_flag(skel, 7L)) # subsite 0, letter E
  expect_true(.bcc_flag(skel, 8L)) # another record in the same week still counts
})

test_that("add_cods: BCC veto applies to underlying and multiple causes", {
  skel <- swereg::create_skeleton(1:3, 2020, "2020-52")
  cods <- data.table::data.table(
    lopnr = 1:3,
    dodsdat = as.Date("2020-03-02"),
    ulorsak = c("C449B", "C509", "C509"),
    morsak1 = c(NA, NA, "C443D")
  )
  swereg::add_cods(skel, cods, id_name = "lopnr", codes = .bcc_codes)

  expect_false(.bcc_flag(skel, 1L))
  expect_true(.bcc_flag(skel, 2L))
  expect_false(.bcc_flag(skel, 3L)) # BCC in morsak1 vetoes the C509 in ulorsak
})

test_that("add_cancer_without_morphology: a B to E veto never matches topography", {
  d <- data.table::data.table(
    lopnr = 1:3,
    indatum = as.Date("2010-03-01"),
    icdo10 = c("C445", "C509", "C437"),
    icdo3 = c("C445", "C509", "C437")
  )
  skel <- swereg::create_skeleton(1:3, 2010, "2010-52")
  swereg::add_cancer_without_morphology(skel, d, "lopnr", codes = .bcc_codes)
  expect_true(.bcc_flag(skel, 1L)) # skin tumour still counts: veto is inert here

  skel2 <- swereg::create_skeleton(1:3, 2010, "2010-52")
  swereg::add_cancer_without_morphology(
    skel2, d, "lopnr",
    codes = list(cancer_excl_bcc = sprintf("C%02d", c(0:43, 45:97)))
  )
  expect_false(.bcc_flag(skel2, 1L)) # leaving C44 out keeps skin tumours out
  expect_true(.bcc_flag(skel2, 2L)) # breast
  expect_true(.bcc_flag(skel2, 3L)) # melanoma of skin is C43
})
