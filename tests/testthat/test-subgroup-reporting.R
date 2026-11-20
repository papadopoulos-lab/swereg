# The Effect modification sheet reads the per-level decision of the
# minimum-events rule.
#
# The fixture of helper-horizon-subgroup.R runs the real `$s3_analyze()` and
# `$export_tables()`. In ETT00001 the whole ETT passes and the level "thin" of
# `rd_sg` fails. In ETT00006 both levels pass. The sheet MUST write NE in the
# ratio cells of a failing level, and in the interaction-test cells of the
# "all" row when any level fails.

skip_if_not_installed("data.table")

# The rows of the Effect modification sheet for one ETT and `rd_sg`, as text.
.sr_rows <- function(xlsx, ett) {
  m <- openxlsx::read.xlsx(
    xlsx,
    sheet = "Effect modification",
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  m[] <- lapply(m, as.character)
  h <- which(apply(m, 1L, function(r) "Subgroup" %in% r))[1L]
  out <- m[-seq_len(h), , drop = FALSE]
  names(out) <- as.character(unlist(m[h, ]))
  return(out[out$Enrollment == ett & out$Subgroup == "rd_sg", , drop = FALSE])
}

# One cell of the sheet, by level and column.
.sr_cell <- function(rows, level, col) {
  hit <- which(rows$Level == level)
  expect_length(hit, 1L)
  return(rows[[col]][hit])
}

# TRUE when the cell holds a finite number.
.sr_num <- function(x) {
  return(is.finite(suppressWarnings(as.numeric(x))))
}

# TRUE when the cell holds an interval "(lo, hi)" of two numbers.
.sr_ci <- function(x) {
  return(!is.na(x) && grepl("^\\(-?[0-9.]+, -?[0-9.]+\\)$", x))
}

SR_RATIO <- list(
  pp = c("PP IRR", "PP 95% CI"),
  itt = c("ITT IRR", "ITT 95% CI")
)
SR_EM <- list(
  pp = c("EM p (PP)", "EM ratio (PP)"),
  itt = c("EM p (ITT)", "EM ratio (ITT)")
)

test_that("(1) the thin level reads NE in its PP and ITT ratio and interval cells", {
  .hl_skip()
  fx <- .hl_fixture()
  expect_null(fx$export_error)
  r <- .sr_rows(fx$xlsx, "ETT00001")
  expect_identical(sort(r$Level), c("all", "big", "thin"))
  for (est in names(SR_RATIO)) {
    for (col in SR_RATIO[[est]]) {
      expect_identical(.sr_cell(r, "thin", col), "NE", info = col)
    }
  }
})

test_that("(2) the passing level and the ratio cells of the \"all\" row hold numbers", {
  .hl_skip()
  fx <- .hl_fixture()
  r <- .sr_rows(fx$xlsx, "ETT00001")
  for (lv in c("big", "all")) {
    for (est in names(SR_RATIO)) {
      cols <- SR_RATIO[[est]]
      expect_true(.sr_num(.sr_cell(r, lv, cols[1L])), info = paste(lv, est))
      expect_true(.sr_ci(.sr_cell(r, lv, cols[2L])), info = paste(lv, est))
    }
  }
})

test_that("(3) the \"all\" row reads NE in every interaction-test cell when a level fails", {
  .hl_skip()
  fx <- .hl_fixture()
  r <- .sr_rows(fx$xlsx, "ETT00001")
  for (est in names(SR_EM)) {
    for (col in SR_EM[[est]]) {
      expect_identical(.sr_cell(r, "all", col), "NE", info = col)
    }
  }
})

test_that("(4) when both levels pass, every ratio and interaction-test cell holds a number", {
  .hl_skip()
  fx <- .hl_fixture()
  r <- .sr_rows(fx$xlsx, "ETT00006")
  expect_identical(sort(r$Level), c("all", "big", "thin"))
  for (lv in c("all", "big", "thin")) {
    for (est in names(SR_RATIO)) {
      cols <- SR_RATIO[[est]]
      expect_true(.sr_num(.sr_cell(r, lv, cols[1L])), info = paste(lv, est))
      expect_true(.sr_ci(.sr_cell(r, lv, cols[2L])), info = paste(lv, est))
    }
  }
  for (est in names(SR_EM)) {
    for (col in SR_EM[[est]]) {
      expect_true(.sr_num(.sr_cell(r, "all", col)), info = col)
    }
  }
})

test_that("(5) a part-year ETT and an exempt enrollment follow their own per-level status", {
  .hl_skip()
  fx <- .hl_fixture()
  # ETT00002 takes the failing level "thin" of its source ETT00001.
  r <- .sr_rows(fx$xlsx, "ETT00002")
  for (est in names(SR_RATIO)) {
    for (col in SR_RATIO[[est]]) {
      expect_identical(.sr_cell(r, "thin", col), "NE", info = col)
    }
    expect_true(.sr_num(.sr_cell(r, "big", SR_RATIO[[est]][1L])), info = est)
    for (col in SR_EM[[est]]) {
      expect_identical(.sr_cell(r, "all", col), "NE", info = col)
    }
  }
  # ETT00005 is exempt: its failing level "thin" is reported.
  r <- .sr_rows(fx$xlsx, "ETT00005")
  for (est in names(SR_RATIO)) {
    expect_true(.sr_num(.sr_cell(r, "thin", SR_RATIO[[est]][1L])), info = est)
    for (col in SR_EM[[est]]) {
      expect_true(.sr_num(.sr_cell(r, "all", col)), info = col)
    }
  }
})
