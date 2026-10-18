# The "Summary" sheet of `$export_tables()` holds one row per ETT.
#
# The test compares each Summary cell with the cells the "ITT results" and
# "PP results" sheets of the SAME workbook hold. It reads both sheets back
# from the written file. It recomputes no estimate, so a Summary that drifts
# from the results sheets fails here.
#
# The fixture is `.xp_plan("new")` from `helper-export_parity.R`. Its eight
# ETTs sweep the states the results sheets branch on: a full result, a
# skipped ratio, an ETT with no result, a ratio of 0 that is not estimable, a
# ratio above 100, and ETTs with and without a stored risk difference.

skip_if_not_installed("openxlsx")
skip_if_not_installed("withr")

ss_export <- function(plan) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = path)))
  return(path)
}

# The XML of one worksheet, found through the workbook relationships.
ss_sheet_xml <- function(path, sheet) {
  dir <- withr::local_tempdir()
  utils::unzip(path, exdir = dir)
  read_all <- function(...) {
    return(paste(readLines(file.path(dir, ...), warn = FALSE), collapse = ""))
  }
  wb_xml <- read_all("xl", "workbook.xml")
  rels <- read_all("xl", "_rels", "workbook.xml.rels")
  tag <- regmatches(
    wb_xml,
    regexpr(sprintf('<sheet [^>]*name="%s"[^>]*/>', sheet), wb_xml)
  )
  stopifnot(length(tag) == 1L)
  rid <- sub('.*r:id="([^"]+)".*', "\\1", tag)
  rel <- regmatches(
    rels,
    regexpr(sprintf('<Relationship [^>]*Id="%s"[^>]*/>', rid), rels)
  )
  stopifnot(length(rel) == 1L)
  target <- sub('.*Target="([^"]+)".*', "\\1", rel)
  return(read_all("xl", target))
}

# `na.strings = character(0)` keeps a cell that holds the text "NA" as
# text. The default would read it back as a missing value, and the test of
# empty cells could then not tell the two apart.
ss_read_summary <- function(path) {
  return(openxlsx::read.xlsx(
    path,
    sheet = "Summary",
    check.names = FALSE,
    sep.names = " ",
    skipEmptyRows = FALSE,
    na.strings = character(0)
  ))
}

# A results sheet, read from its header row, keyed by enrollment name,
# outcome and follow-up. The results sheets carry no ETT id, and that key is
# unique in the fixture.
ss_read_results <- function(path, sheet) {
  raw <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE)
  header_row <- which(raw[[1]] == "Enrollment")
  stopifnot(length(header_row) == 1L)
  d <- openxlsx::read.xlsx(
    path,
    sheet = sheet,
    startRow = header_row,
    check.names = FALSE,
    sep.names = " "
  )
  key <- paste(d$Enrollment, d$Outcome, d[["Follow-up (weeks)"]], sep = "|")
  stopifnot(!anyDuplicated(key))
  rownames(d) <- key
  return(d)
}

# The display of a number under the results sheets' `+0.00;-0.00;0.00`.
ss_signed <- function(x) {
  return(if (x == 0) "0.00" else sprintf("%+.2f", x))
}

ss_cell <- function(row, col) {
  if (is.null(row) || !col %in% names(row)) {
    return(NA)
  }
  return(row[[col]])
}

# What each estimand's Summary cells MUST hold, from one results-sheet row.
ss_expected <- function(row) {
  irr <- ss_cell(row, "IRR")
  ci <- ss_cell(row, "95% CI")
  n_int <- ss_cell(row, "Persons with event (int)")
  n_cmp <- ss_cell(row, "Persons with event (cmp)")
  rd <- ss_cell(row, "Risk difference per 10,000")
  rd_ci <- ss_cell(row, "Risk difference 95% CI")
  return(list(
    irr_ci = if (is.na(irr)) {
      NA_character_
    } else if (is.na(ci)) {
      irr
    } else {
      paste0(irr, " (", ci, ")")
    },
    events = if (is.na(n_int) || is.na(n_cmp)) {
      NA_character_
    } else {
      paste(format(n_int, big.mark = ","), "/", format(n_cmp, big.mark = ","))
    },
    rd = if (is.na(rd)) {
      NA_character_
    } else if (is.na(rd_ci)) {
      ss_signed(rd)
    } else {
      paste0(ss_signed(rd), " (", rd_ci, ")")
    },
    py_int = ss_cell(row, "PY (int)"),
    py_cmp = ss_cell(row, "PY (cmp)")
  ))
}

ss_plan_path <- ss_export(.xp_plan("new"))

SS_HEADER <- c(
  "ETT",
  "Contrast",
  "Outcome",
  "Follow-up (weeks)",
  "Events int/cmp (ITT)",
  "ITT IRR (CI)",
  "Events int/cmp (PP)",
  "PP IRR (CI)",
  "PP PY retention (int / cmp)",
  "PP RD per 10,000 (CI)"
)


test_that("Summary is sheet 2, after Provenance and before Study Specification", {
  nms <- openxlsx::getSheetNames(ss_plan_path)
  expect_identical(nms[1:3], c("Provenance", "Summary", "Study Specification"))
})


test_that("the Summary header is row 1, with one row per ETT in plan order", {
  # `rows = 1L` reads worksheet row 1 itself. Without it, `read.xlsx()` skips
  # the empty rows above the first filled row, so a header in row 3 would
  # read as the first row.
  row1 <- openxlsx::read.xlsx(
    ss_plan_path,
    sheet = "Summary",
    rows = 1L,
    colNames = FALSE
  )
  expect_identical(row1[1, 1], "ETT")
  expect_identical(unname(unlist(row1[1, ])), SS_HEADER)

  s <- ss_read_summary(ss_plan_path)
  plan <- .xp_plan("new")
  expect_identical(s$ETT, plan$ett$ett_id)
  expect_identical(
    s$Contrast,
    paste0(
      plan$ett$enrollment_id,
      ": ",
      c(`01` = "Enrollment one", `02` = "Enrollment two")[plan$ett$enrollment_id]
    )
  )
  expect_identical(s$Outcome, plan$ett$outcome_name)
  expect_equal(s[["Follow-up (weeks)"]], as.numeric(plan$ett$follow_up))
})


test_that("the Summary header row carries an autofilter and is frozen", {
  xml <- ss_sheet_xml(ss_plan_path, "Summary")
  # Ten columns, A to J, and eight ETTs below the header.
  expect_match(xml, '<autoFilter ref="A1:J9"/>', fixed = TRUE)
  expect_match(xml, '<pane [^>]*ySplit="1"[^>]*state="frozen"')
  expect_match(xml, '<pane [^>]*topLeftCell="A2"')
})


test_that("every Summary estimate equals the cell of the ITT and PP results sheets", {
  s <- ss_read_summary(ss_plan_path)
  itt <- ss_read_results(ss_plan_path, "ITT results")
  pp <- ss_read_results(ss_plan_path, "PP results")
  key <- paste(
    sub("^[^:]+: ", "", s$Contrast),
    s$Outcome,
    s[["Follow-up (weeks)"]],
    sep = "|"
  )
  row_of <- function(d, k) {
    if (!k %in% rownames(d)) {
      return(NULL)
    }
    return(d[k, , drop = FALSE])
  }
  for (i in seq_len(nrow(s))) {
    want_itt <- ss_expected(row_of(itt, key[i]))
    want_pp <- ss_expected(row_of(pp, key[i]))
    info <- s$ETT[i]
    expect_identical(s[["ITT IRR (CI)"]][i], want_itt$irr_ci, info = info)
    expect_identical(s[["PP IRR (CI)"]][i], want_pp$irr_ci, info = info)
    expect_identical(
      s[["Events int/cmp (ITT)"]][i],
      want_itt$events,
      info = info
    )
    expect_identical(s[["Events int/cmp (PP)"]][i], want_pp$events, info = info)
    expect_identical(s[["PP RD per 10,000 (CI)"]][i], want_pp$rd, info = info)
    retention <- if (
      anyNA(c(want_pp$py_int, want_pp$py_cmp, want_itt$py_int, want_itt$py_cmp))
    ) {
      NA_character_
    } else {
      sprintf(
        "%.0f%% / %.0f%%",
        100 * want_pp$py_int / want_itt$py_int,
        100 * want_pp$py_cmp / want_itt$py_cmp
      )
    }
    expect_identical(
      s[["PP PY retention (int / cmp)"]][i],
      retention,
      info = info
    )
  }

  # The comparison above passes on an empty sheet. These counts pin that each
  # column holds values, so a Summary of blank cells fails.
  filled <- vapply(s, function(x) sum(!is.na(x)), integer(1))
  expect_identical(
    unname(filled[SS_HEADER[5:10]]),
    c(2L, 5L, 4L, 5L, 6L, 4L)
  )
  # The per-protocol ratio and risk difference are the truncated-weight
  # values, and a positive risk difference carries its sign.
  expect_identical(s[["PP IRR (CI)"]][1], "0.54 (0.40 to 0.71)")
  expect_identical(s[["PP RD per 10,000 (CI)"]][2], "+40.00 (+0.00 to +80.00)")
})


test_that("a missing estimate is an empty cell, not NA text", {
  s <- ss_read_summary(ss_plan_path)
  expect_false(any(vapply(s, function(x) any(x %in% "NA"), logical(1))))
  # ETT00006 has no stored result at all.
  expect_true(all(is.na(unlist(s[s$ETT == "ETT00006", SS_HEADER[5:10]]))))
})


test_that("the Family column shows only when the spec sets a family", {
  s <- ss_read_summary(ss_plan_path)
  expect_false("Family" %in% names(s))

  plan <- .xp_plan("new")
  plan$spec$enrollments[[1]]$family <- "Main"
  plan$spec$enrollments[[2]]$family <- "Sens: age 54-60"
  path <- ss_export(plan)
  s <- ss_read_summary(path)
  expect_identical(names(s), append(SS_HEADER, "Family", after = 1L))
  expect_identical(
    s$Family,
    unname(c(`01` = "Main", `02` = "Sens: age 54-60")[plan$ett$enrollment_id])
  )
  expect_match(
    ss_sheet_xml(path, "Summary"),
    '<autoFilter ref="A1:K9"/>',
    fixed = TRUE
  )
})
