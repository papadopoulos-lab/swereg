# The overview sheets of feasibility.xlsx and tables.xlsx.

test_that("an attrition step reads as the spec criterion it applies", {
  spec <- list(
    exclusion_criteria = list(list(
      name = "Absent genital organs",
      implementation = list(source_variable = "osd_z907")
    )),
    enrollments = list(list(
      id = "M01",
      additional_inclusion = list(list(
        name = "No earlier systemic MHT",
        implementation = list(source_variable = "rd_approach1_single")
      ))
    ))
  )
  f <- swereg:::.overview_step_label
  expect_identical(f("before_exclusions", spec, "M01"), "Start: all person-trials")
  expect_identical(
    f("eligible_no_osd_z907_lifetime_before_baseline", spec, "M01"),
    "Excl. Absent genital organs"
  )
  expect_identical(
    f("eligible_has_rd_approach1_single_lifetime", spec, "M01"),
    "Incl. No earlier systemic MHT"
  )
  expect_identical(f("Landmark: observed", spec, "M01"), "Landmark: observed")
})

test_that("a Table 1 cell splits into its count and share", {
  x <- swereg:::.overview_split_cell(c("14,964 (7.5%)", "51.52 (3.38)", ""))
  expect_identical(x$value, c("14,964", "51.52 (3.38)", ""))
  expect_identical(x$pct, c("7.5", NA, NA))
})

test_that("a cut takes the fill of its band", {
  expect_identical(
    swereg:::.overview_cut_fill(c("-", "1", "4", "5", "10", "20", NA)),
    c("red", "orange", "orange", "yellow", "lightgreen", "green", NA)
  )
})

.ov_plan <- function() {
  att <- data.table::data.table(
    enrollment_id = rep(c("M01", "M02"), each = 3L),
    enrollment_period_id = NA_integer_,
    step_order = rep(1:3, 2L),
    step_name = rep(c("before_exclusions", "eligible_no_osd_z907_lifetime", "eligible_no_osd_i26_lifetime"), 2L),
    n_persons = c(100, 100, 90, 50, 50, 45),
    n_person_trials = c(100, 100, 90, 50, 50, 45),
    n_arm_intervention = c(20, 20, 18, 10, 10, 9),
    n_arm_comparator = c(80, 80, 72, 40, 40, 36)
  )
  counts <- data.table::CJ(
    enrollment_id = c("M01", "M02"),
    variable = c("rd_a", "rd_b"),
    arm = c("intervention", "comparator"),
    sorted = FALSE
  )
  counts[, n_persons := c(300L, 500L, 3L, 90L, 150L, 120L, 40L, 2L)]
  return(list(
    spec = list(
      exclusion_criteria = list(
        list(name = "Absent genital organs", implementation = list(source_variable = "osd_z907")),
        list(name = "VTE", implementation = list(source_variable = "osd_i26"))
      ),
      confounders = list(
        list(name = "Condition A", implementation = list(variable = "rd_a")),
        list(name = "Condition B", implementation = list(variable = "rd_b"))
      ),
      rare_condition_rule = list(min_n = 100, decided_by = list("M01"), candidates = list("rd_a", "rd_b")),
      enrollments = list(list(id = "M01", name = "One"), list(id = "M02", name = "Two"))
    ),
    sparse_counts = counts,
    get_attrition = function() {
      return(att)
    }
  ))
}

test_that("Attrition by arm writes each step once, with intervention and comparator columns", {
  skip_if_not_installed("openxlsx")
  wb <- openxlsx::createWorkbook()
  swereg:::.write_overview_attrition(wb, .ov_plan())
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  d <- openxlsx::read.xlsx(path, sheet = "Attrition by arm", colNames = FALSE, skipEmptyRows = FALSE)
  expect_true("Excl. Absent genital organs" %in% d[[2]])
  expect_true(any(d == "M01 intervention", na.rm = TRUE))
  expect_true(any(d == "M02 comparator", na.rm = TRUE))
})

.ov_rare <- function(plan) {
  wb <- openxlsx::createWorkbook()
  expect_true(swereg:::.write_overview_rare(wb, plan))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  return(openxlsx::read.xlsx(path, sheet = "Rare conditions by comparison", startRow = 7L, check.names = FALSE, sep.names = " "))
}

test_that("Rare conditions by comparison shows the smaller arm, marks c and counts below min_n", {
  skip_if_not_installed("openxlsx")
  d <- .ov_rare(.ov_plan())
  expect_identical(names(d)[1:4], c("Condition", "Decision", "M01 (decides)", "M02"))
  # M01, rd_a: intervention 300, comparator 500 -> 300.
  # M01, rd_b: intervention 3, comparator 90 -> <5.
  # M02, rd_a: intervention 150, comparator 120 -> 120 c.
  # M02, rd_b: intervention 40, comparator 2 -> <5 c.
  expect_identical(d[[3]][1:2], c("300", "<5"))
  expect_identical(d[[4]][1:2], c("120 c", "<5 c"))
  expect_identical(as.numeric(d[["Comparisons < 100"]][1:2]), c(0, 2))
  # The plan holds no s1b decision, so the Decision cells are empty.
  expect_true(all(is.na(d$Decision) | !nzchar(d$Decision)))
})

test_that("Rare conditions by comparison shows the s1b decision of each condition", {
  skip_if_not_installed("openxlsx")
  plan <- .ov_plan()
  # In the order of the decision table, not of the candidates.
  plan$sparse_decision <- data.table::data.table(
    variable = c("rd_b", "rd_a"),
    decision = c("excluded", "adjusted")
  )
  d <- .ov_rare(plan)
  expect_identical(d$Condition[1:2], c("Condition A", "Condition B"))
  expect_identical(d$Decision[1:2], c("adjusted", "excluded"))
  # The total row carries no decision.
  expect_true(is.na(d$Decision[3L]) || !nzchar(d$Decision[3L]))
})

test_that("Rare conditions by comparison keeps the s1b decision of a plan without counts per comparison", {
  skip_if_not_installed("openxlsx")
  plan <- .ov_plan()
  plan$sparse_counts <- NULL
  plan$sparse_decision <- data.table::data.table(
    variable = c("rd_b", "rd_a"),
    decision = c("excluded", "adjusted")
  )
  wb <- openxlsx::createWorkbook()
  expect_true(swereg:::.write_overview_rare(wb, plan))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  d <- openxlsx::read.xlsx(path, sheet = "Rare conditions by comparison", colNames = FALSE, skipEmptyRows = FALSE)
  i <- match("Condition", d[[1]])
  expect_false(is.na(i))
  expect_identical(d[[2]][i + 1:2], c("adjusted", "excluded"))
  expect_identical(d[[1]][i + 1:2], c("Condition A", "Condition B"))
})

test_that("Longest reportable reads the specification threshold from the stored verdict", {
  skip_if_not_installed("openxlsx")
  # One comparison, one outcome, two years. The grid cut is "-" in both
  # years: the expected events fall below 1. A threshold of 0 still passes
  # every year, and the stored verdict says so.
  testthat::local_mocked_bindings(
    .overview_cuts = function(plan, records) {
      return(list(
        ids = "01",
        names = c(`01` = "Statin vs none"),
        outcomes = "Death",
        years = c("1 year", "2 years"),
        cut = list(Death = matrix(c("-", "-"), 1L)),
        yn = list(Death = matrix(c("Y", "Y"), 1L))
      ))
    },
    .package = "swereg"
  )
  plan <- list(spec = list(min_expected_events_per_arm_per_year = 0))
  wb <- openxlsx::createWorkbook()
  swereg:::.write_overview_min_events(wb, plan, list())
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  d <- openxlsx::read.xlsx(path, sheet = "Longest reportable", colNames = FALSE, skipEmptyRows = FALSE)
  row <- d[which(d[[1]] == "01"), , drop = FALSE]
  hdr <- d[which(d[[1]] == "ID")[2L], , drop = FALSE]
  expect_identical(unname(unlist(row[1, which(hdr[1, ] == "threshold 0")])), "2 years")
  expect_identical(unname(unlist(row[1, which(hdr[1, ] == "threshold 5")])), "none")
})

test_that("The overview cuts mark an exempt comparison", {
  testthat::local_mocked_bindings(
    .minimum_events_wide = function(plan, records) {
      tab <- data.frame(
        Outcome = c("Death", NA),
        `#` = 1:2,
        Comparison = c("a", "b"),
        `1 year` = c("5", "-"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      yn <- tab
      yn[["1 year"]] <- c("Y", "-")
      return(list(cut = tab, yn = yn))
    },
    .enrollment_label = function(plan, eid) {
      return(paste("Comparison", eid))
    },
    .package = "swereg"
  )
  plan <- list(
    ett = data.frame(enrollment_id = c("01", "02"), stringsAsFactors = FALSE),
    spec = list(enrollments = list(
      list(id = "01"),
      list(id = "02", minimum_events_rule_exempt = TRUE)
    ))
  )
  x <- swereg:::.overview_cuts(plan, list())
  expect_identical(unname(x$names), c("Comparison 01", "Comparison 02 (exempt)"))
  expect_identical(x$yn$Death[, 1], c("Y", "-"))
})

.ov_nid_sheets <- function(nid) {
  testthat::local_mocked_bindings(
    .overview_cuts = function(plan, records) {
      return(list(
        ids = c("01", "02"),
        names = c(`01` = "First", `02` = "Second"),
        outcomes = c("Absent", "Rare"),
        years = c("1 year", "2 years"),
        cut = list(
          Absent = matrix(c("-", "-", "-", "-"), 2L),
          Rare = matrix(c("5", "2", "1", "-"), 2L)
        ),
        yn = list(
          Absent = matrix(c("-", "-", "-", "-"), 2L),
          Rare = matrix(c("Y", "-", "-", "-"), 2L)
        )
      ))
    },
    .tte_not_in_data = function(plan) {
      return(nid)
    },
    .package = "swereg"
  )
  wb <- openxlsx::createWorkbook()
  swereg:::.write_overview_min_events(wb, list(spec = list(min_expected_events_per_arm_per_year = 5)), list())
  return(wb)
}

.ov_cell <- function(wb, sht, label, col) {
  d <- openxlsx::readWorkbook(wb, sht, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  r <- which(d[[1]] == label)
  return(list(row = r, value = d[r, col]))
}

.ov_fill_at <- function(wb, sht, row, col) {
  st <- openxlsx::getStyles(wb)
  hit <- NULL
  for (s in wb$styleObjects) {
    if (identical(s$sheet, sht) && any(s$rows == row & s$cols == col)) {
      hit <- s$style$fill$fillFg
    }
  }
  return(if (is.null(hit)) NA_character_ else unname(unlist(hit))[1L])
}

test_that("an outcome not in the data gets empty black cells, and a <5 outcome is unchanged", {
  skip_if_not_installed("openxlsx")
  wb <- .ov_nid_sheets(list(outcomes = "Absent", enrollments = character(0)))
  # The Rare outcome starts after the Absent outcome's columns: 2 years on
  # Minimum events, 3 thresholds (5, 10, 20) on Longest reportable.
  rare_col <- c(`Minimum events` = 5L, `Longest reportable` = 6L)
  for (sht in names(rare_col)) {
    first <- .ov_cell(wb, sht, "01", 3L)
    expect_true(is.na(first$value) || !nzchar(first$value))
    expect_match(.ov_fill_at(wb, sht, first$row, 3L), "000000$")
    rare <- .ov_cell(wb, sht, "01", rare_col[[sht]])
    expect_false(is.na(rare$value) || !nzchar(rare$value))
    expect_false(isTRUE(grepl("000000$", .ov_fill_at(wb, sht, rare$row, rare_col[[sht]]))))
  }
  expect_identical(.ov_cell(wb, "Minimum events", "01", 5L)$value, "5")
})

test_that("a comparison with a criterion not in the data has a grey name cell", {
  skip_if_not_installed("openxlsx")
  wb <- .ov_nid_sheets(list(outcomes = character(0), enrollments = "02"))
  for (sht in c("Minimum events", "Longest reportable")) {
    second <- .ov_cell(wb, sht, "02", 2L)
    first <- .ov_cell(wb, sht, "01", 2L)
    expect_match(.ov_fill_at(wb, sht, second$row, 2L), "BFBFBF$")
    expect_true(is.na(.ov_fill_at(wb, sht, first$row, 2L)))
  }
})
