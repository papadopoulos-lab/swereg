# Reporting the rare_condition_rule (#52) through the real pipeline.
#
# One run per file: the real s1 -> s2 -> s3 dispatch on the fixture of
# helper-sparse_report.R, then plan$save(), tteplan_load() and
# $export_tables(). At min_n = 10 the rule excludes "Prior rare condition B"
# (0 holders in the intervention arm of enrollment "01") and adjusts for
# "Prior common condition". Every output MUST name that one exclusion and the
# adjustment set "Age, Prior common condition".

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("openxlsx")

spr_e2e <- new.env(parent = emptyenv())

# The loaded plan and its workbooks, built once for the file:
# `$export_tables()` writes tables.xlsx and feasibility.xlsx, and
# `$excel_spec_summary()` the specification workbook, which keeps the Rare
# conditions sheet.
spr_e2e_get <- function() {
  if (is.null(spr_e2e$plan)) {
    run <- spr_report_pipeline()
    path <- file.path(run$root, "tables.xlsx")
    suppressWarnings(invisible(utils::capture.output(
      run$plan$export_tables(path = path)
    )))
    spec_path <- file.path(run$root, "spec.xlsx")
    suppressMessages(run$plan$excel_spec_summary(path = spec_path))
    spr_e2e$plan <- run$plan
    spr_e2e$path <- path
    spr_e2e$feas_path <- file.path(run$root, "feasibility.xlsx")
    spr_e2e$spec_path <- spec_path
  }
  return(spr_e2e)
}

spr_e2e_sheet <- function(sheet, ...) {
  return(openxlsx::read.xlsx(
    spr_e2e_get()$path,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    ...
  ))
}

# The Rare conditions sheet of the specification workbook.
spr_e2e_spec_rare <- function() {
  return(openxlsx::read.xlsx(
    spr_e2e_get()$spec_path,
    sheet = "Rare conditions",
    colNames = FALSE,
    skipEmptyRows = FALSE
  ))
}

# The Rare conditions by comparison sheet of tables.xlsx: the rows under its
# header row, named by it. `header_row` is the sheet row of that header.
spr_e2e_rare <- function() {
  m <- spr_e2e_sheet("Rare conditions by comparison")
  m[] <- lapply(m, as.character)
  h <- which(m[[1]] %in% "Condition")[1L]
  d <- m[-seq_len(h), , drop = FALSE]
  names(d) <- as.character(unlist(m[h, ]))
  rownames(d) <- NULL
  return(list(d = d, header_row = h))
}

SPR_E2E_CRITERION <- "Excluded (fewer than 10 at baseline): Prior rare condition B"
SPR_E2E_LABEL <- paste(
  "Prior rare condition B (rare-condition rule: fewer than 10 persons at",
  "baseline in the smallest arm of the driving comparison)"
)

test_that("the loaded plan, the Rare conditions by comparison sheet and the Attrition step-2 rows agree", {
  e <- spr_e2e_get()
  d <- e$plan$get_sparse_decision()
  expect_identical(
    d$label,
    c("Prior rare condition B", "Prior common condition")
  )
  expect_identical(d$decision, c("excluded", "adjusted"))
  expect_identical(d$smallest[1], 0L)
  expect_true(d$smallest[2] >= 10L)

  sheets <- openxlsx::getSheetNames(e$path)
  expect_true("Rare conditions by comparison" %in% sheets)
  expect_false("Rare conditions" %in% sheets)
  r <- spr_e2e_rare()
  x <- r$d
  expect_identical(x$Condition[1:2], d$label)
  expect_identical(x$Decision[1:2], d$decision)
  # The deciding comparison holds the smallest arm count of the decision.
  # A count below min_n = 10 is red, and one of at least 2 x 10 light green.
  expect_identical(x[["01 (decides)"]][1:2], as.character(d$smallest))
  col <- match("01 (decides)", names(x))
  expect_identical(
    .ff_fill(e$path, "Rare conditions by comparison", r$header_row + 1L, col),
    "#F4B6B6"
  )
  expect_identical(
    .ff_fill(e$path, "Rare conditions by comparison", r$header_row + 2L, col),
    "#D9EAD3"
  )
  # The Rare conditions sheet of the specification workbook still shows the
  # count below min_n as <10.
  sr <- spr_e2e_spec_rare()
  expect_identical(sr[[1]][5:6], d$label)
  expect_identical(sr[[7]][5:6], d$decision)
  expect_identical(sr[[4]][5:6], c("<10", as.character(d$smallest[2])))

  for (eid in c("01", "02")) {
    att <- spr_e2e_sheet(paste0("Attrition_", eid), startRow = 3L)
    step <- att[[1]][-1L]
    label <- att[[10]][-1L]
    k <- which(step == SPR_E2E_CRITERION)
    expect_length(k, 1L)
    # After every ordinary exclusion, before the comparator draw.
    expect_identical(step[k - 1L], "landmark_event_free")
    expect_identical(step[k + 1L], "enrolled_after_comparator_draw")
    expect_identical(label[k], SPR_E2E_LABEL)
    # The sheet's counts are the global step-2 row of enrollment_counts.
    g <- e$plan$enrollment_counts[[eid]]$attrition
    g <- g[is.na(g$enrollment_period_id) & g$criterion == SPR_E2E_CRITERION]
    expect_identical(as.numeric(att[[3]][-1L][k]), as.numeric(g$n_persons))
    expect_identical(
      as.numeric(att[[4]][-1L][k]),
      as.numeric(g$n_person_trials)
    )
  }
})

test_that("Table 1 of the real run drops the excluded condition and names it in a footnote", {
  t1 <- spr_e2e_sheet("Table 1")
  expect_false(any(grepl("rare_b", t1[[1]], fixed = TRUE)))
  expect_true(any(grepl("rd_no_osd_common_everbefore", t1[[1]], fixed = TRUE)))
  expect_identical(
    t1[[1]][length(t1[[1]])],
    paste0(
      "Not shown: Prior rare condition B. The rare-condition rule made each ",
      "of these conditions an exclusion criterion. See the Rare ",
      "conditions by comparison sheet."
    )
  )
})

test_that("the exported Table 1 drops the rows of an excluded condition from a stored panel", {
  e <- spr_e2e_get()
  # The real panel holds no excluded condition, because s1b removed it from
  # every confounder set. A copy of the plan gets a panel that holds one: the
  # rows of the adjusted condition, renamed to the excluded variable.
  plan <- e$plan$clone(deep = TRUE)
  panel <- plan$results_enrollment[["01"]]$table1_ipw_trunc_main
  hit <- which(panel[[1]] == "rd_no_osd_common_everbefore")
  extra <- panel[c(hit, hit + 1L)]
  extra[[1]][1L] <- "rd_no_osd_rare_b_everbefore"
  added <- rbind(panel, extra)
  data.table::setattr(added, "class", class(panel))
  plan$results_enrollment[["01"]]$table1_ipw_trunc_main <- added
  base <- plan$get_baselines()
  # The stored panel now holds the excluded condition.
  expect_true(any(
    base$enrollment_id == "01" &
      base$variant == "main" &
      base$variable == "rd_no_osd_rare_b_everbefore"
  ))
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  suppressWarnings(invisible(utils::capture.output(
    plan$export_tables(path = path, table1_enrollment = "01")
  )))
  t1 <- openxlsx::read.xlsx(
    path,
    sheet = "Table 1",
    colNames = FALSE,
    skipEmptyRows = FALSE
  )
  expect_false(any(grepl("rd_no_osd_rare_b_everbefore", t1[[1]], fixed = TRUE)))
})

test_that("workbook, CONSORT, TARGET 6a/6g and protocol table name the same exclusions and adjustment set", {
  e <- spr_e2e_get()
  plan <- e$plan
  excluded <- list()
  adjustment <- list()

  # The adjustment set the pipeline applied, by name.
  conf_names <- c(
    rd_age_continuous = "Age",
    rd_no_osd_common_everbefore = "Prior common condition",
    rd_no_osd_rare_b_everbefore = "Prior rare condition B"
  )
  for (cv in plan$ett$confounder_vars) {
    expect_identical(unname(conf_names[cv]), SPR_REPORT_ADJUSTMENT_SET)
  }

  # Workbooks.
  x <- spr_e2e_rare()$d
  excluded$sparse_sheet <- x$Condition[x$Decision %in% "excluded"]
  sr <- spr_e2e_spec_rare()
  excluded$spec_sparse_sheet <- sr[[1]][5:6][sr[[7]][5:6] == "excluded"]
  ss <- spr_e2e_sheet("Study Specification")
  excluded$spec_sheet <- strsplit(
    spr_report_spec_value(ss, "Excluded conditions:"),
    ", "
  )[[1]]
  adjustment$spec_sheet <- strsplit(
    spr_report_spec_value(ss, "Final adjustment set:"),
    ", "
  )[[1]]
  t1 <- spr_e2e_sheet("Table 1")
  foot <- t1[[1]][length(t1[[1]])]
  excluded$table1 <- strsplit(
    sub("\\. The rare-condition.*$", "", sub("^Not shown: ", "", foot)),
    ", "
  )[[1]]

  # CONSORT labels of the step-2 rows, every enrollment.
  for (eid in c("01", "02")) {
    crit <- unique(plan$enrollment_counts[[eid]]$attrition$criterion)
    labels <- swereg:::.build_criterion_label_lookup(plan, eid, crit)
    step2 <- grep("^Excluded \\(fewer than", crit, value = TRUE)
    excluded[[paste0("consort_", eid)]] <- sub(
      "\\\\n.*$",
      "",
      unname(labels[step2])
    )
  }

  # TARGET 6a and 6g.
  txt <- spr_report_checklist(plan)
  excluded$target_6a <- spr_report_list_after(
    spr_report_item(txt, "6a", "6b"),
    "- Excluded under the rare-condition rule: "
  )
  g <- spr_report_item(txt, "6g", "6h")
  adjustment$target_6g <- spr_report_list_after(g, "Final adjustment set: ")
  excluded$target_6g <- spr_report_list_after(
    g,
    "exclusion criterion (item 6a): "
  )

  # Protocol table, as the workbook wrote it.
  pt <- openxlsx::read.xlsx(e$path, sheet = "Target trial protocol", startRow = 3L)
  excluded$protocol <- spr_report_list_after(
    strsplit(pt[[3]][1], "\n")[[1]],
    "Not eligible under the rare-condition rule: "
  )
  excluded$protocol_specification <- sub(
    " at baseline$",
    "",
    spr_report_list_after(
      strsplit(pt[[2]][1], "\n")[[1]],
      "Exclude persons with "
    )
  )
  adjustment$protocol <- spr_report_list_after(
    strsplit(pt[[3]][7], "\n")[[1]],
    "Final adjustment set of the treatment and censoring models, after the rare-condition rule: "
  )

  expect_named(
    excluded,
    c(
      "sparse_sheet",
      "spec_sparse_sheet",
      "spec_sheet",
      "table1",
      "consort_01",
      "consort_02",
      "target_6a",
      "target_6g",
      "protocol",
      "protocol_specification"
    )
  )
  for (nm in names(excluded)) {
    expect_identical(excluded[[nm]], SPR_REPORT_EXCLUDED, label = nm)
  }
  expect_named(adjustment, c("spec_sheet", "target_6g", "protocol"))
  for (nm in names(adjustment)) {
    expect_identical(adjustment[[nm]], SPR_REPORT_ADJUSTMENT_SET, label = nm)
  }
})

test_that("no text that states the rule says it never looked at outcomes", {
  e <- spr_e2e_get()
  basis <- paste(
    "decided from baseline counts in the eligible population, before the",
    "comparator draw and without follow-up outcomes"
  )
  txt <- spr_report_checklist(e$plan)
  x <- spr_e2e_spec_rare()
  rare <- spr_e2e_sheet("Rare conditions by comparison")
  ss <- spr_e2e_sheet("Study Specification")
  pt <- openxlsx::read.xlsx(e$path, sheet = "Target trial protocol", startRow = 3L)
  states_rule <- list(
    target_6a = paste(spr_report_item(txt, "6a", "6b"), collapse = " "),
    target_methods = paste(spr_report_item(txt, "7a-h", "8"), collapse = " "),
    sparse_sheet_title = x[[1]][1],
    spec_sheet_rule = spr_report_spec_value(ss, "Rule:"),
    protocol_eligibility = pt[[3]][1]
  )
  for (nm in names(states_rule)) {
    expect_true(grepl(basis, states_rule[[nm]], fixed = TRUE), label = nm)
  }
  every_text <- c(
    unlist(states_rule),
    txt,
    unlist(x),
    unlist(rare),
    unlist(ss),
    unlist(pt),
    unlist(spr_e2e_sheet("Table 1")),
    unlist(spr_e2e_sheet("Attrition_01"))
  )
  for (phrase in SPR_REPORT_FORBIDDEN) {
    expect_false(
      any(grepl(phrase, every_text, fixed = TRUE)),
      label = phrase
    )
  }
})

test_that("with a rare_condition_rule, tables.xlsx after Provenance and Summary begins with the sheets of feasibility.xlsx", {
  e <- spr_e2e_get()
  tables <- openxlsx::getSheetNames(e$path)
  feas <- openxlsx::getSheetNames(e$feas_path)
  expect_true("Rare conditions by comparison" %in% feas)
  expect_identical(tables[1:2], c("Provenance", "Summary"))
  expect_identical(tables[2L + seq_along(feas)], feas)
  expect_identical(tables[3L + length(feas)], "Target trial protocol")
})
