# Reporting the frozen sparse_rule decision (#52), one output at a time.
#
# Every test here builds the plan of spr_report_fast_plan(): a hand-made
# decision with no s1 run. At min_n = 10 it excludes "Prior rare condition B"
# and "Prior rare condition A", and adjusts for "Prior common condition".
# test-sparse-report-e2e.R runs the real s1 -> s2 -> s3 dispatch.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("openxlsx")

FAST_EXCLUDED <- c("Prior rare condition B", "Prior rare condition A")
FAST_ADJUSTMENT_SET <- c("Age", "Prior common condition")
FAST_DRIVER <- "enrollment 01 (Treated vs control 01)"

test_that("the Sparse conditions sheet shows each candidate, and a small count as <min_n", {
  plan <- spr_report_fast_plan()
  x <- spr_report_read_sheet(
    function(wb) {
      expect_true(swereg:::.write_sparse_conditions_sheet(wb, plan))
    },
    "Sparse conditions"
  )
  expect_identical(
    x[[1]][1],
    paste0(
      "Sparse conditions: the rare-condition rule of ",
      FAST_DRIVER,
      ", decided from baseline counts in the eligible population, before ",
      "the comparator draw and without follow-up outcomes"
    )
  )
  expect_identical(
    x[[1]][2],
    paste(
      "Baseline counts of eligible persons per arm before the comparator",
      "draw, without follow-up outcomes. A person counts once in an arm when",
      "any of her eligible person-trials in that arm holds the condition at",
      "its recruiting-week covariate snapshot. The same person can count in",
      "both arms. A count below the threshold shows as <10."
    )
  )
  expect_identical(
    unname(unlist(x[4, 1:7])),
    c(
      "Condition",
      "Variable",
      "Threshold",
      "Smallest arm count",
      "Comparison",
      "Arm",
      "Decision"
    )
  )
  expect_identical(
    x[[1]][5:7],
    c("Prior rare condition B", "Prior common condition", "Prior rare condition A")
  )
  expect_identical(as.character(x[[3]][5:7]), rep("10", 3L))
  expect_identical(x[[4]][5:7], c("<10", "50", "<10"))
  expect_identical(x[[5]][5:7], rep(FAST_DRIVER, 3L))
  expect_identical(x[[6]][5:7], rep("intervention (Treated)", 3L))
  expect_identical(x[[7]][5:7], c("excluded", "adjusted", "excluded"))
})

test_that("the Sparse conditions sheet is absent without a decision", {
  plan <- spr_report_fast_plan()
  plan$sparse_decision <- NULL
  wb <- openxlsx::createWorkbook()
  expect_false(swereg:::.write_sparse_conditions_sheet(wb, plan))
  expect_identical(openxlsx::sheets(wb), character())
})

test_that("Study Specification states the rule, the exclusions and the adjustment set", {
  plan <- spr_report_fast_plan()
  ss <- spr_report_read_sheet(
    function(wb) swereg:::.write_spec_summary(wb, plan),
    "Study Specification"
  )
  expect_true("Rare-condition rule (sparse_rule)" %in% ss[[1]])
  expect_identical(spr_report_spec_value(ss, "Threshold (min_n):"), "10")
  expect_identical(spr_report_spec_value(ss, "Decided by:"), FAST_DRIVER)
  expect_identical(
    spr_report_spec_value(ss, "Candidates:"),
    "Prior rare condition B, Prior common condition, Prior rare condition A"
  )
  expect_identical(
    spr_report_spec_value(ss, "Excluded conditions:"),
    paste(FAST_EXCLUDED, collapse = ", ")
  )
  expect_identical(
    spr_report_spec_value(ss, "Final adjustment set:"),
    paste(FAST_ADJUSTMENT_SET, collapse = ", ")
  )
  expect_match(
    spr_report_spec_value(ss, "Rule:"),
    "decided from baseline counts in the eligible population, before the comparator draw and without follow-up outcomes",
    fixed = TRUE
  )
})

test_that("Study Specification carries no rule rows without a sparse_rule", {
  plan <- spr_report_fast_plan()
  plan$spec$sparse_rule <- NULL
  plan$sparse_decision <- NULL
  ss <- spr_report_read_sheet(
    function(wb) swereg:::.write_spec_summary(wb, plan),
    "Study Specification"
  )
  expect_false(any(grepl("Rare-condition", ss[[1]], fixed = TRUE)))
})

test_that("Table 1 drops the rows of an excluded condition", {
  plan <- spr_report_fast_plan()
  baselines <- data.table::data.table(
    enrollment_id = "01",
    variable = c(
      "rd_age_continuous (mean (SD))",
      "rd_no_osd_rare_b_everbefore",
      "rd_no_osd_rare_b_everbefore",
      "rd_no_osd_common_everbefore",
      "rd_no_osd_common_everbefore",
      "rd_no_osd_rare_a_everbefore (%)"
    ),
    level = c("", "FALSE", "TRUE", "FALSE", "TRUE", "FALSE")
  )
  out <- swereg:::.tte_sparse_drop_baseline_rows(baselines, plan)
  expect_identical(
    out$variable,
    c(
      "rd_age_continuous (mean (SD))",
      "rd_no_osd_common_everbefore",
      "rd_no_osd_common_everbefore"
    )
  )
  # Without a decision, the table is unchanged.
  plan$sparse_decision <- NULL
  expect_identical(
    swereg:::.tte_sparse_drop_baseline_rows(baselines, plan),
    baselines
  )
})

test_that("the Table 1 footnote names the excluded conditions", {
  plan <- spr_report_fast_plan()
  note <- swereg:::.tte_sparse_table1_footnote(plan)
  expect_identical(
    note,
    paste0(
      "Not shown: Prior rare condition B, Prior rare condition A. The ",
      "rare-condition rule made each of these conditions an exclusion ",
      "criterion. See the Sparse conditions sheet."
    )
  )
  t1 <- data.table::data.table(Variable = c("N", "Age"), Level = c("", ""))
  x <- spr_report_read_sheet(
    function(wb) {
      swereg:::.write_tableone_sheet(wb, "Table 1", t1, "T1", footnote = note)
    },
    "Table 1"
  )
  # Title, blank, header, 2 rows, blank, footnote.
  expect_identical(x[[1]][7], note)
})

test_that("no Table 1 footnote without exclusions", {
  none <- spr_report_fast_plan(none_excluded = TRUE)
  t1 <- data.table::data.table(Variable = c("N", "Age"), Level = c("", ""))
  x <- spr_report_read_sheet(
    function(wb) {
      swereg:::.write_tableone_sheet(
        wb,
        "Table 1",
        t1,
        "T1",
        footnote = swereg:::.tte_sparse_table1_footnote(none)
      )
    },
    "Table 1"
  )
  # Title, blank, header and the 2 rows. Nothing below the table.
  expect_identical(x[[1]], c("T1", NA, "Variable", "N", "Age"))
})

# The rare_condition_rule column of an overview sheet below its header, or
# NULL when the sheet has no such column.
spr_report_role <- function(x) {
  col <- match("rare_condition_rule", unlist(x[1, ]))
  if (is.na(col)) {
    return(NULL)
  }
  return(x[[col]][-1L])
}

test_that("Enrollments says whether each enrollment drove or inherited the decision", {
  plan <- spr_report_fast_plan()
  enr <- spr_report_read_sheet(
    function(wb) swereg:::.write_enrollment_overview(wb, plan),
    "Enrollments"
  )
  expect_identical(enr[[1]][2:3], c("01", "02"))
  expect_identical(
    spr_report_role(enr),
    c("drove the decision", "inherited the decision")
  )
  # Without a decision, the sheet carries no such column.
  plan$sparse_decision <- NULL
  enr <- spr_report_read_sheet(
    function(wb) swereg:::.write_enrollment_overview(wb, plan),
    "Enrollments"
  )
  expect_null(spr_report_role(enr))
})

test_that("ETTs says whether each enrollment drove or inherited the decision", {
  plan <- spr_report_fast_plan()
  etts <- spr_report_read_sheet(
    function(wb) swereg:::.write_ett_overview(wb, plan),
    "ETTs"
  )
  eid <- etts[[match("enrollment_id", unlist(etts[1, ]))]][-1L]
  expect_identical(
    spr_report_role(etts),
    ifelse(eid == "01", "drove the decision", "inherited the decision")
  )
})

test_that("CONSORT labels each step-2 row with its condition and the rule", {
  plan <- spr_report_fast_plan()
  crit_b <- "Excluded (fewer than 10 at baseline): Prior rare condition B"
  crit_a <- "Excluded (fewer than 10 at baseline): Prior rare condition A"
  labels <- swereg:::.build_criterion_label_lookup(
    plan,
    "01",
    observed_criteria = c("before_exclusions", crit_b, crit_a)
  )
  rule_line <- paste0(
    "\\n(rare-condition rule: fewer than 10 persons at baseline in the ",
    "smallest arm of the driving comparisons)"
  )
  expect_identical(
    unname(labels[c(crit_b, crit_a)]),
    paste0(c("Prior rare condition B", "Prior rare condition A"), rule_line)
  )
  # The excluded box of the diagram lists both, under the exclusion heading.
  att <- data.table::data.table(
    enrollment_period_id = NA_integer_,
    criterion = c("before_exclusions", crit_b, crit_a),
    n_persons = c(100, 96, 93),
    n_person_trials = c(1000, 950, 920),
    n_intervention = c(300, 300, 300),
    n_comparator = c(700, 650, 620)
  )
  flow <- swereg:::.build_cohort_flow(list(attrition = att))
  dot <- swereg:::.build_consort_dot(
    flow,
    "01",
    "Treated vs control 01",
    "Treated",
    "Control",
    criterion_labels = labels
  )
  expect_match(
    dot,
    "- Prior rare condition B (rare-condition rule: fewer than 10 persons at baseline in the smallest arm of the driving comparisons) (n = 4 persons / 50 person-trials)",
    fixed = TRUE
  )
  expect_match(
    dot,
    "- Prior rare condition A (rare-condition rule: fewer than 10 persons at baseline in the smallest arm of the driving comparisons) (n = 3 persons / 30 person-trials)",
    fixed = TRUE
  )
})

test_that("TARGET 6a lists the rule and the final exclusions", {
  plan <- spr_report_fast_plan()
  a <- spr_report_item(spr_report_checklist(plan), "6a", "6b")
  expect_true(any(grepl(
    paste0(
      "- Rare-condition rule: A candidate condition held by fewer than 10 ",
      "persons in the smallest arm of the driving comparisons becomes an ",
      "exclusion criterion of every comparison. The driving comparisons are ",
      FAST_DRIVER,
      ". The rule is decided from baseline counts in the eligible ",
      "population, before the comparator draw and without follow-up ",
      "outcomes. Candidates: Prior rare condition B, Prior common condition, ",
      "Prior rare condition A."
    ),
    a,
    fixed = TRUE
  )))
  expect_identical(
    spr_report_list_after(a, "- Excluded under the rare-condition rule: "),
    FAST_EXCLUDED
  )
})

test_that("TARGET 6g lists the final adjustment set", {
  plan <- spr_report_fast_plan()
  g <- spr_report_item(spr_report_checklist(plan), "6g", "6h")
  expect_identical(
    spr_report_list_after(g, "Final adjustment set: "),
    FAST_ADJUSTMENT_SET
  )
  # The confounder lines name the adjustment set and no excluded condition.
  expect_true(any(grepl("^\\s*Age \\(variable: ", g)))
  expect_true(any(grepl("^\\s*Prior common condition \\(computed from: ", g)))
  expect_false(any(grepl("^\\s*Prior rare condition [AB] \\(computed", g)))
  expect_identical(
    spr_report_list_after(g, "exclusion criterion (item 6a): "),
    FAST_EXCLUDED
  )
})

test_that("the TARGET methods text states the rule, the threshold and the inheritance", {
  plan <- spr_report_fast_plan()
  txt <- paste(spr_report_checklist(plan), collapse = " ")
  expect_match(
    txt,
    paste0(
      "A pre-specified rare-condition rule was applied, with a threshold of ",
      "10 persons. The rule was decided from baseline counts in the eligible ",
      "population, before the comparator draw and without follow-up ",
      "outcomes. The counts were of eligible persons per arm in the driving ",
      "comparisons: ",
      FAST_DRIVER,
      ". A person counted once in an arm when any of her eligible ",
      "person-trials in that arm held the condition at its recruiting-week ",
      "covariate snapshot. The same person could count in both arms. A ",
      "candidate condition held by fewer than 10 persons in the smallest arm ",
      "became an exclusion criterion of every comparison. Every other ",
      "candidate stayed a confounder. The comparisons that did not drive the ",
      "rule inherited its decision and the adjustment set of the driving ",
      "comparisons. The rule excluded Prior rare condition B, Prior rare ",
      "condition A. The final adjustment set was Age, Prior common condition."
    ),
    fixed = TRUE
  )
})

test_that("the protocol table states the rule, the frozen decision and the adjustment set", {
  plan <- spr_report_fast_plan()
  ctx <- swereg:::.protocol_context(plan, plan$ett$ett_id[1])
  pt <- swereg:::.build_protocol_table(plan$spec, ctx)
  elig_spec <- strsplit(pt$`Target trial specification`[1], "\n")[[1]]
  elig_emul <- strsplit(pt$`Target trial emulation`[1], "\n")[[1]]
  plan_emul <- strsplit(pt$`Target trial emulation`[7], "\n")[[1]]
  expect_identical(pt$`Protocol component`[c(1, 7)], c(
    "Eligibility criteria",
    "Analysis plan"
  ))
  expect_true(
    paste0(
      "The rule is decided from baseline counts in the eligible population, ",
      "before the comparator draw and without follow-up outcomes"
    ) %in%
      elig_spec
  )
  expect_true(
    paste0(
      "Candidates: Prior rare condition B, Prior common condition, ",
      "Prior rare condition A"
    ) %in%
      elig_spec
  )
  expect_identical(
    spr_report_list_after(
      elig_emul,
      "Not eligible under the rare-condition rule: "
    ),
    FAST_EXCLUDED
  )
  expect_identical(
    spr_report_list_after(
      plan_emul,
      "Final adjustment set, after the rare-condition rule: "
    ),
    FAST_ADJUSTMENT_SET
  )
  expect_true("Adjust for rd_no_osd_common_everbefore (lifetime before baseline)" %in% plan_emul)
  expect_false(any(grepl("rare_[ab]", plan_emul)))
})

test_that("the protocol eligibility specification cell states the feasibility rule", {
  plan <- spr_report_fast_plan()
  ctx <- swereg:::.protocol_context(plan, plan$ett$ett_id[1])
  pt <- swereg:::.build_protocol_table(plan$spec, ctx)
  expect_identical(
    pt$`Target trial specification`[1],
    paste(
      c(
        "Study period: ISO years 2018 to 2019",
        "Exclude: No prior intervention (new-user)",
        "Include (enrollment 01): age 40 to 80",
        paste0(
          "Rare-condition rule, a pre-specified feasibility rule. A ",
          "candidate condition held by fewer than 10 persons in the smallest ",
          "arm of the driving comparisons becomes an exclusion criterion of ",
          "every comparison. The driving comparisons are ",
          FAST_DRIVER,
          ". Every other candidate stays a confounder"
        ),
        paste0(
          "Candidates: Prior rare condition B, Prior common condition, ",
          "Prior rare condition A"
        ),
        paste0(
          "The rule is decided from baseline counts in the eligible ",
          "population, before the comparator draw and without follow-up ",
          "outcomes"
        ),
        paste0(
          "A randomised trial cannot make eligibility depend on enrolled ",
          "counts. The emulation applies this rule for feasibility"
        )
      ),
      collapse = "\n"
    )
  )
})

test_that("without a sparse_rule no output names the rule", {
  plan <- spr_report_fast_plan()
  plan$spec$sparse_rule <- NULL
  plan$sparse_decision <- NULL
  txt <- spr_report_checklist(plan)
  expect_false(any(grepl("rare-condition", txt, fixed = TRUE)))
  ctx <- swereg:::.protocol_context(plan, plan$ett$ett_id[1])
  pt <- swereg:::.build_protocol_table(plan$spec, ctx)
  expect_false(any(grepl("rare-condition", unlist(pt), ignore.case = TRUE)))
  expect_identical(
    swereg:::.build_criterion_label_lookup(plan, "01", "x"),
    swereg:::.build_criterion_label_lookup(list(spec = plan$spec), "01", "x")
  )
})
