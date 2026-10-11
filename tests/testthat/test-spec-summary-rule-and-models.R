# Two texts that name the confounders.
#
# `$print_spec_summary()` states the rare-condition rule with the rows of the
# Study Specification sheet: min_n, the driving enrollments, the candidates,
# and the decision or "not decided yet".
#
# The Study Specification sheet and item 6g of the TARGET checklist say which
# models the adjustment set enters. The treatment model holds no time term
# and no calendar term. The outcome model holds no confounder.
#
# Every plan here is spr_report_fast_plan() of helper-sparse_report.R: a
# hand-made decision with no s1 run. At min_n = 10 it excludes "Prior rare
# condition B" and "Prior rare condition A", and adjusts for "Age" and "Prior
# common condition".

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("openxlsx")

SSM_MODELS <- paste0(
  "The adjustment set enters the treatment model and the per-protocol ",
  "censoring models. The treatment model and the time-zero censoring model ",
  "hold the adjustment set alone, with no time term and no calendar term. ",
  "The loss and deviation censoring models add the time since time zero and ",
  "the calendar period of follow-up. The outcome model holds no confounder: ",
  "it holds the treatment, the time since time zero and the trial index."
)

SSM_RULE <- paste0(
  "A candidate condition held by fewer than 10 persons in the smallest arm ",
  "of the driving comparison becomes an exclusion criterion of every ",
  "comparison. The driving comparison is enrollment 01 (Treated vs control ",
  "01). Every other candidate stays a confounder. The rule is decided from ",
  "baseline counts in the eligible population, before the comparator draw ",
  "and without follow-up outcomes. The other comparisons inherit the ",
  "decision."
)

# The printed summary as plain lines, without the ANSI colours.
ssm_print <- function(plan) {
  return(trimws(
    gsub("\033\\[[0-9;]*m", "", utils::capture.output(plan$print_spec_summary())),
    which = "right"
  ))
}

# The block of the printed summary from the rule header to the blank line.
ssm_rule_block <- function(txt) {
  a <- which(txt == "Rare-condition rule (sparse_rule):")
  expect_length(a, 1L)
  b <- a + which(txt[(a + 1L):length(txt)] == "")[1L]
  return(txt[a:(b - 1L)])
}


test_that("print_spec_summary states the rule and the frozen decision", {
  plan <- spr_report_fast_plan()
  expect_identical(
    ssm_rule_block(ssm_print(plan)),
    c(
      "Rare-condition rule (sparse_rule):",
      "  Threshold (min_n):    10",
      "  Decided by:           enrollment 01 (Treated vs control 01)",
      paste0("  Rule:                 ", SSM_RULE),
      paste0(
        "  Candidates:           Prior rare condition B, Prior common ",
        "condition, Prior rare condition A"
      ),
      paste0(
        "  Excluded conditions:  Prior rare condition B, Prior rare ",
        "condition A"
      ),
      "  Final adjustment set: Age, Prior common condition",
      paste0("  Models:               ", SSM_MODELS)
    )
  )
})


test_that("print_spec_summary says the decision is not decided yet before s1", {
  plan <- spr_report_fast_plan()
  plan$sparse_decision <- NULL
  block <- ssm_rule_block(ssm_print(plan))
  expect_identical(
    block[length(block)],
    "  Decision:             not decided yet (s1 has not run)"
  )
  expect_false(any(grepl("Excluded conditions:", block, fixed = TRUE)))
  expect_false(any(grepl("Final adjustment set:", block, fixed = TRUE)))
})


test_that("print_spec_summary prints no rule without a sparse_rule", {
  plan <- spr_report_fast_plan()
  plan$sparse_decision <- NULL
  plan$spec$sparse_rule <- NULL
  txt <- ssm_print(plan)
  expect_false(any(grepl("Rare-condition rule", txt, fixed = TRUE)))
  # The Confounders block still prints, so the summary is not empty.
  expect_true("Confounders:" %in% txt)
})


test_that("Study Specification says which models the final adjustment set enters", {
  plan <- spr_report_fast_plan()
  ss <- spr_report_read_sheet(
    function(wb) swereg:::.write_spec_summary(wb, plan),
    "Study Specification"
  )
  at <- which(ss[[1]] == "Final adjustment set:")
  expect_length(at, 1L)
  expect_identical(ss[[2]][at], "Age, Prior common condition")
  expect_identical(ss[[1]][at + 1L], "Models:")
  expect_identical(ss[[2]][at + 1L], SSM_MODELS)
})


test_that("TARGET 6g says which models the adjustment set enters", {
  plan <- spr_report_fast_plan()
  g <- trimws(spr_report_item(spr_report_checklist(plan), "6g", "6h"))
  g <- g[nzchar(g)]
  # The last line before the fill-in marker.
  expect_identical(g[length(g) - 1L], SSM_MODELS)
  expect_identical(
    g[length(g) - 2L],
    paste0(
      "Not adjusted for, because the rare-condition rule made each an ",
      "exclusion criterion (item 6a): Prior rare condition B, Prior rare ",
      "condition A"
    )
  )
})


test_that("TARGET 6g says which models the adjustment set enters without a sparse_rule", {
  plan <- spr_report_fast_plan()
  plan$sparse_decision <- NULL
  plan$spec$sparse_rule <- NULL
  g <- trimws(spr_report_item(spr_report_checklist(plan), "6g", "6h"))
  g <- g[nzchar(g)]
  expect_identical(g[length(g) - 1L], SSM_MODELS)
  expect_false(any(grepl("Final adjustment set", g, fixed = TRUE)))
})


# The Analysis plan cell of the target trial protocol table, as lines.
ssm_analysis_cell <- function(plan) {
  ctx <- swereg:::.protocol_context(plan, plan$ett$ett_id[1])
  pt <- swereg:::.build_protocol_table(plan$spec, ctx)
  expect_identical(pt$`Protocol component`[7], "Analysis plan")
  return(strsplit(pt$`Target trial emulation`[7], "\n", fixed = TRUE)[[1]])
}

SSM_OUTCOME_LINE <- paste0(
  "Outcome model: survey-weighted quasi-Poisson regression on the ",
  "treatment, the time since time zero and the trial index, with a ",
  "person-time offset and person-level clustered standard errors. It holds ",
  "no confounder"
)


test_that("the protocol Analysis plan cell says which models adjust for each confounder", {
  cell <- ssm_analysis_cell(spr_report_fast_plan())
  expect_identical(
    cell[3:5],
    c(
      "Treatment and censoring models adjust for rd_age_continuous",
      paste0(
        "Treatment and censoring models adjust for ",
        "rd_no_osd_common_everbefore (lifetime before baseline)"
      ),
      paste0(
        "Final adjustment set of the treatment and censoring models, after ",
        "the rare-condition rule: Age, Prior common condition"
      )
    )
  )
  # No confounder line follows the outcome model line.
  at <- which(cell == SSM_OUTCOME_LINE)
  expect_length(at, 1L)
  expect_false(any(grepl("adjust for", cell[at:length(cell)], fixed = TRUE)))
  expect_false(any(startsWith(cell, "Adjust for ")))
})


test_that("the protocol Analysis plan cell keeps the order without a sparse_rule", {
  plan <- spr_report_fast_plan()
  plan$sparse_decision <- NULL
  plan$spec$sparse_rule <- NULL
  cell <- ssm_analysis_cell(plan)
  adjust <- grep("^Treatment and censoring models adjust for ", cell)
  expect_identical(adjust, 3:6)
  expect_identical(cell[7], paste0(
    "Truncation: the intention-to-treat weight, and the per-protocol ",
    "product of the treatment and censoring weights, at the 1st and 99th ",
    "percentiles"
  ))
  expect_identical(cell[8], SSM_OUTCOME_LINE)
})


test_that("TARGET item 7 says which models the final adjustment set enters", {
  txt <- paste(
    spr_report_item(spr_report_checklist(spr_report_fast_plan()), "7a-h", "8"),
    collapse = " "
  )
  expect_match(
    txt,
    paste0(
      "The final adjustment set of the treatment model and the censoring ",
      "models was Age, Prior common condition. The outcome model held no ",
      "confounder."
    ),
    fixed = TRUE
  )
  expect_no_match(txt, "The final adjustment set was", fixed = TRUE)
})
