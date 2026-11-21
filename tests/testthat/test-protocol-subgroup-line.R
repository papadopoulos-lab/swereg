# The minimum-events line of the Analysis plan cell of the protocol table
# states the subgroup withholding of swereg 27.3.21 exactly when the spec has
# a non-empty `subgroups:` and a threshold above 0.

# The fixture spec of helper-sparse_report.R, written to YAML and read back by
# tteplan_read_spec(). Enrollment 02 is exempt when `exempt` is TRUE.
.psl_spec <- function(subgroups, k = 5, exempt = FALSE, env = parent.frame()) {
  spec <- spr_report_spec()
  spec$min_expected_events_per_arm_per_year <- k
  if (subgroups) {
    spec$subgroups <- list(list(
      name = "Age group",
      implementation = list(variable = "rd_agegroup")
    ))
  }
  if (exempt) {
    spec$enrollments[[2]]$minimum_events_rule_exempt <- TRUE
  }
  dir <- withr::local_tempdir(.local_envir = env)
  out <- suppressWarnings(swereg::tteplan_read_spec(spr_write_spec(spec, dir)))
  expect_identical(length(out$subgroups), if (subgroups) 1L else 0L)
  return(out)
}

# The lines of swereg 27.3.21 (commit 2e2701aa) for this fixture at threshold
# 5, captured from that tree. Without subgroups the line MUST stay this.
PSL_27_3_21 <- "Minimum-events rule: at least 5 expected events per arm in each year of follow-up, computed under the null as the pooled weighted event rate of both arms times the weighted person-years of the arm. Follow-up years are 52 weeks from time zero. When any year up to the horizon fails, the rate ratio, risk difference and number needed to treat of that estimand read NE"
PSL_27_3_21_EXEMPT <- "Minimum-events rule: at least 5 expected events per arm in each year of follow-up, computed under the null as the pooled weighted event rate of both arms times the weighted person-years of the arm. Follow-up years are 52 weeks from time zero. When any year up to the horizon fails, except in an exempt comparison, the rate ratio, risk difference and number needed to treat of that estimand read NE. The specification exempts Treated vs control 02 from the rule, so the rule withholds none of its estimates."

PSL_SG <- c(
  "Its subgroup rate ratios and interaction tests read NE too",
  "A subgroup rate ratio also reads NE when its own subgroup level fails the rule",
  "The interaction test of a subgroup reads NE when any of its levels fails"
)
PSL_EX <- ", except in an exempt comparison"
PSL_ONE <- "The specification exempts Treated vs control 02 from the rule, so the rule withholds none of its estimates."

test_that("(a) a threshold above 0 with subgroups: carries the three subgroup sentences", {
  x <- swereg:::.protocol_horizon_rule_line(.psl_spec(TRUE))
  expect_identical(
    x,
    paste0(PSL_27_3_21, ". ", paste(PSL_SG, collapse = ". "))
  )
  for (s in PSL_SG) {
    expect_match(x, s, fixed = TRUE)
  }
})

test_that("(b) a threshold above 0 without subgroups: gives the 27.3.21 line byte for byte", {
  x <- swereg:::.protocol_horizon_rule_line(.psl_spec(FALSE))
  expect_identical(x, PSL_27_3_21)
  expect_identical(charToRaw(x), charToRaw(PSL_27_3_21))
  y <- swereg:::.protocol_horizon_rule_line(.psl_spec(FALSE, exempt = TRUE))
  expect_identical(y, PSL_27_3_21_EXEMPT)
  expect_identical(charToRaw(y), charToRaw(PSL_27_3_21_EXEMPT))
})

test_that("(c) a threshold of 0 with subgroups: carries no subgroup sentence", {
  for (exempt in c(FALSE, TRUE)) {
    x <- swereg:::.protocol_horizon_rule_line(
      .psl_spec(TRUE, k = 0, exempt = exempt)
    )
    want <- paste0(
      "Minimum-events rule: at least 0 expected events per arm in each year of follow-up, so no estimate is withheld. The expected events are computed under the null as the pooled weighted event rate of both arms times the weighted person-years of the arm",
      if (exempt) paste0(". ", PSL_ONE)
    )
    expect_identical(x, want, info = paste("exempt", exempt))
    expect_false(
      grepl("subgroup", x, fixed = TRUE),
      info = paste("exempt", exempt)
    )
  }
})

test_that("(d) with an exempt enrollment all three subgroup sentences carry the exemption", {
  x <- swereg:::.protocol_horizon_rule_line(.psl_spec(TRUE, exempt = TRUE))
  expect_identical(
    x,
    paste0(
      sub(paste0(". ", PSL_ONE), "", PSL_27_3_21_EXEMPT, fixed = TRUE),
      ". ",
      paste(paste0(PSL_SG, PSL_EX), collapse = ". "),
      ". ",
      PSL_ONE
    )
  )
  for (s in PSL_SG) {
    expect_match(x, paste0(s, PSL_EX), fixed = TRUE)
  }
  # The withholding sentence and the three subgroup sentences, four in all.
  expect_identical(
    lengths(gregexpr("except in an exempt comparison", x, fixed = TRUE)),
    4L
  )
})
