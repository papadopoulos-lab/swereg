# The generated prose states the minimum-events rule that runs. TARGET item
# 6h, the OSF Inference criteria field and the Analysis plan row of the target
# trial protocol table each read `spec$min_expected_events_per_arm_per_year`.
# A changed value changes the sentence. Each says that the expected events are
# computed under the null. No generated text names persons at risk as a
# withholding rule.
#
# The plan is spr_report_fast_plan() of helper-sparse_report.R. The OSF text
# reads spr_report_spec() of the same helper, written to a YAML file.

skip_if_not_installed("yaml")
skip_if_not_installed("withr")

# The first sentence of each text, for threshold `k`.
hrp_target_sentence <- function(k) {
  return(paste0(
    "A prespecified reporting rule, the minimum-events rule, required at ",
    "least ",
    k,
    " expected events per arm in each year of follow-up"
  ))
}
hrp_osf_sentence <- function(k) {
  return(paste0(
    "A prespecified reporting rule, the minimum-events rule, requires at ",
    "least ",
    k,
    " expected events per arm in each year of follow-up"
  ))
}

# Item 6h of the TARGET checklist of the fixture plan, at threshold `k`, as
# one string.
hrp_target_6h <- function(k, env = parent.frame()) {
  plan <- spr_report_fast_plan(env = env)
  plan$spec$min_expected_events_per_arm_per_year <- k
  txt <- spr_report_checklist(plan)
  return(paste(spr_report_item(txt, "6h", "7a-h"), collapse = " "))
}

# The OSF text of the fixture spec, at threshold `k`.
hrp_osf <- function(k, subgroups = FALSE) {
  spec <- spr_report_spec()
  spec$min_expected_events_per_arm_per_year <- k
  if (subgroups) {
    spec$subgroups <- list(list(
      name = "Age group",
      implementation = list(variable = "rd_agegroup")
    ))
  }
  dir <- withr::local_tempdir()
  path <- spr_write_spec(spec, dir)
  return(suppressWarnings(swereg::tteplan_osf_text(path)))
}

# The Analysis plan cell of the protocol table, at threshold `k`, as lines.
hrp_protocol_lines <- function(k, env = parent.frame()) {
  plan <- spr_report_fast_plan(env = env)
  plan$spec$min_expected_events_per_arm_per_year <- k
  ctx <- swereg:::.protocol_context(plan, plan$ett$ett_id[1])
  pt <- swereg:::.build_protocol_table(plan$spec, ctx)
  expect_identical(pt$`Protocol component`[7], "Analysis plan")
  return(strsplit(pt$`Target trial emulation`[7], "\n", fixed = TRUE)[[1]])
}

# The old rule: "fewer than <k> persons at risk".
HRP_OLD_RULE <- "fewer\\s+than\\s+\\S+\\s+persons\\s+at\\s+risk"


test_that("TARGET item 6h states the minimum-events value from the spec, per arm in each year of follow-up", {
  for (k in c(5, 7, 2.5)) {
    x <- hrp_target_6h(k)
    expect_identical(
      lengths(regmatches(x, gregexpr(hrp_target_sentence(k), x, fixed = TRUE))),
      1L,
      info = paste("threshold", k)
    )
    expect_match(x, paste0(hrp_target_sentence(k), "."), fixed = TRUE)
    # The sentence of another threshold is absent.
    for (other in setdiff(c(5, 7, 2.5), k)) {
      expect_false(
        grepl(hrp_target_sentence(other), x, fixed = TRUE),
        info = paste("threshold", k, "carries", other)
      )
    }
  }
  x <- hrp_target_6h(5)
  expect_match(
    x,
    "Follow-up was cut into years of 52 weeks from time zero, and the last year ended at the horizon.",
    fixed = TRUE
  )
  expect_match(
    x,
    "The expected events of an arm in a year were computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms.",
    fixed = TRUE
  )
  expect_match(
    x,
    "When any year failed the rule, the incidence rate ratio, risk difference and number needed to treat of that estimand were not reported.",
    fixed = TRUE
  )
  expect_match(
    x,
    "Which years passed depended on the observed events, through the pooled rate.",
    fixed = TRUE
  )
  expect_match(
    x,
    "The rule was a reporting rule and did not guarantee precision or coverage.",
    fixed = TRUE
  )
  expect_false(grepl("design-based", x, fixed = TRUE))
  expect_false(grepl(HRP_OLD_RULE, x, perl = TRUE))
})

test_that("the OSF Inference criteria field states the minimum-events value from the spec, per arm in each year of follow-up", {
  for (k in c(5, 7)) {
    osf <- hrp_osf(k)
    x <- osf[["Inference criteria"]]
    expect_match(x, paste0(hrp_osf_sentence(k), "."), fixed = TRUE)
    expect_false(
      grepl(hrp_osf_sentence(setdiff(c(5, 7), k)), x, fixed = TRUE),
      info = paste("threshold", k)
    )
    # Each fact sits in one field.
    expect_identical(
      sum(grepl("minimum-events rule", osf, fixed = TRUE)),
      1L,
      info = paste("threshold", k)
    )
  }
  x <- hrp_osf(5)[["Inference criteria"]]
  expect_match(
    x,
    "The expected events of an arm in a year are computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms.",
    fixed = TRUE
  )
  expect_match(
    x,
    "Its incidence rate ratio, risk difference and number needed to treat are reported as NE, not estimable.",
    fixed = TRUE
  )
  expect_match(
    x,
    "The reportable horizon is the end of the last year before the first year that fails.",
    fixed = TRUE
  )
  expect_match(
    x,
    "Which years pass depends on the observed events, through the pooled rate. The rule is a reporting rule and does not guarantee precision or coverage.",
    fixed = TRUE
  )
  expect_false(grepl("subgroup", x, fixed = TRUE))
  expect_match(
    hrp_osf(5, subgroups = TRUE)[["Inference criteria"]],
    "Its subgroup rate ratios and interaction tests are withheld too.",
    fixed = TRUE
  )
})

test_that("the protocol table Analysis plan cell states the minimum-events value from the spec", {
  for (k in c(5, 7)) {
    cell <- hrp_protocol_lines(k)
    rule <- cell[startsWith(cell, "Minimum-events rule: ")]
    expect_identical(
      rule,
      paste0(
        "Minimum-events rule: at least ",
        k,
        " expected events per arm in each year of follow-up, computed under ",
        "the null as the pooled weighted event rate of both arms times the ",
        "weighted person-years of the arm. Follow-up years are 52 weeks from ",
        "time zero. When any year up to the horizon fails, the rate ratio, ",
        "risk difference and number needed to treat of that estimand read NE"
      )
    )
  }
})

test_that("a threshold of 0 says that the rule withholds no estimate, and defines the expected events under the null", {
  expect_match(
    hrp_target_6h(0),
    paste0(
      hrp_target_sentence(0),
      ", so it withheld no estimate. The expected events of an arm in a year ",
      "were computed under the null: its weighted person-time multiplied by ",
      "the pooled weighted event rate of both arms."
    ),
    fixed = TRUE
  )
  expect_identical(
    hrp_osf(0)[["Inference criteria"]],
    paste0(
      hrp_osf_sentence(0),
      ", so it withholds no estimate. The expected events of an arm in a year ",
      "are computed under the null: its weighted person-time multiplied by ",
      "the pooled weighted event rate of both arms."
    )
  )
  cell <- hrp_protocol_lines(0)
  expect_identical(
    cell[startsWith(cell, "Minimum-events rule: ")],
    paste0(
      "Minimum-events rule: at least 0 expected events per arm in each year of ",
      "follow-up, computed under the null as the pooled weighted event rate ",
      "of both arms times the weighted person-years of the arm, so no ",
      "estimate is withheld"
    )
  )
})

test_that("no generated text names persons at risk as a withholding rule", {
  target <- hrp_target_6h(5)
  osf <- paste(hrp_osf(5), collapse = "\n")
  protocol <- paste(hrp_protocol_lines(5), collapse = "\n")
  for (x in list(target, osf, protocol)) {
    expect_false(grepl(HRP_OLD_RULE, x, perl = TRUE, ignore.case = TRUE))
    expect_false(grepl("too few at risk", x, fixed = TRUE))
  }
})

test_that("the TARGET checklist stops on a spec without the minimum-events key", {
  plan <- spr_report_fast_plan()
  plan$spec$min_expected_events_per_arm_per_year <- NULL
  expect_error(
    utils::capture.output(plan$print_target_checklist()),
    "print_target_checklist(): the plan's spec has no min_expected_events_per_arm_per_year",
    fixed = TRUE
  )
})
