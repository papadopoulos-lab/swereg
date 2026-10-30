# The ETT table, print() output and OSF text of swereg 27.3.1 (13556d35)
# for the spec of test (c) in test-follow-up-only-enrollments.R: follow-ups
# 52 and 104 weeks, two outcomes, enrollments "01" and "02", no
# only_enrollments. Measured by running that version with pkgload::load_all().
# `print` is the whole print() output, measured with
# `plan$created_at <- as.POSIXct("2026-10-30 12:00:00", tz = "UTC")`, so
# the "Created:" line is fixed. The test sets the same value. `osf` holds
# "<version>" where the text names the package version, and the test puts
# the running version back before it compares.

OE_2731 <- list(
  ett = structure(
    list(
      enrollment_id = c("01", "01", "01", "01", "02", "02", "02", "02"),
      ett_id = c(
        "ETT00001",
        "ETT00002",
        "ETT00003",
        "ETT00004",
        "ETT00005",
        "ETT00006",
        "ETT00007",
        "ETT00008"
      ),
      age_group = c(
        "40_80",
        "40_80",
        "40_80",
        "40_80",
        "40_55",
        "40_55",
        "40_55",
        "40_55"
      ),
      age_min = c(40, 40, 40, 40, 40, 40, 40, 40),
      age_max = c(80, 80, 80, 80, 55, 55, 55, 55),
      follow_up = c(52L, 104L, 52L, 104L, 52L, 104L, 52L, 104L),
      outcome_var = c(
        "osd_a",
        "osd_a",
        "osd_b",
        "osd_b",
        "osd_a",
        "osd_a",
        "osd_b",
        "osd_b"
      ),
      outcome_name = c(
        "Outcome A",
        "Outcome A",
        "Outcome B",
        "Outcome B",
        "Outcome A",
        "Outcome A",
        "Outcome B",
        "Outcome B"
      ),
      outcome_description = c("a", "a", "b", "b", "a", "a", "b", "b"),
      outcome_role = c(
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_
      ),
      description = c(
        "ETT00001: Outcome A (01, 52w, age 40-80)",
        "ETT00002: Outcome A (01, 104w, age 40-80)",
        "ETT00003: Outcome B (01, 52w, age 40-80)",
        "ETT00004: Outcome B (01, 104w, age 40-80)",
        "ETT00005: Outcome A (02, 52w, age 40-55)",
        "ETT00006: Outcome A (02, 104w, age 40-55)",
        "ETT00007: Outcome B (02, 52w, age 40-55)",
        "ETT00008: Outcome B (02, 104w, age 40-55)"
      ),
      file_raw = c(
        "sprtest_raw_01.qs2",
        "sprtest_raw_01.qs2",
        "sprtest_raw_01.qs2",
        "sprtest_raw_01.qs2",
        "sprtest_raw_02.qs2",
        "sprtest_raw_02.qs2",
        "sprtest_raw_02.qs2",
        "sprtest_raw_02.qs2"
      ),
      file_imp = c(
        "sprtest_imp_01.qs2",
        "sprtest_imp_01.qs2",
        "sprtest_imp_01.qs2",
        "sprtest_imp_01.qs2",
        "sprtest_imp_02.qs2",
        "sprtest_imp_02.qs2",
        "sprtest_imp_02.qs2",
        "sprtest_imp_02.qs2"
      ),
      file_analysis = c(
        "sprtest_analysis_ETT00001.qs2",
        "sprtest_analysis_ETT00002.qs2",
        "sprtest_analysis_ETT00003.qs2",
        "sprtest_analysis_ETT00004.qs2",
        "sprtest_analysis_ETT00005.qs2",
        "sprtest_analysis_ETT00006.qs2",
        "sprtest_analysis_ETT00007.qs2",
        "sprtest_analysis_ETT00008.qs2"
      ),
      file_analysis_itt = c(
        "sprtest_analysis_itt_ETT00001.qs2",
        "sprtest_analysis_itt_ETT00002.qs2",
        "sprtest_analysis_itt_ETT00003.qs2",
        "sprtest_analysis_itt_ETT00004.qs2",
        "sprtest_analysis_itt_ETT00005.qs2",
        "sprtest_analysis_itt_ETT00006.qs2",
        "sprtest_analysis_itt_ETT00007.qs2",
        "sprtest_analysis_itt_ETT00008.qs2"
      ),
      confounder_vars = list(
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        ),
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        ),
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        ),
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        ),
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        ),
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        ),
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        ),
        c(
          "rd_age_continuous",
          "rd_no_osd_rare_a_everbefore",
          "rd_no_osd_common_everbefore",
          "rd_no_osd_rare_b_everbefore"
        )
      ),
      subgroup_vars = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
      person_id_var = c("id", "id", "id", "id", "id", "id", "id", "id"),
      treatment_var = c(
        "baseline_intervention",
        "baseline_intervention",
        "baseline_intervention",
        "baseline_intervention",
        "baseline_intervention",
        "baseline_intervention",
        "baseline_intervention",
        "baseline_intervention"
      ),
      time_treatment_var = c(
        "rd_intervention",
        "rd_intervention",
        "rd_intervention",
        "rd_intervention",
        "rd_intervention",
        "rd_intervention",
        "rd_intervention",
        "rd_intervention"
      ),
      eligible_var = c(
        "eligible",
        "eligible",
        "eligible",
        "eligible",
        "eligible",
        "eligible",
        "eligible",
        "eligible"
      ),
      observed_var = list(
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        ),
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        ),
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        ),
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        ),
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        ),
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        ),
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        ),
        structure(
          list(column = NA_character_, sentinel = "row_presence"),
          class = "tte_observed_var"
        )
      ),
      intervention_tolerance_weeks = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
      comparator_tolerance_weeks = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
      feasibility_horizon = c(
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE
      ),
      treatment_impl = list(
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 1L
        ),
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 1L
        ),
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 1L
        ),
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 1L
        ),
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 2L
        ),
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 2L
        ),
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 2L
        ),
        list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_tx",
          intervention_value = "treated",
          comparator_value = "control",
          seed = 2L
        )
      ),
      comparator_to_intervention_ratio = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
      seed = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)
    ),
    row.names = c(NA, -8L),
    class = "data.frame"
  ),
  print = c(
    "<TTEPlan> sprtest ",
    "  Created: 2026-10-30 12:00:00 ",
    "  Skeleton files: 1",
    "  2 outcome(s) x 2 follow-up x 2 enrollment(s) = 8 ETT(s)",
    "  Enrollments:",
    " enrollment_id max_follow_up n_ett",
    "            01          104w     4",
    "            02          104w     4",
    "",
    "  ETTs:",
    "   ett_id outcome_name follow_up enrollment_id",
    " ETT00001    Outcome A       52w            01",
    " ETT00002    Outcome A      104w            01",
    " ETT00003    Outcome B       52w            01",
    " ETT00004    Outcome B      104w            01",
    " ETT00005    Outcome A       52w            02",
    " ETT00006    Outcome A      104w            02",
    " ETT00007    Outcome B       52w            02",
    " ETT00008    Outcome B      104w            02",
    ""
  ),
  osf = c(
    `Study design` = "The study emulates a sequence of target trials in register data. Calendar time from ISO year 2018 to ISO year 2019 is cut into consecutive enrollment periods of 4 weeks, and each enrollment period opens one trial in each comparison. Eligibility is assessed in every week. A person can enter more than one trial.\n\nEach person is assigned to a treatment strategy from the weeks of the enrollment period, as described under 'Measured variables/Exposures'.\n\nTime zero is the first week after the enrollment period closes, in both arms. Only persons who reach time zero under observation and free of every outcome listed under 'Measured variables/Outcomes' enter the trial.\n\nFollow-up starts at time zero. It stops at the first of: the outcome; loss of observation, which is the first week after time zero without observation; the administrative end of the data; and the end of the analysis horizon. Death censors follow-up, and no competing-risk model is fitted. The horizons are 1 year (52 weeks) and 2 years (104 weeks).\n\nUnder the per-protocol estimand, follow-up also stops at the first week that does not hold the assigned strategy, beyond the tolerance of the arm. A week with a missing treatment value counts as such a week. Under the intention-to-treat estimand, a deviation does not stop follow-up.\n\nThe tolerance is 0 weeks in every arm, so the first such week is a deviation.\n\nThe statistical analysis is described under 'Statistical models'.",
    `Sample size` = "In each comparison and each trial, every eligible person in the intervention arm is enrolled. Comparators are drawn at random from the eligible persons in the comparator arm: 2 for each enrolled person in the intervention arm, or all of them if the trial holds fewer. The draw is made within each trial and creates no matched sets, so no analysis conditions on matched sets.\n\nA person can enter several trials, so the number of person-trials can exceed the number of persons. The number of person-trials in each comparison follows from this rule and the data.",
    `Measured variables/Exposures` = "The study has 2 comparisons. Each compares an intervention strategy, named first, with a comparator strategy.\n\n1. Treated versus Control.\n2. Treated versus Control.\n\nAssignment uses only the weeks of the enrollment period in which the person is eligible and holds one of the two compared strategies. A person enters the intervention arm when at least one of those weeks holds the intervention strategy, and the comparator arm when all of them hold the comparator strategy. A person with no such week does not enter that trial. Eligibility is assessed week by week, so a treatment started earlier in the enrollment period can make a later week ineligible.",
    `Measured variables/Outcomes` = "1. Outcome A: a\n2. Outcome B: b\n\nEach outcome counts its first occurrence at or after time zero. The codes are listed under 'Code list'.",
    `Measured variables/Baseline covariates` = "- Age.\n- Prior rare condition A, at any time before baseline.\n- Prior common condition, at any time before baseline.\n- Prior rare condition B, at any time before baseline.\n\nEach covariate is measured at the recruiting week: the first week of the enrollment period in which the person is eligible and holds one of the two compared strategies.\n\nEvery covariate enters both weight models. The treatment weights use its value at the recruiting week. The censoring weights use its value in each follow-up interval, together with the time since time zero and calendar time. Calendar time does not enter the treatment weights. The outcome model holds no confounder. It adjusts for the trial, which is calendar time at time zero, as described under 'Statistical models'.",
    `Statistical models` = "Each comparison is analysed separately, for each outcome and each horizon.\n\nThe analysis uses the R package swereg, version <version> at registration. The version used will be reported with the results. swereg carries out sequential target trial emulation as its methods article describes (https://papadopoulos-lab.github.io/swereg/articles/tte-methods.html). Any change to the method between registration and analysis will be recorded in the deviations log.\n\nThe analysis estimates the per-protocol and intention-to-treat effects. The treatment weights are stabilized inverse probability of treatment weights, from a logistic regression of the assigned arm on the covariates. The per-protocol analysis adds stabilized inverse probability of censoring weights, from three models in each arm: loss of observation, deviation, and deviation at time zero. The intention-to-treat weight, and the per-protocol product of the treatment and censoring weights, are truncated at the 1st and 99th percentiles.\n\nThe outcome model is a weighted quasi-Poisson regression of the outcome on the treatment, the time since time zero and the trial, with a person-time offset. It holds no confounder, so the exponentiated treatment coefficient is the marginal incidence rate ratio. Standard errors are clustered on the person, because a person can enter several trials. The absolute effect is the risk difference at the end of each horizon, with a percentile interval from 500 bootstrap replicates that resample persons. Each effect is reported with its 95% confidence interval.\n\nCovariate balance is reported as standardized mean differences before and after weighting, together with the distribution of the weights.\n\nThe study-specific inputs are given elsewhere in this registration: the enrollment period and follow-up under 'Study design', the sampling under 'Sample size', the treatment strategies under 'Measured variables/Exposures' and the covariates under 'Measured variables/Baseline covariates'.",
    Transformations = "In each comparison, the treatment is coded 1 for the intervention strategy and 0 for the comparator strategy. Each outcome is a binary indicator.\n\nFollow-up is split into intervals of 4 weeks from time zero. The outcome model and the loss and deviation censoring models carry the person-time of each interval as an offset. The treatment model and the time-zero censoring model are logistic regressions on one row per person-trial.\n\nCovariates enter the weight models as main effects, with no interaction and no spline. A numeric covariate enters as one linear term, and a binary covariate as one term. A categorical covariate enters as indicator variables, with its first level as the reference; the choice of reference does not change the fitted weights. The categories are listed under 'Code list'.\n\nThe time since time zero, the trial and calendar time each enter as a spline, as a factor, or not at all, chosen by the number of distinct values, as section 1.8.9 of the methods article describes.",
    `Inference criteria` = "A prespecified reporting rule, the minimum-events rule, requires at least 0 expected events per arm in each year of follow-up, so it withholds no estimate. The expected events of an arm in a year are computed under the null: its weighted person-time multiplied by the pooled weighted event rate of both arms.",
    `Data inclusion and exclusion` = "Each criterion is assessed in each week of the enrollment period. A look-back window counts the calendar weeks before the week assessed, not that week itself. 'At any time before baseline' means from the start of the data.\n\nInclusion criteria:\n\n1. Trial entry in ISO years 2018 to 2019.\n2. In comparison 1 (Treated versus Control) only: Age 40 to 80 years.\n3. In comparison 2 (Treated versus Control) only: Age 40 to 55 years.\n\nExclusion criteria:\n\n1. No prior intervention (new-user), at any time before baseline.\n\nNo person is excluded for an extreme value of a measured variable. Extreme weights are truncated, as described under 'Statistical models'.",
    `Missing data` = "A missing covariate value at the recruiting week is replaced by one value drawn at random from the observed values of that covariate (single hot-deck imputation). During follow-up, a missing covariate value is carried forward from the last observed value of the person-trial. A missing treatment value removes that week from the assignment in the enrollment period. During follow-up it is handled as 'Study design' describes. The rules are those of section 1.8.7 of the methods article (https://papadopoulos-lab.github.io/swereg/articles/tte-methods.html).\n\nThe number of missing values of each baseline covariate will be reported before imputation.",
    `Other planned analysis` = "The per-protocol analysis is repeated with untruncated weights, and the result is reported beside the truncated one.",
    `Code list` = "- No prior intervention (new-user) (exclusion). Codes: not resolved. Window: at any time before baseline.\n- Age (covariate). Codes: not resolved.\n- Prior rare condition A (covariate). Codes: not resolved. Window: at any time before baseline.\n- Prior common condition (covariate). Codes: not resolved. Window: at any time before baseline.\n- Prior rare condition B (covariate). Codes: not resolved. Window: at any time before baseline.\n- Outcome A (outcome). Codes: not resolved.\n- Outcome B (outcome). Codes: not resolved.\n\nEach code matches every code that starts with it. A code that starts with ! is removed from the codes before it."
  )
)
