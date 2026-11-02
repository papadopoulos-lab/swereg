# The two supplement images speak to a reader of the paper. The CONSORT title
# names the comparison and no internal id. A washout criterion and a Love plot
# row use the name that the specification gives them.

skip_if_not_installed("data.table")

.erl_spec <- function() {
  washout <- function(name, type, value, sv) {
    return(list(
      name = name,
      implementation = list(
        type = type,
        value = value,
        source_variable = sv,
        source_variable_combined = sv,
        window = "lifetime_before_baseline",
        window_weeks = Inf
      )
    ))
  }
  return(list(
    inclusion_criteria = list(isoyears = c(2008L, 2023L)),
    exclusion_criteria = list(
      washout("Prior statin use", "no_prior_value", TRUE, "rx_statin"),
      washout("No earlier hormone therapy", "no_prior_value", TRUE, "rx_hormone"),
      washout(NULL, "no_prior_value", TRUE, "rx_unnamed"),
      list(
        name = "No recorded visit",
        implementation = list(
          source_variable = "rd_novisit",
          source_variable_combined = "rd_novisit",
          window = "lifetime_before_baseline",
          window_weeks = Inf
        )
      )
    ),
    enrollments = list(list(
      id = "01",
      additional_inclusion = list(
        washout("No earlier systemic MHT", "only_prior_value", "none", "rd_mht")
      )
    )),
    confounders = list(
      list(name = "Education level", implementation = list(variable = "rd_education")),
      list(
        name = "Psychotropic use",
        implementation = list(variable = "rd_no_rx_n05_52wk", computed = TRUE)
      ),
      list(name = "Unmapped", implementation = list())
    )
  ))
}

.erl_labels <- function(crit) {
  plan <- list(spec = .erl_spec(), get_sparse_decision = function() return(NULL))
  labels <- swereg:::.build_criterion_label_lookup(
    plan,
    enrollment_id = "01",
    observed_criteria = crit
  )
  return(unname(labels[crit]))
}

test_that("the CONSORT title names no enrollment id", {
  flow <- swereg:::.build_cohort_flow(list(
    attrition = data.table::data.table(
      enrollment_period_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_isoyears", "landmark_observed"),
      n_persons = c(100, 90, 80),
      n_person_trials = c(500, 450, 400),
      n_intervention = c(10, 9, 8),
      n_comparator = c(490, 441, 392)
    )
  ))
  dot <- swereg:::.build_consort_dot(
    flow = flow,
    eid = "03",
    label = "Drug vs none",
    intervention_label = "Drug",
    comparator_label = "None"
  )
  expect_match(dot, "title [label = 'Drug vs none'", fixed = TRUE)
  expect_no_match(dot, "Enrollment 03", fixed = TRUE)
})

test_that("a washout criterion uses its spec name in the CONSORT lookup", {
  expect_match(
    .erl_labels("eligible_only_rd_mht_everbefore"),
    "^No earlier systemic MHT"
  )
  expect_match(.erl_labels("eligible_no_rx_statin_everbefore"), "^Prior statin use")
  # An exclusion step counts the people who failed the rule.
  expect_match(
    .erl_labels("eligible_no_rx_hormone_everbefore"),
    "^Earlier hormone therapy"
  )
  # With no name, the generated rule text is the only label there is.
  expect_match(.erl_labels("eligible_no_rx_unnamed_everbefore"), "^Prior rx_unnamed")
  # Only a washout rule has a failure voice.
  expect_match(.erl_labels("eligible_no_rd_novisit_everbefore"), "^No recorded visit")
})

test_that("a Love plot row uses the confounder name from the spec", {
  panel <- function(smd) {
    return(data.table::data.table(
      variable = c("rd_education", "rd_no_rx_n05_52wk", "rd_age (mean (SD))"),
      smd_numeric = smd
    ))
  }
  df <- swereg:::.build_love_df(
    panel(c(0.3, 0.2, 0.1)),
    panel(c(0.03, 0.02, 0.01)),
    labels = swereg:::.tte_confounder_labels(.erl_spec())
  )
  # A variable the spec does not name keeps its own name.
  expect_setequal(
    as.character(df$variable),
    c("Education level", "Psychotropic use", "rd_age")
  )
  # No labels leaves every row as the data names it.
  expect_setequal(
    as.character(swereg:::.build_love_df(panel(0.1), panel(0.1))$variable),
    c("rd_education", "rd_no_rx_n05_52wk", "rd_age")
  )
})

test_that("two confounders with one name keep separate Love plot rows", {
  spec <- list(confounders = list(
    list(name = "Same", implementation = list(variable = "rd_a")),
    list(name = "Same", implementation = list(variable = "rd_b")),
    list(name = "Other", implementation = list(variable = "rd_c"))
  ))
  panel <- data.table::data.table(
    variable = c("rd_a", "rd_b", "rd_c"),
    smd_numeric = c(0.3, 0.2, 0.1)
  )
  df <- swereg:::.build_love_df(
    panel,
    panel,
    labels = swereg:::.tte_confounder_labels(spec)
  )
  expect_identical(data.table::uniqueN(df$variable), 3L)
  expect_setequal(as.character(df$variable), c("rd_a", "rd_b", "Other"))
})
