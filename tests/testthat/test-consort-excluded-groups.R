# =============================================================================
# The red "Excluded" box groups its bullets as CONSORT 2010 groups them
# =============================================================================
#
# CONSORT 2010 sorts the people who did not enter the cohort into three groups:
#
#   - they did not meet an inclusion criterion;
#   - they met an exclusion criterion;
#   - another reason.
#
# `.build_consort_dot()` prints one heading per non-empty group, and puts each
# criterion under its own heading.
#
# The heading carries the direction, and the label does not. An inclusion label
# reads "No prior MHT other than local", which is criterion-voiced. An exclusion
# label reads "Prior breast cancer", which is failure-voiced. Negating one voice
# breaks the other voice and breaks the protocol table. So no label changes.
#
# THE GROUP COMES FROM THE SPEC, NEVER FROM THE COLUMN-NAME PREFIX. A
# `no_prior_value` rule builds an `eligible_no_` column under an inclusion block
# and under an exclusion block alike. `eligible_no_rd_statin_1yr` below sits
# under an inclusion block, so prefix inference puts it in the wrong group.

skip_if_not_installed("data.table")

# --- fixtures ---------------------------------------------------------------

# One realistic eligibility cascade, in the pipeline's own order. The step
# names are the ones `R/tteplan_s1_prepare.R`, `R/tteplan_apply_exclusions.R`
# and `R/tteplan_s1a_worker.R` write.
.CEG_STEPS <- c(
  "before_exclusions",
  "eligible_valid_treatment",
  "eligible_isoyears",
  "eligible_has_osd_f20_to_f29_104wk",
  "eligible_age",
  "eligible_only_rd_approach1_single_everbefore",
  "eligible_no_rd_statin_1yr",
  "eligible_no_rd_exposure_everbefore",
  "landmark_observed"
)

# The five steps the spec's inclusion blocks build. The two washout columns in
# it carry an `eligible_only_` and an `eligible_no_` prefix, and both are
# inclusion criteria.
.CEG_INCLUSION <- c(
  "eligible_isoyears",
  "eligible_age",
  "eligible_has_osd_f20_to_f29_104wk",
  "eligible_only_rd_approach1_single_everbefore",
  "eligible_no_rd_statin_1yr"
)

.CEG_PERSONS <- c(10000, 9500, 9000, 8000, 7500, 7000, 6800, 6000, 5500)
.CEG_PERSON_TRIALS <- c(
  50000, 47000, 44000, 40000, 37000, 35000, 34000, 30000, 27000
)

.ceg_flow <- function() {
  ec <- list(
    attrition = data.table::data.table(
      trial_id = NA_integer_,
      criterion = .CEG_STEPS,
      n_persons = .CEG_PERSONS,
      n_person_trials = .CEG_PERSON_TRIALS,
      n_intervention = .CEG_PERSONS,
      n_comparator = .CEG_PERSON_TRIALS
    )
  )
  return(swereg:::.build_cohort_flow(ec))
}

.ceg_dot <- function(inclusion_steps = .CEG_INCLUSION) {
  return(swereg:::.build_consort_dot(
    flow = .ceg_flow(),
    eid = "01",
    label = "Systemic MHT (vs local or none)",
    intervention_label = "Systemic",
    comparator_label = "Local or none",
    inclusion_steps = inclusion_steps
  ))
}

# The `e1` node's label, split back into the lines the reader sees. `\l` is a
# left-justified newline, so it is the line separator inside the one node.
.ceg_excluded_lines <- function(dot) {
  all_lines <- strsplit(dot, "\n", fixed = TRUE)[[1]]
  node <- all_lines[grepl("^  e1 \\[label = '", all_lines)]
  body <- sub("^  e1 \\[label = '", "", node)
  body <- sub("', style = filled.*$", "", body)
  parts <- strsplit(body, "\\l", fixed = TRUE)[[1]]
  return(parts[nzchar(parts)])
}

# Group the lines: every line that is not a bullet opens a group, and the
# bullets under it belong to it.
.ceg_parse <- function(lines) {
  opener <- which(!startsWith(lines, "- "))
  out <- list()
  for (k in seq_along(opener)) {
    from <- opener[k] + 1L
    to <- if (k < length(opener)) opener[k + 1L] - 1L else length(lines)
    out[[lines[opener[k]]]] <- if (from > to) character() else lines[from:to]
  }
  return(out)
}

# The two counts a heading or a bullet carries.
.ceg_n <- function(s) {
  hit <- regmatches(
    s,
    regexec("\\(n = ([0-9,]+) persons / ([0-9,]+) person-trials\\)", s)
  )[[1]]
  return(c(
    persons = as.numeric(gsub(",", "", hit[[2]], fixed = TRUE)),
    person_trials = as.numeric(gsub(",", "", hit[[3]], fixed = TRUE))
  ))
}

.ceg_counts <- function(s) {
  return(t(vapply(s, .ceg_n, c(persons = 0, person_trials = 0))))
}

# The step name each bullet names, with its counts dropped.
.ceg_steps_of <- function(bullets) {
  return(sub(" \\(n = .*$", "", sub("^- ", "", bullets)))
}

# One washout entry, in the shape `additional_inclusion` accepts. This is the
# `test-only-prior-value.R` rule, placed under an inclusion block.
.ceg_washout <- function(type) {
  return(list(
    name = "MHT-naive at baseline",
    rationale = "The trial recruits MHT-naive women.",
    implementation = list(
      computed = TRUE,
      source_variable = "rd_approach1_single",
      type = type,
      value = "local_or_none_mht",
      window = "lifetime_before_baseline"
    )
  ))
}

# A minimal spec carrying one age rule and one washout rule, both under
# `additional_inclusion`. Read through the production reader, so the
# eligibility column names are the ones the pipeline writes.
.ceg_spec <- function(type = "only_prior_value") {
  spec <- list(
    study = list(
      title = "CONSORT excluded groups",
      implementation = list(project_prefix = "ceg", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2015L, 2015L)),
    enrollments = list(list(
      id = "01",
      name = "Systemic MHT vs local or none",
      observed_var = list(sentinel = "row_presence"),
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      additional_inclusion = list(
        list(
          name = "Age 40-80",
          type = "age_range",
          min = 40,
          max = 80,
          implementation = list(variable = "rd_age_continuous")
        ),
        .ceg_washout(type)
      ),
      treatment = list(
        description = "Initiation of systemic MHT.",
        arms = list(intervention = "Systemic", comparator = "Local or none"),
        implementation = list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_approach1_single",
          intervention_value = "systemic_mht",
          comparator_value = "local_or_none_mht",
          seed = 1L
        )
      )
    )),
    outcomes = list(list(
      name = "Outcome A",
      implementation = list(variable = "osd_a")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L))
  )
  dir <- withr::local_tempdir()
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(suppressMessages(swereg::tteplan_read_spec(path)))
}


# --- the three groups -------------------------------------------------------

test_that("every bullet sits under the CONSORT 2010 heading its spec block implies", {
  groups <- .ceg_parse(.ceg_excluded_lines(.ceg_dot()))

  expect_identical(
    sub(" \\(n = .*$", "", names(groups)),
    c(
      "Excluded",
      "Not meeting inclusion criteria",
      "Meeting exclusion criteria",
      "Other reasons"
    )
  )
  # The box total opens the label and carries no bullet of its own.
  expect_identical(groups[[1L]], character())
  # Pipeline order inside a group, and the spec decides the group. Two of these
  # five carry a washout prefix, and both are inclusion criteria.
  expect_identical(
    .ceg_steps_of(groups[[2L]]),
    c(
      "eligible_isoyears",
      "eligible_has_osd_f20_to_f29_104wk",
      "eligible_age",
      "eligible_only_rd_approach1_single_everbefore",
      "eligible_no_rd_statin_1yr"
    )
  )
  expect_identical(
    .ceg_steps_of(groups[[3L]]),
    "eligible_no_rd_exposure_everbefore"
  )
  expect_identical(
    .ceg_steps_of(groups[[4L]]),
    c("eligible_valid_treatment", "landmark_observed")
  )
})


test_that("a hand-built flow puts every criterion under the exclusion heading", {
  groups <- .ceg_parse(.ceg_excluded_lines(.ceg_dot(character())))

  expect_identical(
    sub(" \\(n = .*$", "", names(groups)),
    c("Excluded", "Meeting exclusion criteria", "Other reasons")
  )
  expect_identical(
    .ceg_steps_of(groups[[3L]]),
    c("eligible_valid_treatment", "landmark_observed")
  )
})


test_that("each heading count is the sum of its bullets, and the groups sum to the box total", {
  groups <- .ceg_parse(.ceg_excluded_lines(.ceg_dot()))
  totals <- .ceg_counts(names(groups))

  for (k in 2:nrow(totals)) {
    bullets <- .ceg_counts(groups[[k]])
    expect_equal(
      unname(totals[k, "persons"]),
      sum(bullets[, "persons"]),
      info = rownames(totals)[k]
    )
    expect_equal(
      unname(totals[k, "person_trials"]),
      sum(bullets[, "person_trials"]),
      info = rownames(totals)[k]
    )
  }
  expect_equal(sum(totals[-1L, "persons"]), unname(totals[1L, "persons"]))
  expect_equal(
    sum(totals[-1L, "person_trials"]),
    unname(totals[1L, "person_trials"])
  )
  # The box total is the cascade's own reduction, so the line above is not the
  # bullets restating themselves.
  expect_equal(
    unname(totals[1L, "persons"]),
    .CEG_PERSONS[[1L]] - .CEG_PERSONS[[length(.CEG_PERSONS)]]
  )
  expect_equal(
    unname(totals[1L, "person_trials"]),
    .CEG_PERSON_TRIALS[[1L]] - .CEG_PERSON_TRIALS[[length(.CEG_PERSON_TRIALS)]]
  )
})


# --- where the inclusion steps come from ------------------------------------

test_that("an inclusion-block washout rule is an inclusion step, whatever its column prefix", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("withr")

  for (type in c("only_prior_value", "no_prior_value")) {
    spec <- .ceg_spec(type)
    steps <- swereg:::.tte_inclusion_step_names(spec, spec$enrollments[[1L]])
    prefix <- if (identical(type, "only_prior_value")) {
      "eligible_only_"
    } else {
      "eligible_no_"
    }
    expect_true(
      paste0(prefix, "rd_approach1_single_everbefore") %in% steps,
      info = type
    )
    expect_true("eligible_isoyears" %in% steps, info = type)
    expect_true("eligible_age" %in% steps, info = type)
  }
})


# --- the production render path ---------------------------------------------

test_that("the production render path fills inclusion_steps from the spec", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("withr")
  skip_if_not_installed("DiagrammeR")
  skip_if_not_installed("DiagrammeRsvg")
  skip_if_not_installed("rsvg")

  spec <- .ceg_spec("no_prior_value")
  washout_col <- "eligible_no_rd_approach1_single_everbefore"
  ec <- list(
    attrition = data.table::data.table(
      trial_id = NA_integer_,
      criterion = c(
        "before_exclusions", "eligible_valid_treatment", "eligible_age",
        washout_col, "eligible_no_rd_cancer_everbefore"
      ),
      n_persons = c(10000, 9500, 9000, 8000, 7000),
      n_person_trials = c(50000, 47000, 44000, 40000, 35000),
      n_intervention = c(10000, 9500, 9000, 8000, 7000),
      n_comparator = c(20000, 19000, 18000, 16000, 14000)
    )
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "ceg",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = data.table::data.table(
      ett_id = "ETT00001",
      enrollment_id = "01",
      age_group = "40_80",
      age_min = 40L,
      age_max = 80L,
      follow_up = 52L,
      outcome_var = "osd_a",
      outcome_name = "Outcome A",
      outcome_description = "The fixture outcome",
      outcome_role = "primary",
      description = "ETT00001",
      confounder_vars = "rd_age_continuous",
      person_id_var = "lopnr",
      treatment_var = "rd_approach1_single",
      comparator_to_intervention_ratio = 2L,
      seed = 1L
    )
  )
  plan$spec <- spec
  plan$enrollment_counts <- list(`01` = ec)

  # `.render_consort_sidecars()` is the one caller of `.build_consort_dot()`,
  # and `$export_tables()` is what calls it. Capture the argument it fills and
  # the dot it gets back, then let the real renderer run.
  seen <- new.env(parent = emptyenv())
  orig <- swereg:::.build_consort_dot
  testthat::local_mocked_bindings(
    .build_consort_dot = function(...) {
      seen$args <- list(...)
      seen$dot <- orig(...)
      return(seen$dot)
    }
  )
  out <- swereg:::.render_consort_sidecars(
    plan = plan,
    ec = ec,
    eid = "01",
    label = "Systemic MHT (vs local or none)",
    output_dir = withr::local_tempdir()
  )

  expect_true(file.exists(out$png))
  expect_true(washout_col %in% seen$args$inclusion_steps)

  groups <- .ceg_parse(.ceg_excluded_lines(seen$dot))
  expect_identical(
    sub(" \\(n = .*$", "", names(groups)),
    c(
      "Excluded",
      "Not meeting inclusion criteria",
      "Meeting exclusion criteria",
      "Other reasons"
    )
  )
  # The spec labels these two, so the box shows prose and not a column name.
  expect_identical(
    .ceg_steps_of(groups[[2L]]),
    c(
      "Outside of age range (40 - 80 years)",
      paste(
        "No prior rd_approach1_single equal to local_or_none_mht",
        "(lifetime before baseline)"
      )
    )
  )
  expect_identical(.ceg_steps_of(groups[[4L]]), "Has invalid treatment")
})
