# =============================================================================
# The landmark exclusions in the CONSORT Excluded box
# =============================================================================
#
# `.tte_qualify_bands()` appends three landmark steps to the attrition table.
# The landmark is the week after the entry band closes, and follow-up starts
# there.
#
#   - `landmark_candidates`: every band reaches this step. Its mask is
#     `rep(TRUE, nrow(bands))`, so the step excludes nobody.
#   - `landmark_observed`: the person must hold a row at the landmark week.
#   - `landmark_event_free`: the person must not already have had the outcome.
#
# The two real exclusions carry a label, so the box shows prose and not a
# column name. `landmark_candidates` carries no label and gets no bullet. A
# step that excludes nobody is not a reason for exclusion.
#
# The landmark steps come from the qualification code and never from the spec.
# Their labels therefore sit above the `spec = NULL` return of
# `.build_criterion_label_lookup()`. One test below reads them with a NULL
# spec, and one reads them with a populated spec.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")

.cll_period_width <- 4L

# Eight consecutive ISO year-weeks that start on a band boundary. Under
# `period_width = 4` they form two whole bands.
.cll_weeks <- function(n_weeks = 8L) {
  wk <- data.table::copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  start_idx <- wk[
    isoyearweek >= "2020-01" & (idx - 1L) %% .cll_period_width == 0L
  ]$idx[1]
  return(wk$isoyearweek[start_idx:(start_idx + n_weeks - 1L)])
}

# One person. `tx` names the 1-indexed weeks she is in the intervention arm.
# `events` names the weeks the outcome fires. `drop` names the weeks she holds
# no row at all.
.cll_person <- function(
  id,
  weeks,
  tx = integer(0),
  events = integer(0),
  drop = integer(0),
  eligible = seq_along(weeks)
) {
  keep <- setdiff(seq_along(weeks), drop)
  return(data.table::data.table(
    id = id,
    isoyearweek = weeks[keep],
    exposed = keep %in% tx,
    eligible = keep %in% eligible,
    died = keep %in% events,
    age = 50L
  ))
}

.cll_design <- function() {
  return(TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = .cll_period_width,
    period_width = .cll_period_width
  ))
}

# The real cascade. `.s1a_finalize_on_skeleton()` is the production frame that
# `.s1a_worker_multi()` calls, so these three landmark rows are the rows the
# pipeline writes.
#
# I1 and C1 qualify. IEV and CEV have the outcome inside the entry band. INR
# holds no row at the landmark week. X1 loses eligibility from week 4 on.
.cll_attrition <- function() {
  weeks <- .cll_weeks()
  sk <- data.table::rbindlist(list(
    .cll_person("I1", weeks, tx = 4L),
    .cll_person("IEV", weeks, tx = 4L, events = 2L),
    .cll_person("INR", weeks, tx = 4L, drop = 5L),
    .cll_person("C1", weeks),
    .cll_person("CEV", weeks, events = 2L),
    .cll_person("X1", weeks, eligible = 1:3)
  ))
  sk[, rd_intervention := exposed]
  data.table::setattr(sk, "eligible_cols", "eligible")
  res <- swereg:::.s1a_finalize_on_skeleton(
    skeleton = sk,
    enrollment_spec = list(design = .cll_design(), enrollment_id = "01"),
    spec = NULL,
    cache_path = NULL
  )
  return(res$attrition)
}

.cll_flow <- function(att = .cll_attrition()) {
  return(swereg:::.build_cohort_flow(list(attrition = att)))
}

# A populated spec, in the shape the lookup reads: one study-year window and
# one age rule under `additional_inclusion`.
.cll_spec <- function() {
  return(list(
    inclusion_criteria = list(isoyears = c(2020L, 2020L)),
    enrollments = list(list(
      id = "01",
      additional_inclusion = list(list(
        name = "Age 40-80",
        type = "age_range",
        min = 40,
        max = 80,
        implementation = list(variable = "rd_age_continuous")
      )),
      treatment = list(
        arms = list(intervention = "Intervention", comparator = "Comparator")
      )
    ))
  ))
}

.cll_labels <- function(spec = NULL, att = .cll_attrition()) {
  return(swereg:::.build_criterion_label_lookup(
    list(spec = spec),
    enrollment_id = "01",
    observed_criteria = unique(as.character(att$criterion))
  ))
}

# The `e1` node's label, split back into the lines the reader sees. `\l` is a
# left-justified newline, so it separates the lines inside the one node.
.cll_excluded_lines <- function(dot) {
  all_lines <- strsplit(dot, "\n", fixed = TRUE)[[1]]
  node <- all_lines[grepl("^  e1 \\[label = '", all_lines)]
  body <- sub("^  e1 \\[label = '", "", node)
  body <- sub("', style = filled.*$", "", body)
  parts <- strsplit(body, "\\l", fixed = TRUE)[[1]]
  return(parts[nzchar(parts)])
}

.cll_box <- function(spec = NULL) {
  att <- .cll_attrition()
  dot <- swereg:::.build_consort_dot(
    flow = .cll_flow(att),
    eid = "01",
    label = "Landmark labels",
    intervention_label = "Intervention",
    comparator_label = "Comparator",
    criterion_labels = .cll_labels(spec, att),
    inclusion_steps = character()
  )
  return(.cll_excluded_lines(dot))
}

# The two counts one heading or one bullet carries.
.cll_n <- function(s) {
  hit <- regmatches(
    s,
    regexec("\\(n = ([0-9,]+) persons / ([0-9,]+) person-trials\\)", s)
  )[[1]]
  return(c(
    as.numeric(gsub(",", "", hit[[2]], fixed = TRUE)),
    as.numeric(gsub(",", "", hit[[3]], fixed = TRUE))
  ))
}


# --- the label lookup -------------------------------------------------------

test_that("the lookup labels both landmark exclusions when the spec is NULL", {
  # The landmark steps never come from the spec, so the labels MUST sit above
  # the `spec = NULL` return. Below it they never reach a plan with no spec.
  labels <- .cll_labels(spec = NULL)
  expect_identical(
    unname(labels["landmark_observed"]),
    "Censored before landmark"
  )
  expect_identical(
    unname(labels["landmark_event_free"]),
    "Event before landmark"
  )
})

test_that("the lookup labels both landmark exclusions from a populated spec", {
  labels <- .cll_labels(spec = .cll_spec())
  # The spec reaches the lookup: this window comes from its age rule.
  expect_identical(
    unname(labels["eligible_age"]),
    "Outside of age range\\n(40 - 80 years)"
  )
  expect_identical(
    unname(labels["landmark_observed"]),
    "Censored before landmark"
  )
  expect_identical(
    unname(labels["landmark_event_free"]),
    "Event before landmark"
  )
})

test_that("the lookup gives landmark_candidates no label", {
  expect_false("landmark_candidates" %in% names(.cll_labels(spec = NULL)))
  expect_false(
    "landmark_candidates" %in% names(.cll_labels(spec = .cll_spec()))
  )
})


# --- the rendered Excluded box ----------------------------------------------

test_that("the Excluded box shows both landmark labels and no raw column name", {
  lines <- .cll_box()
  expect_true(any(grepl(
    "- Censored before landmark (n = ",
    lines,
    fixed = TRUE
  )))
  expect_true(any(grepl("- Event before landmark (n = ", lines, fixed = TRUE)))
  expect_false(any(grepl("landmark_", lines, fixed = TRUE)))
})

test_that("the Excluded box lists no landmark_candidates line", {
  lines <- .cll_box()
  expect_length(grep("landmark_candidates", lines, fixed = TRUE), 0L)
  # The filter drops one row, not all three. Both real landmark exclusions
  # keep their bullet.
  expect_length(grep("landmark", lines, fixed = TRUE), 2L)
})

test_that("landmark_candidates excludes nobody on the production path", {
  flow <- .cll_flow()
  row <- flow[flow$step == "landmark_candidates"]
  expect_identical(nrow(row), 1L)
  # `.consort_excluded_label()` drops this row. These two zeros are why that
  # loses nothing. A non-zero delta fails here, instead of vanishing with the
  # row it dropped.
  expect_identical(row$change_persons, 0)
  expect_identical(row$change_person_trials, 0)
})

test_that("the Excluded box totals still add up once the candidates row goes", {
  lines <- .cll_box()
  box <- .cll_n(lines[[1L]])
  headings <- lines[!startsWith(lines, "- ")][-1L]
  bullets <- lines[startsWith(lines, "- ")]
  expect_equal(rowSums(vapply(headings, .cll_n, numeric(2))), box)
  expect_equal(rowSums(vapply(bullets, .cll_n, numeric(2))), box)
})


# --- the production render path ---------------------------------------------

test_that("the production render path carries both landmark labels into the box", {
  skip_if_not_installed("DiagrammeR")
  skip_if_not_installed("DiagrammeRsvg")
  skip_if_not_installed("rsvg")
  skip_if_not_installed("withr")

  # `.render_consort_sidecars()` is the one caller of
  # `.build_consort_dot()`, and it is what `$export_tables()` calls. Capture
  # the dot it gets back, then let the real renderer run.
  ec <- list(attrition = .cll_attrition())
  plan <- swereg::TTEPlan$new(
    project_prefix = "cll",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = data.table::data.table(
      ett_id = "ETT00001",
      enrollment_id = "01",
      age_group = "40_80",
      age_min = 40L,
      age_max = 80L,
      follow_up = 52L,
      outcome_var = "died",
      outcome_name = "Death",
      outcome_description = "The fixture outcome",
      outcome_role = "primary",
      description = "ETT00001",
      confounder_vars = "age",
      person_id_var = "id",
      treatment_var = "exposed",
      comparator_to_intervention_ratio = 2L,
      seed = 1L
    )
  )
  plan$spec <- .cll_spec()
  plan$enrollment_counts <- list(`01` = ec)

  seen <- new.env(parent = emptyenv())
  orig <- swereg:::.build_consort_dot
  testthat::local_mocked_bindings(
    .build_consort_dot = function(...) {
      seen$dot <- orig(...)
      return(seen$dot)
    }
  )
  out <- swereg:::.render_consort_sidecars(
    plan = plan,
    ec = ec,
    eid = "01",
    label = "Landmark labels",
    output_dir = withr::local_tempdir()
  )

  expect_true(file.exists(out$png))
  lines <- .cll_excluded_lines(seen$dot)
  expect_true(any(grepl(
    "- Censored before landmark (n = ",
    lines,
    fixed = TRUE
  )))
  expect_true(any(grepl("- Event before landmark (n = ", lines, fixed = TRUE)))
  expect_false(any(grepl("landmark_", lines, fixed = TRUE)))
})
