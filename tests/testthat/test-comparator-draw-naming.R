# swereg never calls its comparator draw matching, and never claims that it
# allocates comparators to individual initiators.
#
# The draw runs `by = trial_id`, and `trial_id` is the week index divided by
# `period_width`. It is ONE sample per trial. Its size is
# `comparator_to_intervention_ratio` times that trial's count of intervention
# individuals, capped at the comparators the trial holds. It attaches no
# comparator to an intervention individual, so no matched set exists, and
# nothing downstream conditions on one.
#
# The generated protocol table, the generated TARGET methods text and the
# CONSORT node labels all reach a manuscript. Three claims there are wrong, and
# each one is wrong in its own direction.
#
#   1. Any "matching" word, with or without a stratum, names a scheme swereg
#      does not run. A reviewer then asks which matched sets the analysis
#      conditions on, and there are none.
#   2. A denial that the draw read a covariate is wrong the other way. A
#      reviewer who inspects `trial_id` finds a covariate, because calendar
#      time is one.
#   3. A per-initiator count claims an allocation the code never makes. The
#      draw takes one trial-level sample and pairs nobody with anybody.
#
# `.cdn_bad_match()` pins the first. Every sentence carrying a "match" word
# fails, with ONE carve-out: `no matched set`, which is how the text states
# that the sets do not exist. The split is per sentence, so a positive claim in
# one sentence cannot hide behind a denial in the next.
#
# `.cdn_retired()` pins the second, and it also re-pins the two superseded
# formulations of the first. `.cdn_per_initiator()` pins the third.

skip_if_not_installed("data.table")

# The one negated form the generated text MAY carry.
.CDN_MATCH_ALLOWED <- "no matched set"

# Formulations swereg has shipped and withdrawn. Each was false.
.CDN_RETIRED <- c(
  "exactly matched",
  "matched on the entry",
  "no covariate",
  "not covariate matching"
)

.CDN_PER_INITIATOR <- paste(
  c(
    "per initiator",
    "per intervention individual",
    "per exposed",
    "for (every|each) (observed )?(initiator|intervention individual)",
    "(comparator|control)s? (to|for) (every|each)"
  ),
  collapse = "|"
)

# One sentence per element. Split on sentence end and on newline, per element,
# so a denial in one table cell cannot rescue a matching claim in another.
.cdn_sentences <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  s <- unlist(strsplit(x, "(?<=[.!?]) +|\n", perl = TRUE))
  s <- trimws(s)
  s[nzchar(s)]
}

.cdn_bad_match <- function(x) {
  s <- grep("match", .cdn_sentences(x), ignore.case = TRUE, value = TRUE)
  s[!grepl(.CDN_MATCH_ALLOWED, s, ignore.case = TRUE)]
}

.cdn_retired <- function(x) {
  s <- .cdn_sentences(x)
  hit <- Reduce(
    `|`,
    lapply(.CDN_RETIRED, function(p) grepl(p, s, ignore.case = TRUE))
  )
  s[hit]
}

.cdn_per_initiator <- function(x) {
  grep(.CDN_PER_INITIATOR, .cdn_sentences(x), ignore.case = TRUE, value = TRUE)
}

# The source tree, for the sweep below. `R CMD check` does not copy it, so the
# sweep skips when it is absent.
.cdn_root <- function() {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  ok <- file.exists(file.path(root, "DESCRIPTION")) &&
    dir.exists(file.path(root, "R")) &&
    dir.exists(file.path(root, "vignettes"))
  if (!ok) {
    return(NULL)
  }
  pkg <- tryCatch(
    unname(read.dcf(file.path(root, "DESCRIPTION"), "Package")[1, 1]),
    error = function(e) NA_character_
  )
  if (!identical(pkg, "swereg")) {
    return(NULL)
  }
  root
}

.cdn_spec <- function() {
  list(
    study = list(
      title = "Comparator draw naming fixture",
      design = "Sequential target trial emulation",
      implementation = list(project_prefix = "cdn", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2010L, 2020L)),
    exclusion_criteria = list(
      list(
        name = "Prior outcome event",
        implementation = list(
          source_variable = "osd_x",
          source_variable_combined = "osd_x",
          window = 104,
          window_weeks = 104L,
          computed = TRUE
        )
      )
    ),
    confounders = list(
      list(
        name = "Age (continuous)",
        implementation = list(variable = "rd_age_continuous")
      )
    ),
    outcomes = list(
      list(
        name = "Outcome A",
        role = "primary",
        description = "The primary fixture outcome",
        implementation = list(variable = "osd_a", variable_combined = "osd_a")
      )
    ),
    follow_up = list(list(label = "5 years", weeks = 260)),
    enrollments = list(
      list(
        id = "01",
        name = "Arm A vs Arm B, age 50-54",
        observed_var = list(sentinel = "row_presence"),
        intervention_tolerance_weeks = 0L,
        comparator_tolerance_weeks = 0L,
        additional_inclusion = list(
          list(
            name = "Age 50-54",
            type = "age_range",
            min = 50,
            max = 54,
            implementation = list(variable = "rd_age_continuous")
          )
        ),
        treatment = list(
          description = "Initiation of Arm A compared with Arm B.",
          arms = list(intervention = "Arm A", comparator = "Arm B"),
          implementation = list(
            comparator_to_intervention_ratio = 2,
            variable = "rd_tx",
            intervention_value = "arm_a",
            comparator_value = "arm_b",
            seed = 7
          )
        )
      )
    )
  )
}

.cdn_counts <- function() {
  list(
    attrition = data.table::data.table(
      enrollment_id = "01",
      trial_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_age"),
      n_persons = c(1000, 800),
      n_person_trials = c(5000, 4000),
      n_intervention = c(1000, 800),
      n_comparator = c(4000, 3200)
    ),
    matching = data.table::data.table(
      trial_id = 1L,
      n_intervention_total = 800,
      n_comparator_total = 3200,
      n_intervention_enrolled = 700,
      n_comparator_enrolled = 1400
    )
  )
}

.cdn_plan <- function() {
  ett <- data.table::data.table(
    ett_id = "ETT00001",
    enrollment_id = "01",
    age_group = "50_54",
    age_min = 50L,
    age_max = 54L,
    follow_up = 260L,
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_description = "The primary fixture outcome",
    outcome_role = "primary",
    description = "ETT00001",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    comparator_to_intervention_ratio = 2L,
    seed = 7L
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "cdn",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$spec <- .cdn_spec()
  plan$enrollment_counts <- list(`01` = .cdn_counts())
  plan
}


test_that("the generated protocol table names incidence density sampling and the trial-level draw", {
  plan <- .cdn_plan()
  ctx <- swereg:::.protocol_context(plan, "ETT00001")
  tab <- swereg:::.build_protocol_table(plan$spec, ctx)
  cells <- unlist(lapply(tab, as.character), use.names = FALSE)
  console <- utils::capture.output(plan$print_spec_summary())

  expect_gt(length(cells), 0L)
  expect_gt(length(console), 0L)
  # The ratio and the seed must still reach the output, so the assertions
  # below cannot pass on an empty table or on empty console text.
  expect_true(any(grepl(
    "Comparator-to-intervention ratio: 2:1",
    cells,
    fixed = TRUE
  )))
  expect_true(any(grepl("Comparator draw seed: 7", cells, fixed = TRUE)))
  expect_true(any(grepl(
    "Comparator-to-intervention ratio: 2:1",
    console,
    fixed = TRUE
  )))
  # The scheme.
  expect_true(any(grepl(
    "Comparator draw: incidence density sampling within each sequential trial",
    cells,
    fixed = TRUE
  )))
  # The size, counted over the whole trial, and the cap.
  expect_true(any(grepl(
    paste0(
      "Comparator draw size: 2 times that trial's count of intervention ",
      "individuals, capped at the comparators that trial holds"
    ),
    cells,
    fixed = TRUE
  )))
  # No matched set exists.
  expect_true(any(grepl(
    paste0(
      "Comparator pairing: none, so no matched set exists and no later step ",
      "conditions on one"
    ),
    cells,
    fixed = TRUE
  )))
  # The stratum row names the band and its width, and closes the set.
  expect_true(any(grepl(
    "Comparator draw stratum: the 4-week entry band, and nothing else",
    cells,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Confounding adjustment: inverse probability weighting on the covariates taken at the recruiting week",
    cells,
    fixed = TRUE
  )))
  expect_identical(.cdn_bad_match(cells), character(0))
  expect_identical(.cdn_bad_match(console), character(0))
  expect_identical(.cdn_retired(cells), character(0))
  expect_identical(.cdn_retired(console), character(0))
  expect_identical(.cdn_per_initiator(cells), character(0))
  expect_identical(.cdn_per_initiator(console), character(0))
})


test_that("the generated methods text names incidence density sampling and no pairing", {
  plan <- .cdn_plan()
  txt <- utils::capture.output(plan$print_target_checklist())

  expect_gt(length(txt), 0L)
  # Items 6c and 7c are the two paragraphs that describe assignment. Both must
  # be present, or the assertions below pass on absent text.
  expect_gte(
    sum(grepl(
      "Comparator individuals entered by incidence density sampling within each sequential trial.",
      txt,
      fixed = TRUE
    )),
    2L
  )
  expect_true(any(grepl(
    "Assignment (6c): Comparator individuals entered by incidence density sampling",
    txt,
    fixed = TRUE
  )))
  # The draw is one trial-level sample, sized from that trial's intervention
  # count. Stated twice: once in item 6c and once in item 7c.
  expect_gte(sum(grepl("The draw took one sample per trial.", txt, fixed = TRUE)), 2L)
  expect_gte(
    sum(grepl(
      "In enrollment 01, the draw took 2 times that trial's count of intervention individuals.",
      txt,
      fixed = TRUE
    )),
    2L
  )
  # The cap is the trial's own supply of comparators.
  expect_gte(
    sum(grepl(
      "Where a trial held fewer comparator individuals than that, the draw took all of them.",
      txt,
      fixed = TRUE
    )),
    2L
  )
  # No matched set exists, and nothing downstream conditions on one.
  expect_gte(
    sum(grepl(
      paste0(
        "It attached no comparator individual to an intervention individual, ",
        "so it formed no matched set."
      ),
      txt,
      fixed = TRUE
    )),
    2L
  )
  expect_gte(sum(grepl("No later step conditions on one.", txt, fixed = TRUE)), 2L)
  expect_gte(
    sum(grepl(
      paste0(
        "A person can be an intervention individual in one trial and a ",
        "comparator individual in another."
      ),
      txt,
      fixed = TRUE
    )),
    2L
  )
  # The stratum, stated twice: once in item 6c and once in item 7c.
  expect_gte(
    sum(grepl("Each sequential trial was one entry band of 4 weeks.", txt, fixed = TRUE)),
    2L
  )
  expect_gte(
    sum(grepl(
      "The sampling was stratified by trial, and not by week.",
      txt,
      fixed = TRUE
    )),
    2L
  )
  # The band is 4 weeks wide, so two individuals in one trial cover 3 weeks.
  expect_gte(sum(grepl("differed by up to 3 weeks", txt, fixed = TRUE)), 2L)
  expect_gte(sum(grepl("The draw read no other variable.", txt, fixed = TRUE)), 2L)
  # Confounding adjustment is by weighting, at the recruiting week.
  expect_true(any(grepl("taken at the recruiting week", txt, fixed = TRUE)))
  # Item 8 reports the counts after the draw.
  expect_true(any(grepl("After the comparator draw:", txt, fixed = TRUE)))
  # swereg uses "comparator", never "control".
  expect_false(any(grepl("control", txt, ignore.case = TRUE)))
  expect_identical(.cdn_bad_match(txt), character(0))
  expect_identical(.cdn_retired(txt), character(0))
  expect_identical(.cdn_per_initiator(txt), character(0))
})


test_that("the generated methods text reads period_width and never prints its name", {
  plan <- .cdn_plan()
  plan$period_width <- 8L
  txt <- utils::capture.output(plan$print_target_checklist())

  # The manuscript prose carries the number, never the variable name.
  expect_false(any(grepl("period_width", txt, fixed = TRUE)))
  expect_true(any(grepl(
    "grouped into enrollment periods of 8 weeks, and each period defined one sequential trial.",
    txt,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "The enrollment period width, 8 weeks, determines the granularity",
    txt,
    fixed = TRUE
  )))
  expect_gte(
    sum(grepl("Each sequential trial was one entry band of 8 weeks.", txt, fixed = TRUE)),
    2L
  )
  expect_gte(sum(grepl("differed by up to 7 weeks", txt, fixed = TRUE)), 2L)
  expect_false(any(grepl("entry band of 4 weeks", txt, fixed = TRUE)))
  expect_identical(.cdn_bad_match(txt), character(0))
  expect_identical(.cdn_per_initiator(txt), character(0))

  # A width of 2 leaves a one-week span, so the sentence takes the singular.
  plan$period_width <- 2L
  txt2 <- utils::capture.output(plan$print_target_checklist())
  expect_gte(sum(grepl("differed by up to 1 week.", txt2, fixed = TRUE)), 2L)
  expect_false(any(grepl("differed by up to 1 weeks", txt2, fixed = TRUE)))

  # A width of 1 makes the band one week, so the text drops the band wording.
  plan$period_width <- 1L
  txt1 <- utils::capture.output(plan$print_target_checklist())
  expect_false(any(grepl("period_width", txt1, fixed = TRUE)))
  expect_true(any(grepl(
    "grouped into enrollment periods of 1 week, and each period defined one sequential trial.",
    txt1,
    fixed = TRUE
  )))
  expect_gte(
    sum(grepl(
      "Each sequential trial was one entry week, so the sampling was stratified by week.",
      txt1,
      fixed = TRUE
    )),
    2L
  )
  expect_false(any(grepl("differed by up to", txt1, fixed = TRUE)))
  expect_identical(.cdn_bad_match(txt1), character(0))
  expect_identical(.cdn_per_initiator(txt1), character(0))
})


test_that("the CONSORT flow and node labels name incidence density sampling", {
  flow <- swereg:::.build_cohort_flow(
    .cdn_counts(),
    analysis_n = 2050,
    analysis_n_intervention = 690,
    analysis_n_comparator = 1360
  )
  dot <- swereg:::.build_consort_dot(
    flow = flow,
    eid = "01",
    label = "Arm A vs Arm B",
    intervention_label = "Arm A",
    comparator_label = "Arm B",
    period_width = 4L
  )

  expect_true("selection" %in% flow$kind)
  expect_true(grepl("Enrolled after the comparator draw", dot, fixed = TRUE))
  expect_true(grepl(
    "incidence density sampling, stratified by the 4-week entry band",
    dot,
    fixed = TRUE
  ))
  expect_identical(
    .cdn_bad_match(c(flow$step, flow$kind, flow$change_kind)),
    character(0)
  )
  expect_identical(.cdn_bad_match(strsplit(dot, "\n")[[1]]), character(0))
  expect_identical(.cdn_retired(strsplit(dot, "\n")[[1]]), character(0))
  expect_identical(.cdn_per_initiator(strsplit(dot, "\n")[[1]]), character(0))
})


test_that("no source file carries either retired formulation", {
  root <- .cdn_root()
  skip_if(is.null(root), "source tree not available")

  # `R/`, `vignettes/`, `man/` and `NEWS.md` are the files a reader meets.
  # `tests/` is excluded on purpose: this file holds the phrases it hunts.
  files <- c(
    list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "\\.Rmd$", full.names = TRUE),
    list.files(file.path(root, "man"), pattern = "\\.Rd$", full.names = TRUE),
    file.path(root, "NEWS.md")
  )
  files <- files[file.exists(files)]
  expect_gt(length(files), 20L)

  hits <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    for (p in c(.CDN_RETIRED, .CDN_PER_INITIATOR)) {
      i <- grep(p, lines, ignore.case = TRUE)
      if (length(i) > 0L) {
        hits <- c(hits, sprintf("%s:%d", substring(f, nchar(root) + 2L), i))
      }
    }
  }
  expect_identical(hits, character())
})
