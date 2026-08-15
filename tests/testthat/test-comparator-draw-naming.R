# swereg never names its comparator draw without naming the stratum.
#
# The draw runs `by = trial_id`, and `trial_id` is the week index divided by
# `period_width`. The draw is therefore exact matching on the entry band, and
# the band is the only stratum. Confounding adjustment for the remaining
# measured covariates is by inverse probability weighting on the covariates
# taken at the recruiting week.
#
# The generated protocol table, the generated TARGET methods text and the
# CONSORT node labels all reach a manuscript. Two claims there are wrong, and
# each one is wrong in its own direction.
#
#   1. A bare "matching" with no stratum reads as matching on covariates. A
#      reviewer then asks for balance diagnostics for covariates the draw
#      never read.
#   2. "The draw read no covariate" denies the matching outright. A reviewer
#      who inspects `trial_id` finds a covariate, because calendar time is
#      one.
#
# `.cdn_unstratified()` pins the first. It returns every sentence that names
# matching and does not name its stratum. `.CDN_STRATUM` lists the three
# strings that name one: the band, the week, and the closed set "nothing
# else". `.cdn_no_covariate()` pins the second.

skip_if_not_installed("data.table")

.CDN_STRATUM <- "entry band|entry week|nothing else"
.CDN_DENIALS <- c("no covariate", "not covariate matching")

# One sentence per element. Split on sentence end and on newline, per element,
# so a stratum named in one table cell cannot rescue a bare "matching" in
# another.
.cdn_sentences <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  s <- unlist(strsplit(x, "(?<=[.!?]) +|\n", perl = TRUE))
  s <- trimws(s)
  s[nzchar(s)]
}

.cdn_unstratified <- function(x) {
  s <- grep("match", .cdn_sentences(x), ignore.case = TRUE, value = TRUE)
  s[!grepl(.CDN_STRATUM, s, ignore.case = TRUE)]
}

.cdn_no_covariate <- function(x) {
  s <- .cdn_sentences(x)
  hit <- Reduce(
    `|`,
    lapply(.CDN_DENIALS, function(p) grepl(p, s, ignore.case = TRUE))
  )
  s[hit]
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


test_that("the generated protocol table names the stratum of the comparator draw", {
  plan <- .cdn_plan()
  ctx <- swereg:::.protocol_context(plan, "ETT00001")
  tab <- swereg:::.build_protocol_table(plan$spec, ctx)
  cells <- unlist(lapply(tab, as.character), use.names = FALSE)
  console <- utils::capture.output(plan$print_spec_summary())

  expect_gt(length(cells), 0L)
  expect_gt(length(console), 0L)
  # The ratio and the seed must still reach the output, so the assertions
  # below cannot pass on an empty table or on empty console text.
  expect_true(any(grepl("Comparator ratio: 1:2", cells, fixed = TRUE)))
  expect_true(any(grepl("Comparator draw seed: 7", cells, fixed = TRUE)))
  expect_true(any(grepl("Comparator ratio: 1:2", console, fixed = TRUE)))
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
  expect_identical(.cdn_unstratified(cells), character(0))
  expect_identical(.cdn_unstratified(console), character(0))
  expect_identical(.cdn_no_covariate(cells), character(0))
  expect_identical(.cdn_no_covariate(console), character(0))
})


test_that("the generated TARGET methods text says what the draw is matched on", {
  plan <- .cdn_plan()
  txt <- utils::capture.output(plan$print_target_checklist())

  expect_gt(length(txt), 0L)
  # Items 6c and 7c are the two paragraphs that describe assignment. Both must
  # be present, or the assertions below pass on absent text.
  expect_gte(sum(grepl("seeded random draw", txt, fixed = TRUE)), 2L)
  expect_true(any(grepl(
    "Assignment (6c): Comparator individuals entered by a seeded random draw",
    txt,
    fixed = TRUE
  )))
  # The stratum, stated twice: once in item 6c and once in item 7c.
  expect_gte(
    sum(grepl("Each sequential trial was one entry band of 4 weeks.", txt, fixed = TRUE)),
    2L
  )
  expect_gte(
    sum(grepl(
      "The draw was exactly matched on the entry band, and not on the week.",
      txt,
      fixed = TRUE
    )),
    2L
  )
  # The band is 4 weeks wide, so two individuals in one trial cover 3 weeks.
  expect_gte(
    sum(grepl(
      "differed by up to 3 weeks",
      txt,
      fixed = TRUE
    )),
    2L
  )
  expect_gte(sum(grepl("The draw matched on nothing else.", txt, fixed = TRUE)), 2L)
  # Confounding adjustment is by weighting, at the recruiting week.
  expect_true(any(grepl("taken at the recruiting week", txt, fixed = TRUE)))
  # Item 8 reports the counts after the draw.
  expect_true(any(grepl("After the comparator draw:", txt, fixed = TRUE)))
  expect_identical(.cdn_unstratified(txt), character(0))
  expect_identical(.cdn_no_covariate(txt), character(0))
})


test_that("the generated methods text reads period_width and not a hard-coded 4", {
  plan <- .cdn_plan()
  plan$period_width <- 8L
  txt <- utils::capture.output(plan$print_target_checklist())

  expect_gte(
    sum(grepl("Each sequential trial was one entry band of 8 weeks.", txt, fixed = TRUE)),
    2L
  )
  expect_gte(sum(grepl("differed by up to 7 weeks", txt, fixed = TRUE)), 2L)
  expect_false(any(grepl("entry band of 4 weeks", txt, fixed = TRUE)))
  expect_identical(.cdn_unstratified(txt), character(0))

  # A width of 1 makes the band one week, so the text drops the band wording.
  plan$period_width <- 1L
  txt1 <- utils::capture.output(plan$print_target_checklist())
  expect_gte(
    sum(grepl("Each sequential trial was one entry week.", txt1, fixed = TRUE)),
    2L
  )
  expect_gte(
    sum(grepl(
      "The draw was exactly matched on the entry week, and on nothing else.",
      txt1,
      fixed = TRUE
    )),
    2L
  )
  expect_false(any(grepl("differed by up to", txt1, fixed = TRUE)))
  expect_identical(.cdn_unstratified(txt1), character(0))
})


test_that("the CONSORT flow and node labels name the stratum of the comparator draw", {
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
    "matched on the 4-week entry band, and on nothing else",
    dot,
    fixed = TRUE
  ))
  expect_identical(
    .cdn_unstratified(c(flow$step, flow$kind, flow$change_kind)),
    character(0)
  )
  expect_identical(.cdn_unstratified(strsplit(dot, "\n")[[1]]), character(0))
  expect_identical(.cdn_no_covariate(strsplit(dot, "\n")[[1]]), character(0))
})


test_that("no source file claims the draw read no covariate", {
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
    for (p in .CDN_DENIALS) {
      i <- grep(p, lines, ignore.case = TRUE)
      if (length(i) > 0L) {
        hits <- c(hits, sprintf("%s:%d", substring(f, nchar(root) + 2L), i))
      }
    }
  }
  expect_identical(hits, character())
})
