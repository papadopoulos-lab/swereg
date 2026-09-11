# Pin tteplan_validate_spec(), the front-line defence against
# spec-drift bugs (e.g. spec references `osd_e10_to_e14`, but the
# code registry actually produces `osd_e10_e14`). Without this
# function failing fast, mismatched names slip through s0_init and
# blow up much later in s1 / s2 / s3.

skip_if_not_installed("data.table")

# Build a (spec, skeleton) pair that passes validation cleanly.
# Tests mutate one field and assert the validator catches it.
.valid_pair <- function() {
  spec <- list(
    study = list(
      title = "T",
      principal_investigator = "PI",
      description = "d",
      implementation = list(project_prefix = "p", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2010L, 2020L)),
    exclusion_criteria = list(
      list(
        name = "Lifetime",
        implementation = list(source_variable = "osd_x", window_weeks = Inf)
      )
    ),
    confounders = list(
      list(name = "Age", implementation = list(variable = "rd_age_continuous"))
    ),
    outcomes = list(
      list(name = "Outcome A", implementation = list(variable = "osd_a"))
    ),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    enrollments = list(
      list(
        id = "01",
        name = "E01",
        additional_inclusion = list(),
        treatment = list(
          arms = list(intervention = "I", comparator = "C"),
          implementation = list(
            comparator_to_intervention_ratio = 1L,
            variable = "rd_tx",
            intervention_value = "i_val",
            comparator_value = "c_val",
            seed = 1L
          )
        )
      )
    )
  )

  skel <- data.table::data.table(
    id = c(1L, 2L, 3L),
    isoyear = 2020L,
    isoyearweek = "2020-01",
    osd_x = FALSE,
    osd_a = FALSE,
    rd_age_continuous = c(50, 55, 60),
    rd_tx = c("i_val", "c_val", "i_val") # contains both arm values
  )
  list(spec = spec, skeleton = skel)
}

test_that("tteplan_validate_spec: passes on a fully-valid (spec, skeleton)", {
  p <- .valid_pair()
  err_msg <- NULL
  tryCatch(
    swereg::tteplan_validate_spec(p$spec, p$skeleton) |>
      suppressMessages() |>
      suppressWarnings(),
    error = function(e) {
      err_msg <<- conditionMessage(e)
    }
  )
  expect_null(
    err_msg,
    info = paste0("validation should pass; got error: ", err_msg)
  )
})

test_that("tteplan_validate_spec: passes with a valid subgroup (confounder + in skeleton)", {
  p <- .valid_pair()
  p$spec$subgroups <- list(
    list(name = "Age", implementation = list(variable = "rd_age_continuous"))
  )
  err_msg <- NULL
  tryCatch(
    swereg::tteplan_validate_spec(p$spec, p$skeleton) |>
      suppressMessages() |>
      suppressWarnings(),
    error = function(e) err_msg <<- conditionMessage(e)
  )
  expect_null(
    err_msg,
    info = paste0("validation should pass; got error: ", err_msg)
  )
})

test_that("tteplan_validate_spec: errors when a subgroup var is missing from the skeleton", {
  p <- .valid_pair()
  p$spec$subgroups <- list(
    list(name = "X", implementation = list(variable = "rd_not_a_column"))
  )
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "not found in skeleton"
  )
})

test_that("tteplan_validate_spec: errors when a subgroup var is not a confounder", {
  p <- .valid_pair()
  # rd_tx exists in the skeleton but is not among the confounders
  p$spec$subgroups <- list(
    list(name = "Tx", implementation = list(variable = "rd_tx"))
  )
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "must also be a confounder"
  )
})

test_that("tteplan_validate_spec: errors on missing exclusion source_variable", {
  p <- .valid_pair()
  p$spec$exclusion_criteria[[1]]$implementation$source_variable <- "osd_typo"
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "exclusion_criteria.*Lifetime.*osd_typo"
  )
})

test_that("tteplan_validate_spec: errors on missing outcome variable", {
  p <- .valid_pair()
  p$spec$outcomes[[1]]$implementation$variable <- "osd_missing"
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "outcomes.*Outcome A.*osd_missing"
  )
})

test_that("tteplan_validate_spec: errors on missing non-computed confounder variable", {
  p <- .valid_pair()
  p$spec$confounders[[1]]$implementation$variable <- "rd_age_typo"
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "confounders.*Age.*rd_age_typo"
  )
})

test_that("tteplan_validate_spec: rejects non-data.table skeleton with helpful message", {
  p <- .valid_pair()
  expect_error(
    swereg::tteplan_validate_spec(p$spec, "not a data.table"),
    "skeleton must be a data.table"
  )
})

test_that("tteplan_validate_spec: passes for a computed confounder when source_variable is present", {
  p <- .valid_pair()
  p$skeleton[, rx_n05_n06 := FALSE]
  p$spec$confounders <- list(
    list(name = "Age", implementation = list(variable = "rd_age_continuous")),
    list(
      name = "Recent psychotropics",
      implementation = list(
        computed = TRUE,
        source_variable = "rx_n05_n06",
        variable = "rd_recent_psychotropics_52w", # created later
        window_weeks = 52L
      )
    )
  )
  err_msg <- NULL
  tryCatch(
    swereg::tteplan_validate_spec(p$spec, p$skeleton) |>
      suppressMessages() |>
      suppressWarnings(),
    error = function(e) {
      err_msg <<- conditionMessage(e)
    }
  )
  expect_null(
    err_msg,
    info = paste0("validation should pass; got error: ", err_msg)
  )
})

test_that("tteplan_validate_spec: errors when computed confounder's source_variable is missing", {
  p <- .valid_pair()
  p$spec$confounders <- c(
    p$spec$confounders,
    list(
      list(
        name = "Recent psychotropics",
        implementation = list(
          computed = TRUE,
          source_variable = "rx_typo",
          variable = "rd_recent_psychotropics_52w",
          window_weeks = 52L
        )
      )
    )
  )
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "confounders.*Recent psychotropics.*rx_typo"
  )
})

test_that("tteplan_validate_spec: errors when intervention_value is not in the treatment column", {
  p <- .valid_pair()
  p$spec$enrollments[[
    1
  ]]$treatment$implementation$intervention_value <- "missing_val"
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "intervention_value.*missing_val"
  )
})

# The prevalent-user guard measures prevalent weeks, and `.valid_pair()` gives
# each person one week. Person 1 gets a second week at the intervention level,
# so exactly one prevalent week exists.
.prevalent_pair <- function() {
  p <- .valid_pair()
  p$skeleton <- data.table::data.table(
    id = c(1L, 1L, 2L, 3L),
    isoyear = 2020L,
    isoyearweek = c("2020-01", "2020-02", "2020-01", "2020-01"),
    osd_x = FALSE,
    osd_a = FALSE,
    rd_age_continuous = c(50, 50, 55, 60),
    rd_tx = c("i_val", "i_val", "c_val", "i_val")
  )
  return(p)
}

test_that("tteplan_validate_spec: warns when no washout covers the level", {
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  p <- .prevalent_pair()
  # The one exclusion is not a washout: it carries no `implementation$type`.
  # The skeleton has no `is_isoyear` column, so every row counts as a weekly
  # row.
  expect_warning(
    suppressMessages(swereg::tteplan_validate_spec(p$spec, p$skeleton)),
    "1 prevalent week at rd_tx == \"i_val\". No washout applies",
    fixed = TRUE
  )
})

test_that("tteplan_validate_spec: the warning names the batch it measured", {
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  p <- .prevalent_pair()
  expect_warning(
    suppressMessages(
      swereg::tteplan_validate_spec(p$spec, p$skeleton, skeleton_batch = 12L)
    ),
    "On skeleton batch 12,",
    fixed = TRUE
  )
})

test_that("tteplan_validate_spec: a covering washout silences it", {
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  p <- .prevalent_pair()
  p$spec$exclusion_criteria[[1]]$implementation <- list(
    source_variable = "rd_tx",
    value = "i_val",
    type = "no_prior_value",
    window_weeks = Inf
  )
  expect_no_warning(
    suppressMessages(swereg::tteplan_validate_spec(p$spec, p$skeleton))
  )
})

test_that("tteplan_validate_spec: the warning names each washout and its count", {
  withr::local_options(swereg.warn_prevalent_user = TRUE)
  p <- .prevalent_pair()
  # `osd_x` is FALSE in every week, so no prior week carries TRUE and the
  # washout excludes nothing. The one prevalent week stays uncovered.
  p$spec$exclusion_criteria[[1]]$name <- "Prior osd_x"
  p$spec$exclusion_criteria[[1]]$implementation <- list(
    source_variable = "osd_x",
    value = TRUE,
    type = "no_prior_value",
    window_weeks = Inf
  )
  expect_warning(
    suppressMessages(swereg::tteplan_validate_spec(p$spec, p$skeleton)),
    "Washout 'Prior osd_x' leaves 1 uncovered.",
    fixed = TRUE
  )
})
