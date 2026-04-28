# Pin tteplan_validate_spec(), the front-line defence against
# spec-drift bugs (e.g. spec references `osd_f20_to_f29`, but the
# code registry actually produces `osd_f20_f29`). Without this
# function failing fast, mismatched names slip through s0_init and
# blow up much later in s1 / s2 / s3.

skip_if_not_installed("data.table")

# Build a (spec, skeleton) pair that passes validation cleanly.
# Tests mutate one field and assert the validator catches it.
.valid_pair <- function() {
  spec <- list(
    study = list(
      title = "T", principal_investigator = "PI", description = "d",
      implementation = list(project_prefix = "p", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2010L, 2020L)),
    exclusion_criteria = list(
      list(name = "Lifetime",
           implementation = list(source_variable = "osd_x",
                                 window_weeks = Inf))
    ),
    confounders = list(
      list(name = "Age",
           implementation = list(variable = "rd_age_continuous"))
    ),
    outcomes = list(
      list(name = "Outcome A",
           implementation = list(variable = "osd_a"))
    ),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    enrollments = list(
      list(id = "01", name = "E01",
           additional_inclusion = list(),
           treatment = list(
             arms = list(intervention = "I", comparator = "C"),
             implementation = list(matching_ratio = 1L,
                                   variable = "rd_tx",
                                   intervention_value = "i_val",
                                   comparator_value   = "c_val",
                                   seed = 1L)))
    )
  )

  skel <- data.table::data.table(
    id = c(1L, 2L, 3L),
    isoyear = 2020L,
    isoyearweek = "2020-01",
    osd_x = FALSE,
    osd_a = FALSE,
    rd_age_continuous = c(50, 55, 60),
    rd_tx = c("i_val", "c_val", "i_val")  # contains both arm values
  )
  list(spec = spec, skeleton = skel)
}

test_that("tteplan_validate_spec: passes on a fully-valid (spec, skeleton)", {
  p <- .valid_pair()
  err_msg <- NULL
  tryCatch(
    swereg::tteplan_validate_spec(p$spec, p$skeleton) |>
      suppressMessages() |> suppressWarnings(),
    error = function(e) {
      err_msg <<- conditionMessage(e)
    }
  )
  expect_null(err_msg,
              info = paste0("validation should pass; got error: ", err_msg))
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
    list(name = "Age",
         implementation = list(variable = "rd_age_continuous")),
    list(name = "Recent psychotropics",
         implementation = list(
           computed = TRUE,
           source_variable = "rx_n05_n06",
           variable = "rd_recent_psychotropics_52w",  # created later
           window_weeks = 52L))
  )
  err_msg <- NULL
  tryCatch(
    swereg::tteplan_validate_spec(p$spec, p$skeleton) |>
      suppressMessages() |> suppressWarnings(),
    error = function(e) {
      err_msg <<- conditionMessage(e)
    }
  )
  expect_null(err_msg,
              info = paste0("validation should pass; got error: ", err_msg))
})

test_that("tteplan_validate_spec: errors when computed confounder's source_variable is missing", {
  p <- .valid_pair()
  p$spec$confounders <- c(p$spec$confounders, list(
    list(name = "Recent psychotropics",
         implementation = list(
           computed = TRUE,
           source_variable = "rx_typo",
           variable = "rd_recent_psychotropics_52w",
           window_weeks = 52L))
  ))
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "confounders.*Recent psychotropics.*rx_typo"
  )
})

test_that("tteplan_validate_spec: errors when intervention_value is not in the treatment column", {
  p <- .valid_pair()
  p$spec$enrollments[[1]]$treatment$implementation$intervention_value <- "missing_val"
  expect_error(
    swereg::tteplan_validate_spec(p$spec, p$skeleton),
    "intervention_value.*missing_val"
  )
})
