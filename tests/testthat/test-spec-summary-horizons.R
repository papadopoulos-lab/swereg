# The horizons and the minimum-events rule in the plan print, the spec summary
# and the Study Specification sheet (swereg 27.3.9).
#
#   * The sheet shows `min_expected_events_per_arm_per_year`, the exempt
#     enrollments and the feasibility horizon, where the spec sets them.
#   * A part-year horizon takes the verdict of the next whole year, through a
#     horizon-only ETT. The spec summary and the sheet say so.
#   * The ETT count line of print() adds the feasibility-horizon ETTs.
#
# The plans come from `.ff_plan()` and `.ff_spec()` of
# helper-feasibility_follow_up.R: enrollments 01 and 02, one outcome, and
# follow-ups of 52 and 104 weeks unless a test sets others.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")

.sh_plan <- function(spec, env = parent.frame()) {
  plan <- .ff_plan(spec, withr::local_tempdir(.local_envir = env))
  plan$created_at <- FF_EXPORT_TIME
  return(plan)
}

.sh_print <- function(plan) {
  return(gsub(
    "\033\\[[0-9;]*m",
    "",
    utils::capture.output(plan$print_spec_summary())
  ))
}

# The Study Specification sheet as a two-column data.frame of text.
.sh_sheet <- function(plan) {
  x <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(suppressWarnings(plan$excel_spec_summary(path = x)))
  m <- openxlsx::read.xlsx(
    x,
    sheet = "Study Specification",
    colNames = FALSE,
    skipEmptyRows = FALSE
  )
  names(m) <- c("a", "b")
  return(m)
}

# The value of the sheet row labelled `label`, or character(0) without one.
.sh_value <- function(sheet, label) {
  return(sheet$b[which(sheet$a == label)])
}

.SH_FU_78 <- list(list(label = "18 months", weeks = 78L))
.SH_VERDICT_78 <- "at 104 weeks, the next whole year, through a horizon-only ETT"

# ---- the sheet: rule, exemption and feasibility horizon ----------------------

test_that("the Study Specification sheet shows the threshold, the exempt enrollments and the feasibility horizon", {
  skip_if_not_installed("openxlsx")
  spec <- .ff_spec(FF_W)
  spec$enrollments[[1]]$minimum_events_rule_exempt <- TRUE
  spec$enrollments[[2]]$minimum_events_rule_exempt <- TRUE
  sheet <- .sh_sheet(.sh_plan(spec))
  expect_identical(
    .sh_value(sheet, "Min expected events:"),
    "1 per arm, per follow-up year"
  )
  expect_identical(
    .sh_value(sheet, "Exempt:"),
    "Treated vs control 01; Treated vs control 02"
  )
  expect_identical(
    .sh_value(sheet, "Feasibility horizon:"),
    "260 weeks, minimum-events rule only"
  )
  expect_true("Minimum-events rule" %in% sheet$a)
})

test_that("the Study Specification sheet shows no exemption and no feasibility horizon that the spec does not set", {
  skip_if_not_installed("openxlsx")
  sheet <- .sh_sheet(.sh_plan(.ff_spec(NULL, min_events = 3)))
  expect_identical(
    .sh_value(sheet, "Min expected events:"),
    "3 per arm, per follow-up year"
  )
  expect_identical(.sh_value(sheet, "Exempt:"), character(0))
  expect_identical(.sh_value(sheet, "Feasibility horizon:"), character(0))
})

# ---- part-year horizons -----------------------------------------------------

test_that("the spec summary says that a 78-week horizon takes the 104-week verdict, through a horizon-only ETT", {
  spec <- .ff_spec(NULL)
  spec$follow_up <- .SH_FU_78
  plan <- .sh_plan(spec)
  # The grid holds one horizon-only 104-week ETT per enrollment.
  expect_identical(
    plan$ett$follow_up[plan$ett$feasibility_horizon],
    c(104L, 104L)
  )
  out <- .sh_print(plan)
  at <- which(out == "  - 18 months (78 weeks)")
  expect_length(at, 1L)
  expect_identical(
    out[at + 1L],
    paste0("    Minimum-events verdict: ", .SH_VERDICT_78)
  )
})

test_that("the Study Specification sheet says that a 78-week horizon takes the 104-week verdict, through a horizon-only ETT", {
  skip_if_not_installed("openxlsx")
  spec <- .ff_spec(NULL)
  spec$follow_up <- .SH_FU_78
  sheet <- .sh_sheet(.sh_plan(spec))
  at <- which(sheet$a == "18 months")
  expect_length(at, 1L)
  expect_identical(sheet$b[at], "78 weeks")
  expect_identical(sheet$a[at + 1L], "Minimum-events verdict:")
  expect_identical(sheet$b[at + 1L], .SH_VERDICT_78)
})

test_that("a part-year horizon whose whole year is a spec horizon names no horizon-only ETT", {
  spec <- .ff_spec(NULL)
  spec$follow_up <- c(.SH_FU_78, list(list(label = "2 years", weeks = 104L)))
  plan <- .sh_plan(spec)
  expect_false(any(plan$ett$feasibility_horizon))
  out <- .sh_print(plan)
  at <- which(out == "  - 18 months (78 weeks)")
  expect_identical(
    out[at + 1L],
    "    Minimum-events verdict: at 104 weeks, the next whole year"
  )
  # A whole-year horizon gets no verdict line.
  expect_identical(out[which(out == "  - 2 years (104 weeks)") + 1L], "")
})

# ---- the ETT count line ------------------------------------------------------

test_that("print adds the feasibility-horizon ETTs to the ETT count line only when there are some", {
  count_line <- function(plan) {
    return(grep(
      "ETT(s)",
      utils::capture.output(print(plan)),
      fixed = TRUE,
      value = TRUE
    ))
  }
  expect_identical(
    count_line(.sh_plan(.ff_spec(FF_W))),
    "  1 outcome(s) x 2 follow-up x 2 enrollment(s) = 4 ETT(s) + 2 feasibility-horizon ETT(s)"
  )
  expect_identical(
    count_line(.sh_plan(.ff_spec(NULL))),
    "  1 outcome(s) x 2 follow-up x 2 enrollment(s) = 4 ETT(s)"
  )
})
