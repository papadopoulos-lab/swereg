# The `only_enrollments` key of a follow-up entry.
#
# A follow-up horizon with `only_enrollments` applies to the enrollments it
# names. A horizon without the key applies to every enrollment, as in swereg
# 27.3.1. The ETT grid holds a horizon for an enrollment if and only if the
# horizon has no `only_enrollments` or lists the id of that enrollment.
#
# The blocks follow the brief, (a) to (e):
#   (a) the grid of a restricted spec;
#   (b) each bad value, refused by tteplan_read_spec() and tteplan_check_spec();
#   (c) a spec without the key gives the grid, print() and OSF text of 27.3.1;
#   (d) the generated texts name the comparison of a restricted horizon;
#   (e) the feasibility horizon is decided per enrollment.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")
skip_if_not_installed("qs2")

# The spec of helper-sparse_rule.R, enrollments "01" and "02", with two
# outcomes and the follow-ups `fu`. `w` is `feasibility_follow_up_weeks`, or
# `NULL` for no key.
.oe_spec <- function(fu, w = NULL) {
  spec <- spr_spec_list()
  spec$outcomes <- list(
    list(
      name = "Outcome A",
      description = "a",
      implementation = list(variable = "osd_a")
    ),
    list(
      name = "Outcome B",
      description = "b",
      implementation = list(variable = "osd_b")
    )
  )
  spec$follow_up <- fu
  spec$feasibility_follow_up_weeks <- w
  return(spec)
}

# The follow-ups of test (a): 26 and 52 weeks for both enrollments, and 156
# weeks for enrollment "01" only.
.OE_FU_A <- list(
  list(label = "6 months", weeks = 26L),
  list(label = "1 year", weeks = 52L),
  list(label = "3 years", weeks = 156L, only_enrollments = list("01"))
)

# Build the plan of `spec` under a fresh temporary root. Returns the plan and
# the path of the written spec.
.oe_plan <- function(spec, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  plan <- .ff_plan(spec, root)
  return(list(plan = plan, path = file.path(root, "spec", "spec_v001.yaml")))
}

.oe_osf <- function(path) {
  return(suppressWarnings(swereg::tteplan_osf_text(path)))
}

.oe_cols <- c(
  "ett_id",
  "enrollment_id",
  "outcome_var",
  "follow_up",
  "feasibility_horizon"
)

.oe_key <- function(plan) {
  return(as.data.frame(plan$ett)[, .oe_cols])
}

# ---- (a): the grid --------------------------------------------------------------

test_that("(a) a restricted horizon gives ETTs to the named enrollment only", {
  plan <- .oe_plan(.oe_spec(.OE_FU_A))$plan
  ett <- plan$ett
  expect_identical(nrow(ett), 10L)
  expect_false(any(ett$feasibility_horizon))
  expect_identical(unique(ett$enrollment_id[ett$follow_up == 156L]), "01")
  expect_identical(sum(ett$follow_up == 156L), 2L)
  expect_identical(as.vector(table(ett$enrollment_id)), c(6L, 4L))
  # A dropped cell takes no ETT id, so the ids run without a gap.
  expect_identical(ett$ett_id, sprintf("ETT%05d", 1:10))
  expect_identical(
    ett$follow_up,
    c(26L, 52L, 156L, 26L, 52L, 156L, 26L, 52L, 26L, 52L)
  )
})

test_that("(a) print() shows the ETT count without the product", {
  plan <- .oe_plan(.oe_spec(.OE_FU_A))$plan
  out <- utils::capture.output(print(plan))
  expect_true("  10 ETT(s)" %in% out)
  expect_false(any(grepl("follow-up x", out, fixed = TRUE)))
})

# ---- (b): the refusals ----------------------------------------------------------

test_that("(b) each bad value stops tteplan_read_spec() and gives one check row", {
  bad <- list(
    empty = list(
      value = list(),
      message = paste0(
        "follow_up[3] '3 years' has only_enrollments []. The value is not ",
        "a non-empty list of strings."
      )
    ),
    duplicate = list(
      value = c("01", "01"),
      message = paste0(
        "follow_up[3] '3 years' has only_enrollments [01, 01]. It repeats ",
        "the id '01'."
      )
    ),
    unknown = list(
      value = c("01", "09"),
      message = paste0(
        "follow_up[3] '3 years' has only_enrollments [01, 09]. No enrollment ",
        "has the id '09'."
      )
    ),
    not_string = list(
      value = 1L,
      message = paste0(
        "follow_up[3] '3 years' has only_enrollments [1]. The value is not ",
        "a non-empty list of strings."
      )
    )
  )
  tail_msg <- paste0(
    " only_enrollments MUST be a non-empty list of enrollment ids, ",
    "each id once (a single id is a list of one)."
  )
  for (case in names(bad)) {
    fu <- .OE_FU_A
    fu[[3]]$only_enrollments <- bad[[case]]$value
    dir <- withr::local_tempdir()
    path <- spr_write_spec(.oe_spec(fu), dir)
    want <- paste0(bad[[case]]$message, tail_msg)
    err <- tryCatch(
      {
        suppressWarnings(swereg::tteplan_read_spec(path))
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    expect_identical(err, want, info = case)
    found <- swereg::tteplan_check_spec(path)
    expect_identical(found$kind, "follow_up_only_enrollments", info = case)
    expect_identical(found$path, "$/follow_up[]/only_enrollments", info = case)
    expect_identical(found$message, want, info = case)
  }
})

test_that("(b) a good value reads, and gives no check row", {
  dir <- withr::local_tempdir()
  path <- spr_write_spec(.oe_spec(.OE_FU_A), dir)
  spec <- suppressWarnings(swereg::tteplan_read_spec(path))
  expect_identical(spec$follow_up[[3]]$only_enrollments, "01")
  expect_identical(nrow(swereg::tteplan_check_spec(path)), 0L)
})

# ---- (b2): a single id is a list of one -----------------------------------------

test_that("(b2) a bare id and a list of one id give identical grids", {
  fu_bare <- .OE_FU_A
  fu_bare[[3]]$only_enrollments <- "01"
  bare <- .oe_plan(.oe_spec(fu_bare))
  list_one <- .oe_plan(.oe_spec(.OE_FU_A))
  # The two files hold the two YAML forms.
  expect_true(any(grepl(
    "^  only_enrollments: '01'$",
    readLines(bare$path)
  )))
  expect_true(any(grepl(
    "^  only_enrollments:$",
    readLines(list_one$path)
  )))
  expect_identical(bare$plan$ett, list_one$plan$ett)
  expect_identical(nrow(bare$plan$ett), 10L)
  expect_identical(nrow(swereg::tteplan_check_spec(bare$path)), 0L)
})

# ---- (c): a spec without the key ------------------------------------------------

# The spec of the 27.3.1 measurement: follow-ups 52 and 104 weeks, two
# outcomes, two enrollments, no key. The full literal values measured on
# swereg 27.3.1 (13556d35) are in fixtures/follow-up-only-enrollments-2731.R.
.OE_FU_C <- list(
  list(label = "1 year", weeks = 52L),
  list(label = "2 years", weeks = 104L)
)

.oe_2731 <- function() {
  env <- new.env()
  sys.source(
    testthat::test_path("fixtures", "follow-up-only-enrollments-2731.R"),
    envir = env
  )
  return(env$OE_2731)
}

# The whole print() output. The plan gets the creation time the 27.3.1
# measurement used, so the "Created:" line compares too.
.OE_CREATED_AT <- as.POSIXct("2026-10-30 12:00:00", tz = "UTC")

.oe_print <- function(plan) {
  plan$created_at <- .OE_CREATED_AT
  return(utils::capture.output(print(plan)))
}

test_that("(c) a spec without the key gives the grid, print and OSF text of 27.3.1", {
  want <- .oe_2731()
  built <- .oe_plan(.oe_spec(.OE_FU_C))
  plan <- built$plan
  expect_identical(as.data.frame(plan$ett), want$ett)
  expect_identical(.oe_print(plan), want$print)
  # The literal holds "<version>" where 27.3.1 wrote its own version.
  osf_want <- gsub(
    "<version>",
    as.character(utils::packageVersion("swereg")),
    want$osf,
    fixed = TRUE
  )
  expect_identical(.oe_osf(built$path), osf_want)

  # A key that lists every enrollment keeps the whole ETT table and print().
  fu_all <- lapply(.OE_FU_C, function(f) {
    return(c(f, list(only_enrollments = list("01", "02"))))
  })
  plan_all <- .oe_plan(.oe_spec(fu_all))$plan
  expect_identical(as.data.frame(plan_all$ett), want$ett)
  expect_identical(.oe_print(plan_all), want$print)
})

# ---- (d): the texts name the comparison -----------------------------------------

test_that("(d) the OSF text names the comparison of a restricted horizon", {
  built <- .oe_plan(.oe_spec(.OE_FU_A))
  osf <- .oe_osf(built$path)
  expect_match(
    osf[["Study design"]],
    paste0(
      "The horizons are 6 months (26 weeks), 1 year (52 weeks) and 3 years ",
      "(156 weeks). The 3-year horizon applies only to comparison 1 ",
      "(Treated versus Control)."
    ),
    fixed = TRUE
  )
  expect_match(
    osf[["Statistical models"]],
    paste0(
      "Each comparison is analysed separately, for each outcome and each ",
      "horizon that applies to it."
    ),
    fixed = TRUE
  )
})

test_that("(d) horizons with the same comparisons share one OSF sentence", {
  fu <- list(
    list(label = "1 year", weeks = 52L),
    list(label = "3 years", weeks = 156L, only_enrollments = list("01")),
    list(label = "5 years", weeks = 260L, only_enrollments = list("01"))
  )
  osf <- .oe_osf(.oe_plan(.oe_spec(fu))$path)
  expect_match(
    osf[["Study design"]],
    paste0(
      "(260 weeks). The 3- and 5-year horizons apply only to comparison 1 ",
      "(Treated versus Control)."
    ),
    fixed = TRUE
  )
})

test_that("(d) TARGET 6d names the enrollment of a restricted horizon", {
  plan <- .oe_plan(.oe_spec(.OE_FU_A))$plan
  out <- trimws(utils::capture.output(plan$print_target_checklist()))
  expect_true("Horizon: 3 years (156 weeks), only for enrollment 01" %in% out)
  # An unrestricted horizon keeps the line of 27.3.1.
  expect_true("Horizon: 1 year (52 weeks)" %in% out)
})

test_that("(d) the spec summary and the protocol table name the enrollment", {
  plan <- .oe_plan(.oe_spec(.OE_FU_A))$plan
  out <- utils::capture.output(plan$print_spec_summary())
  expect_true("  - 3 years (156 weeks), only for enrollment 01" %in% out)
  expect_true("  - 1 year (52 weeks)" %in% out)

  spec_cell <- swereg:::.protocol_specification(
    plan$spec,
    "follow_up_period",
    swereg:::.protocol_context(plan, "ETT00007")
  )
  expect_match(
    spec_cell,
    "Horizons in the spec: 6 months, 1 year, 3 years (only enrollment 01)",
    fixed = TRUE
  )
  # The emulation column lists the horizons of the ETT's own enrollment.
  emu <- function(id) {
    return(swereg:::.protocol_emulation(
      plan$spec,
      "follow_up_period",
      swereg:::.protocol_context(plan, id)
    ))
  }
  expect_match(
    emu("ETT00001"),
    "Horizons in the ETT grid: 26, 52, 156 weeks",
    fixed = TRUE
  )
  expect_match(
    emu("ETT00007"),
    "Horizons in the ETT grid: 26, 52 weeks",
    fixed = TRUE
  )
})

test_that("(d) the spec summary sheet names the enrollment", {
  skip_if_not_installed("openxlsx")
  plan <- .oe_plan(.oe_spec(.OE_FU_A))$plan
  x <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(suppressWarnings(plan$excel_spec_summary(path = x)))
  cells <- unlist(openxlsx::read.xlsx(x, sheet = 1, colNames = FALSE))
  expect_true("156 weeks, only for enrollment 01" %in% cells)
  expect_true("52 weeks" %in% cells)
})

# ---- (e): the feasibility horizon per enrollment --------------------------------

.OE_FU_E <- list(
  list(label = "1 year", weeks = 52L),
  list(label = "3 years", weeks = 156L, only_enrollments = list("01"))
)

test_that("(e) the feasibility horizon goes to the enrollments it is longer for", {
  plan <- .oe_plan(.oe_spec(.OE_FU_E, w = 104L))$plan
  expect_identical(
    .oe_key(plan),
    data.frame(
      ett_id = sprintf("ETT%05d", 1:8),
      enrollment_id = c("01", "01", "01", "01", "02", "02", "02", "02"),
      outcome_var = c(
        "osd_a",
        "osd_a",
        "osd_b",
        "osd_b",
        "osd_a",
        "osd_b",
        "osd_a",
        "osd_b"
      ),
      follow_up = c(52L, 156L, 52L, 156L, 52L, 52L, 104L, 104L),
      feasibility_horizon = c(rep(FALSE, 6L), TRUE, TRUE)
    )
  )
  out <- utils::capture.output(plan$print_spec_summary())
  expect_true(
    paste0(
      "  - Feasibility horizon: 2 years (104 weeks), minimum-events rule ",
      "only, only for enrollment 02"
    ) %in%
      out
  )
})

test_that("(e) without the key, the grid is the grid of 27.3.1", {
  fu <- .OE_FU_E
  fu[[2]]$only_enrollments <- NULL
  plan <- .oe_plan(.oe_spec(fu, w = 104L))$plan
  # Measured on swereg 27.3.1 (13556d35): 104 is not longer than 156, so
  # the key adds no ETT.
  expect_identical(
    .oe_key(plan),
    data.frame(
      ett_id = sprintf("ETT%05d", 1:8),
      enrollment_id = rep(c("01", "02"), each = 4L),
      outcome_var = rep(rep(c("osd_a", "osd_b"), each = 2L), 2L),
      follow_up = rep(c(52L, 156L), 4L),
      feasibility_horizon = rep(FALSE, 8L)
    )
  )
  out <- utils::capture.output(plan$print_spec_summary())
  expect_false(any(grepl("Feasibility horizon", out, fixed = TRUE)))
})
