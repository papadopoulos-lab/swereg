# Two properties, one file, because they are the two halves of one decision:
# which packages a plain `install.packages("swereg")` MUST carry.
#
# 1. Every package in `.RUNTIME_REQUIRED` is a DESCRIPTION Imports entry and
#    is installed. `survey` sat in Suggests until 26.4.27. A missing `survey`
#    failed inside a worker subprocess, a `tryCatch()` caught it, and the
#    result surfaced as a `skipped = TRUE` placeholder. That produced 135 of
#    135 IRRs reading `"there is no package called 'survey'"`, plus an empty
#    forest plot.
#
# 2. Every package in `.CONSORT_OPTIONAL` stays in Suggests, and
#    `.require_consort_stack()` in R/consort.R guards it. A plain install
#    does not carry these three, so the guard MUST stop and MUST name the
#    absent package. It stops rather than warns because the caller in
#    R/tteplan_export.R returns the sidecar path whatever the renderer
#    returns. A warning left the caller naming a PNG that was never written.
#    The guard MUST also let a complete install through, so the last two
#    tests render a CONSORT diagram and read the bytes they wrote. One calls
#    the renderer, and one calls `$export_tables()`.
#
# The two vectors MUST NOT overlap. A package cannot be a hard requirement
# and an optional extra at the same time.

.RUNTIME_REQUIRED <- c(
  "survey", # IRR / KM via svyglm / svykm in r6_tteenrollment.R
  "mgcv", # mgcv::bam() for IPCW with GAM
  "MASS", # MASS::ginv() in table1.R
  "scales", # scales::percent in tte_survival_render.R
  "openxlsx", # workbook export
  "patchwork" # forest plot composition
)

.CONSORT_OPTIONAL <- c("DiagrammeR", "DiagrammeRsvg", "rsvg")

# Reads the installed DESCRIPTION, so it needs no source tree and never
# skips. `system.file()` resolves to the source DESCRIPTION under
# `pkgload::load_all()` and to the installed one under `R CMD check`.
.declared_deps <- function(field) {
  desc <- read.dcf(system.file("DESCRIPTION", package = "swereg"))
  entries <- strsplit(desc[1, field], ",", fixed = TRUE)[[1]]
  trimws(sub("\\(.*", "", entries))
}

test_that("every runtime-required package is declared in Imports", {
  absent <- setdiff(.RUNTIME_REQUIRED, .declared_deps("Imports"))
  expect_equal(
    absent,
    character(),
    info = paste("required but not in DESCRIPTION Imports:", toString(absent))
  )
})

test_that("runtime-required packages are installed", {
  for (p in .RUNTIME_REQUIRED) {
    expect_true(
      requireNamespace(p, quietly = TRUE),
      info = paste0(
        "runtime-required package not installed: ",
        p,
        " -- if intentionally moved back to Suggests, remove it from ",
        ".RUNTIME_REQUIRED here AND guard every call site."
      )
    )
  }
})

test_that("the CONSORT diagram stack is optional, not required", {
  suggests <- .declared_deps("Suggests")
  for (p in .CONSORT_OPTIONAL) {
    expect_true(
      p %in% suggests,
      info = paste0(
        "optional CONSORT package not in DESCRIPTION Suggests: ",
        p,
        " -- the guard in R/consort.R assumes a plain install lacks it."
      )
    )
  }
  both <- intersect(.CONSORT_OPTIONAL, .RUNTIME_REQUIRED)
  expect_equal(
    both,
    character(),
    info = paste("listed as both required and optional:", toString(both))
  )
})

# One package absent at a time. Each call drives the production entry point
# `.render_consort_sidecars()`, which calls the guard before it reads `plan`.
expect_consort_guard_fires <- function(absent) {
  testthat::local_mocked_bindings(
    .consort_stack_absent = function() absent,
    .package = "swereg"
  )
  err <- expect_error(
    swereg:::.render_consort_sidecars(
      plan = NULL,
      ec = NULL,
      eid = "01",
      label = "test enrollment",
      output_dir = tempdir()
    )
  )
  msg <- conditionMessage(err)
  expect_match(msg, paste0("Not installed: ", absent), fixed = TRUE)
  expect_match(msg, "Enrollment: 01", fixed = TRUE)
  expect_match(
    msg,
    'pak::pak(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))',
    fixed = TRUE
  )
}

test_that("the CONSORT guard stops and names each absent package", {
  for (p in .CONSORT_OPTIONAL) {
    expect_consort_guard_fires(p)
  }
})

test_that("the CONSORT guard passes when all three packages are installed", {
  for (p in .CONSORT_OPTIONAL) {
    skip_if_not_installed(p)
  }
  expect_equal(swereg:::.consort_stack_absent(), character())
  expect_true(swereg:::.require_consort_stack("01"))
})

# The smallest input `.build_cohort_flow()` accepts. The shape comes from
# `test-cohort_flow.R`: one global row per criterion, so `trial_id` is `NA`,
# plus a matching table with one row per trial.
.consort_fixture_counts <- function() {
  list(
    attrition = data.table::data.table(
      trial_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_age", "eligible_no_x"),
      n_persons = c(1000, 800, 700),
      n_person_trials = c(5000, 4000, 3500),
      n_intervention = c(1000, 800, 700),
      n_comparator = c(4000, 3200, 2800)
    ),
    matching = data.table::data.table(
      trial_id = 1:2,
      n_intervention_enrolled = c(350, 350),
      n_comparator_enrolled = c(700, 700)
    )
  )
}

# The guard MUST NOT block a machine that carries all three packages. Every
# other CONSORT test above drives the guard with a package removed, so none of
# them can see a guard that blocks everything.
#
# This renders. `.render_consort_sidecars()` holds every `DiagrammeR::`,
# `DiagrammeRsvg::` and `rsvg::` call in the package. One call therefore
# covers the whole chain: cohort flow, Graphviz DOT, SVG, then PNG and PDF.
#
# `plan` is a plain list. The renderer reads `$spec`, `$period_width` and
# `$project_prefix` from it, and calls `$get_baselines()` inside a
# `tryCatch()`. A list satisfies all four.
#
# This route costs about 0.3 seconds. The test below renders the same diagram
# through `$export_tables()`, and costs about 1.7 seconds.
test_that("CONSORT renders end-to-end when the stack is present", {
  for (p in .CONSORT_OPTIONAL) {
    skip_if_not_installed(p)
  }

  out <- file.path(tempdir(), "consort_end_to_end")
  unlink(out, recursive = TRUE)
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  # `expect_no_error()` names the property and returns the value. A guard that
  # wrongly reports a package absent stops the renderer, and this expectation
  # is then the one that fails. A bare call would abort the test instead, and
  # the report would name no assertion.
  paths <- expect_no_error(
    swereg:::.render_consort_sidecars(
      plan = list(spec = NULL, period_width = 4L, project_prefix = "guardtest"),
      ec = .consort_fixture_counts(),
      eid = "01",
      label = "guard happy path",
      output_dir = out,
      img_basename = "guard_consort"
    )
  )

  expect_type(paths, "list")
  expect_true(file.exists(paths$png))
  expect_true(file.exists(paths$pdf))

  # A stub file satisfies `file.exists()`. These floors do not. On this
  # fixture the PNG measures 89601 bytes and the PDF 12499. Each floor sits
  # under an eighth of the real size.
  expect_gt(file.size(paths$png), 10000)
  expect_gt(file.size(paths$pdf), 1000)
})

# The same property, through a production export. The test above calls the
# renderer directly, so it cannot see a caller that never reaches the renderer.
# This one calls `$export_tables()`, which reaches it at
# R/tteplan_export.R:390.
#
# The fixture is `.xp_plan()` from `helper-export_parity.R`, the plan every
# export test uses. The export writes the workbook, the Love plot and the
# CONSORT sidecars into one directory.
#
# This route costs about 1.7 seconds, against about 0.3 for the direct
# render. Both stay. The direct one pins the render chain cheaply, and this
# one pins the caller.
test_that("a production export writes the CONSORT sidecar", {
  for (p in c(.CONSORT_OPTIONAL, "openxlsx", "ggplot2", "patchwork", "withr")) {
    skip_if_not_installed(p)
  }

  out <- withr::local_tempdir()
  plan <- .xp_plan("new", subgroups = TRUE)

  # A guard that wrongly reports a package absent stops the export here. The
  # two suppressors sit outside `expect_no_error()`, so a failure names
  # `plan$export_tables(...)` rather than `suppressMessages(...)`. Neither
  # suppressor touches a testthat expectation: those signal on the condition
  # class `expectation`, and not as a message or a warning.
  suppressMessages(suppressWarnings(
    expect_no_error(
      plan$export_tables(
        path = file.path(out, "tables.xlsx"),
        protocol_ett_id = "ETT00003"
      )
    )
  ))

  png <- list.files(out, pattern = "consort_.*\\.png$", full.names = TRUE)
  expect_length(png, 1L)

  # `file.size()` on an empty vector returns `numeric(0)`, and `expect_gt()`
  # errors on that instead of failing. `c(0, ...)` keeps the report a failure.
  # The measured size on this fixture is 104398 bytes.
  expect_gt(max(c(0, file.size(png))), 10000)
})
