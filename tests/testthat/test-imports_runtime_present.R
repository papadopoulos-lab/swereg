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
