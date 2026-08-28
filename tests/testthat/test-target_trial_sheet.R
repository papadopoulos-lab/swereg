# Target trial protocol sheet (Dickerman et al., Table S1 layout).
#
# The sheet has three columns, and the two content columns have deliberately
# DIFFERENT provenance:
#
#   Target trial specification -- the spec's clinical fields plus the study
#     team's own prose under the spec's `target_trial:` key.
#   Target trial emulation     -- RENDERED from the nested `implementation:`
#     blocks. Never read from YAML prose, and there is no `emulation:` key
#     under `target_trial:` for it to read.
#
# That difference is what these tests exist to pin. `rd_age_continuous` is the
# discriminator: the fixture puts it inside `implementation:` blocks only
# (never under `target_trial:`), so it can reach the emulation column only by
# being rendered.
#
# The fixture MIRRORS THE REAL SPEC'S NESTING. `implementation:` never appears
# at top level in a real spec -- it appears inside `study`, inside every
# `inclusion_criteria` / `exclusion_criteria` / `confounders` entry, and inside
# every enrollment's criteria and treatment. A flat top-level `implementation:`
# fixture would test a structure that does not exist.
#
# NOT COVERED HERE: that the sheet reaches a REAL `$export_tables()` workbook.
# No fixture plan is complete enough to run that end to end. The ritual-balance
# and source-grep tests below are a proxy for the three-step sheet ritual, not
# a substitute for an end-to-end run.

skip_if_not_installed("openxlsx")
skip_if_not_installed("data.table")

# --- fixture ---------------------------------------------------------------

.tt_fixture_spec <- function() {
  list(
    study = list(
      title = "Fixture target trial",
      design = "Sequential target trial emulation",
      implementation = list(
        project_prefix = "fixture-target-trial",
        version = "v001"
      )
    ),
    inclusion_criteria = list(isoyears = c(2010L, 2020L)),
    exclusion_criteria = list(
      list(
        name = "Prior outcome event",
        implementation = list(
          source_variable = "osd_x",
          source_variable_combined = "osd_x",
          window = 104,
          computed = TRUE
        )
      )
    ),
    confounders = list(
      list(
        name = "Age (continuous)",
        implementation = list(variable = "rd_age_continuous")
      ),
      list(
        name = "Education level",
        implementation = list(variable = "rd_education")
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
    follow_up = list(
      list(label = "1 year", weeks = 52),
      list(label = "5 years", weeks = 260)
    ),
    enrollments = list(
      list(
        id = "01",
        name = "Arm A vs Arm B, age 50-54",
        additional_inclusion = list(
          list(
            name = "Age 50-54",
            type = "age_range",
            min = 50,
            max = 54,
            implementation = list(variable = "rd_age_continuous")
          )
        ),
        additional_exclusion = list(
          list(
            name = "Prior Arm A use",
            implementation = list(
              type = "no_prior_intervention",
              source_variable = "rd_tx",
              source_variable_combined = "rd_tx",
              intervention_value = "arm_a",
              window = "lifetime_before_baseline",
              computed = TRUE
            )
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
    ),
    # Authored prose only. Deliberately no `emulation:` key, and deliberately
    # no variable name anywhere in it.
    target_trial = list(
      eligibility_criteria = list(
        specification = "PLACEHOLDER eligibility criteria (fixture)"
      ),
      treatment_strategies = list(
        specification = "PLACEHOLDER treatment strategies (fixture)"
      ),
      assignment_procedure = list(
        specification = "PLACEHOLDER assignment procedure (fixture)"
      ),
      outcome = list(specification = "PLACEHOLDER outcome (fixture)"),
      follow_up_period = list(
        specification = "PLACEHOLDER follow-up period; time zero and the censoring list are not specified (fixture)"
      ),
      causal_contrast = list(
        specification = "PLACEHOLDER causal contrast (fixture)"
      ),
      analysis_plan = list(
        specification = "PLACEHOLDER analysis plan (fixture)"
      )
    )
  )
}

.tt_fixture_plan <- function() {
  list(
    spec = .tt_fixture_spec(),
    ett = data.table::data.table(
      ett_id = "ETT00001",
      enrollment_id = "01",
      outcome_var = "osd_a",
      outcome_name = "Outcome A",
      follow_up = 260
    )
  )
}

.tt_render <- function(plan = .tt_fixture_plan(), ett_id = "ETT00001") {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_protocol_table(wb, "Target trial protocol", plan, ett_id)
  f <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

.tt_read <- function(f) {
  openxlsx::read.xlsx(f, sheet = "Target trial protocol", startRow = 3)
}

# Cell lookup by ROW. The discriminator must pin WHICH row carries a rendered
# variable name, not merely that some row does: `rd_age_continuous` legitimately
# renders in two of the seven rows, so a column-wide `any()` passes when either
# one alone renders. That is the same class of hole as an assertion that passes
# for a reason other than the one it states.
.tt_cell <- function(d, component, column) {
  i <- which(d$Protocol.component == component)
  if (length(i) != 1L) {
    return(NA_character_)
  }
  as.character(d[[column]][i])
}

.tt_emulation <- function(d, component) {
  .tt_cell(d, component, "Target.trial.emulation")
}

.tt_specification <- function(d, component) {
  .tt_cell(d, component, "Target.trial.specification")
}

.tt_title <- function(f) {
  as.character(openxlsx::read.xlsx(
    f,
    sheet = "Target trial protocol",
    startRow = 1,
    colNames = FALSE,
    skipEmptyRows = FALSE
  )[1, 1])
}

# --- the fixture must mirror the real spec's nesting -----------------------

test_that("fixture: implementation blocks are nested, never top-level", {
  spec <- .tt_fixture_spec()
  expect_false("implementation" %in% names(spec))
  expect_true(!is.null(spec$study$implementation))
  expect_true(!is.null(spec$exclusion_criteria[[1]]$implementation))
  expect_true(!is.null(spec$confounders[[1]]$implementation))
  expect_true(!is.null(spec$outcomes[[1]]$implementation))
  expect_true(
    !is.null(spec$enrollments[[1]]$additional_inclusion[[1]]$implementation)
  )
  expect_true(!is.null(spec$enrollments[[1]]$treatment$implementation))

  # The discriminator variable lives in implementation blocks only.
  expect_identical(
    spec$confounders[[1]]$implementation$variable,
    "rd_age_continuous"
  )
  expect_identical(
    spec$enrollments[[1]]$additional_inclusion[[1]]$implementation$variable,
    "rd_age_continuous"
  )
  # ... and nowhere under target_trial, at any depth.
  expect_false(any(grepl(
    "rd_age_continuous",
    unlist(spec$target_trial, use.names = FALSE),
    fixed = TRUE
  )))
  # There is no `emulation:` key to read the emulation column from.
  for (k in names(spec$target_trial)) {
    expect_false("emulation" %in% names(spec$target_trial[[k]]))
  }
})

# --- assertion 1: exactly three columns ------------------------------------

test_that("target trial protocol sheet renders exactly three columns", {
  d <- .tt_read(.tt_render())
  expect_equal(ncol(d), 3L)
  expect_identical(
    names(d),
    c(
      "Protocol.component",
      "Target.trial.specification",
      "Target.trial.emulation"
    )
  )
})

# --- assertion 2: THE DISCRIMINATOR ----------------------------------------

test_that("emulation column is rendered from implementation blocks", {
  d <- .tt_read(.tt_render())

  # ROW-SPECIFIC, not a column-wide any(). `rd_age_continuous` sits in two
  # implementation blocks in the fixture -- the enrollment's age_range and the
  # first confounder -- so it legitimately renders in exactly two rows. A
  # column-wide any() would pass if only ONE of them rendered, which is a
  # different claim from the one this test makes. Both rows are asserted, and
  # so are the five that must NOT carry it.
  expect_true(grepl(
    "rd_age_continuous",
    .tt_emulation(d, "Eligibility criteria"),
    fixed = TRUE
  ))
  expect_true(grepl(
    "rd_age_continuous",
    .tt_emulation(d, "Analysis plan"),
    fixed = TRUE
  ))
  for (component in c(
    "Treatment strategies",
    "Assignment procedure",
    "Outcome",
    "Follow-up period",
    "Causal contrast"
  )) {
    expect_false(
      grepl("rd_age_continuous", .tt_emulation(d, component), fixed = TRUE),
      info = paste0(component, ": must not carry rd_age_continuous")
    )
  }

  # The specification column never carries it, in any row.
  expect_false(any(grepl(
    "rd_age_continuous",
    d$Target.trial.specification,
    fixed = TRUE
  )))

  # Other implementation-only names, also pinned to the rows they belong in,
  # so a single accidental match cannot carry the test.
  expect_true(grepl(
    "rd_tx",
    .tt_emulation(d, "Treatment strategies"),
    fixed = TRUE
  ))
  expect_true(grepl(
    "rd_tx",
    .tt_emulation(d, "Assignment procedure"),
    fixed = TRUE
  ))
  expect_true(grepl("osd_a", .tt_emulation(d, "Outcome"), fixed = TRUE))
  expect_true(grepl(
    "osd_x",
    .tt_emulation(d, "Eligibility criteria"),
    fixed = TRUE
  ))
  expect_false(any(grepl("rd_tx", d$Target.trial.specification, fixed = TRUE)))
  expect_false(any(grepl("osd_a", d$Target.trial.specification, fixed = TRUE)))
})

# --- global inclusion criterion objects reach the emulation cell -----------
#
# THIS CANNOT BE TESTED FROM ANY SPEC WRITTEN SO FAR. In every one of them,
# `inclusion_criteria` is a named list holding only an `isoyears` pair: zero
# criterion objects, and therefore zero implementation blocks. The loop covered
# here renders an empty set against all of them, so nothing is missing from the
# protocol table as it renders today. The loop exists so that a later spec
# cannot silently drop a global inclusion criterion that carries its own
# implementation block. A synthetic fixture is the only way to exercise it.
#
# `inclusion_criteria` holds `isoyears` and `criteria`, and the renderer reads
# `criteria`. A criterion written as a direct child of the container reaches
# nothing, and the cell then renders without it.

test_that("a global inclusion criterion's implementation reaches the eligibility emulation cell", {
  spec <- .tt_fixture_spec()
  spec$inclusion_criteria <- list(
    isoyears = c(2010L, 2020L),
    criteria = list(
      list(
        name = "Registered in the prescribed drug register",
        type = "has_event",
        implementation = list(
          source_variable = "ri_in_lmed",
          source_variable_combined = "ri_in_lmed",
          window = "lifetime_before_baseline"
        )
      )
    )
  )
  plan <- .tt_fixture_plan()
  plan$spec <- spec

  d <- .tt_read(.tt_render(plan))
  cell <- .tt_emulation(d, "Eligibility criteria")
  expect_true(grepl("ri_in_lmed", cell, fixed = TRUE))
  expect_true(grepl("lifetime before baseline", cell, fixed = TRUE))

  # The `isoyears` scalar shares the same list and must NOT fall into the
  # criterion loop and render as a variable-less row.
  expect_true(grepl("Require isoyear in 2010 to 2020", cell, fixed = TRUE))
  expect_false(grepl("Require NA", cell, fixed = TRUE))

  # The clinical name still belongs to the specification column only.
  expect_true(grepl(
    "Registered in the prescribed drug register",
    .tt_specification(d, "Eligibility criteria"),
    fixed = TRUE
  ))
  expect_false(grepl(
    "Registered in the prescribed drug register",
    cell,
    fixed = TRUE
  ))
})

# --- assertion 3: the sheet names the ETT it documents ---------------------

test_that("sheet names its enrollment, outcome and follow-up horizon", {
  title <- .tt_title(.tt_render())
  expect_true(nzchar(title))
  expect_true(grepl("ETT00001", title, fixed = TRUE))
  expect_true(grepl("enrollment 01", title, fixed = TRUE))
  expect_true(grepl("Arm A vs Arm B, age 50-54", title, fixed = TRUE))
  expect_true(grepl("Outcome A", title, fixed = TRUE))
  expect_true(grepl("260 weeks", title, fixed = TRUE))
  expect_true(grepl("5 years", title, fixed = TRUE))
})

# --- assertion 4: seven components, in the Dickerman order -----------------

test_that("seven protocol components appear in the Dickerman order", {
  d <- .tt_read(.tt_render())
  expect_identical(
    d$Protocol.component,
    c(
      "Eligibility criteria",
      "Treatment strategies",
      "Assignment procedure",
      "Outcome",
      "Follow-up period",
      "Causal contrast",
      "Analysis plan"
    )
  )
  expect_equal(nrow(d), 7L)
})

# --- assertion 7: a component with no spec source shows its placeholder ----

test_that("components with no spec source render their target_trial placeholder", {
  d <- .tt_read(.tt_render())
  spec_col <- d$Target.trial.specification
  for (component in c("Causal contrast", "Analysis plan")) {
    i <- which(d$Protocol.component == component)
    expect_equal(length(i), 1L)
    cell <- spec_col[i]
    expect_false(is.na(cell), info = paste0(component, ": cell is NA"))
    expect_true(nzchar(cell), info = paste0(component, ": cell is empty"))
    expect_true(
      grepl("PLACEHOLDER", cell, fixed = TRUE),
      info = paste0(component, ": placeholder text missing")
    )
  }
  # Every one of the seven carries its authored placeholder, so an emptied
  # target_trial cannot hide behind the derived clinical text.
  expect_true(all(grepl("PLACEHOLDER", spec_col, fixed = TRUE)))
})

# --- assertion 5: .write_spec_summary() stays two-column --------------------

test_that(".write_spec_summary() still writes at most two columns", {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_spec_summary(wb, .tt_fixture_plan())
  f <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  d2 <- openxlsx::read.xlsx(
    f,
    sheet = "Study Specification",
    colNames = FALSE
  )
  expect_lte(ncol(d2), 2L)
})

# --- assertion 6: the three-step sheet ritual stays balanced ----------------
#
# `.plan_export_tables()` adds a sheet in three steps: write it, append to
# `toc_names`, append to `toc_desc`. Miss the third and every later
# description shifts by one, silently. Assert EQUALITY, not a literal count,
# so a later phase adding a sheet does not have to edit this test.

test_that("export_tables() sheet ritual stays balanced", {
  src_path <- testthat::test_path("..", "..", "R", "tteplan_export.R")
  skip_if_not(
    file.exists(src_path),
    "R/tteplan_export.R not found (installed pkg?)"
  )
  src <- readLines(src_path, warn = FALSE)
  n_names <- sum(grepl(
    "toc_names[[:space:]]*<-[[:space:]]*c\\(toc_names,",
    src
  ))
  n_desc <- sum(grepl("toc_desc[[:space:]]*<-[[:space:]]*c\\(", src))
  expect_gt(n_names, 0L)
  expect_equal(n_names, n_desc)
})

test_that("export_tables() performs all three ritual steps for the protocol sheet", {
  src_path <- testthat::test_path("..", "..", "R", "tteplan_export.R")
  skip_if_not(
    file.exists(src_path),
    "R/tteplan_export.R not found (installed pkg?)"
  )
  src <- paste(readLines(src_path, warn = FALSE), collapse = "\n")
  expect_true(grepl(".write_protocol_table(", src, fixed = TRUE))
  # Whitespace-tolerant: `air format` may reflow either append across lines.
  expect_true(grepl(
    "toc_names[[:space:]]*<-[[:space:]]*c\\([[:space:]]*toc_names,[[:space:]]*\"Target trial protocol\"",
    src
  ))
  expect_true(grepl(
    "toc_desc[[:space:]]*<-[[:space:]]*c\\([[:space:]]*toc_desc,[^)]*Dickerman Table S1",
    src
  ))
})

# --- lock, NOT a discriminator ---------------------------------------------
#
# `tteplan_read_spec()` checks only that the REQUIRED sections are present and
# never rejects an extra one, so this is green before any of the work above
# existed. It is kept as a lock against someone adding a key whitelist later,
# and it is deliberately not counted as proof of this phase.

test_that("tteplan_read_spec() preserves an unknown target_trial key", {
  skip_if_not_installed("yaml")
  spec_path <- testthat::test_path("fixtures", "spec_3x2x2.yaml")
  skip_if_not(file.exists(spec_path), "fixture YAML missing")
  txt <- readLines(spec_path, warn = FALSE)
  f <- tempfile(fileext = ".yaml")
  writeLines(
    c(
      txt,
      "",
      "target_trial:",
      "  causal_contrast:",
      "    specification: \"PLACEHOLDER causal contrast\""
    ),
    f
  )
  spec <- swereg::tteplan_read_spec(f)
  expect_identical(
    spec$target_trial$causal_contrast$specification,
    "PLACEHOLDER causal contrast"
  )
})
