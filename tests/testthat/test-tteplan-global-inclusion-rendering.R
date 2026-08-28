# A global inclusion criterion must be visible to a human reader.
#
# Phase 1 made the `inclusion_criteria$criteria` container compute: the
# criterion reaches the eligibility filter and restricts the cohort. Five
# consumers still rendered `isoyears` alone, so a criterion that dropped people
# from the study appeared in no human-facing output at all.
#
# The five consumers are the spec workbook, the protocol table, the console
# specification summary, the TARGET checklist and the CONSORT label lookup.
# This file asserts the criterion in each one.
#
# The workbook test carries the design decision. swereg applies the criterion
# in place, once per skeleton, rather than copying it into each enrollment. A
# copy would render 18 times in a spec with 18 enrollments. The exactly-once
# count is what pins that choice against a later refactor.
#
# The CONSORT tests call `.build_criterion_label_lookup()` directly. Rendering
# a diagram needs DiagrammeR and DiagrammeRsvg, which this installation does
# not carry, and the lookup is the whole of what phase 2 changes there.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# The criterion every test below renders. Its window is 104 weeks, which
# `.format_window_human()` prints as "2 years before baseline".
gir_criterion <- function(
  name = "Prior psychotic disorder (ICD-10 F20-F29)",
  source_variable = "osd_f20_to_f29",
  window = 104L
) {
  impl <- list(source_variable = source_variable, computed = TRUE)
  if (!is.null(window)) {
    impl$window <- window
  }
  return(list(
    name = name,
    rationale = "Restricts the study population.",
    type = "has_event",
    implementation = impl
  ))
}

# Write a minimal valid spec and read it back through `tteplan_read_spec()`.
#
# `n_enrollments` sets how many enrollments the spec carries. Enrollment "01"
# declares an age range of its own, and takes `enrollment_has_event` when the
# caller supplies one. Every other enrollment declares no additional inclusion,
# so an eligibility column on those can only come from the global container.
#
# The `no_prior_intervention` exclusion on `rd_exposure` silences the
# prevalent-user warning.
gir_spec <- function(
  criteria = NULL,
  n_enrollments = 2L,
  enrollment_has_event = NULL
) {
  inclusion <- list(isoyears = c(2015L, 2016L))
  if (!is.null(criteria)) {
    inclusion$criteria <- criteria
  }
  treatment <- list(
    implementation = list(
      variable = "rd_exposure",
      intervention_value = "treated",
      comparator_value = "control",
      comparator_to_intervention_ratio = 2L,
      seed = 42L
    )
  )
  enrollments <- lapply(seq_len(n_enrollments), function(i) {
    extra <- if (i == 1L) {
      c(
        list(list(
          name = "Age 50-60",
          type = "age_range",
          min = 50,
          max = 60,
          implementation = list(variable = "rd_age_continuous")
        )),
        if (is.null(enrollment_has_event)) NULL else list(enrollment_has_event)
      )
    } else {
      NULL
    }
    return(list(
      id = sprintf("%02d", i),
      name = paste("Enrollment", i),
      observed_var = list(sentinel = "row_presence"),
      additional_inclusion = extra,
      treatment = treatment
    ))
  })
  spec <- list(
    study = list(
      title = "Global inclusion rendering test",
      implementation = list(project_prefix = "test_project", version = "v001")
    ),
    inclusion_criteria = inclusion,
    enrollments = enrollments,
    outcomes = list(list(
      name = "Event A",
      implementation = list(variable = "diag_event_a")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    exclusion_criteria = list(list(
      name = "Prior intervention",
      implementation = list(
        type = "no_prior_intervention",
        source_variable = "rd_exposure",
        intervention_value = "treated",
        window = "lifetime_before_baseline",
        computed = TRUE
      )
    ))
  )
  dir <- tempfile("spec_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(tteplan_read_spec(path))
}

# A plan-shaped list. Every consumer under test reads `$spec`, and the TARGET
# checklist also calls `$get_attrition()`. An attrition table with no rows
# leaves the checklist's item 8 loop with nothing to iterate.
gir_plan <- function(spec) {
  return(list(
    spec = spec,
    code_registry = NULL,
    period_width = 4L,
    get_attrition = function() {
      return(data.table::data.table(enrollment_id = character()))
    }
  ))
}

# The "Study Specification" sheet, read back with its row order intact.
gir_sheet <- function(spec) {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_spec_summary(wb, gir_plan(spec))
  return(openxlsx::readWorkbook(
    wb,
    "Study Specification",
    colNames = FALSE,
    skipEmptyRows = FALSE
  ))
}

# The context the eligibility cell of the protocol table reads. That cell uses
# `ctx$enrollment` and `ctx$enrollment_id` and no other field.
gir_ctx <- function(spec) {
  return(list(enrollment = spec$enrollments[[1]], enrollment_id = "01"))
}

gir_name <- "Prior psychotic disorder (ICD-10 F20-F29)"


# ---------------------------------------------------------------------------
# 1. The spec workbook
# ---------------------------------------------------------------------------

test_that("the spec workbook renders a global criterion once across 18 enrollments", {
  spec <- gir_spec(list(gir_criterion()), n_enrollments = 18L)
  expect_length(spec$enrollments, 18L)

  d <- gir_sheet(spec)
  col_a <- as.character(d[[1]])
  col_b <- as.character(d[[2]])

  # The criterion's name row. One occurrence, across all 18 enrollments.
  hits <- which(!is.na(col_a) & col_a == gir_name)
  expect_length(hits, 1L)

  # Its two child rows, in order, directly below the name row.
  i <- hits[1]
  expect_identical(col_a[i + 1L], "Variable:")
  expect_identical(col_b[i + 1L], "osd_f20_to_f29")
  expect_identical(col_a[i + 2L], "Window:")
  expect_identical(col_b[i + 2L], "2 years before baseline")

  # The name row sits under the global header, above the exclusion header.
  incl <- which(!is.na(col_a) & col_a == "Inclusion criteria (global)")
  excl <- which(!is.na(col_a) & col_a == "Exclusion criteria (global)")
  expect_length(incl, 1L)
  expect_true(i > incl[1] && i < excl[1])
})

test_that("the spec workbook renders no criterion block when the container holds none", {
  d <- gir_sheet(gir_spec(criteria = NULL, n_enrollments = 2L))
  col_a <- as.character(d[[1]])
  expect_false(any(!is.na(col_a) & col_a == gir_name))
})

test_that("the spec workbook renders a defaulted window as the lifetime default", {
  # The criterion declares no `window`, so normalization gives it
  # `window_weeks = Inf`. `.format_window_human()` reads `window` alone and
  # would call that "(not specified)".
  spec <- gir_spec(list(gir_criterion(window = NULL)))
  d <- gir_sheet(spec)
  col_a <- as.character(d[[1]])
  col_b <- as.character(d[[2]])
  i <- which(!is.na(col_a) & col_a == gir_name)[1]
  expect_identical(col_b[i + 2L], "lifetime before baseline")
})


# ---------------------------------------------------------------------------
# 2. The protocol table
# ---------------------------------------------------------------------------

test_that("the protocol table specification cell names the global criterion", {
  spec <- gir_spec(list(gir_criterion()))
  cell <- swereg:::.protocol_specification(
    spec,
    "eligibility_criteria",
    gir_ctx(spec)
  )
  lines <- strsplit(cell, "\n", fixed = TRUE)[[1]]
  expect_true(paste0("Include: ", gir_name) %in% lines)
  # The container itself is not a criterion, so no bare "Include: " row.
  expect_false("Include: " %in% lines)
})

test_that("the protocol table emulation cell names the global criterion's variable", {
  spec <- gir_spec(list(gir_criterion()))
  cell <- swereg:::.protocol_emulation(
    spec,
    "eligibility_criteria",
    gir_ctx(spec)
  )
  lines <- strsplit(cell, "\n", fixed = TRUE)[[1]]
  expect_true(
    "Require osd_f20_to_f29 (2 years before baseline)" %in% lines
  )
})


# ---------------------------------------------------------------------------
# 3. The console specification summary
# ---------------------------------------------------------------------------

test_that("the console summary prints the global inclusion criterion", {
  spec <- gir_spec(list(gir_criterion()))
  out <- capture.output(swereg:::.plan_print_spec_summary(gir_plan(spec)))
  # Drop the ANSI escapes the summary uses for colour.
  out <- gsub("\033\\[[0-9;]*m", "", out)

  head_at <- grep("^Inclusion criteria \\(global\\):", out)
  expect_length(head_at, 1L)
  block <- out[seq(head_at[1], head_at[1] + 4L)]
  expect_true(any(grepl(gir_name, block, fixed = TRUE)))
  expect_true(any(grepl("Variable:    osd_f20_to_f29", block, fixed = TRUE)))
  expect_true(any(grepl(
    "Window:      2 years before baseline",
    block,
    fixed = TRUE
  )))
})


# ---------------------------------------------------------------------------
# 4. The TARGET checklist
# ---------------------------------------------------------------------------

test_that("the TARGET checklist eligibility item names the global criterion", {
  # Item 6a is the eligibility item. Its block runs from the item heading to
  # the ">> [FILL IN]" line the checklist puts after every item.
  #
  # The assertions name the criterion's own content: its name, its source
  # variable and its window. They do not name the "- Inclusion: " prefix. A
  # prefix is wording. Changing it leaves the criterion in the checklist, and
  # a test that goes red on it pins nothing a reader needs.
  spec <- gir_spec(list(gir_criterion()))
  out <- capture.output(swereg:::.plan_print_target_checklist(gir_plan(spec)))
  out <- gsub("\033\\[[0-9;]*m", "", out)

  head_at <- grep("^Item 6a\\.", out)
  expect_length(head_at, 1L)
  end_at <- grep(">> [FILL IN]", out, fixed = TRUE)
  end_at <- end_at[end_at > head_at[1]][1]
  expect_false(is.na(end_at))
  block <- out[seq(head_at[1], end_at)]

  expect_true(any(grepl("- ISO years: 2015-2016", block, fixed = TRUE)))

  # One line of item 6a carries the criterion. That line carries its source
  # variable and its window too, so the three cannot be satisfied separately
  # by three unrelated lines.
  hit <- block[grepl(gir_name, block, fixed = TRUE)]
  expect_length(hit, 1L)
  expect_true(grepl("osd_f20_to_f29", hit, fixed = TRUE))
  expect_true(grepl("2 years before baseline", hit, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# 5. The CONSORT label lookup
# ---------------------------------------------------------------------------

test_that("the CONSORT lookup labels the global criterion's eligibility column", {
  spec <- gir_spec(list(gir_criterion()))
  labels <- swereg:::.build_criterion_label_lookup(
    gir_plan(spec),
    enrollment_id = "01",
    observed_criteria = c(
      "before_exclusions",
      "eligible_isoyears",
      "eligible_has_osd_f20_to_f29_104wk",
      "eligible_age",
      "eligible_no_rd_exposure_everbefore"
    )
  )
  expect_true("eligible_has_osd_f20_to_f29_104wk" %in% names(labels))
  expect_identical(
    unname(labels["eligible_has_osd_f20_to_f29_104wk"]),
    paste0(gir_name, "\\n(104 weeks before baseline)")
  )
})

test_that("the CONSORT lookup labels the same global column on an enrollment with no inclusion of its own", {
  # Enrollment "02" declares no `additional_inclusion` at all. The label can
  # only come from the global container.
  spec <- gir_spec(list(gir_criterion()), n_enrollments = 2L)
  labels <- swereg:::.build_criterion_label_lookup(
    gir_plan(spec),
    enrollment_id = "02",
    observed_criteria = "eligible_has_osd_f20_to_f29_104wk"
  )
  expect_identical(
    unname(labels["eligible_has_osd_f20_to_f29_104wk"]),
    paste0(gir_name, "\\n(104 weeks before baseline)")
  )
})

test_that("the CONSORT lookup labels a per-enrollment has_event column", {
  # The per-enrollment gap predates phase 1. An `eligible_has_*` column from
  # `additional_inclusion` fell through the lookup unlabelled.
  spec <- gir_spec(
    criteria = NULL,
    enrollment_has_event = gir_criterion(
      name = "Prior mood disorder (ICD-10 F30-F39)",
      source_variable = "osd_f30_to_f39",
      window = NULL
    )
  )
  labels <- swereg:::.build_criterion_label_lookup(
    gir_plan(spec),
    enrollment_id = "01",
    observed_criteria = "eligible_has_osd_f30_to_f39_everbefore"
  )
  expect_identical(
    unname(labels["eligible_has_osd_f30_to_f39_everbefore"]),
    "Prior mood disorder (ICD-10 F30-F39)\\n(ever before baseline)"
  )
})

test_that("the CONSORT lookup leaves an unmatched has_event column unlabelled", {
  # No global criterion generates this column, so the lookup must not invent a
  # label for it.
  spec <- gir_spec(list(gir_criterion()))
  labels <- swereg:::.build_criterion_label_lookup(
    gir_plan(spec),
    enrollment_id = "01",
    observed_criteria = "eligible_has_osd_f99_to_f99_52wk"
  )
  expect_false("eligible_has_osd_f99_to_f99_52wk" %in% names(labels))
})
