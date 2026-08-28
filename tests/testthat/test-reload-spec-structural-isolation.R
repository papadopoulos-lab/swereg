# `$reload_spec()` MUST NOT change a structurally-classified field of
# `plan$spec`.
#
# `.diff_specs()` sorts every changed spec path into cosmetic or structural. A
# cosmetic path is a label. A cached result stays valid when a label changes.
# A structural path is a definition, and the cached result was computed
# against the old value.
#
# `$reload_spec()` used to warn about a structural change, record it in
# `$spec_reload_skipped_diffs`, and then assign the whole new spec. The
# eligibility builder reads `$spec`, so a refused change still reached the
# next run. An export could then describe a population that did not produce
# its results.
#
# The newest route into that defect is the global cohort criterion under
# `inclusion_criteria$criteria`, which `.diff_specs()` calls structural. This
# file uses it as the structural change.
#
# The file pins both halves. The structural field keeps its old value, and
# every cosmetic field takes the new one.

skip_if_not_installed("yaml")
skip_if_not_installed("data.table")

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# One spec. Every cosmetic label carries `tag`, and the global cohort
# criterion names `criterion_source`.
#
# The labels cover all five `push_c()` sites in `.diff_specs()`:
# `study$title`, `study$description` and `study$principal_investigator`;
# `outcomes[[1]]$name`; `enrollments[01]$name`;
# `enrollments[01]$treatment$description`; and both arms under
# `enrollments[01]$treatment$arms`.
rsi_spec <- function(tag = "old", criterion_source = "osd_f20_to_f29") {
  lab <- function(x) {
    return(paste0(x, " ", tag))
  }
  return(list(
    study = list(
      title = lab("Structural isolation fixture"),
      description = lab("Study description"),
      principal_investigator = lab("Principal investigator"),
      implementation = list(project_prefix = "rsi", version = "v001")
    ),
    inclusion_criteria = list(
      isoyears = c(2015L, 2016L),
      criteria = list(list(
        name = "Prior psychotic disorder",
        rationale = "Restricts the study population.",
        type = "has_event",
        implementation = list(
          source_variable = criterion_source,
          computed = TRUE,
          window = "lifetime_before_baseline"
        )
      ))
    ),
    enrollments = list(list(
      id = "01",
      name = lab("Enrollment 01"),
      observed_var = list(sentinel = "row_presence"),
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      treatment = list(
        description = lab("Treatment"),
        arms = list(
          intervention = lab("Initiators"),
          comparator = lab("Comparators")
        ),
        implementation = list(
          variable = "rd_exposure",
          intervention_value = "treated",
          comparator_value = "control",
          comparator_to_intervention_ratio = 2L,
          seed = 42L
        )
      )
    )),
    outcomes = list(list(
      name = lab("Event A"),
      implementation = list(variable = "osd_a")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    confounders = list(list(
      name = "Age",
      implementation = list(variable = "rd_age_continuous")
    ))
  ))
}

rsi_write <- function(spec) {
  dir <- tempfile("rsi_spec_")
  dir.create(dir)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(path)
}

# A plan that carries the old spec and no ETT grid. `$reload_spec()` reads
# `plan$spec` and writes it back, which is the path under test.
rsi_plan <- function(path_old) {
  plan <- swereg::TTEPlan$new(
    project_prefix = "rsi",
    skeleton_files = "rsi_skeleton.qs2",
    global_max_isoyearweek = "2016-52"
  )
  plan$spec <- swereg::tteplan_read_spec(path_old)
  return(plan)
}

# The old spec, the new spec, and a plan reloaded from the new one. The new
# spec changes every cosmetic label and the global cohort criterion.
rsi_reloaded <- function() {
  path_old <- rsi_write(rsi_spec("old", "osd_f20_to_f29"))
  path_new <- rsi_write(rsi_spec("new", "osd_f30_to_f39"))
  plan <- rsi_plan(path_old)
  suppressWarnings(plan$reload_spec(spec_path = path_new, quiet = TRUE))
  return(list(
    plan = plan,
    old = swereg::tteplan_read_spec(path_old),
    new = swereg::tteplan_read_spec(path_new)
  ))
}

# ---------------------------------------------------------------------------
# The structural field
# ---------------------------------------------------------------------------

test_that("reload_spec leaves the global cohort criterion at its old value", {
  f <- rsi_reloaded()

  # The two specs must genuinely differ here, or the assertion below holds for
  # the wrong reason.
  expect_false(identical(
    f$old$inclusion_criteria,
    f$new$inclusion_criteria
  ))

  expect_identical(f$plan$spec$inclusion_criteria, f$old$inclusion_criteria)
  expect_identical(
    f$plan$spec$inclusion_criteria$criteria[[1]]$implementation$source_variable,
    "osd_f20_to_f29"
  )
})

test_that("reload_spec leaves no structural difference in the reloaded spec", {
  # This is the post-condition itself, read off the production path. It stays
  # true when someone adds a field to `.diff_specs()` later.
  f <- rsi_reloaded()
  expect_length(swereg:::.diff_specs(f$old, f$plan$spec)$structural, 0L)
})

test_that("reload_spec records the skipped structural path", {
  f <- rsi_reloaded()
  expect_true(any(grepl(
    "inclusion_criteria",
    f$plan$spec_reload_skipped_diffs,
    fixed = TRUE
  )))
})

test_that("reload_spec warns about the structural change it refused", {
  path_old <- rsi_write(rsi_spec("old", "osd_f20_to_f29"))
  path_new <- rsi_write(rsi_spec("new", "osd_f30_to_f39"))
  plan <- rsi_plan(path_old)
  expect_warning(
    plan$reload_spec(spec_path = path_new, quiet = TRUE),
    "inclusion_criteria"
  )
})

# ---------------------------------------------------------------------------
# The cosmetic fields, one assertion per `push_c()` site
# ---------------------------------------------------------------------------

test_that("reload_spec applies every cosmetic field", {
  f <- rsi_reloaded()
  spec <- f$plan$spec

  # Site 1: `study$<k>`, for each of the three keys `.diff_specs()` reads.
  expect_identical(spec$study$title, "Structural isolation fixture new")
  expect_identical(spec$study$description, "Study description new")
  expect_identical(
    spec$study$principal_investigator,
    "Principal investigator new"
  )

  # Site 2: `outcomes[[i]]$name`.
  expect_identical(spec$outcomes[[1]]$name, "Event A new")

  # Site 3: `enrollments[<id>]$name`.
  expect_identical(spec$enrollments[[1]]$name, "Enrollment 01 new")

  # Site 4: `enrollments[<id>]$treatment$description`.
  expect_identical(spec$enrollments[[1]]$treatment$description, "Treatment new")

  # Site 5: `enrollments[<id>]$treatment$arms$<k>`, for both arms.
  expect_identical(
    spec$enrollments[[1]]$treatment$arms$intervention,
    "Initiators new"
  )
  expect_identical(
    spec$enrollments[[1]]$treatment$arms$comparator,
    "Comparators new"
  )

  # The structural implementation under the same enrollment does not move.
  expect_identical(
    spec$enrollments[[1]]$treatment$implementation,
    f$old$enrollments[[1]]$treatment$implementation
  )
})

test_that("a cosmetic-only reload refuses nothing and warns about nothing", {
  # The passing direction. A gate proved red on one input only is never run on
  # the input it exists to accept.
  path_old <- rsi_write(rsi_spec("old", "osd_f20_to_f29"))
  path_new <- rsi_write(rsi_spec("new", "osd_f20_to_f29"))
  plan <- rsi_plan(path_old)

  expect_warning(plan$reload_spec(spec_path = path_new, quiet = TRUE), NA)
  expect_null(plan$spec_reload_skipped_diffs)
  expect_identical(plan$spec$study$title, "Structural isolation fixture new")
  expect_identical(
    plan$spec$inclusion_criteria,
    swereg::tteplan_read_spec(path_old)$inclusion_criteria
  )
})

# ---------------------------------------------------------------------------
# The post-condition
# ---------------------------------------------------------------------------

test_that("the post-condition stops a reload that copies a structural field", {
  # `.copy_cosmetic_spec_fields()` returns the whole new spec, which is what
  # the wholesale assignment used to do. The check must catch it and name the
  # leaked path.
  path_old <- rsi_write(rsi_spec("old", "osd_f20_to_f29"))
  path_new <- rsi_write(rsi_spec("new", "osd_f30_to_f39"))
  plan <- rsi_plan(path_old)

  testthat::local_mocked_bindings(
    .copy_cosmetic_spec_fields = function(old, new) new,
    .package = "swereg"
  )
  expect_error(
    suppressWarnings(plan$reload_spec(spec_path = path_new, quiet = TRUE)),
    "inclusion_criteria"
  )
})

test_that("a refused reload leaves plan$spec exactly as it was", {
  # An error is not the invariant. `plan` is an R6 object and carries
  # reference semantics, so `stop()` rolls back no assignment. A check placed
  # after the write raises this same error and still changes the field.
  #
  # This test asserts the STATE that follows the error.
  path_old <- rsi_write(rsi_spec("old", "osd_f20_to_f29"))
  path_new <- rsi_write(rsi_spec("new", "osd_f30_to_f39"))
  plan <- rsi_plan(path_old)
  before <- plan$spec

  testthat::local_mocked_bindings(
    .copy_cosmetic_spec_fields = function(old, new) new,
    .package = "swereg"
  )
  expect_error(
    suppressWarnings(plan$reload_spec(spec_path = path_new, quiet = TRUE)),
    "inclusion_criteria"
  )

  expect_identical(plan$spec, before)
  expect_identical(
    plan$spec$inclusion_criteria$criteria[[1]]$implementation$source_variable,
    "osd_f20_to_f29"
  )
  # The cosmetic half of the same reload does not land either. A refused
  # reload writes nothing at all.
  expect_identical(plan$spec$study$title, before$study$title)
})

# ---------------------------------------------------------------------------
# Every declared spec path is classified
# ---------------------------------------------------------------------------
#
# The post-condition in `.apply_cosmetic_spec_updates()` only guards the paths
# `.diff_specs()` looks at. A path it never opens is invisible to both sides:
# it neither warns nor copies, and a display field then freezes on reload.
#
# `.TTE_SPEC_SCHEMA` in `R/tteplan_spec_schema.R` is the one list of legal
# paths, so these tests drive it. A new schema key fails the first test until
# somebody classifies it.
#
# Legacy paths are excluded. `tteplan_read_spec()` rejects a spec that carries
# one, so no plan can hold one.

# A spec carrying a value at every consumed and metadata path the schema
# declares. It is a fixture for the classifier, which reads plain lists.
#
# `observed_var` accepts `column` or `sentinel` and refuses both together, so
# the argument selects which one this fixture carries.
rsi_full_spec <- function(observed = "sentinel") {
  obs <- if (identical(observed, "column")) {
    list(column = "rd_observed")
  } else {
    list(sentinel = "row_presence")
  }
  return(list(
    study = list(
      title = "Full fixture",
      description = "Study description",
      design = "Nested target trials",
      principal_investigator = "A Person",
      implementation = list(
        conf_level = 0.95,
        date = "2026-01-01",
        project_prefix = "rsi",
        status = "draft",
        version = "v001"
      )
    ),
    inclusion_criteria = list(
      isoyears = c(2015L, 2016L),
      criteria = list(list(
        name = "Prior psychotic disorder",
        rationale = "It restricts the study population.",
        type = "has_event",
        implementation = list(
          computed = TRUE,
          source_variable = "osd_f20_to_f29",
          window = "lifetime_before_baseline"
        )
      ))
    ),
    exclusion_criteria = list(list(
      name = "No prior intervention",
      rationale = "The design admits new users only.",
      implementation = list(
        computed = TRUE,
        intervention_value = "treated",
        source_variable = "rd_exposure",
        type = "no_prior_intervention",
        window = "lifetime_before_baseline"
      )
    )),
    confounders = list(list(
      name = "Age",
      categories = "Demographic",
      codes = "n/a",
      rationale = "It confounds the exposure and the outcome.",
      implementation = list(
        computed = FALSE,
        source_variable = "rd_age_source",
        variable = "rd_age_continuous",
        window = "lifetime_before_baseline"
      )
    )),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    outcomes = list(list(
      name = "Event A",
      description = "The first event of type A",
      role = "primary",
      implementation = list(variable = "osd_a")
    )),
    subgroups = list(list(
      name = "Sex",
      implementation = list(variable = "rd_sex")
    )),
    enrollments = list(list(
      id = "01",
      name = "Enrollment 01",
      observed_var = obs,
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      additional_inclusion = list(list(
        name = "Age 50-60",
        rationale = "The trial recruits this band.",
        type = "age_range",
        min = 50,
        max = 60,
        implementation = list(
          computed = FALSE,
          source_variable = "rd_age_source",
          variable = "rd_age_continuous",
          window = "lifetime_before_baseline"
        )
      )),
      additional_exclusion = list(list(
        name = "No prior intervention",
        rationale = "The design admits new users only.",
        implementation = list(
          computed = TRUE,
          intervention_value = "treated",
          source_variable = "rd_exposure",
          type = "no_prior_intervention",
          window = "lifetime_before_baseline"
        )
      )),
      treatment = list(
        description = "Treatment",
        arms = list(intervention = "Initiators", comparator = "Comparators"),
        implementation = list(
          comparator_to_intervention_ratio = 2L,
          comparator_value = "control",
          intervention_value = "treated",
          seed = 42L,
          variable = "rd_exposure"
        )
      )
    )),
    standing_methods = list(
      calendar_time = list(handling = "Spline", note = "One per trial"),
      admin_censoring = list(handling = "At the last week", note = "Global"),
      comparator_to_intervention_ratio_default = list(
        handling = "2 to 1",
        note = "Per trial"
      ),
      matching_ratio_default = list(handling = "2 to 1", note = "Superseded")
    ),
    target_trial = list(
      analysis_plan = list(specification = "Pooled logistic regression"),
      assignment_procedure = list(
        specification = "Incidence density sampling"
      ),
      causal_contrast = list(specification = "Per-protocol"),
      eligibility_criteria = list(specification = "Age 50 to 60"),
      follow_up_period = list(specification = "52 weeks"),
      outcome = list(specification = "The first event of type A"),
      treatment_strategies = list(specification = "Initiate, or do not")
    ),
    open_questions = list(list(
      question = "Which comparator window?",
      raised_by = "A Person",
      resolution = "Open",
      status = "open"
    ))
  ))
}

# Every consumed and metadata path the schema declares.
rsi_schema_paths <- function() {
  return(sort(union(
    swereg:::.tte_spec_paths("consumed"),
    swereg:::.tte_spec_paths("metadata")
  )))
}

# A path is a container when the schema opens it as a mapping context. Every
# other path holds a scalar, and this file calls that a leaf.
rsi_is_container <- function(path) {
  sch <- swereg:::.TTE_SPEC_SCHEMA
  return(!is.null(sch[[path]]) || !is.null(sch[[paste0(path, "[]")]]))
}

rsi_segments <- function(path) {
  return(strsplit(sub("^\\$/", "", path), "/", fixed = TRUE)[[1]])
}

# Read one path out of a spec. A `[]` segment reads the first entry.
rsi_get <- function(node, segs) {
  k <- sub("\\[\\]$", "", segs[1])
  child <- if (grepl("\\[\\]$", segs[1])) node[[k]][[1]] else node[[k]]
  if (length(segs) == 1L) {
    return(child)
  }
  return(rsi_get(child, segs[-1]))
}

# Write one path into a spec. A `[]` segment writes the first entry. Writing
# NULL deletes the key, which is how a container is perturbed.
rsi_set <- function(node, segs, value) {
  k <- sub("\\[\\]$", "", segs[1])
  if (length(segs) == 1L) {
    node[[k]] <- value
    return(node)
  }
  if (grepl("\\[\\]$", segs[1])) {
    node[[k]][[1]] <- rsi_set(node[[k]][[1]], segs[-1], value)
  } else {
    node[[k]] <- rsi_set(node[[k]], segs[-1], value)
  }
  return(node)
}

rsi_perturb <- function(x) {
  if (is.character(x)) {
    return(paste0(x, "_changed"))
  }
  if (is.logical(x)) {
    return(!x)
  }
  if (is.numeric(x)) {
    return(x + 1)
  }
  stop("no perturbation rule for class ", class(x)[1], call. = FALSE)
}

# The fixture that carries a value at `path`. Only `observed_var$column`
# needs the second variant.
rsi_base_for <- function(path) {
  if (identical(path, "$/enrollments[]/observed_var/column")) {
    return(rsi_full_spec("column"))
  }
  return(rsi_full_spec())
}

# The declared side of every leaf path, hand-written so each decision is
# deliberate. `structural` means a definition the cached results were computed
# against. `cosmetic` means a label.
RSI_LEAF_SIDE <- c(
  # Confounders. The variable set enters the model and `implementation` states
  # how the column is built. Everything else reaches the spec summary sheet.
  "$/confounders[]/categories" = "cosmetic",
  "$/confounders[]/codes" = "cosmetic",
  "$/confounders[]/name" = "cosmetic",
  "$/confounders[]/rationale" = "cosmetic",
  "$/confounders[]/implementation/computed" = "structural",
  "$/confounders[]/implementation/source_variable" = "structural",
  "$/confounders[]/implementation/variable" = "structural",
  "$/confounders[]/implementation/window" = "structural",

  # Enrollments. The two additional criteria containers, the treatment
  # implementation, the observation encoding and both tolerances each decide
  # who is enrolled and when swereg censors them.
  "$/enrollments[]/name" = "cosmetic",
  "$/enrollments[]/treatment/description" = "cosmetic",
  "$/enrollments[]/treatment/arms/comparator" = "cosmetic",
  "$/enrollments[]/treatment/arms/intervention" = "cosmetic",
  "$/enrollments[]/additional_exclusion[]/implementation/computed" = "structural",
  "$/enrollments[]/additional_exclusion[]/implementation/intervention_value" = "structural",
  "$/enrollments[]/additional_exclusion[]/implementation/source_variable" = "structural",
  "$/enrollments[]/additional_exclusion[]/implementation/type" = "structural",
  "$/enrollments[]/additional_exclusion[]/implementation/window" = "structural",
  "$/enrollments[]/additional_exclusion[]/name" = "structural",
  "$/enrollments[]/additional_exclusion[]/rationale" = "structural",
  "$/enrollments[]/additional_inclusion[]/implementation/computed" = "structural",
  "$/enrollments[]/additional_inclusion[]/implementation/source_variable" = "structural",
  "$/enrollments[]/additional_inclusion[]/implementation/variable" = "structural",
  "$/enrollments[]/additional_inclusion[]/implementation/window" = "structural",
  "$/enrollments[]/additional_inclusion[]/max" = "structural",
  "$/enrollments[]/additional_inclusion[]/min" = "structural",
  "$/enrollments[]/additional_inclusion[]/name" = "structural",
  "$/enrollments[]/additional_inclusion[]/rationale" = "structural",
  "$/enrollments[]/additional_inclusion[]/type" = "structural",
  "$/enrollments[]/comparator_tolerance_weeks" = "structural",
  "$/enrollments[]/id" = "structural",
  "$/enrollments[]/intervention_tolerance_weeks" = "structural",
  "$/enrollments[]/observed_var/column" = "structural",
  "$/enrollments[]/observed_var/sentinel" = "structural",
  "$/enrollments[]/treatment/implementation/comparator_to_intervention_ratio" = "structural",
  "$/enrollments[]/treatment/implementation/comparator_value" = "structural",
  "$/enrollments[]/treatment/implementation/intervention_value" = "structural",
  "$/enrollments[]/treatment/implementation/seed" = "structural",
  "$/enrollments[]/treatment/implementation/variable" = "structural",

  # Exclusion criteria. The whole container decides who leaves the study, and
  # the cached attrition table counts what the old container removed.
  "$/exclusion_criteria[]/implementation/computed" = "structural",
  "$/exclusion_criteria[]/implementation/intervention_value" = "structural",
  "$/exclusion_criteria[]/implementation/source_variable" = "structural",
  "$/exclusion_criteria[]/implementation/type" = "structural",
  "$/exclusion_criteria[]/implementation/window" = "structural",
  "$/exclusion_criteria[]/name" = "structural",
  "$/exclusion_criteria[]/rationale" = "structural",

  # Follow-up. Each entry names one follow-up window of the ETT grid.
  "$/follow_up[]/label" = "structural",
  "$/follow_up[]/weeks" = "structural",

  # Global inclusion. The container decides who enters the study.
  "$/inclusion_criteria/criteria[]/implementation/computed" = "structural",
  "$/inclusion_criteria/criteria[]/implementation/source_variable" = "structural",
  "$/inclusion_criteria/criteria[]/implementation/window" = "structural",
  "$/inclusion_criteria/criteria[]/name" = "structural",
  "$/inclusion_criteria/criteria[]/rationale" = "structural",
  "$/inclusion_criteria/criteria[]/type" = "structural",
  "$/inclusion_criteria/isoyears" = "structural",

  # Open questions. A note. Nothing in R/ acts on it.
  "$/open_questions[]/question" = "cosmetic",
  "$/open_questions[]/raised_by" = "cosmetic",
  "$/open_questions[]/resolution" = "cosmetic",
  "$/open_questions[]/status" = "cosmetic",

  # Outcomes. `plan$ett` carries a copy of all three labels, and
  # `R/tteplan_export.R` reads `role` for the forest row label.
  "$/outcomes[]/description" = "cosmetic",
  "$/outcomes[]/name" = "cosmetic",
  "$/outcomes[]/role" = "cosmetic",
  "$/outcomes[]/implementation/variable" = "structural",

  # Standing methods. Prose for the spec summary sheet.
  "$/standing_methods/admin_censoring/handling" = "cosmetic",
  "$/standing_methods/admin_censoring/note" = "cosmetic",
  "$/standing_methods/calendar_time/handling" = "cosmetic",
  "$/standing_methods/matching_ratio_default/handling" = "cosmetic",
  "$/standing_methods/matching_ratio_default/note" = "cosmetic",
  "$/standing_methods/calendar_time/note" = "cosmetic",
  "$/standing_methods/comparator_to_intervention_ratio_default/handling" = "cosmetic",
  "$/standing_methods/comparator_to_intervention_ratio_default/note" = "cosmetic",

  # Study. `conf_level` is the level s3 computed every interval at.
  # `project_prefix` names every cached file in `plan$ett`. `version` selects
  # `plan$dir_results`. The rest is a label or metadata.
  "$/study/description" = "cosmetic",
  "$/study/design" = "cosmetic",
  "$/study/principal_investigator" = "cosmetic",
  "$/study/title" = "cosmetic",
  "$/study/implementation/date" = "cosmetic",
  "$/study/implementation/status" = "cosmetic",
  "$/study/implementation/conf_level" = "structural",
  "$/study/implementation/project_prefix" = "structural",
  "$/study/implementation/version" = "structural",

  # Subgroups. The variable enters the effect-modification analysis.
  "$/subgroups[]/name" = "cosmetic",
  "$/subgroups[]/implementation/variable" = "structural",

  # Target trial. The study team's own prose, rendered by
  # `R/protocol_table.R`.
  "$/target_trial/analysis_plan/specification" = "cosmetic",
  "$/target_trial/assignment_procedure/specification" = "cosmetic",
  "$/target_trial/causal_contrast/specification" = "cosmetic",
  "$/target_trial/eligibility_criteria/specification" = "cosmetic",
  "$/target_trial/follow_up_period/specification" = "cosmetic",
  "$/target_trial/outcome/specification" = "cosmetic",
  "$/target_trial/treatment_strategies/specification" = "cosmetic"
)

test_that("the fixture carries every path the schema declares", {
  spec <- rsi_full_spec()
  expect_true(swereg:::.tte_spec_check_keys(spec, "<rsi fixture>"))
  present <- unique(swereg:::.tte_spec_walk_keys(spec))
  # `observed_var$column` is the one path the sentinel fixture cannot carry.
  wanted <- setdiff(rsi_schema_paths(), "$/enrollments[]/observed_var/column")
  expect_identical(setdiff(wanted, present), character(0))
  expect_true(
    "$/enrollments[]/observed_var/column" %in%
      unique(swereg:::.tte_spec_walk_keys(rsi_full_spec("column")))
  )
})

test_that("the classification table names exactly the schema's leaf paths", {
  # This is the anti-rot gate. Add a key to `.TTE_SPEC_SCHEMA` and this test
  # fails until somebody decides whether it is a label or a definition.
  paths <- rsi_schema_paths()
  leaf <- paths[!vapply(paths, rsi_is_container, logical(1))]
  # Floors, so a collapsed enumeration cannot pass this test by comparing two
  # empty vectors. They are floors and not exact counts, because the schema
  # MAY grow and the setdiff below is what governs growth.
  expect_gt(length(paths), 100L)
  expect_gt(length(leaf), 50L)
  expect_gt(length(RSI_LEAF_SIDE), 50L)
  expect_identical(
    paste(setdiff(leaf, names(RSI_LEAF_SIDE)), collapse = " | "),
    ""
  )
  expect_identical(
    paste(setdiff(names(RSI_LEAF_SIDE), leaf), collapse = " | "),
    ""
  )
})

test_that("every leaf path lands on its declared side, and only that side", {
  expect_gt(length(RSI_LEAF_SIDE), 50L)
  wrong <- character(0)
  for (p in names(RSI_LEAF_SIDE)) {
    segs <- rsi_segments(p)
    old <- rsi_base_for(p)
    new <- rsi_set(old, segs, rsi_perturb(rsi_get(old, segs)))
    d <- swereg:::.diff_specs(old, new)
    want <- RSI_LEAF_SIDE[[p]]
    other <- if (identical(want, "cosmetic")) "structural" else "cosmetic"
    if (length(d[[want]]) == 0L || length(d[[other]]) > 0L) {
      wrong <- c(
        wrong,
        sprintf(
          "%s declared %s, got cosmetic=%d structural=%d",
          p,
          want,
          length(d$cosmetic),
          length(d$structural)
        )
      )
    }
  }
  # Collapsed to one string, so a failure names the offending path. A bare
  # character vector reports only that the lengths differ.
  expect_identical(paste(wrong, collapse = " | "), "")
})

test_that("every container path reports at least one change", {
  # A container is perturbed by deletion. Its children split across the two
  # sides, so this test asserts only that the change is reported.
  paths <- rsi_schema_paths()
  containers <- paths[vapply(paths, rsi_is_container, logical(1))]
  expect_gt(length(containers), 10L)
  silent <- character(0)
  for (p in containers) {
    segs <- rsi_segments(p)
    old <- rsi_base_for(p)
    new <- rsi_set(old, segs, NULL)
    d <- swereg:::.diff_specs(old, new)
    if (length(d$cosmetic) + length(d$structural) == 0L) {
      silent <- c(silent, p)
    }
  }
  expect_identical(paste(silent, collapse = " | "), "")
})

test_that("no declared path produces an unclassified message", {
  # `.ds_residual()` marks a key that no branch of `.diff_specs()` handles.
  # A declared path that reaches it is a classification gap.
  paths <- rsi_schema_paths()
  expect_gt(length(paths), 100L)
  unclassified <- character(0)
  for (p in paths) {
    segs <- rsi_segments(p)
    old <- rsi_base_for(p)
    new <- if (rsi_is_container(p)) {
      rsi_set(old, segs, NULL)
    } else {
      rsi_set(old, segs, rsi_perturb(rsi_get(old, segs)))
    }
    d <- swereg:::.diff_specs(old, new)
    hits <- grep("(unclassified)", c(d$cosmetic, d$structural), fixed = TRUE)
    if (length(hits) > 0L) {
      unclassified <- c(unclassified, p)
    }
  }
  expect_identical(paste(unclassified, collapse = " | "), "")
})

test_that(".ds_residual reports a key the schema does not declare", {
  # The passing direction above says nothing about whether the marker can
  # fire at all. This is the input it exists to reject.
  old <- rsi_full_spec()
  new <- old
  new$a_key_swereg_has_never_seen <- "value"
  d <- swereg:::.diff_specs(old, new)
  expect_length(d$cosmetic, 0L)
  expect_true(any(grepl(
    "a_key_swereg_has_never_seen (unclassified)",
    d$structural,
    fixed = TRUE
  )))
})

# ---------------------------------------------------------------------------
# The reload applies the extended cosmetic set
# ---------------------------------------------------------------------------

# The old spec, the new spec, and a plan reloaded from the new one. Every
# display field differs, and `study$implementation$conf_level` differs.
rsi_full_reloaded <- function() {
  spec_old <- rsi_full_spec()
  spec_new <- rsi_full_spec()
  spec_new$study$design <- "Design new"
  spec_new$study$implementation$conf_level <- 0.9
  spec_new$study$implementation$date <- "2026-02-02"
  spec_new$study$implementation$status <- "final"
  spec_new$outcomes[[1]]$description <- "Description new"
  spec_new$outcomes[[1]]$role <- "secondary"
  spec_new$confounders[[1]]$name <- "Age new"
  spec_new$confounders[[1]]$categories <- "Categories new"
  spec_new$confounders[[1]]$codes <- "Codes new"
  spec_new$confounders[[1]]$rationale <- "Rationale new"
  spec_new$subgroups[[1]]$name <- "Sex new"
  spec_new$standing_methods$calendar_time$handling <- "Handling new"
  spec_new$target_trial$outcome$specification <- "Outcome new"
  spec_new$open_questions[[1]]$status <- "closed"

  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    age_group = "50_60",
    age_min = 50,
    age_max = 60,
    follow_up = 52L,
    outcome_var = "osd_a",
    outcome_name = "Event A",
    outcome_description = "The first event of type A",
    outcome_role = "primary",
    description = "ETT00001",
    confounder_vars = list("rd_age_continuous"),
    person_id_var = "id",
    treatment_var = "baseline_intervention"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "rsi",
    skeleton_files = "rsi_skeleton.qs2",
    global_max_isoyearweek = "2016-52",
    ett = ett
  )
  plan$spec <- spec_old
  suppressWarnings(swereg:::.apply_cosmetic_spec_updates(plan, spec_new))
  return(list(plan = plan, old = spec_old, new = spec_new))
}

test_that("a reload refreshes every display-only block", {
  f <- rsi_full_reloaded()
  s <- f$plan$spec
  expect_identical(s$study$design, "Design new")
  expect_identical(s$study$implementation$date, "2026-02-02")
  expect_identical(s$study$implementation$status, "final")
  expect_identical(s$outcomes[[1]]$description, "Description new")
  expect_identical(s$outcomes[[1]]$role, "secondary")
  expect_identical(s$confounders[[1]]$name, "Age new")
  expect_identical(s$confounders[[1]]$categories, "Categories new")
  expect_identical(s$confounders[[1]]$codes, "Codes new")
  expect_identical(s$confounders[[1]]$rationale, "Rationale new")
  expect_identical(s$subgroups[[1]]$name, "Sex new")
  expect_identical(s$standing_methods$calendar_time$handling, "Handling new")
  expect_identical(s$target_trial$outcome$specification, "Outcome new")
  expect_identical(s$open_questions[[1]]$status, "closed")
})

test_that("a reload refuses conf_level and keeps the level s3 used", {
  f <- rsi_full_reloaded()
  expect_identical(f$plan$spec$study$implementation$conf_level, 0.95)
  expect_identical(f$plan$spec$study$implementation$project_prefix, "rsi")
  expect_identical(f$plan$spec$study$implementation$version, "v001")
  d <- swereg:::.diff_specs(f$old, f$new)
  expect_true(any(grepl(
    "study$implementation$conf_level",
    d$structural,
    fixed = TRUE
  )))
})

test_that("plan$ett and plan$spec agree on the outcome labels after a reload", {
  # One object holding two answers is the defect. `plan$ett` refreshes from
  # the new spec, so `plan$spec` must carry the same three labels.
  f <- rsi_full_reloaded()
  o <- f$plan$spec$outcomes[[1]]
  expect_identical(f$plan$ett$outcome_name, o$name)
  expect_identical(f$plan$ett$outcome_description, o$description)
  expect_identical(f$plan$ett$outcome_role, o$role)
  expect_identical(f$plan$ett$outcome_description, "Description new")
  expect_identical(f$plan$ett$outcome_role, "secondary")
})
