# `only_prior_value` keeps a person-week only when every prior week in the
# window that holds an observation carries `value`.
#
# The rule exists for one case the older washout misses. A woman takes tibolone,
# then switches to systemic MHT. `no_prior_value: systemic_mht` tests one value,
# and her tibolone weeks are not that value, so she passes the washout and
# enrols as an initiator of systemic MHT. She is not MHT-naive.
#
# All four rule blocks accept both washout types: the global
# `inclusion_criteria$criteria` and `exclusion_criteria`, and an enrollment's
# `additional_inclusion` and `additional_exclusion`.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")
skip_if_not_installed("qs2")

# --- fixtures --------------------------------------------------------------

# One washout entry, in the shape every rule block accepts.
.opv_entry <- function(
  type,
  value,
  source_variable = "rd_approach1_single",
  window = "lifetime_before_baseline",
  name = "MHT-naive at baseline"
) {
  return(list(
    name = name,
    rationale = "The trial recruits MHT-naive women.",
    implementation = list(
      computed = TRUE,
      source_variable = source_variable,
      type = type,
      value = value,
      window = window
    )
  ))
}

# A minimal readable spec, with `entry` placed in one of the four rule blocks.
# `block` is NULL to leave every block empty.
.opv_spec_list <- function(block = NULL, entry = NULL) {
  spec <- list(
    study = list(
      title = "only_prior_value",
      implementation = list(project_prefix = "opv", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2015L, 2015L)),
    enrollments = list(list(
      id = "01",
      name = "Systemic MHT vs local or none",
      observed_var = list(sentinel = "row_presence"),
      intervention_tolerance_weeks = 0L,
      comparator_tolerance_weeks = 0L,
      additional_inclusion = list(list(
        name = "Age 40-80",
        type = "age_range",
        min = 40,
        max = 80,
        implementation = list(variable = "rd_age_continuous")
      )),
      treatment = list(
        description = "Initiation of systemic MHT.",
        arms = list(intervention = "Systemic", comparator = "Local or none"),
        implementation = list(
          comparator_to_intervention_ratio = 2L,
          variable = "rd_approach1_single",
          intervention_value = "systemic_mht",
          comparator_value = "local_or_none_mht",
          seed = 1L
        )
      )
    )),
    outcomes = list(list(
      name = "Outcome A",
      implementation = list(variable = "osd_a")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L))
  )
  if (identical(block, "global_inclusion")) {
    spec$inclusion_criteria$criteria <- list(entry)
  }
  if (identical(block, "global_exclusion")) {
    spec$exclusion_criteria <- list(entry)
  }
  if (identical(block, "additional_inclusion")) {
    spec$enrollments[[1]]$additional_inclusion <- c(
      spec$enrollments[[1]]$additional_inclusion,
      list(entry)
    )
  }
  if (identical(block, "additional_exclusion")) {
    spec$enrollments[[1]]$additional_exclusion <- list(entry)
  }
  return(spec)
}

# Write a spec and read it back through the production reader.
.opv_write <- function(spec, dir = tempfile("opv_spec_")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(path)
}

.opv_read <- function(block = NULL, entry = NULL) {
  path <- .opv_write(.opv_spec_list(block, entry))
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  return(suppressMessages(swereg::tteplan_read_spec(path)))
}

# One person, one week per element of `values`.
.opv_skeleton <- function(values, id = 1L) {
  return(data.table::data.table(
    id = as.integer(id),
    isoyear = 2015L,
    isoyearweek = sprintf("2015-%02d", seq_along(values)),
    is_isoyear = FALSE,
    rd_approach1_single = values,
    rd_age_continuous = 55,
    osd_a = FALSE
  ))
}

# The tibolone switcher. Weeks 1-10 tibolone, weeks 11-20 systemic MHT.
.opv_switch_values <- c(rep("tibolone", 10L), rep("systemic_mht", 10L))

.opv_apply <- function(spec, skel) {
  return(swereg::tteplan_apply_exclusions(
    data.table::copy(skel),
    spec,
    list(enrollment_id = "01")
  ))
}

# --- 1. the switcher fails the new rule and passes the old one -------------

test_that("only_prior_value excludes a tibolone switcher at her first systemic week", {
  spec <- .opv_read(
    "global_exclusion",
    .opv_entry("only_prior_value", "local_or_none_mht")
  )
  r <- .opv_apply(spec, .opv_skeleton(.opv_switch_values))
  col <- "eligible_only_rd_approach1_single_everbefore"
  expect_true(col %in% names(r))
  expect_false(r[[col]][11L])
  # Week 1 has no prior week at all, so the rule keeps it.
  expect_true(r[[col]][1L])
})

test_that("no_prior_value keeps the same tibolone switcher at her first systemic week", {
  # This is the defect `only_prior_value` exists to repair. The old rule tests
  # one value, and her prior weeks hold a different one.
  spec <- .opv_read(
    "global_exclusion",
    .opv_entry("no_prior_value", "systemic_mht")
  )
  r <- .opv_apply(spec, .opv_skeleton(.opv_switch_values))
  col <- "eligible_no_rd_approach1_single_everbefore"
  expect_true(col %in% names(r))
  expect_true(r[[col]][11L])
})

# --- 2. a prior week with no observation violates nothing ------------------

test_that("only_prior_value treats a missing prior week as no violation", {
  values <- c(
    rep(NA_character_, 5L),
    rep("local_or_none_mht", 5L),
    rep("systemic_mht", 10L)
  )
  spec <- .opv_read(
    "global_exclusion",
    .opv_entry("only_prior_value", "local_or_none_mht")
  )
  r <- .opv_apply(spec, .opv_skeleton(values))
  col <- "eligible_only_rd_approach1_single_everbefore"
  expect_true(r[[col]][11L])
  # The rule still fires once a prior week holds another value.
  expect_false(r[[col]][12L])
})

# --- 3. the same rule in each of the four blocks ---------------------------

test_that("both washout types give the same column in all four rule blocks", {
  blocks <- c(
    "global_inclusion",
    "global_exclusion",
    "additional_inclusion",
    "additional_exclusion"
  )
  cases <- list(
    only_prior_value = list(
      value = "local_or_none_mht",
      col = "eligible_only_rd_approach1_single_everbefore"
    ),
    no_prior_value = list(
      value = "systemic_mht",
      col = "eligible_no_rd_approach1_single_everbefore"
    )
  )
  skel <- .opv_skeleton(.opv_switch_values)
  for (type in names(cases)) {
    case <- cases[[type]]
    got <- lapply(blocks, function(block) {
      spec <- .opv_read(block, .opv_entry(type, case$value))
      r <- .opv_apply(spec, skel)
      expect_true(case$col %in% names(r), info = paste(type, block))
      return(r[[case$col]])
    })
    for (j in seq_along(blocks)) {
      expect_identical(
        got[[j]],
        got[[1L]],
        info = paste(type, blocks[j], "differs from", blocks[1L])
      )
    }
  }
})

# --- 4. the prevalent-user check reads every rule block --------------------

# Two persons, eight weeks each. Person 1 is a clean initiator: four weeks on
# local or no MHT, then four weeks on systemic MHT. Her prevalent weeks are
# weeks 6, 7 and 8, and the rule makes all three ineligible. Person 2 supplies
# the comparator level the validator checks for.
.opv_naive_skeleton <- function() {
  return(rbind(
    .opv_skeleton(c(
      rep("local_or_none_mht", 4L),
      rep("systemic_mht", 4L)
    ), id = 1L),
    .opv_skeleton(rep("local_or_none_mht", 8L), id = 2L)
  ))
}

# The guard MUST find the enrollment's only washout wherever the specification
# puts it. One test per rule block, so a failure names the block that lost it.
for (.opv_blk in c(
  "global_inclusion",
  "global_exclusion",
  "additional_inclusion",
  "additional_exclusion"
)) {
  local({
    block <- .opv_blk
    test_that(paste0("the prevalent-user check reads ", block), {
      withr::local_options(swereg.warn_prevalent_user = TRUE)
      spec <- .opv_read(
        block,
        .opv_entry("only_prior_value", "local_or_none_mht")
      )
      expect_no_warning(
        suppressMessages(
          swereg::tteplan_validate_spec(spec, .opv_naive_skeleton())
        )
      )
    })
  })
}

# --- 5. the reader refuses the retired type name ---------------------------

test_that("the reader refuses no_prior_intervention and names the new types", {
  path <- .opv_write(.opv_spec_list(
    "global_exclusion",
    .opv_entry("no_prior_intervention", "systemic_mht")
  ))
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  expect_error(
    suppressMessages(swereg::tteplan_read_spec(path)),
    "'no_prior_value' and 'only_prior_value'",
    fixed = TRUE
  )
})

# --- 6. the generated prose names the rule ---------------------------------

# A plan-shaped list. Every renderer under test reads `$spec`, and the TARGET
# checklist also calls `$get_attrition()`.
.opv_plan <- function(spec) {
  return(list(
    spec = spec,
    code_registry = NULL,
    period_width = 4L,
    get_attrition = function() {
      return(data.table::data.table(enrollment_id = character()))
    }
  ))
}

.opv_rule_text <- paste0(
  "No prior rd_approach1_single other than local_or_none_mht"
)

test_that("the TARGET checklist names the rule in plain language", {
  spec <- .opv_read(
    "global_exclusion",
    .opv_entry("only_prior_value", "local_or_none_mht")
  )
  out <- capture.output(
    swereg:::.plan_print_target_checklist(.opv_plan(spec))
  )
  out <- gsub("\033\\[[0-9;]*m", "", out)
  hit <- out[grepl(.opv_rule_text, out, fixed = TRUE)]
  expect_length(hit, 1L)
  expect_true(grepl("MHT-naive at baseline", hit, fixed = TRUE))
})

test_that("the protocol table emulation cell names the rule", {
  spec <- .opv_read(
    "global_exclusion",
    .opv_entry("only_prior_value", "local_or_none_mht")
  )
  cell <- swereg:::.protocol_emulation(
    spec,
    "eligibility_criteria",
    list(enrollment = spec$enrollments[[1]], enrollment_id = "01")
  )
  lines <- strsplit(cell, "\n", fixed = TRUE)[[1]]
  expect_true(
    paste0(.opv_rule_text, " (lifetime before baseline)") %in% lines
  )
})

test_that("the CONSORT lookup labels the eligibility column with the rule", {
  for (block in c("global_exclusion", "additional_inclusion")) {
    spec <- .opv_read(
      block,
      .opv_entry("only_prior_value", "local_or_none_mht")
    )
    labels <- swereg:::.build_criterion_label_lookup(
      .opv_plan(spec),
      enrollment_id = "01",
      observed_criteria = "eligible_only_rd_approach1_single_everbefore"
    )
    expect_identical(
      unname(labels["eligible_only_rd_approach1_single_everbefore"]),
      paste0(.opv_rule_text, "\\n(lifetime before baseline)"),
      info = block
    )
  }
})

# --- 7. the s1a projection keeps the source column -------------------------

# The plan `.s1a_worker_multi()` reads, built from the same specification and
# skeleton it would meet in production. The washout sits in
# `additional_inclusion`, so it reaches the projection through the enrollment.
#
# It names `rd_approach1_wide` and not the treatment column. The projection
# already keeps the treatment column for its own reason, so a washout on that
# column could not show that the projection reads the rule at all.
.opv_s1a_plan <- function(env) {
  root <- tempfile("opv_plan_")
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  withr::defer(unlink(root, recursive = TRUE, force = TRUE), envir = env)

  sk <- rbind(
    .opv_skeleton(.opv_switch_values, id = 1L),
    .opv_skeleton(rep("local_or_none_mht", 20L), id = 2L)
  )
  sk[, rd_approach1_wide := rd_approach1_single]
  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)

  .opv_write(
    .opv_spec_list(
      "additional_inclusion",
      .opv_entry(
        "only_prior_value",
        "local_or_none_mht",
        source_variable = "rd_approach1_wide"
      )
    ),
    dir_spec
  )
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = max(sk$isoyearweek)
  ))
  return(list(plan = plan, skel_path = skel_path))
}

# The first of the two production steps `.s1a_worker_multi()` runs: the
# projection to the union of columns any enrollment needs.
.opv_s1a_projection <- function(env) {
  built <- .opv_s1a_plan(env)
  canonical <- swereg:::.s1_load_skeleton(built$skel_path, 1L)
  needed <- swereg:::.tte_canonical_needed_cols(
    built$plan$spec,
    list(built$plan[[1]]),
    names(canonical)
  )
  return(list(plan = built$plan, canonical = canonical, needed = needed))
}

test_that("the s1a projection keeps the rule's source column", {
  p <- .opv_s1a_projection(environment())
  expect_true("rd_approach1_wide" %in% p$needed)
})

test_that("the s1a projection builds the rule's eligibility column", {
  # The second production step, over the projection the first one produced.
  p <- .opv_s1a_projection(environment())
  drop_cols <- setdiff(names(p$canonical), p$needed)
  if (length(drop_cols) > 0L) {
    p$canonical[, (drop_cols) := NULL]
  }
  prepared <- swereg:::.s1_prepare_loaded(
    p$canonical,
    p$plan[[1]],
    p$plan$spec,
    derive_confounders = FALSE
  )
  col <- "eligible_only_rd_approach1_wide_everbefore"
  expect_true(col %in% names(prepared))
  expect_true(col %in% attr(prepared, "eligible_cols"))
  expect_false(prepared[[col]][11L])
})
