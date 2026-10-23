# Fixtures for the rare_condition_rule tests (test-s1b-sparse-rule.R).
#
# The skeleton is the truth-matrix DGP of helper-tteplan_truth.R, with three
# event columns added. The study years start one ISO year after the skeleton.
#
#   osd_rare_b  4 persons, all never-treated and aged under 55. The event sits
#               in the first skeleton week, so it precedes every recruiting
#               week.
#   osd_rare_a  3 persons: 2 of the rare_b holders and 1 more. Same week.
#   osd_common  every even id aged 55 or more, in the first skeleton week.
#               Also 2 never-treated persons aged under 55, whose event sits
#               in the last week of an enrollment period in mid 2018
#               (`SPR_MID_WEEK`). Their person-trials recruited before that
#               week do not hold the condition, and those recruited after it
#               do. The first recruiting week after the event is the next
#               period's first week, so an off-by-one read of the recruiting
#               week reads the event week itself, which does not count.
#
# Enrollment "01" takes ages 40-80 and drives the decision. Enrollment "02"
# takes ages 40-55. In "02" fewer than 10 persons per arm hold osd_common, so
# "02" alone would exclude it at min_n = 10.

SPR_SKELETON <- "skel_a.qs2"

# The computed confounder variable of each event column.
spr_var <- function(src) {
  return(paste0("rd_no_", src, "_everbefore"))
}

# The last week of the enrollment period that holds 2018-26. Enrollment periods
# are blocks of 4 weeks of the 0-based week index (`.assign_period_ids()`), so
# the next week starts a period.
spr_mid_week <- function() {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek
  idx0 <- match("2018-26", weeks) - 1L
  return(weeks[idx0 - idx0 %% 4L + 3L + 1L])
}

# The hand count of who holds a condition at baseline: TRUE on a tuple when the
# person has an event of `src` in a week before the tuple's recruiting week.
# The recruiting week itself does not count (the window excludes week 0).
spr_holds <- function(sk, tuples, src) {
  ev <- sk[sk[[src]] %in% TRUE, list(ev = min(isoyearweek)), by = "id"]
  recruit <- cstime::dates_by_isoyearweek$isoyearweek[
    tuples$recruit_week_index + 1L
  ]
  first_event <- ev$ev[match(tuples$id, ev$id)]
  return(!is.na(first_event) & first_event < recruit)
}

spr_skeleton <- function(n_persons = 400L, seed = 5252L) {
  sk <- ttm_skeleton(
    scenario = "A",
    n_persons = n_persons,
    date_min = "2017-01-05",
    date_max = "2019-06-30",
    n_init_periods = 32L,
    seed = seed
  )
  first_week <- min(sk$isoyearweek)
  person <- sk[,
    .(
      control = all(rd_tx == "control"),
      age = rd_age_continuous[1L]
    ),
    by = id
  ]
  young_controls <- person[control == TRUE & age < 55, sort(id)]
  rare_b <- young_controls[1:4]
  rare_a <- c(young_controls[1:2], young_controls[5])
  common_early <- person[age >= 55 & id %% 2L == 0L, id]
  common_mid <- young_controls[6:7]
  common <- c(common_early, common_mid)
  sk[, osd_rare_a := id %in% rare_a & isoyearweek == first_week]
  sk[, osd_rare_b := id %in% rare_b & isoyearweek == first_week]
  mid <- spr_mid_week()
  sk[,
    osd_common := (id %in% common_early & isoyearweek == first_week) |
      (id %in% common_mid & isoyearweek == mid)
  ]
  data.table::setattr(
    sk,
    "spr_holders",
    list(rare_a = rare_a, rare_b = rare_b, common = sort(common))
  )
  return(sk[])
}

spr_enrollment <- function(id, age_max, seed) {
  return(list(
    id = id,
    name = paste("Treated vs control", id),
    observed_var = list(sentinel = "row_presence"),
    intervention_tolerance_weeks = 0L,
    comparator_tolerance_weeks = 0L,
    additional_inclusion = list(list(
      name = "Age range",
      type = "age_range",
      min = 40,
      max = age_max,
      implementation = list(variable = "rd_age_continuous")
    )),
    treatment = list(
      arms = list(intervention = "Treated", comparator = "Control"),
      implementation = list(
        comparator_to_intervention_ratio = 2L,
        variable = "rd_tx",
        intervention_value = "treated",
        comparator_value = "control",
        seed = seed
      )
    )
  ))
}

spr_computed <- function(name, src) {
  return(list(
    name = name,
    implementation = list(
      computed = TRUE,
      source_variable = src,
      window = "lifetime_before_baseline"
    )
  ))
}

# The specification as an R list. `rule` is the rare_condition_rule block, or NULL.
spr_spec_list <- function(rule = NULL, project_prefix = "sprtest") {
  spec <- list(
    study = list(
      title = "Sparse rule fixture",
      design = "Sequential target trial emulation",
      principal_investigator = "sparse-rule test",
      implementation = list(project_prefix = project_prefix, version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2018L, 2019L)),
    exclusion_criteria = list(list(
      name = "No prior intervention (new-user)",
      implementation = list(
        type = "no_prior_value",
        source_variable = "rd_tx",
        value = "treated",
        window = "lifetime_before_baseline",
        computed = TRUE
      )
    )),
    confounders = list(
      list(
        name = "Age",
        implementation = list(variable = "rd_age_continuous")
      ),
      spr_computed("Prior rare condition A", "osd_rare_a"),
      spr_computed("Prior common condition", "osd_common"),
      spr_computed("Prior rare condition B", "osd_rare_b")
    ),
    outcomes = list(list(
      name = "Outcome A",
      description = "planted constant-hazard event",
      implementation = list(variable = "osd_a")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    min_expected_events_per_arm_per_year = 0,
    enrollments = list(
      spr_enrollment("01", age_max = 80, seed = 1L),
      spr_enrollment("02", age_max = 55, seed = 2L)
    )
  )
  if (!is.null(rule)) {
    spec$rare_condition_rule <- rule
  }
  return(spec)
}

# The rule the decision tests use. Candidates in spec order: rare_b, common,
# rare_a. rare_b comes first, so step 2 drops its 4 holders before rare_a.
spr_rule <- function(min_n = 10L) {
  return(list(
    min_n = min_n,
    decided_by = "01",
    candidates = c(
      spr_var("osd_rare_b"),
      spr_var("osd_common"),
      spr_var("osd_rare_a")
    )
  ))
}

# Write a spec list to `dir`, and append raw YAML text when given. The raw text
# is how a test writes an unquoted enrollment id: yaml::write_yaml() quotes it.
spr_write_spec <- function(spec, dir, extra_yaml = NULL) {
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  if (!is.null(extra_yaml)) {
    cat(extra_yaml, file = path, append = TRUE)
  }
  return(path)
}

# Run the real s1 dispatch on the fixture, and keep the s1 work directory so a
# test can read the s1a pre files. Returns the plan, the skeleton and the
# tuples of each enrollment, as s1a wrote them.
spr_run_s1 <- function(rule = NULL, sk = spr_skeleton()) {
  root <- tempfile("spr_")
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  skel_path <- file.path(dir_tteplan, SPR_SKELETON)
  qs2::qs_save(sk, skel_path)
  spr_write_spec(spr_spec_list(rule), dir_spec)

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )

  # The success path deletes the work directory. Keep it: the pre files are
  # the s1a output the parity test reads.
  clear <- swereg:::.clear_s1_work_dir
  testthat::local_mocked_bindings(
    .clear_s1_work_dir = function(
      work_dir,
      label = "Cleared s1 work directory",
      fatal = TRUE,
      ...
    ) {
      if (identical(label, "Removed s1 work directory")) {
        return(invisible(0L))
      }
      return(clear(work_dir, label = label, fatal = fatal, ...))
    },
    .package = "swereg"
  )
  # The rare conditions separate the propensity model of enrollment 02 when
  # they stay confounders, and s1e warns about it. The tests read counts, not
  # weights, so the warning is muffled here.
  suppressWarnings(invisible(utils::capture.output(
    plan$s1_generate_enrollments_and_ipw(
      n_workers = 1L,
      swereg_dev_path = swereg:::.swereg_dev_path(),
      check_skeletons = FALSE
    ),
    type = "output"
  )))

  work_dir <- swereg:::.s1_work_dir(plan, ensure_exists = FALSE)
  pre <- lapply(c("01", "02"), function(eid) {
    return(swereg::qs2_read(
      swereg:::.s1a_pre_path(work_dir, eid, SPR_SKELETON),
      nthreads = 1L
    ))
  })
  names(pre) <- c("01", "02")
  return(list(plan = plan, sk = sk, pre = pre, root = root))
}

# A plan built from the fixture spec, with no s1 run. The decision is built by
# `.s1b_decide()` from a hand-made count table, and the hash is that of the
# plan's own rule, as s1b leaves them. The tests that read only plan fields
# use it, so they run in seconds. The plan carries the stamps of a completed
# s1, which the writers need.
spr_plan_no_s1 <- function(rule = spr_rule(), env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dirs <- file.path(root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  sk <- spr_skeleton(n_persons = 40L)
  skel_path <- file.path(dirs[2], SPR_SKELETON)
  qs2::qs_save(sk, skel_path)
  spr_write_spec(spr_spec_list(rule), dirs[1])
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs[4]),
    candidate_dir_spec = dirs[1],
    candidate_dir_tteplan = dirs[2],
    candidate_dir_results = dirs[3],
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  ))
  r <- plan$spec$rare_condition_rule
  counts <- data.table::CJ(
    enrollment_id = r$decided_by,
    variable = r$candidates,
    arm = c("intervention", "comparator"),
    sorted = FALSE
  )
  counts[, n_persons := rep(c(3L, 50L, 4L), each = 2L, length.out = .N)]
  labels <- stats::setNames(r$candidates, r$candidates)
  plan$sparse_decision <- swereg:::.s1b_decide(counts, r, labels)
  plan$sparse_rule_hash <- swereg:::.tte_sparse_rule_hash(r)
  return(tte_stamp_stages(plan))
}
