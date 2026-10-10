# The sparse_rule (#52): the parser, the counting rule of s1b, the
# stale-decision guard, persistence, reload_spec and the schema refusal.
#
# Nothing in this file runs s1, so it runs in seconds. test-s1b-sparse-rule.R
# drives the real s1 dispatch. The fixtures are in helper-sparse_rule.R.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

# Read the fixture spec with `extra_yaml` appended as written.
spr_parse <- function(extra_yaml) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- spr_write_spec(spr_spec_list(), dir, extra_yaml)
  return(swereg::tteplan_read_spec(path))
}

# The message the parser stops with, or "no error". A test compares it with
# an expectation, so a RED is always that expectation failing, never a
# different error escaping the test.
spr_parse_error <- function(extra_yaml) {
  return(tryCatch(
    {
      spr_parse(extra_yaml)
      "no error"
    },
    error = function(e) conditionMessage(e)
  ))
}

spr_rule_yaml <- function(
  decided_by = "[\"01\"]",
  candidates = "[rd_no_osd_rare_a_everbefore]",
  min_n = "10"
) {
  lines <- c("sparse_rule:", paste0("  min_n: ", min_n))
  if (!is.null(decided_by)) {
    lines <- c(lines, paste0("  decided_by: ", decided_by))
  }
  lines <- c(lines, paste0("  candidates: ", candidates))
  return(paste0(paste(lines, collapse = "\n"), "\n"))
}

# ---------------------------------------------------------------------------
# The parser
# ---------------------------------------------------------------------------

test_that("a quoted sparse_rule parses into its normal form", {
  spec <- spr_parse(spr_rule_yaml(
    decided_by = "[\"01\", \"02\"]",
    candidates = "[rd_no_osd_rare_b_everbefore, rd_no_osd_common_everbefore]"
  ))
  expect_identical(
    spec$sparse_rule,
    list(
      min_n = 10L,
      decided_by = c("01", "02"),
      candidates = c(
        "rd_no_osd_rare_b_everbefore",
        "rd_no_osd_common_everbefore"
      )
    )
  )
})

test_that("an unquoted integer id in decided_by stops with a message to quote it", {
  # YAML reads [01] as the integer 1.
  expect_match(
    spr_parse_error(spr_rule_yaml(decided_by = "[01]")),
    "Quote every enrollment id",
    fixed = TRUE
  )
})

test_that("decided_by naming an enrollment the spec lacks stops", {
  expect_match(
    spr_parse_error(spr_rule_yaml(decided_by = "[\"09\"]")),
    "names an enrollment the spec does not declare: 09",
    fixed = TRUE
  )
})

test_that("a candidate that is not a computed confounder stops", {
  # An exclusion column, and a confounder that is not computed.
  expect_match(
    spr_parse_error(spr_rule_yaml(candidates = "[eligible_no_rd_tx_everbefore]")),
    "not a computed confounder of this spec: eligible_no_rd_tx_everbefore",
    fixed = TRUE
  )
  expect_match(
    spr_parse_error(spr_rule_yaml(candidates = "[rd_age_continuous]")),
    "not a computed confounder of this spec: rd_age_continuous",
    fixed = TRUE
  )
})

test_that("a sparse_rule without decided_by stops", {
  expect_match(
    spr_parse_error(spr_rule_yaml(decided_by = NULL)),
    "sparse_rule has no decided_by",
    fixed = TRUE
  )
})

test_that("a sparse_rule key that is present but null or empty stops", {
  # An absent key is no rule. A key written with no value, or with an empty
  # mapping, is a rule left unfinished.
  expect_match(
    spr_parse_error("sparse_rule:\n"),
    "sparse_rule is present but empty",
    fixed = TRUE
  )
  expect_match(
    spr_parse_error("sparse_rule: {}\n"),
    "sparse_rule is present but empty",
    fixed = TRUE
  )
  expect_null(spr_parse(NULL)$sparse_rule)
})

test_that("min_n NA stops with a message naming min_n", {
  expect_match(
    spr_parse_error(spr_rule_yaml(min_n = ".nan")),
    "sparse_rule$min_n MUST be one whole number from 1 to 2147483647. It is NA.",
    fixed = TRUE
  )
})

test_that("min_n infinite stops with a message naming min_n", {
  expect_match(
    spr_parse_error(spr_rule_yaml(min_n = ".inf")),
    "sparse_rule$min_n MUST be one whole number from 1 to 2147483647. It is infinite.",
    fixed = TRUE
  )
})

test_that("min_n that is not a whole number stops with a message naming min_n", {
  expect_match(
    spr_parse_error(spr_rule_yaml(min_n = "10.5")),
    "sparse_rule$min_n MUST be one whole number from 1 to 2147483647. It is 10.5, which is not a whole number.",
    fixed = TRUE
  )
})

test_that("min_n below 1 stops with a message naming min_n", {
  expect_match(
    spr_parse_error(spr_rule_yaml(min_n = "0")),
    "sparse_rule$min_n MUST be one whole number from 1 to 2147483647. It is 0, which is below 1.",
    fixed = TRUE
  )
})

test_that("min_n above the largest R integer stops with a message naming min_n", {
  # Written as a float: YAML reads the integer 3000000000 as NA.
  expect_match(
    spr_parse_error(spr_rule_yaml(min_n = "3000000000.0")),
    "sparse_rule$min_n MUST be one whole number from 1 to 2147483647. It is 3000000000, which is above the largest R integer.",
    fixed = TRUE
  )
})

# ---------------------------------------------------------------------------
# The counting rule of s1b: a person counts once per arm, when ANY of her
# tuples in that arm holds the condition
# ---------------------------------------------------------------------------

test_that("s1b counts a person once per arm when any of her tuples there holds the condition", {
  work_dir <- withr::local_tempdir()
  v <- "rd_no_x"
  # holds = rd_no_x FALSE. Per person:
  #   A  comparator only; holds on her 2nd tuple of 3, not her 1st or 3rd
  #   B  comparator tuple does not hold, intervention tuple holds
  #   C  holds on her tuples in both arms
  #   D  comparator only; holds on no tuple
  #   E  comparator only; no row at the recruiting week (NA)
  #   F  comparator only, in a second skeleton; holds on both tuples
  tup <- function(id, period, intervention, value) {
    out <- data.table::data.table(
      id = id,
      enrollment_period_id = period,
      intervention = intervention,
      recruit_week_index = 100L + period
    )
    out[[v]] <- value
    return(out)
  }
  pre_a <- list(
    tuples = data.table::rbindlist(list(
      tup("A", 1:3, FALSE, c(TRUE, FALSE, TRUE)),
      tup("B", 1L, FALSE, TRUE),
      tup("B", 4L, TRUE, FALSE),
      tup("C", 1L, FALSE, FALSE),
      tup("C", 3L, TRUE, FALSE),
      tup("D", 1:2, FALSE, c(TRUE, TRUE)),
      tup("E", 1L, FALSE, NA)
    )),
    attrition = data.table::data.table()
  )
  pre_b <- list(
    tuples = tup("F", 1:2, FALSE, c(FALSE, FALSE)),
    attrition = data.table::data.table()
  )
  qs2::qs_save(pre_a, swereg:::.s1a_pre_path(work_dir, "01", "skel_a.qs2"))
  qs2::qs_save(pre_b, swereg:::.s1a_pre_path(work_dir, "01", "skel_b.qs2"))
  es <- list(
    enrollment_id = "01",
    design = list(person_id_var = "id"),
    n_threads = 1L
  )
  res <- swereg:::.s1b_worker(
    enrollment_specs = list(es),
    work_dir = work_dir,
    skel_basenames = c("skel_a.qs2", "skel_b.qs2"),
    condition_vars = v
  )$counts
  got <- stats::setNames(res$n_persons, res$arm)
  # Intervention: B and C. Comparator: A, C and F.
  expect_identical(got[["intervention"]], 2L)
  expect_identical(got[["comparator"]], 3L)
})

# ---------------------------------------------------------------------------
# The stale-decision guard
# ---------------------------------------------------------------------------

# Change one field of the plan's rule, then call s2. Returns the error message,
# or "no error".
spr_s2_after_change <- function(field, value) {
  plan <- spr_plan_no_s1()
  plan$spec$sparse_rule[[field]] <- value
  return(tryCatch(
    {
      suppressWarnings(utils::capture.output(
        plan$s2_generate_analysis_files_and_ipcw_pp(
          n_workers = 1L,
          swereg_dev_path = swereg:::.swereg_dev_path()
        ),
        type = "output"
      ))
      "no error"
    },
    error = function(e) conditionMessage(e)
  ))
}

test_that("the guard passes the rule the decision was made under", {
  plan <- spr_plan_no_s1()
  expect_true(swereg:::.tte_assert_sparse_decision_current(plan, "test"))
})

test_that("a changed min_n stops s2 until s1 decides again", {
  expect_match(
    spr_s2_after_change("min_n", 11L),
    "does not match the rule of the frozen decision"
  )
})

test_that("a changed decided_by stops s2 until s1 decides again", {
  expect_match(
    spr_s2_after_change("decided_by", c("01", "02")),
    "does not match the rule of the frozen decision"
  )
})

test_that("changed candidates stop s2 until s1 decides again", {
  expect_match(
    spr_s2_after_change("candidates", spr_var("osd_common")),
    "does not match the rule of the frozen decision"
  )
})

# ---------------------------------------------------------------------------
# Persistence
# ---------------------------------------------------------------------------

test_that("tteplan_load() keeps the frozen decision", {
  plan <- spr_plan_no_s1()
  loaded <- swereg::tteplan_load(plan$save())
  expect_identical(
    as.data.frame(loaded$get_sparse_decision()),
    as.data.frame(plan$get_sparse_decision())
  )
})

test_that("tteplan_load() keeps the rule hash", {
  plan <- spr_plan_no_s1()
  hash <- plan$sparse_rule_hash
  loaded <- swereg::tteplan_load(plan$save())
  expect_identical(loaded$sparse_rule_hash, hash)
})

# ---------------------------------------------------------------------------
# $reload_spec() refuses every change to the rule
# ---------------------------------------------------------------------------

spr_reload_change <- function(field, value, env = parent.frame()) {
  dir_old <- withr::local_tempdir(.local_envir = env)
  dir_new <- withr::local_tempdir(.local_envir = env)
  rule_new <- spr_rule()
  rule_new[[field]] <- value
  path_old <- spr_write_spec(spr_spec_list(spr_rule()), dir_old)
  path_new <- spr_write_spec(spr_spec_list(rule_new), dir_new)
  plan <- swereg::TTEPlan$new(
    project_prefix = "sprtest",
    skeleton_files = "skel_a.qs2",
    global_max_isoyearweek = "2019-26"
  )
  plan$spec <- swereg::tteplan_read_spec(path_old)
  return(list(plan = plan, old = plan$spec$sparse_rule, path_new = path_new))
}

for (spr_field in c("min_n", "decided_by", "candidates")) {
  local({
    field <- spr_field
    value <- switch(
      field,
      min_n = 20L,
      decided_by = c("01", "02"),
      candidates = spr_var("osd_common")
    )
    test_that(
      paste0("sparse_rule$", field, ": reload_spec refuses a change to it"),
      {
        x <- spr_reload_change(field, value)
        expect_warning(
          x$plan$reload_spec(spec_path = x$path_new, quiet = TRUE),
          paste0("sparse_rule$", field),
          fixed = TRUE
        )
        expect_identical(x$plan$spec$sparse_rule, x$old)
      }
    )
  })
}

# ---------------------------------------------------------------------------
# Objects from before the rule are refused
# ---------------------------------------------------------------------------

spr_stamp <- function(obj, version) {
  assign(
    ".schema_version",
    as.integer(version),
    envir = obj$.__enclos_env__$private
  )
  return(obj)
}

test_that("an enrollment stamped with the 27.2.0 schema is refused", {
  weeks <- cstime::dates_by_isoyearweek$isoyearweek
  wk <- weeks[which(weeks >= "2020-01")[1] + 0:7]
  d <- data.table::rbindlist(lapply(
    c(paste0("I", 1:3), paste0("C", 1:6)),
    function(nm) {
      return(data.table::data.table(
        id = nm,
        isoyearweek = wk,
        exposed = startsWith(nm, "I"),
        eligible = TRUE,
        died = FALSE,
        age = 50
      ))
    }
  ))
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = 4L,
    period_width = 4L
  )
  en <- swereg::TTEEnrollment$new(
    data = d,
    design = design,
    ratio = 2,
    seed = 4,
    extra_cols = "isoyearweek"
  )
  path <- file.path(withr::local_tempdir(), "enrollment_schema6.qs2")
  qs2::qs_save(spr_stamp(en, 6L), path)
  expect_error(
    swereg::qs2_read(path),
    "TTEEnrollment at schema version 6, and this swereg requires version 7"
  )
})

test_that("a plan stamped with the 27.2.0 schema is refused", {
  dir <- withr::local_tempdir()
  plan <- swereg::TTEPlan$new(
    project_prefix = "sprtest",
    skeleton_files = file.path(dir, "skel_a.qs2"),
    global_max_isoyearweek = "2020-08"
  )
  path <- file.path(dir, "plan_schema5.qs2")
  qs2::qs_save(spr_stamp(plan, 5L), path)
  expect_error(
    swereg::qs2_read(path),
    "TTEPlan at schema version 5, and this swereg requires version 6"
  )
})
