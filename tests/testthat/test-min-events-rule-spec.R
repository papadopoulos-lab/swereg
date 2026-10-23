# swereg 27.2.15 renamed the two rule keys of a specification.
# `horizon_rule: {min_expected_events: k}` became the flat key
# `min_expected_events_per_arm_per_year: k`, and `sparse_rule` became
# `rare_condition_rule`. The old keys are retired: `tteplan_read_spec()` and
# `tteplan_check_spec()` refuse them, and the message names the new key. A plan
# saved by 27.2.14 still loads, with its stored spec under the new keys.

skip_if_not_installed("yaml")

# The fixture spec text, with the line of the minimum-events key replaced by
# `lines`. `extra` is appended at the end of the file.
mer_write <- function(lines, extra = character(0)) {
  txt <- readLines(testthat::test_path("fixtures", "spec_3x2x2.yaml"))
  i <- which(txt == "min_expected_events_per_arm_per_year: 0")
  stopifnot(length(i) == 1L)
  txt <- c(txt[seq_len(i - 1L)], lines, txt[-seq_len(i)], extra)
  dir <- tempfile("mer_spec_")
  dir.create(dir)
  path <- file.path(dir, "spec_v001.yaml")
  writeLines(txt, path)
  return(path)
}

# A rare-condition block under `key`. The fixture has no computed confounder,
# so the refusal of the old key is the only finding the gate can make.
mer_rare_block <- function(key) {
  return(c(
    paste0(key, ":"),
    "  min_n: 100",
    "  decided_by: [\"01\"]",
    "  candidates: [rd_age_continuous]"
  ))
}

mer_read_error <- function(path) {
  return(tryCatch(
    {
      tteplan_read_spec(path)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  ))
}


test_that("a spec with horizon_rule is refused, naming min_expected_events_per_arm_per_year", {
  path <- mer_write(c("horizon_rule:", "  min_expected_events: 5"))

  msg <- mer_read_error(path)
  expect_match(msg, "carries 1 key swereg does not accept", fixed = TRUE)
  expect_match(msg, "$/horizon_rule\n", fixed = TRUE)
  expect_match(msg, "min_expected_events_per_arm_per_year", fixed = TRUE)

  r <- tteplan_check_spec(path)
  expect_identical(r$path, "$/horizon_rule")
  expect_identical(r$kind, "retired_key")
  expect_match(r$message, "min_expected_events_per_arm_per_year", fixed = TRUE)
})

test_that("a spec with sparse_rule is refused, naming rare_condition_rule", {
  path <- mer_write(
    "min_expected_events_per_arm_per_year: 5",
    mer_rare_block("sparse_rule")
  )

  msg <- mer_read_error(path)
  expect_match(msg, "carries 1 key swereg does not accept", fixed = TRUE)
  expect_match(msg, "$/sparse_rule\n", fixed = TRUE)
  expect_match(msg, "rare_condition_rule", fixed = TRUE)

  r <- tteplan_check_spec(path)
  expect_identical(r$path, "$/sparse_rule")
  expect_identical(r$kind, "retired_key")
  expect_match(r$message, "rare_condition_rule", fixed = TRUE)
})

test_that("a spec with the new keys reads, and the plan holds min_expected_events_per_arm_per_year 5", {
  path <- mer_write("min_expected_events_per_arm_per_year: 5")
  expect_identical(nrow(tteplan_check_spec(path)), 0L)
  spec <- tteplan_read_spec(path)
  expect_identical(spec$min_expected_events_per_arm_per_year, 5)

  # The plan reads the value through `$reload_spec()`, which copies it onto a
  # plan that holds another value.
  plan <- swereg::TTEPlan$new(
    project_prefix = "fixture-3x2x2",
    skeleton_files = "mer_skeleton.qs2",
    global_max_isoyearweek = "2020-52"
  )
  plan$spec <- tteplan_read_spec(
    mer_write("min_expected_events_per_arm_per_year: 0")
  )
  plan$reload_spec(spec_path = path, quiet = TRUE)
  expect_true(plan$spec$min_expected_events_per_arm_per_year == 5)
  expect_null(plan$spec_reload_skipped_diffs)
})

test_that("a plan saved with the 27.2.14 keys loads under the new keys", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- spr_plan_no_s1()
  hash <- plan$sparse_rule_hash
  rule <- plan$spec$rare_condition_rule
  k <- plan$spec$min_expected_events_per_arm_per_year

  # The stored spec of a plan that swereg 27.2.14 saved.
  old <- plan$spec
  old$rare_condition_rule <- NULL
  old$min_expected_events_per_arm_per_year <- NULL
  old$sparse_rule <- rule
  old$horizon_rule <- list(min_expected_events = k)
  plan$spec <- old

  loaded <- swereg::tteplan_load(plan$save())
  expect_null(loaded$spec$horizon_rule)
  expect_null(loaded$spec$sparse_rule)
  expect_identical(loaded$spec$min_expected_events_per_arm_per_year, k)
  expect_identical(loaded$spec$rare_condition_rule, rule)
  expect_identical(loaded$sparse_rule_hash, hash)
  expect_no_error(
    swereg:::.tte_assert_sparse_decision_current(loaded, "the test")
  )
})

# ---------------------------------------------------------------------------
# A result that swereg 27.2.14 stored reads NE on export
# ---------------------------------------------------------------------------
# 27.2.14 wrote the same status text with another prefix. The fixture below
# withholds ETT00005 (per-protocol) and ETT00001 (intention-to-treat) through
# the real parent merge, then rewrites every stored status to the 27.2.14
# prefix. Both plans MUST export the same cells.

# Rows are (tstart, tstop]: the panel of test-horizon-sheet.R.
mer_panel <- function() {
  return(data.table::data.table(
    rd_tx = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    tstart = c(0, 52, 104, 0, 52, 104),
    tstop = c(52, 104, 130, 52, 104, 156),
    w_pp = c(2, 1, 1, 4, 3, 2),
    w_itt = 1,
    event = c(1L, 0L, 1L, 1L, 1L, 0L)
  ))
}

mer_withheld_plan <- function() {
  rule <- list(min_expected_events = 2)
  plan <- .xp_plan("new")
  plan$spec$min_expected_events_per_arm_per_year <- 2
  for (id in names(plan$results_ett)) {
    for (est in c("pp", "itt")) {
      plan$results_ett[[id]][[paste0("horizon_", est)]]$rule <- rule
    }
  }
  d <- mer_panel()
  for (id in c("ETT00001", "ETT00005")) {
    fu <- plan$ett$follow_up[plan$ett$ett_id == id]
    for (est in c("pp", "itt")) {
      plan$results_ett[[id]][[paste0("horizon_", est)]] <-
        swereg:::.tte_horizon_record(
          d[tstart < fu],
          weight_col = paste0("w_", est),
          follow_up = fu,
          rule = rule,
          treatment_var = "rd_tx"
        )
    }
  }
  plan$results_ett <- swereg:::.s3_apply_horizon_rule(
    plan$results_ett,
    c("ETT00001", "ETT00005")
  )
  return(plan)
}

# Every stored string that starts with the current prefix, rewritten to the
# 27.2.14 prefix. `n` counts the rewritten strings.
mer_to_legacy <- function(x, n) {
  new <- swereg:::.HORIZON_STATUS_PREFIX
  old <- swereg:::.HORIZON_STATUS_PREFIX_LEGACY
  swap <- function(v) {
    hit <- !is.na(v) & startsWith(v, new)
    n$k <- n$k + sum(hit)
    v[hit] <- paste0(old, substring(v[hit], nchar(new) + 1L))
    return(v)
  }
  if (data.table::is.data.table(x)) {
    x <- data.table::copy(x)
    for (j in names(x)[vapply(x, is.character, logical(1))]) {
      data.table::set(x, j = j, value = swap(x[[j]]))
    }
    return(x)
  }
  if (is.character(x)) {
    return(swap(x))
  }
  if (is.list(x) && !is.data.frame(x)) {
    for (i in seq_along(x)) {
      if (!is.null(x[[i]])) x[[i]] <- mer_to_legacy(x[[i]], n)
    }
  }
  return(x)
}

# The PP results, ITT results and Summary sheets, and the text of the forest
# figure, of one export.
mer_outputs <- function(plan, dir) {
  dir.create(dir, recursive = TRUE)
  xlsx <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  sheets <- lapply(
    c(pp = "PP results", itt = "ITT results", summary = "Summary"),
    function(sh) {
      m <- openxlsx::read.xlsx(xlsx, sheet = sh, colNames = FALSE)
      m[] <- lapply(m, as.character)
      return(m)
    }
  )
  forest <- swereg:::.build_forest_df(plan)
  forest_irr <- swereg:::.ff_irr_withheld(
    rep("x", nrow(forest)),
    forest$horizon_status
  )
  return(list(sheets = sheets, forest = forest, forest_irr = forest_irr))
}

test_that("a result stored with the 27.2.14 status prefix exports NE like a 27.2.15 result", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  root <- withr::local_tempdir()

  current <- mer_withheld_plan()
  legacy <- mer_withheld_plan()
  n <- new.env()
  n$k <- 0L
  legacy$results_ett <- mer_to_legacy(legacy$results_ett, n)
  # The rewrite reached the stored statuses, and left none in the new form.
  expect_gt(n$k, 0L)
  expect_false(any(grepl(
    swereg:::.HORIZON_STATUS_PREFIX,
    unlist(legacy$results_ett),
    fixed = TRUE
  )))

  a <- mer_outputs(current, file.path(root, "current"))
  b <- mer_outputs(legacy, file.path(root, "legacy"))

  for (sh in names(a$sheets)) {
    expect_true(any(a$sheets[[sh]] == "NE", na.rm = TRUE), info = sh)
    expect_identical(b$sheets[[sh]], a$sheets[[sh]], info = sh)
  }
  expect_true(any(a$forest_irr == "NE"))
  expect_identical(b$forest_irr, a$forest_irr)
  expect_identical(
    swereg:::.tte_horizon_withheld(b$forest$horizon_status),
    swereg:::.tte_horizon_withheld(a$forest$horizon_status)
  )
})

test_that("tteplan_load() rewrites the 27.2.14 status prefix to the current prefix", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  root <- withr::local_tempdir()
  new <- swereg:::.HORIZON_STATUS_PREFIX
  old <- swereg:::.HORIZON_STATUS_PREFIX_LEGACY

  plan <- mer_withheld_plan()
  n <- new.env()
  n$k <- 0L
  plan$results_ett <- mer_to_legacy(plan$results_ett, n)
  expect_gt(n$k, 0L)

  path <- file.path(root, "tteplan.qs2")
  qs2::qs_save(plan, path)
  loaded <- swereg::tteplan_load(path)
  est <- loaded$get_estimates()
  win <- loaded$get_horizon_windows()
  statuses <- c(est$horizon_status, est$interval_status, win$horizon_status)
  statuses <- statuses[!is.na(statuses)]
  expect_false(any(startsWith(statuses, old)))
  expect_true(any(startsWith(
    est$horizon_status[!is.na(est$horizon_status)],
    new
  )))
  expect_true(any(startsWith(
    est$interval_status[!is.na(est$interval_status)],
    new
  )))
  expect_true(any(startsWith(win$horizon_status, new)))
  # No stored string keeps the legacy prefix.
  expect_false(any(
    startsWith(
      as.character(unlist(loaded$results_ett)),
      old
    ),
    na.rm = TRUE
  ))

  out <- mer_outputs(loaded, file.path(root, "loaded"))
  for (sh in names(out$sheets)) {
    expect_true(any(out$sheets[[sh]] == "NE", na.rm = TRUE), info = sh)
  }
})
