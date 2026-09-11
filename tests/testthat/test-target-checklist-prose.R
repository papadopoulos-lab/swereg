# The TARGET checklist and the supplement's Table S1 caption are manuscript
# prose. A paper carries item 6f, the item 7a-h paragraph and the Table S1
# caption verbatim, so a wrong sentence in any of them is a false methods claim
# that no other test has an opinion about.
#
# These tests read the GENERATED text. They never read the paste0() or the
# writeData() call that builds it. The checklist plan runs no stage:
# `$print_target_checklist()` needs only `plan$spec`. The caption comes from one
# real `$export_tables()` run.
#
# `.tcp_build()` repeats `.tfp_build()` from `test-tte-fill-prose.R`. testthat
# evaluates each test file in its own environment, so one test file cannot call
# a function another one defines.

skip_if_not_installed("qs2")
skip_if_not_installed("openxlsx")
skip_if_not_installed("ggplot2")
skip_if_not_installed("patchwork")
skip_if_not_installed("withr")

.tcp_cache <- new.env(parent = emptyenv())

#' A plan built from a written specification and a small skeleton.
#'
#' The plan runs no stage. It exists for its `$spec`, which is everything
#' `$print_target_checklist()` reads.
.tcp_build <- function(dir) {
  dir_spec <- file.path(dir, "spec")
  dir_tteplan <- file.path(dir, "tteplan")
  dir_results <- file.path(dir, "results")
  dir_meta <- file.path(dir, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  sk <- ttm_skeleton(
    "A",
    n_persons = 20L,
    date_max = "2016-12-31",
    n_init_bands = 4L
  )
  skel <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel)
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    "tcp",
    "ri_highrisk"
  )
  return(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = max(sk$isoyearweek, na.rm = TRUE)
  ))
}

# One build for the file. The plan is cached, not the printed lines, so a test
# that changes `.TTE_ESTIMANDS` can print the checklist again cheaply.
.tcp_plan <- function() {
  if (is.null(.tcp_cache$plan)) {
    dir <- withr::local_tempdir(.local_envir = teardown_env())
    .tcp_cache$plan <- .tcp_build(dir)
  }
  return(.tcp_cache$plan)
}

# `$print_target_checklist()` prints and returns nothing, so the captured lines
# are the only artefact.
.tcp_lines <- function(plan = .tcp_plan()) {
  return(utils::capture.output(plan$print_target_checklist()))
}

# One segment of the item 7a-h paragraph. That paragraph prints on one line, so
# the pattern needs no newline handling.
.tcp_seg <- function(lines, pattern) {
  txt <- paste(lines, collapse = "\n")
  m <- regexpr(pattern, txt, perl = TRUE)
  if (m[1] == -1L) {
    return(NA_character_)
  }
  return(regmatches(txt, m))
}

# One printed item, from its own title line to the next item's title line.
.tcp_item <- function(lines, n, next_n) {
  i <- grep(paste0("Item ", n, "\\. "), lines)[1]
  j <- grep(paste0("Item ", next_n, "\\. "), lines)[1]
  if (is.na(i) || is.na(j)) {
    return(NA_character_)
  }
  return(paste(lines[i:(j - 1L)], collapse = "\n"))
}

# The caption of the "Table S1 Missing data" sheet, read back off one real
# export. `.xp_plan()` comes from `helper-export_parity.R`. The export writes
# that sheet for every plan. `fill_summary = TRUE` is what puts the table in
# it, in place of the one line a plan that filled nothing gets.
.tcp_caption <- function() {
  if (is.null(.tcp_cache$caption)) {
    dir <- withr::local_tempdir(.local_envir = teardown_env())
    path <- file.path(dir, "tables.xlsx")
    plan <- .xp_plan("new", subgroups = FALSE, fill_summary = TRUE)
    suppressMessages(suppressWarnings(
      utils::capture.output(plan$export_tables(path = path))
    ))
    cells <- openxlsx::read.xlsx(
      path,
      sheet = "Table S1 Missing data",
      colNames = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    )
    .tcp_cache$caption <- cells[[1]][1]
  }
  return(.tcp_cache$caption)
}


# The s3 ETT work item list, captured without starting a worker. `.batch_run()`
# is stubbed: the first call carries the enrollment items and returns an empty
# result for each, the second carries the ETT items and stops. `estimands`
# replaces `.TTE_ESTIMANDS` for the length of the call.
.tcp_s3_items <- function(estimands = NULL) {
  cap <- new.env(parent = emptyenv())
  orig_set <- swereg:::.TTE_ESTIMANDS
  orig_run <- swereg:::.batch_run
  withr::defer(
    {
      assignInNamespace(".TTE_ESTIMANDS", orig_set, "swereg")
      assignInNamespace(".batch_run", orig_run, "swereg")
    },
    envir = parent.frame()
  )
  if (!is.null(estimands)) {
    assignInNamespace(".TTE_ESTIMANDS", estimands, "swereg")
  }
  assignInNamespace(
    ".batch_run",
    function(target, items, ...) {
      if (is.null(cap$enr)) {
        cap$enr <- items
        return(lapply(items, function(x) list()))
      }
      cap$ett <- items
      stop("tcp-captured-s3", call. = FALSE)
    },
    "swereg"
  )
  plan <- .xp_plan("new", subgroups = TRUE, fill_summary = FALSE)
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  expect_error(
    suppressWarnings(suppressMessages(utils::capture.output(
      plan$s3_analyze(output_dir = dir, n_workers = 1L)
    ))),
    "tcp-captured-s3"
  )
  return(cap$ett)
}


test_that("checklist item 6f names the estimands the pipeline builds", {
  lines <- .tcp_lines()
  seg <- .tcp_seg(
    lines,
    "Causal contrasts \\(6f\\):.*?Confounders \\(6g\\):"
  )
  expect_false(is.na(seg))

  # Every plan builds both estimands: s2 writes one analysis file for each and
  # s3 dispatches one weight column for each.
  expect_match(seg, "intention-to-treat estimands", fixed = TRUE)

  # The sentence this replaced. It was false for every plan.
  expect_false(grepl(
    "Intention-to-treat and as-treated analyses were not conducted",
    seg,
    fixed = TRUE
  ))

  # Design register. `tte_stage()` prints the checklist after s1, before s2 or
  # s3 run, so no estimate exists when these sentences are written.
  expect_false(grepl("were estimated", seg, fixed = TRUE))
  expect_false(grepl("are estimated", seg, fixed = TRUE))

  # Item 6f's own summary line follows the same set.
  it <- .tcp_item(lines, "6f", "6g")
  expect_false(is.na(it))
  expect_match(
    it,
    paste0(
      "Supported: per-protocol (IPW + IPCW-PP), ",
      "intention-to-treat (baseline IPW)."
    ),
    fixed = TRUE
  )
  expect_match(it, "Not supported: as-treated", fixed = TRUE)
})


test_that("checklist item 6h points at item 6f and cites the estimator", {
  lines <- .tcp_lines()
  seg <- .tcp_seg(lines, "Analysis \\(6h\\):[^\n]*")
  expect_false(is.na(seg))

  # It used to cross-reference itself. Baseline IPTW is described in 6c and
  # IPCW in 6f.
  expect_false(grepl("6c and 6h", seg, fixed = TRUE))
  expect_match(seg, "items 6c and 6f", fixed = TRUE)
  expect_match(seg, "Danaei", fixed = TRUE)

  # Item 6h itself names the methods literature where it names IPCW.
  it <- .tcp_item(lines, "6h", "7a-h")
  expect_false(is.na(it))
  expect_match(it, "Danaei", fixed = TRUE)
})


test_that("the Table S1 caption names the plan's impute_fn", {
  cap <- .tcp_caption()
  expect_false(is.na(cap))

  # The plan records no `impute_fn`, so the caption states the rule and names
  # the hot-deck draw as the default. It used to state the default as fact.
  expect_match(cap, "impute_fn", fixed = TRUE)
  expect_match(cap, "the default is a single hot-deck draw", fixed = TRUE)
  expect_false(grepl(
    "with no observation is a single hot-deck draw",
    cap,
    fixed = TRUE
  ))
})


test_that("the estimand set drives the s2 item list and the 6f sentence", {
  # Drop `itt` from `.TTE_ESTIMANDS` for the length of this test. The s2 item
  # loop and checklist item 6f both read that constant, so both MUST follow it.
  # A hard-coded loop keeps building ITT items and fails here.
  captured <- new.env(parent = emptyenv())
  orig_set <- swereg:::.TTE_ESTIMANDS
  orig_run <- swereg:::.batch_run_and_write
  withr::defer({
    assignInNamespace(".TTE_ESTIMANDS", orig_set, "swereg")
    assignInNamespace(".batch_run_and_write", orig_run, "swereg")
  })
  assignInNamespace(".TTE_ESTIMANDS", c(pp = "per-protocol"), "swereg")
  assignInNamespace(
    ".batch_run_and_write",
    function(target, items, outputs, ...) {
      captured$items <- items
      stop("tcp-captured", call. = FALSE)
    },
    "swereg"
  )

  # `.xp_plan()` holds eight emulated trials, so the full set builds sixteen
  # items and the reduced set builds eight.
  plan <- .xp_plan("new", subgroups = FALSE, fill_summary = FALSE)
  dir <- withr::local_tempdir()
  expect_error(
    suppressWarnings(utils::capture.output(
      plan$s2_generate_analysis_files_and_ipcw_pp(output_dir = dir)
    )),
    "tcp-captured"
  )
  est <- vapply(captured$items, function(x) x$estimand, character(1))
  expect_identical(unique(unname(est)), "pp")
  expect_identical(length(est), nrow(plan$ett))

  # The checklist moves the dropped estimand into the not-planned list.
  seg <- .tcp_seg(
    .tcp_lines(),
    "Causal contrasts \\(6f\\):.*?Confounders \\(6g\\):"
  )
  expect_match(
    seg,
    "intention-to-treat and as-treated analyses are not planned",
    fixed = TRUE
  )
})


test_that("the estimand set drives the s3 item list", {
  # The full set first. Both estimands MUST appear, so the reduced-set
  # assertions below cannot pass by observing an empty list.
  #
  # 72 items for the `.xp_plan()` fixture: 8 ETTs take 1 summary call, 2
  # per-protocol ratio calls, 2 intention-to-treat ratio calls and 2 absolute
  # scale calls, which is 56. Four subgroup variables across two ETTs take 2
  # calls for each of the 2 estimands, which is 16.
  full <- .tcp_s3_items()
  expect_identical(length(full), 72L)
  expect_true(any(
    vapply(full, function(x) x$weight_col, character(1)) == "ipw_trunc"
  ))

  # Now drop `itt` from `.TTE_ESTIMANDS`. Membership and order are the
  # constant's, so every intention-to-treat item MUST go: the baseline IPW
  # column, and the item id that names it. A hard-coded s3 builder keeps them.
  #
  # 40 items: 8 ETTs take 1 + 2 + 1, which is 32, and the four subgroup
  # variables take 2 calls for the one remaining estimand, which is 8.
  reduced <- .tcp_s3_items(c(pp = "per-protocol"))
  w <- vapply(reduced, function(x) x$weight_col, character(1))
  expect_false(any(w == "ipw_trunc"))
  expect_false(any(grepl("ipw_trunc", names(reduced), fixed = TRUE)))
  expect_identical(length(reduced), 40L)
})
