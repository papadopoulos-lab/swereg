# =============================================================================
# `$export_tables()` sources every stored result through the accessors, and the
# workbook and the figures do not move
# =============================================================================
#
# Every consumer in the export path now reads one of the six accessors in place
# of a result slot: `$get_estimates()`, `$get_curves()`, `$get_baselines()`,
# `$get_attrition()`, `$get_matching()` or `$get_subgroups()`. The test pins the
# OUTPUT of that path against a stored snapshot.
#
# Four properties are pinned:
#
#   1. every sheet's cell content, compared per sheet;
#   2. every generated image, on inventory, pixel dimensions and the data its
#      renderer received;
#   3. no consumer reads `results_ett`, `results_enrollment` or
#      `enrollment_counts` outside the accessor module;
#   4. the participant flow reads the same rows through the accessors that it
#      read from the stored tables.
#
# THE TEST PINS THE DATA GOING INTO EACH FIGURE, NEVER THE PIXELS. A pixel
# check reports the rasteriser, not the result. See the comment on
# `test_that("images match on inventory, dimensions and renderer input")`.
#
# TWO FIXTURES, because one hides half the export path. `legacy` is a
# pre-risk-difference result: no stored curve, no stored `irr_estimable`.
# `new` is a current one. Three live projects still hold legacy results.
#
# FOUR PROJECT SHAPES, because `protocol_ett_id` changes which trial the
# Target trial protocol sheet documents, and the `subgroups:` block decides
# whether one sheet exists at all. The four shapes mirror the four projects
# that call `$export_tables()`.
#
# The snapshot is `fixtures/export_tables_snapshot.qs2`, and
# `.xp_regenerate_snapshot()` rewrites it. See `helper-export_parity.R`.

skip_if_not_installed("openxlsx")
skip_if_not_installed("ggplot2")
skip_if_not_installed("patchwork")
skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")
# The CONSORT sidecars need the optional diagram stack. Without it the export
# writes no CONSORT image and the inventory check would fail for a reason that
# has nothing to do with the accessors.
skip_if_not_installed("DiagrammeR")
skip_if_not_installed("DiagrammeRsvg")
skip_if_not_installed("rsvg")

# One export per case, run once and read by the tests below. Eight exports take
# about 95 seconds.
.xp_expected <- qs2::qs_read(.xp_snapshot_path())
.xp_root <- withr::local_tempdir(.local_envir = teardown_env())
.xp_actual <- .xp_capture_all(.xp_root)


test_that("sheet cell content is unchanged", {
  expect_identical(names(.xp_actual), names(.xp_expected$cases))
  for (case in names(.xp_expected$cases)) {
    want <- .xp_expected$cases[[case]]
    got <- .xp_actual[[case]]
    expect_identical(
      got$sheet_names,
      want$sheet_names,
      info = paste0(case, ": sheet names")
    )
    for (sheet in want$sheet_names) {
      expect_equal(
        got$sheets[[sheet]],
        want$sheets[[sheet]],
        info = paste0(case, ": sheet '", sheet, "'")
      )
    }
  }
})


test_that("images match on inventory, dimensions and renderer input", {
  # NOTHING HERE COMPARES PIXELS, and that is deliberate.
  #
  # `ggplot2::ggsave()` picks its rasteriser by probing for `ragg`. `ragg` is in
  # neither Imports nor Suggests, so `R CMD check` hides it and the same figure
  # rasterises through cairo there and through ragg in an ordinary session. A
  # byte digest reports that as a difference. It is not one: 3.62% of pixels
  # move, every glyph doubles, and the numbers are identical.
  #
  # The test pins what the figure IS MADE OF instead. `renderer` holds the data
  # frame each renderer received, and that is the check worth keeping.
  for (case in names(.xp_expected$cases)) {
    want <- .xp_expected$cases[[case]]
    got <- .xp_actual[[case]]

    expect_identical(
      got$image_names,
      want$image_names,
      info = paste0(case, ": image inventory")
    )

    for (img in want$image_names) {
      # DIMENSIONS for every image, whatever its format. A PNG carries its
      # pixel size in the IHDR chunk. A PDF carries its page size in points,
      # inside a compressed object stream that `.xp_pdf_dim()` inflates.
      #
      # This is a WEAK check and it is kept because it costs nothing. The
      # `IHDR` was byte-identical under both rasterisers and both font sets
      # measured, so a dimension is portable where a digest is not.
      expect_identical(
        got$images[[img]]$dim,
        want$images[[img]]$dim,
        info = paste0(case, ": image '", img, "' dimensions")
      )
      # A stored dimension of `NA` would make the line above pass on nothing,
      # so every recorded dimension MUST be a real number.
      expect_false(
        anyNA(want$images[[img]]$dim),
        info = paste0(case, ": image '", img, "' has no recorded dimensions")
      )
      # A file of zero bytes would satisfy the inventory and the dimensions,
      # so every image MUST carry bytes.
      expect_gt(got$images[[img]]$size, 0)
    }

    # The DATA each renderer received. This is the figure's content, before a
    # rasteriser turns it into pixels, so it holds on every machine.
    for (renderer in c("forest", "overlay", "love", "consort")) {
      expect_equal(
        got$renderer[[renderer]],
        want$renderer[[renderer]],
        info = paste0(case, ": renderer input '", renderer, "'")
      )
    }
  }
})


# Every function outside the accessor module that MAY still name one of the
# three stored result lists, and why. A function absent from this list MUST NOT
# name any of them: the accessors are the one route from a plan to its results.
#
# `$export_tables()` appears here as a cache inspector, and for that reason
# only. It calls `.baseline_panel_is_stale()` over `results_enrollment` to
# decide whether to refresh, and it reads `results_enrollment` again to choose
# the Table 1 enrollment by the largest `n_baseline`. Every NUMBER it reports
# comes from an accessor.
.XP_SLOT_ALLOWLIST <- c(
  # The accessor module itself.
  ".acc_attrition" = "accessor module",
  ".acc_baselines" = "accessor module",
  ".acc_curves" = "accessor module",
  ".acc_estimate_rows" = "accessor module",
  ".acc_estimates" = "accessor module",
  ".acc_matching" = "accessor module",
  ".acc_ett_labels" = "accessor module",
  ".acc_subgroups" = "accessor module",
  # Producers and the persistence layer. These WRITE the lists.
  "TTEPlan$public_methods$s1_generate_enrollments_and_ipw" = "producer",
  "TTEPlan$public_methods$s3_analyze" = "producer",
  "TTEPlan$public_methods$recompute_baselines" = "producer",
  ".restore_enrollment_counts" = "persistence loader",
  "tteplan_load" = "persistence loader",
  # Readers of the KEYS. They answer "did this stage run", not "what does it
  # report", and no accessor answers that.
  ".plan_analysed_ett_ids" = "key reader",
  ".plan_analysed_enrollment_ids" = "key reader",
  ".plan_counted_enrollment_ids" = "key reader",
  # A DIAGNOSTIC. `$results_summary()` reports which slots are absent and which
  # were skipped, with the reason. A tool that reports ABSENCE cannot read
  # through an interface that hides absence: the accessors report a missing
  # slot and a skipped slot the same way, and expose no skip envelope. It
  # reports on the CACHE and never on a number.
  "TTEPlan$public_methods$results_summary" = "diagnostic",
  # A reporter ON THE CACHE. `$export_tables()` calls
  # `.baseline_panel_is_stale()` over `results_enrollment` to decide whether to
  # refresh, and it tests both lists for emptiness. Every NUMBER it reports
  # comes from an accessor, and `.XP_SLOT_LINES` pins that.
  "TTEPlan$public_methods$export_tables" = "cache inspector",
  # A filename literal, not a read.
  ".enrollment_counts_path" = "filename literal",
  # Unreachable. Nothing in the package calls these three, and they hand raw
  # slot objects to `tteenrollment_rates_combine()`, which no accessor feeds.
  ".prepare_combine_data" = "unreachable",
  ".write_combined_rates_irr" = "unreachable"
)

# The participant-flow renderers used to sit on this list. They read
# `plan$enrollment_counts` because `.build_cohort_flow()` needs the matching
# block and the per-trial attrition rows, and `$get_attrition()` returned the
# global rows only. `$get_attrition()` now returns every stored row and
# `$get_matching()` returns the matching block, so `.write_attrition_sheet()`,
# `.format_enrollment_summary()`, `$print_target_checklist()` and
# `.export_figure()` all read an accessor. The class is empty, and the second
# assertion below fails if any of them returns to it.



# The EXACT lines of each allowlisted function that name one of the three
# stored lists. A name-level allowlist exempts a whole function, so a NEW read
# added inside an exempted function is invisible to it. This pins the lines
# themselves, so a `$results_enrollment$n_baseline` read is caught as loudly as
# a `results_enrollment[[eid]]` one.
#
# `deparse(removeSource(f))` is normalised, so `air format` does not move these
# lines. The accessor module is not pinned here: it is the permitted route, and
# its internals MAY grow.
.XP_SLOT_LINES <- list(
  `.enrollment_counts_path` = c(
    "file.path(output_dir, paste0(prefix, \"_enrollment_counts_\","
  ),
  `.plan_analysed_enrollment_ids` = c(
    "ids <- names(plan$results_enrollment)"
  ),
  `.plan_analysed_ett_ids` = c(
    "ids <- names(plan$results_ett)"
  ),
  `.plan_counted_enrollment_ids` = c(
    "ids <- names(plan$enrollment_counts)"
  ),
  `.prepare_combine_data` = c(
    "results <- plan$results_ett"
  ),
  `.restore_enrollment_counts` = c(
    "if (!is.null(plan$enrollment_counts[[eid]])) {",
    "counts_path <- .enrollment_counts_path(output_dir, plan$project_prefix,",
    "plan$enrollment_counts[[eid]] <- qs2_read(counts_path)"
  ),
  `.write_combined_rates_irr` = c(
    "results <- plan$results_ett"
  ),
  `tteplan_load` = c(
    "fields <- c(\"spec\", \"enrollment_counts\", \"period_width\",",
    "\"output_dir\", \"results_enrollment\", \"results_ett\", \"spec_reloaded_at\",",
    "if (is.null(plan$enrollment_counts)) {",
    "plan$enrollment_counts <- list()",
    ".restore_enrollment_counts(plan, plan$output_dir, unique(plan$ett$enrollment_id))"
  ),
  `TTEPlan$public_methods$s1_generate_enrollments_and_ipw` = c(
    "if (is.null(self$enrollment_counts)) {",
    "self$enrollment_counts <- list()",
    ".restore_enrollment_counts(self, output_dir, enrollment_ids)",
    "counts_path <- .enrollment_counts_path(output_dir, self$project_prefix,",
    "eid), counts = .enrollment_counts_path(out_abs, self$project_prefix,",
    "self$enrollment_counts[[eid]] <- qs2_read(counts_path)"
  ),
  `TTEPlan$public_methods$s3_analyze` = c(
    "if (is.null(self$results_enrollment)) {",
    "self$results_enrollment <- list()",
    "if (is.null(self$results_ett)) {",
    "self$results_ett <- list()",
    "self$results_enrollment <- list()",
    "self$results_ett <- list()",
    "self$results_enrollment[[eid]] <- NULL",
    "self$results_ett[[eid]] <- NULL",
    "self$results_enrollment[[enr_todo[i]]] <- enr_results[[i]]",
    "if (is.null(self$results_ett[[eid]])) {",
    "self$results_ett[[eid]] <- list(enrollment_id = ett_todo$enrollment_id[m$ett_i],",
    "self$results_ett[[eid]][[k]] <- all_results[[j]][[k]]"
  ),
  `TTEPlan$public_methods$results_summary` = c(
    "if (is.null(self$results_ett) || length(self$results_ett) ==",
    "rows <- lapply(names(self$results_ett), function(ett_id) {",
    "r <- self$results_ett[[ett_id]]",
    "if (!is.null(self$results_enrollment)) {",
    "length(self$results_enrollment), length(unique(self$ett$enrollment_id))))"
  ),
  `TTEPlan$public_methods$recompute_baselines` = c(
    "if (is.null(self$results_enrollment) || length(self$results_enrollment) ==",
    "enrollment_ids <- names(self$results_enrollment)",
    "prev <- self$results_enrollment[[eid]]",
    "self$results_enrollment[[eid]] <- new_result"
  ),
  `TTEPlan$public_methods$export_tables` = c(
    "if (is.null(self$results_enrollment) || length(self$results_enrollment) ==",
    "if (is.null(self$results_ett) || length(self$results_ett) ==",
    "stale <- vapply(self$results_enrollment, .baseline_panel_is_stale,"
  )
)

test_that("no consumer reads a result slot directly", {
  pattern <- "results_ett|results_enrollment|enrollment_counts"
  ns <- asNamespace("swereg")

  named_hits <- function() {
    hits <- character(0)
    for (nm in ls(ns, all.names = TRUE)) {
      obj <- tryCatch(get(nm, envir = ns), error = function(e) NULL)
      if (!is.function(obj)) {
        next
      }
      src <- deparse(utils::removeSource(obj))
      if (any(grepl(pattern, src))) {
        hits <- c(hits, nm)
      }
    }
    generator <- get("TTEPlan", envir = ns)
    for (kind in c("public_methods", "private_methods")) {
      methods <- generator[[kind]]
      for (nm in names(methods)) {
        src <- deparse(utils::removeSource(methods[[nm]]))
        if (any(grepl(pattern, src))) {
          hits <- c(hits, paste0("TTEPlan$", kind, "$", nm))
        }
      }
    }
    sort(hits)
  }

  found <- named_hits()
  unlisted <- setdiff(found, names(.XP_SLOT_ALLOWLIST))
  expect_identical(
    unlisted,
    character(0),
    info = paste0(
      "these read a stored result list and are not on the allowlist: ",
      paste(unlisted, collapse = ", ")
    )
  )

  # The allowlist is a record, so a name that stops reading the lists MUST be
  # taken off it. A stale entry would hide the next one.
  stale <- setdiff(names(.XP_SLOT_ALLOWLIST), found)
  expect_identical(
    stale,
    character(0),
    info = paste0(
      "these are on the allowlist and no longer read a stored result list: ",
      paste(stale, collapse = ", ")
    )
  )

  # Every line of an exempted function that names one of the three lists. A
  # new read inside an exempted function fails here, whatever its syntax.
  for (nm in names(.XP_SLOT_LINES)) {
    obj <- if (startsWith(nm, "TTEPlan$")) {
      parts <- strsplit(nm, "$", fixed = TRUE)[[1L]]
      get("TTEPlan", envir = ns)[[parts[2L]]][[parts[3L]]]
    } else {
      get(nm, envir = ns)
    }
    got <- trimws(grep(pattern, deparse(utils::removeSource(obj)), value = TRUE))
    expect_identical(got, .XP_SLOT_LINES[[nm]], info = nm)
  }

  # The writers and builders of the export path read an accessor and nothing
  # else. The last three are the participant-flow renderers, which read
  # `plan$enrollment_counts` until `$get_attrition()` returned every stored row
  # and `$get_matching()` was added.
  for (nm in c(
    ".write_enrollment_overview",
    ".write_ett_overview",
    ".write_results_single",
    ".write_combined_sensitivity",
    ".write_effect_modification",
    ".write_combined_baseline",
    ".build_forest_df",
    ".build_itt_vs_pp_df",
    ".plan_cohort_counts",
    ".write_attrition_sheet",
    ".format_enrollment_summary"
  )) {
    src <- deparse(utils::removeSource(get(nm, envir = ns)))
    expect_false(
      any(grepl(pattern, src)),
      info = paste0(nm, " still names a stored result list")
    )
  }
})


test_that(".prepare_combine_data survives an absent description", {
  # One emulated trial with no stored `description` used to stop the whole
  # call, including the trials the caller asked for. The description now comes
  # from `plan$ett`, which holds one row per trial.
  plan <- .xp_plan("new")
  plan$results_ett[["ETT00002"]]$description <- NULL
  plan$results_ett[["ETT00003"]]$description <- c("two", "strings")

  prep <- swereg:::.prepare_combine_data(plan, "rates_pp_trunc")
  expect_false(is.null(prep))
  expect_true("ETT00002" %in% names(prep$wrapped))
  expect_identical(prep$ett_desc[["ETT00002"]], "ETT00002")
  expect_identical(prep$ett_desc[["ETT00003"]], "ETT00003")

  # And a subset the caller asked for is unaffected by a neighbour's gap.
  kept <- swereg:::.prepare_combine_data(
    plan,
    "rates_pp_trunc",
    keep_ett_ids = c("ETT00001", "ETT00002")
  )
  expect_identical(names(kept$wrapped), c("ETT00001", "ETT00002"))
  expect_identical(
    unname(kept$ett_desc),
    c("ETT00001", "ETT00002")
  )
})


test_that(".ff_irr_ci reads the stored estimability decision", {
  # The stored decision wins. A ratio the producer called inestimable prints an
  # empty cell whatever its value, and one it called estimable prints.
  expect_identical(swereg:::.ff_irr_ci(0.49, 0.30, 0.81, TRUE), "0.49 (0.30 to 0.81)")
  expect_identical(swereg:::.ff_irr_ci(0.49, 0.30, 0.81, FALSE), "")

  # No stored decision. `.tte_irr_estimable()` answers, so a result cached
  # before the column existed renders what it always did.
  expect_identical(swereg:::.ff_irr_ci(0.49, 0.30, 0.81, NA), "0.49 (0.30 to 0.81)")
  expect_identical(swereg:::.ff_irr_ci(0, 0, 0, NA), "")
  expect_identical(swereg:::.ff_irr_ci(0.005, 0.001, 0.02, NA), "")

  # The upper cap is a DISPLAY convention and is not the estimability
  # decision. A ratio of 150 is estimable, and it prints as the cap.
  expect_true(swereg:::.tte_irr_estimable(150))
  expect_identical(swereg:::.ff_irr_ci(150, 21, 1071, TRUE), ">100")

  # `.tte_irr_estimable()` holds the threshold, and nothing else does.
  src <- deparse(
    utils::removeSource(swereg:::.ff_irr_ci)
  )
  expect_false(any(grepl("0\\.01", src)))
})


test_that("the cohort flow reads through the accessor", {
  # `.build_cohort_flow()` needs the per-trial attrition rows and the matching
  # block. `.plan_cohort_counts()` reads both through the accessors and renames
  # them to the producer's column names. It selects and renames, and it sums
  # nothing.
  plan <- .xp_plan("new")
  counts <- swereg:::.plan_cohort_counts(plan, "01")
  stored <- plan$enrollment_counts[["01"]]

  expect_identical(nrow(counts$attrition), nrow(stored$attrition))
  expect_identical(counts$attrition$trial_id, stored$attrition$trial_id)
  expect_identical(
    counts$attrition$criterion,
    as.character(stored$attrition$criterion)
  )
  expect_identical(
    counts$attrition$n_persons,
    as.numeric(stored$attrition$n_persons)
  )
  expect_identical(nrow(counts$matching), nrow(stored$matching))
  expect_identical(
    counts$matching$n_comparator_enrolled,
    as.numeric(stored$matching$n_comparator_enrolled)
  )

  # Enrollment 02 stores NO matching block, and the accessor invents none.
  counts02 <- swereg:::.plan_cohort_counts(plan, "02")
  expect_null(counts02$matching)
  expect_false(is.null(counts02$attrition))

  # And the flow the renderer builds from accessor output is the flow it built
  # from the raw stored tables. This is what the Attrition sheet and the
  # CONSORT diagram both draw.
  for (eid in c("01", "02")) {
    from_accessor <- swereg:::.build_cohort_flow(
      swereg:::.plan_cohort_counts(plan, eid),
      analysis_n = 1000,
      analysis_n_intervention = 300,
      analysis_n_comparator = 700
    )
    from_stored <- swereg:::.build_cohort_flow(
      plan$enrollment_counts[[eid]],
      analysis_n = 1000,
      analysis_n_intervention = 300,
      analysis_n_comparator = 700
    )
    expect_equal(from_accessor, from_stored, info = eid)
  }
})


test_that("the provenance sheet survives an absent timestamp", {
  # `format(NA, "%Y-%m-%d %H:%M:%S")` reads the format string as the `trim`
  # argument of `format.default()` and stops. A TTEPlan built without a
  # RegistryStudy carries three NULL timestamps, so it could not export at all.
  plan <- .xp_plan("new")
  plan$registry_study_created_at <- NULL
  plan$skeleton_created_at <- NULL
  plan$created_at <- NULL

  dir <- withr::local_tempdir()
  path <- file.path(dir, "tables.xlsx")
  expect_no_error(suppressMessages(suppressWarnings(
    plan$export_tables(path = path)
  )))
  expect_true(file.exists(path))

  prov <- openxlsx::read.xlsx(
    path,
    sheet = "Provenance",
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  # The three rows are present and their value cells are empty.
  for (item in c(
    "RegistryStudy created",
    "Skeletons created",
    "TTEPlan created"
  )) {
    hit <- which(!is.na(prov[[1L]]) & prov[[1L]] == item)
    expect_length(hit, 1L)
    expect_true(is.na(prov[[2L]][hit]), info = item)
  }
})


test_that("supported states keep their content", {
  # Four stored-result states that the eight snapshot cases do not reach. Each
  # is supported, and each lost a cell when the export path first moved onto
  # the accessors. The cells below are the ones that blanked.
  export <- function(mutate) {
    plan <- mutate(.xp_plan("new"))
    dir <- withr::local_tempdir(.local_envir = parent.frame(2))
    path <- file.path(dir, "tables.xlsx")
    suppressMessages(suppressWarnings(
      plan$export_tables(path = path)
    ))
    sheets <- .xp_read_sheets(path)
    sheets$sheets
  }
  cell <- function(sheet, row, col) as.character(sheet[[col]][row])

  # 1. Counts stored, NO baseline panel. `.baseline_panel_is_stale()` calls
  # that result CURRENT, so the enrollment reaches the sheets and its stored
  # size MUST reach them with it.
  s <- export(function(plan) {
    for (nm in c(
      "table1_raw", "table1_unweighted", "table1_ipw",
      "table1_ipw_trunc", "table1_ipw_trunc_main"
    )) {
      plan$results_enrollment[["02"]][[nm]] <- NULL
    }
    plan
  })
  expect_identical(cell(s$Enrollments, 3L, "C7"), "640")
  # And the CONSORT analysis step, which is built from the same stored count.
  expect_identical(cell(s$Attrition_02, 7L, "C1"), "analysis_dataset")
  expect_identical(cell(s$Attrition_02, 7L, "C4"), "640")

  # 2. The specification names no arms. The panel headers belong to the numbers
  # the panel holds, so they come from the stored panel and not from the
  # specification as it stands today.
  s <- export(function(plan) {
    for (i in seq_along(plan$spec$enrollments)) {
      plan$spec$enrollments[[i]]$treatment$arms <- NULL
    }
    plan
  })
  expect_identical(cell(s[["Table 1"]], 3L, "C4"), "Untreated")
  expect_identical(cell(s[["Table 1"]], 3L, "C5"), "Treated")

  # 3. A stored rate ratio whose four values are all NA. The arm counts are
  # still reportable, so the row stays and only the ratio cells blank.
  s <- export(function(plan) {
    na_irr <- data.table::data.table(
      IRR = NA_real_, IRR_lower = NA_real_, IRR_upper = NA_real_,
      IRR_pvalue = NA_real_, warn = FALSE
    )
    data.table::setattr(na_irr, "swereg_type", "irr")
    plan$results_ett[["ETT00003"]]$irr_pp_trunc <- na_irr
    plan
  })
  expect_identical(cell(s[["PP results"]], 6L, "C1"), "Enrollment two")
  expect_identical(cell(s[["PP results"]], 6L, "C6"), "10.4")
  expect_identical(cell(s[["PP results"]], 6L, "C7"), "62816")
  expect_true(is.na(s[["PP results"]][["C12"]][6L]))

  # 4. A stored result holding a summary and no estimate slot. The summary is a
  # stored slot, so the event count still reaches the `ETTs` sheet.
  s <- export(function(plan) {
    plan$results_ett[["ETT00003"]] <- list(
      enrollment_id = "02",
      description = "ETT00003",
      summary = list(n_events = 31L)
    )
    plan
  })
  expect_identical(cell(s$ETTs, 4L, "C1"), "ETT00003")
  expect_identical(cell(s$ETTs, 4L, "C7"), "31")
})


test_that("a stored all-NA shape keeps its row", {
  # The class this closes: a consumer that decides "the plan stored nothing"
  # from a missing VALUE rather than from the stored SHAPE. Every stored table
  # below is well formed, and every value inside it is `NA`. The row belongs on
  # the sheet, with blank cells where the numbers are missing.
  export <- function(mutate) {
    plan <- mutate(.xp_plan("new"))
    dir <- withr::local_tempdir(.local_envir = parent.frame(2))
    path <- file.path(dir, "tables.xlsx")
    suppressMessages(suppressWarnings(
      plan$export_tables(path = path)
    ))
    .xp_read_sheets(path)$sheets
  }
  cell <- function(sheet, row, col) as.character(sheet[[col]][row])

  # 1. A rates table of the right shape whose six numbers are all NA, beside a
  # valid rate ratio. The emulated trial keeps its identifiers and its ratio.
  s <- export(function(plan) {
    na_rates <- .xp_rates(
      NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
    )
    for (nm in c("rates_pp_trunc", "rates_pp", "rates_itt")) {
      plan$results_ett[["ETT00003"]][[nm]] <- na_rates
    }
    plan
  })
  expect_identical(cell(s[["PP results"]], 6L, "C1"), "Enrollment two")
  expect_identical(cell(s[["PP results"]], 6L, "C4"), "Outcome A")
  expect_identical(cell(s[["PP results"]], 6L, "C12"), "0.81")
  expect_true(is.na(s[["PP results"]][["C6"]][6L]))
  expect_identical(cell(s[["ITT results"]], 6L, "C1"), "Enrollment two")

  # 2. One stored subgroup level whose per-protocol estimates are all NA. The
  # level stays, so the intention-to-treat result beside it survives. A real
  # finding MUST NOT disappear because a different estimand failed.
  s <- export(function(plan) {
    plan$results_ett[["ETT00001"]]$subgroup_rd_age_band_pp <- .xp_subgroup(
      c("all", "younger", "older"),
      c(0.54, 0.61, NA_real_), c(0.40, 0.42, NA_real_),
      c(0.71, 0.88, NA_real_), c(1e-7, 0.008, NA_real_)
    )
    plan
  })
  expect_identical(cell(s[["Effect modification"]], 6L, "C4"), "older")
  expect_true(is.na(s[["Effect modification"]][["C5"]][6L]))
  expect_identical(cell(s[["Effect modification"]], 6L, "C7"), "0.59")

  # 3. A Table 1 panel that carries an SMD column whose every value is NA. The
  # column is part of the stored panel, so the sheet keeps it.
  s <- export(function(plan) {
    blank <- function(t1) {
      if (is.null(t1)) {
        return(NULL)
      }
      out <- data.table::copy(t1)
      data.table::set(out, j = "smd_numeric", value = NA_real_)
      data.table::set(out, j = "SMD", value = "")
      out
    }
    for (eid in c("01", "02")) {
      for (nm in c(
        "table1_raw", "table1_unweighted", "table1_ipw",
        "table1_ipw_trunc", "table1_ipw_trunc_main"
      )) {
        plan$results_enrollment[[eid]][[nm]] <- blank(
          plan$results_enrollment[[eid]][[nm]]
        )
      }
    }
    plan
  })
  expect_identical(cell(s[["Table 1"]], 3L, "C6"), "SMD")
})


test_that("an all-NA risk difference survives to its consumer", {
  # `.tte_rd_lookup()` feeds the risk-difference columns of a `$export()`
  # forest figure. It decides which emulated trials have a stored risk
  # difference, and it MUST decide that from the stored shape.
  plan <- .xp_plan("new")
  rd <- data.table::copy(plan$results_ett[["ETT00001"]]$rd_pp_trunc)
  for (nm in c(
    "rd", "rd_lo", "rd_hi", "nnt",
    "n_persons_with_event_intervention", "n_persons_with_event_comparator"
  )) {
    data.table::set(rd, j = nm, value = NA_real_)
  }
  data.table::set(rd, j = "nnt_direction", value = NA_character_)
  # `conf_level` is stored metadata, not an estimate, so it stays.
  plan$results_ett[["ETT00001"]]$rd_pp_trunc <- rd

  lookup <- swereg:::.tte_rd_lookup(plan, "rd_pp_trunc", "ETT00001")
  expect_false(is.null(lookup))
  expect_identical(nrow(lookup), 1L)
  expect_identical(lookup$ett_id, "ETT00001")
  expect_true(is.na(lookup$rd))
  expect_identical(lookup$conf_level, 0.95)

  # And the same row reaches `$export()`. The row carries `conf_level = 0.95`,
  # and the study now declares 0.90, so `.forest_rd_conf_level()` MUST refuse
  # to head the column with a level the numbers do not have. A consumer that
  # dropped the row would disable that guard in silence.
  plan$spec$study$implementation$conf_level <- 0.90
  dir <- withr::local_tempdir()
  expect_warning(
    suppressMessages(plan$export(
      list(list(
        type = "forest",
        label = "forest",
        exposures = list(`Group one` = "ETT00001"),
        estimands = "pp",
        risk_difference = TRUE
      )),
      dir = dir
    )),
    "disagrees with the level the intervals were computed at"
  )
})
