# Stage outputs of one real s0 -> s1 -> s2 -> s3 run.
#
# The fixture is helper-sparse_report.R: at min_n = 10 the rule excludes
# "Prior rare condition B" and adjusts for "Prior common condition". The run
# is built once for the file, in this order:
#
#   1. s0. `$excel_spec_summary()` writes the preregistration workbook.
#   2. s1. The workbook gains the Rare conditions sheet.
#   3. s2 and s3. The workbook is written again, beside `$export_tables()`.
#      The plan is saved and loaded back.
#   4. A copy of the plan gets an excluded condition in each supplementary
#      baseline panel, and `$export_tables()` writes its Table S2.
#   5. s2 runs again with other IPCW options. The results stored before it
#      are put back, and every writer is called on them.
#   6. s3 runs again, and `$export_tables()` writes.
#
# Every step catches its own error, so a test reads one stored outcome and a
# failure names the test it belongs to.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("openxlsx")

sto <- new.env(parent = emptyenv())

# The protocol sheet documents the second ETT, so the default ETT of either
# writer cannot make the sheets agree by accident.
STO_PROTOCOL_ETT <- "ETT00002"
STO_PREREG_SHEETS <- c(
  "Study Specification",
  "Rare conditions",
  "Target trial protocol"
)
STO_EXCLUDED_VAR <- "rd_no_osd_rare_b_everbefore"

sto_try <- function(expr) {
  return(tryCatch(
    {
      suppressWarnings(invisible(utils::capture.output(
        value <- expr,
        type = "output"
      )))
      list(value = value, error = NULL)
    },
    error = function(e) list(value = NULL, error = conditionMessage(e))
  ))
}

sto_spec_workbook <- function(plan, name, protocol_ett_id = NULL) {
  path <- file.path(sto$root, paste0(name, ".xlsx"))
  return(sto_try(suppressMessages({
    plan$excel_spec_summary(path = path, protocol_ett_id = protocol_ett_id)
    path
  })))
}

# Each supplementary panel of enrollment "01" gets the rows of the adjusted
# condition again, under the excluded variable.
sto_inject_excluded <- function(plan) {
  slots <- c("table1_raw", "table1_unweighted", "table1_ipw", "table1_ipw_trunc")
  for (slot in slots) {
    panel <- plan$results_enrollment[["01"]][[slot]]
    if (is.null(panel)) {
      next
    }
    hit <- which(panel[[1]] == "rd_no_osd_common_everbefore")
    extra <- panel[c(hit, hit + 1L)]
    extra[[1]][1L] <- STO_EXCLUDED_VAR
    added <- rbind(panel, extra)
    data.table::setattr(added, "class", class(panel))
    plan$results_enrollment[["01"]][[slot]] <- added
  }
  return(plan)
}

sto_run <- function() {
  if (isTRUE(sto$done)) {
    return(sto)
  }
  sto$root <- withr::local_tempdir(.local_envir = testthat::teardown_env())
  dirs <- file.path(sto$root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  sk <- spr_skeleton()
  skel_path <- file.path(dirs[2], SPR_SKELETON)
  qs2::qs_save(sk, skel_path)
  spr_write_spec(spr_report_spec(spr_report_rule()), dirs[1])
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs[4]),
    candidate_dir_spec = dirs[1],
    candidate_dir_tteplan = dirs[2],
    candidate_dir_results = dirs[3],
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  ))
  dev_path <- swereg:::.swereg_dev_path()
  s2 <- function(gam) {
    return(sto_try(plan$s2_generate_analysis_files_and_ipcw_pp(
      n_workers = 1L,
      swereg_dev_path = dev_path,
      estimate_ipcw_pp_with_gam = gam
    )))
  }
  s3 <- function() {
    return(sto_try(plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path)))
  }

  # 1. s0.
  sto$spec_s0 <- sto_spec_workbook(plan, "spec_s0")

  # 2. s1.
  sto$s1 <- sto_try(plan$s1_generate_enrollments_and_ipw(
    n_workers = 1L,
    swereg_dev_path = dev_path,
    check_skeletons = FALSE
  ))
  sto$spec_s1 <- sto_spec_workbook(plan, "spec_s1", STO_PROTOCOL_ETT)

  # 3. s2 and s3, the two workbooks, and a save and load.
  sto$s2_first <- s2(TRUE)
  sto$s2_first_ids <- plan$ett$s2_run_id
  sto$s3_first <- s3()
  sto$spec_s3 <- sto_spec_workbook(plan, "spec_s3", STO_PROTOCOL_ETT)
  tables_path <- file.path(sto$root, "tables_s3.xlsx")
  sto$tables_s3 <- sto_try({
    plan$export_tables(path = tables_path, protocol_ett_id = STO_PROTOCOL_ETT)
    tables_path
  })
  sto$stored_s3 <- list(
    results_enrollment = plan$results_enrollment,
    results_ett = plan$results_ett,
    ett = data.table::copy(plan$ett)
  )
  sto$loaded <- sto_try(swereg::tteplan_load(plan$save()))

  # 3b. A copy whose rule changes after the run, with no new s1.
  stale <- plan$clone(deep = TRUE)
  stale$spec$rare_condition_rule$min_n <- 5L
  sto$stale_tables <- sto_try(stale$export_tables(
    path = file.path(sto$root, "tables_stale.xlsx")
  ))
  sto$stale_export <- sto_try(stale$export(
    list(list(type = "table")),
    dir = file.path(sto$root, "export_stale")
  ))
  sto$stale_checklist <- sto_try(stale$print_target_checklist())
  sto$spec_stale <- sto_spec_workbook(stale, "spec_stale", STO_PROTOCOL_ETT)
  sto$stale_print <- sto_try(gsub(
    "\033\\[[0-9;]*m",
    "",
    utils::capture.output(stale$print_spec_summary())
  ))
  sto$outcome_formula <- attr(
    plan$results_ett[[1L]][["irr_pp_trunc"]],
    "model_formula"
  )
  sto$confounder_vars <- unique(unlist(plan$ett$confounder_vars))

  # 4. Table S2 of a copy with an excluded condition in its panels.
  copy <- sto_inject_excluded(plan$clone(deep = TRUE))
  sto$injected_baselines <- copy$get_baselines()
  s2_path <- file.path(sto$root, "tables_injected.xlsx")
  sto$tables_injected <- sto_try({
    copy$export_tables(
      path = s2_path,
      table1_enrollment = "01",
      protocol_ett_id = STO_PROTOCOL_ETT
    )
    s2_path
  })

  # 5. s2 again, with other IPCW options, then the results of step 3 back.
  sto$s2_second <- s2(FALSE)
  sto$after_s2 <- list(
    results_enrollment = plan$results_enrollment,
    results_ett = plan$results_ett,
    s2_run_id = plan$ett$s2_run_id
  )
  plan$results_enrollment <- sto$stored_s3$results_enrollment
  plan$results_ett <- sto$stored_s3$results_ett
  sto$refused_tables <- sto_try(plan$export_tables(
    path = file.path(sto$root, "tables_refused.xlsx")
  ))
  sto$refused_export <- sto_try(plan$export(
    list(list(type = "table")),
    dir = file.path(sto$root, "export_refused")
  ))
  sto$refused_checklist <- sto_try(plan$print_target_checklist())

  # 6. s3 again.
  sto$s3_second <- s3()
  tables_path <- file.path(sto$root, "tables_rerun.xlsx")
  sto$tables_rerun <- sto_try({
    plan$export_tables(path = tables_path)
    tables_path
  })
  sto$plan <- plan
  sto$done <- TRUE
  return(sto)
}

# Every cell of one sheet, as text.
sto_cells <- function(path, sheet) {
  return(openxlsx::read.xlsx(
    path,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  ))
}


test_that("the stages of the run complete", {
  s <- sto_run()
  for (step in c("s1", "s2_first", "s3_first", "s2_second", "s3_second")) {
    expect_null(s[[step]]$error, label = step)
  }
})


test_that("after s0 the preregistration workbook holds the specification and the protocol", {
  s <- sto_run()
  expect_null(s$spec_s0$error)
  expect_identical(
    openxlsx::getSheetNames(s$spec_s0$value),
    c("Study Specification", "Target trial protocol")
  )
  # Before s1 the protocol says the rule is not decided yet.
  protocol <- sto_cells(s$spec_s0$value, "Target trial protocol")
  expect_true(any(grepl(
    "Rare-condition rule: not decided yet (s1 has not run)",
    unlist(protocol),
    fixed = TRUE
  )))
})


test_that("after s1 the preregistration workbook adds the Rare conditions sheet", {
  s <- sto_run()
  expect_null(s$spec_s1$error)
  expect_identical(
    openxlsx::getSheetNames(s$spec_s1$value),
    STO_PREREG_SHEETS
  )
})


test_that("the preregistration sheets equal the sheets of export_tables", {
  s <- sto_run()
  expect_null(s$tables_s3$error)
  expect_true(all(
    STO_PREREG_SHEETS %in% openxlsx::getSheetNames(s$tables_s3$value)
  ))
  for (sheet in STO_PREREG_SHEETS) {
    want <- sto_cells(s$tables_s3$value, sheet)
    # The same plan state.
    expect_identical(sto_cells(s$spec_s3$value, sheet), want, label = sheet)
    # After s1, before s2 and s3: no s2 or s3 result reaches these sheets.
    expect_identical(sto_cells(s$spec_s1$value, sheet), want, label = sheet)
  }
  protocol <- sto_cells(s$tables_s3$value, "Target trial protocol")
  expect_match(protocol[[1]][1L], STO_PROTOCOL_ETT, fixed = TRUE)
})


test_that("the outcome model holds the time terms and no confounder", {
  s <- sto_run()
  # The text of the Study Specification and of TARGET 6g says so.
  vars <- all.vars(s$outcome_formula)
  expect_true(all(c("tstart", "enrollment_period_id") %in% vars))
  expect_length(intersect(vars, s$confounder_vars), 0L)
  expect_true(length(s$confounder_vars) > 0L)
})


test_that("the supplementary baseline sheets drop the excluded conditions and carry the footnote", {
  s <- sto_run()
  # The copy's stored supplementary panels hold the excluded condition.
  b <- s$injected_baselines
  expect_true(any(
    b$enrollment_id == "01" &
      b$variant == "supplementary" &
      b$variable == STO_EXCLUDED_VAR
  ))
  expect_null(s$tables_injected$error)
  x <- sto_cells(s$tables_injected$value, "Table S2")
  expect_match(x[[1]][1L], "Enrollment 01", fixed = TRUE)
  expect_false(any(grepl(STO_EXCLUDED_VAR, unlist(x), fixed = TRUE)))
  # The adjusted condition is still there.
  expect_true(any(grepl("rd_no_osd_common_everbefore", unlist(x), fixed = TRUE)))
  col1 <- x[[1]][!is.na(x[[1]])]
  expect_identical(
    col1[length(col1)],
    paste0(
      "Not shown: Prior rare condition B. The rare-condition rule made each ",
      "of these conditions an exclusion criterion. See the Rare ",
      "conditions sheet."
    )
  )
})


test_that("s3 stamps each stored result with the s2 run of its ETT", {
  s <- sto_run()
  ids <- s$stored_s3$ett$s2_run_id
  expect_false(anyNA(ids))
  expect_identical(ids, s$s2_first_ids)
  for (k in names(s$stored_s3$results_ett)) {
    expect_identical(
      s$stored_s3$results_ett[[k]][["s2_run_id"]],
      ids[s$stored_s3$ett$ett_id == k]
    )
  }
  for (k in names(s$stored_s3$results_enrollment)) {
    expect_true(
      s$stored_s3$results_enrollment[[k]][["s2_run_id"]] %in%
        ids[s$stored_s3$ett$enrollment_id == k]
    )
  }
})


test_that("a save and load round trip keeps the s2 run stamps", {
  s <- sto_run()
  expect_null(s$loaded$error)
  loaded <- s$loaded$value
  expect_identical(loaded$ett$s2_run_id, s$stored_s3$ett$s2_run_id)
  stamps <- function(results) {
    return(lapply(results, `[[`, "s2_run_id"))
  }
  expect_identical(
    stamps(loaded$results_ett),
    stamps(s$stored_s3$results_ett)
  )
  expect_identical(
    stamps(loaded$results_enrollment),
    stamps(s$stored_s3$results_enrollment)
  )
  expect_length(stamps(loaded$results_ett), 2L)
})


test_that("s2 drops the stored s3 results and stamps a new s2 run", {
  s <- sto_run()
  expect_null(s$after_s2$results_ett)
  expect_null(s$after_s2$results_enrollment)
  expect_false(anyNA(s$after_s2$s2_run_id))
  expect_false(any(s$after_s2$s2_run_id %in% s$s2_first_ids))
})


test_that("the writers refuse s3 results from before an s2 rerun", {
  s <- sto_run()
  stale <- paste0(
    "4 stored s3 result(s) do not come from the current s2 run of their ",
    "ETT: 01, 02, ETT00001, ETT00002. s2 ran after s3, and its options can ",
    "differ. Run $s3_analyze() after the last ",
    "$s2_generate_analysis_files_and_ipcw_pp()."
  )
  expect_identical(s$refused_tables$error, paste0("export_tables(): ", stale))
  expect_identical(s$refused_export$error, paste0("export(): ", stale))
  expect_identical(
    s$refused_checklist$error,
    paste0("print_target_checklist(): ", stale)
  )
})


test_that("export_tables writes again after s3 reruns", {
  s <- sto_run()
  expect_null(s$tables_rerun$error)
  expect_true(file.exists(s$tables_rerun$value))
})


STO_STALE_RULE <- paste0(
  ": the rare_condition_rule in plan$spec does not match the rule of the frozen ",
  "decision. The hash covers min_n, decided_by and candidates. Re-run s1: ",
  "its step s1b decides again from the current rule."
)
STO_STALE_PENDING <- "not decided yet (s1 has not completed for this rule)"


test_that("the result writers refuse a rule changed after the run", {
  s <- sto_run()
  expect_identical(s$stale_tables$error, paste0("export_tables()", STO_STALE_RULE))
  expect_identical(s$stale_export$error, paste0("export()", STO_STALE_RULE))
  expect_identical(
    s$stale_checklist$error,
    paste0("print_target_checklist()", STO_STALE_RULE)
  )
})


test_that("the preregistration workbook shows a rule changed after the run as not decided", {
  s <- sto_run()
  expect_null(s$spec_stale$error)
  path <- s$spec_stale$value
  expect_identical(
    openxlsx::getSheetNames(path),
    c("Study Specification", "Target trial protocol")
  )
  ss <- sto_cells(path, "Study Specification")
  expect_identical(ss[[2]][which(ss[[1]] == "Threshold (min_n):")], "5")
  expect_identical(ss[[2]][which(ss[[1]] == "Decision:")], STO_STALE_PENDING)
  expect_false(any(c("Excluded conditions:", "Final adjustment set:") %in% ss[[1]]))
  expect_false(any(grepl("excluded by the rare-condition rule", ss[[1]], fixed = TRUE)))
  protocol <- unlist(sto_cells(path, "Target trial protocol"))
  expect_true(any(grepl(
    paste0("Rare-condition rule: ", STO_STALE_PENDING),
    protocol,
    fixed = TRUE
  )))
  expect_false(any(grepl("Prior rare condition B at baseline", protocol, fixed = TRUE)))
})


test_that("print_spec_summary shows a rule changed after the run as not decided", {
  s <- sto_run()
  expect_null(s$stale_print$error)
  txt <- trimws(s$stale_print$value, which = "right")
  expect_true(paste0("  Decision:             ", STO_STALE_PENDING) %in% txt)
  expect_false(any(grepl("Excluded conditions:", txt, fixed = TRUE)))
  expect_false(any(grepl("Final adjustment set:", txt, fixed = TRUE)))
})
