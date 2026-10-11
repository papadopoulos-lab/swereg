# Fixtures for the reporting tests of the sparse_rule (test-sparse-report.R).
#
# The study is the fixture of helper-sparse_rule.R with two candidates:
# osd_rare_b (4 holders, all in the comparator arm, so the intervention arm of
# enrollment "01" holds 0) and osd_common. At min_n = 10 the rule excludes
# "Prior rare condition B" and adjusts for "Prior common condition".
# osd_rare_a is not a confounder here.

SPR_REPORT_EXCLUDED <- "Prior rare condition B"
SPR_REPORT_ADJUSTMENT_SET <- c("Age", "Prior common condition")

spr_report_rule <- function(min_n = 10L) {
  return(list(
    min_n = min_n,
    decided_by = "01",
    candidates = c(spr_var("osd_rare_b"), spr_var("osd_common"))
  ))
}

spr_report_spec <- function(rule = spr_report_rule()) {
  spec <- spr_spec_list(rule, project_prefix = "sprreport")
  # Age, Prior common condition and Prior rare condition B.
  spec$confounders <- spec$confounders[-2L]
  return(spec)
}

# Run the real s1 -> s2 -> s3 dispatch, save the plan, and load it back.
# Returns the loaded plan and the root directory.
spr_report_pipeline <- function(rule = spr_report_rule(), sk = spr_skeleton()) {
  root <- tempfile("sprrep_")
  dirs <- file.path(root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  skel_path <- file.path(dirs[2], SPR_SKELETON)
  qs2::qs_save(sk, skel_path)
  spr_write_spec(spr_report_spec(rule), dirs[1])
  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs[4]),
    candidate_dir_spec = dirs[1],
    candidate_dir_tteplan = dirs[2],
    candidate_dir_results = dirs[3],
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )
  dev_path <- swereg:::.swereg_dev_path()
  suppressWarnings(invisible(utils::capture.output(
    {
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev_path,
        check_skeletons = FALSE
      )
      plan$s2_generate_analysis_files_and_ipcw_pp(
        n_workers = 1L,
        swereg_dev_path = dev_path
      )
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path)
    },
    type = "output"
  )))
  loaded <- swereg::tteplan_load(plan$save())
  return(list(plan = loaded, root = root))
}

# A plan with a hand-made decision and no s1 run, from spr_plan_no_s1() of
# helper-sparse_rule.R. The counts per arm are rare_b 3, common 50 and rare_a
# 4, so at min_n = 10 the rule excludes rare_b and rare_a. The labels are the
# confounder names of the spec. `none_excluded = TRUE` marks every candidate
# adjusted. The plan carries the stamps of a completed s1 for that decision.
spr_report_fast_plan <- function(none_excluded = FALSE, env = parent.frame()) {
  plan <- spr_plan_no_s1(env = env)
  d <- data.table::copy(plan$sparse_decision)
  d[,
    label := c(
      "Prior rare condition B",
      "Prior common condition",
      "Prior rare condition A"
    )
  ]
  if (none_excluded) {
    d[, decision := "adjusted"]
  }
  plan$sparse_decision <- d
  return(tte_stamp_stages(plan))
}

# Write one sheet writer's output to a file and read it back as text cells.
spr_report_read_sheet <- function(write, sheet) {
  wb <- openxlsx::createWorkbook()
  write(wb)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  openxlsx::saveWorkbook(wb, path)
  return(openxlsx::read.xlsx(
    path,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE
  ))
}

# The value in column 2 of the Study Specification row labelled `label`.
spr_report_spec_value <- function(ss, label) {
  return(ss[[2]][which(ss[[1]] == label)])
}

# The TARGET checklist as plain lines, and one item's block of it.
spr_report_checklist <- function(plan) {
  txt <- utils::capture.output(plan$print_target_checklist())
  return(gsub("\033\\[[0-9;]*m", "", txt))
}
spr_report_item <- function(txt, item, next_item) {
  a <- grep(paste0("^Item ", item, "\\. "), txt)
  b <- grep(paste0("^Item ", next_item, "\\. "), txt)
  return(txt[a:(b - 1L)])
}

# The comma-joined list after `prefix` on the first line that holds it.
spr_report_list_after <- function(lines, prefix) {
  hit <- grep(prefix, lines, fixed = TRUE, value = TRUE)[1L]
  at <- regexpr(prefix, hit, fixed = TRUE)
  rest <- substr(hit, at + nchar(prefix), nchar(hit))
  rest <- sub(" \\(see the Sparse conditions sheet\\)$", "", rest)
  rest <- sub("\\.?\\s*$", "", rest)
  return(strsplit(rest, ", ", fixed = TRUE)[[1L]])
}

# The two phrases R10 forbids in any text that states the rule.
SPR_REPORT_FORBIDDEN <- c(
  "before any outcome was examined",
  "never looks at outcomes"
)
