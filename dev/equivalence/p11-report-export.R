#!/usr/bin/env Rscript

# Equivalence capture for the TTEPlan reporting and export code.
#
# Run it from the package root:
#   Rscript dev/equivalence/p11-report-export.R capture /tmp/p11-before.rds
#   Rscript dev/equivalence/p11-report-export.R compare /tmp/p11-before.rds
#
# `capture` builds the fixture plan, runs every reporting and export entry
# point, and writes twelve artefacts to the output file. `compare` rebuilds the
# same artefacts and reports identical() against the stored baseline, one
# artefact at a time. It exits with status 1 when any artefact differs.
#
# Every artefact here is something a study reads or ships. A refactor of the
# reporting and export code MUST leave all twelve identical. A dropped console
# line, a reordered workbook sheet or a changed figure is a different
# deliverable, and nothing downstream reports it as an error.
#
# THE FIXTURE
# `tests/testthat/helper-export_parity.R` builds the plan through
# `.xp_plan("new")`. It carries eight emulated trials over two enrollments and
# it sweeps the states the export path branches on. Its spec holds a study
# block and two enrollments, and nothing else.
#
# THE SECOND SPEC
# Three artefacts walk the spec: `$print_spec_summary()`,
# `$print_target_checklist()` and `$excel_spec_summary()`. The fixture spec
# leaves the follow-up, inclusion, exclusion, confounder and outcome loops
# empty, so each artefact is captured a second time against
# `tests/testthat/fixtures/spec_3x2x2.yaml`. That spec fills every loop. The
# second capture raises the spec summary from 33 to 59 lines. It raises the
# checklist from 270 to 289 lines, and the spec workbook from 37 to 62 rows.
#
# THE TWO EXPORTS
# `tests/testthat/fixtures/s4_export_shape.R` is the committed export script.
# It calls `$export()` and `$export_tables()` in the shape a study writes them.
# This script evaluates that file, then reads back the files it wrote.
#
# WHAT IS MASKED, AND WHY
# `$print_target_checklist()` prints `Sys.Date()`. Every line that carries
# today's date is rewritten to `<DATE>`. The fixture's own timestamps are
# 2026-01-02 to 2026-01-04, so the rewrite cannot reach them.
# `.write_provenance()` writes a clock reading and three version strings.
# `.xp_read_sheets()` masks those four rows and keeps every other cell under
# comparison.
#
# WHAT IS NOT CAPTURED
# No PDF carries a digest. Cairo writes a creation timestamp into the file, so
# the compressed length and the bytes move between two runs of one tree. The
# PDF names stay in the file inventory. Every PNG carries an md5.

.libPaths(c("/tmp/plan-baseline-lib", .libPaths()))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !args[[1]] %in% c("capture", "compare")) {
  stop(
    "usage: p11-report-export.R capture <out.rds> | compare <baseline.rds>",
    call. = FALSE
  )
}
mode <- args[[1]]
path <- args[[2]]

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the swereg package root.", call. = FALSE)
}
pkg <- read.dcf("DESCRIPTION")[1, "Package"]
if (!identical(unname(pkg), "swereg")) {
  stop("DESCRIPTION names '", pkg, "', not 'swereg'.", call. = FALSE)
}

# A skip is a failure here. Without the diagram stack the export script writes
# no CONSORT image, the inventory shrinks, and two captures of two different
# trees still compare identical.
stopifnot(
  requireNamespace("DiagrammeR", quietly = TRUE),
  requireNamespace("DiagrammeRsvg", quietly = TRUE),
  requireNamespace("rsvg", quietly = TRUE),
  requireNamespace("openxlsx", quietly = TRUE),
  requireNamespace("ggplot2", quietly = TRUE),
  requireNamespace("patchwork", quietly = TRUE)
)

suppressMessages(pkgload::load_all(".", quiet = TRUE))
progressr::handlers("void")

source("tests/testthat/helper-export_parity.R")

SPEC_YAML <- "tests/testthat/fixtures/spec_3x2x2.yaml"
S4_SCRIPT <- "tests/testthat/fixtures/s4_export_shape.R"

ARTEFACT_NAMES <- c(
  "print_spec_summary",
  "print_spec_summary_yaml",
  "print_target_checklist",
  "print_target_checklist_yaml",
  "results_summary",
  "excel_spec_summary",
  "excel_spec_summary_yaml",
  "s4_file_inventory",
  "s4_png_md5",
  "s4_table1_csv",
  "s4_workbook_sheet_names",
  "s4_workbook_cells"
)

# Rewrite today's date to `<DATE>`. The checklist header is the one line that
# carries it.
norm_date <- function(x) {
  gsub(format(Sys.Date(), "%Y-%m-%d"), "<DATE>", x, fixed = TRUE)
}

# A stray progress bar or a `message()` would land in the compare report and
# make a real divergence hard to find.
quietly <- function(expr) {
  v <- NULL
  invisible(utils::capture.output(
    suppressMessages(suppressWarnings(v <- expr)),
    type = "output"
  ))
  v
}

# The console text of one method, with today's date masked.
console_of <- function(expr) {
  norm_date(utils::capture.output(suppressMessages(suppressWarnings(expr))))
}

# Every cell of every sheet of one workbook, sheet order kept.
workbook_cells <- function(p) {
  wb <- openxlsx::loadWorkbook(p)
  nms <- names(wb)
  out <- lapply(nms, function(s) {
    d <- openxlsx::read.xlsx(
      p,
      sheet = s,
      colNames = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    )
    if (is.null(d)) {
      return(NULL)
    }
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    rownames(d) <- NULL
    names(d) <- paste0("C", seq_len(ncol(d)))
    d
  })
  names(out) <- nms
  list(sheet_names = nms, sheets = out)
}

capture_artefacts <- function() {
  plan <- .xp_plan("new")

  plan_yaml <- .xp_plan("new")
  plan_yaml$spec <- tteplan_read_spec(SPEC_YAML)

  dir_spec <- tempfile("p11-spec-")
  dir.create(dir_spec, recursive = TRUE)
  on.exit(unlink(dir_spec, recursive = TRUE), add = TRUE)
  p_spec <- file.path(dir_spec, "spec.xlsx")
  p_spec_yaml <- file.path(dir_spec, "spec_yaml.xlsx")
  quietly(plan$excel_spec_summary(path = p_spec))
  quietly(plan_yaml$excel_spec_summary(path = p_spec_yaml))

  # The committed export script, evaluated as `test-s4_export_script.R`
  # evaluates it. `s4_dir_out` is the one input it takes.
  dir_s4 <- tempfile("p11-s4-")
  dir.create(dir_s4, recursive = TRUE)
  on.exit(unlink(dir_s4, recursive = TRUE), add = TRUE)
  env_s4 <- new.env(parent = globalenv())
  env_s4$s4_dir_out <- dir_s4
  quietly(source(S4_SCRIPT, local = env_s4))

  files <- sort(list.files(dir_s4))
  pngs <- grep("\\.png$", files, value = TRUE)
  png_md5 <- vapply(
    pngs,
    function(f) unname(tools::md5sum(file.path(dir_s4, f))),
    character(1)
  )
  wb_s4 <- .xp_read_sheets(file.path(dir_s4, "tables.xlsx"))

  list(
    print_spec_summary = console_of(plan$print_spec_summary()),
    print_spec_summary_yaml = console_of(plan_yaml$print_spec_summary()),
    print_target_checklist = console_of(plan$print_target_checklist()),
    print_target_checklist_yaml = console_of(
      plan_yaml$print_target_checklist()
    ),
    results_summary = console_of(plan$results_summary()),
    excel_spec_summary = workbook_cells(p_spec),
    excel_spec_summary_yaml = workbook_cells(p_spec_yaml),
    s4_file_inventory = files,
    s4_png_md5 = png_md5,
    s4_table1_csv = readLines(
      file.path(dir_s4, "02_table1_01.csv"),
      warn = FALSE
    ),
    s4_workbook_sheet_names = wb_s4$sheet_names,
    s4_workbook_cells = wb_s4$sheets
  )
}

# A capture of empty text, or of a workbook with no sheet, compares identical
# against another one just like it. The assertion below stops that from
# becoming a baseline.
assert_non_degenerate <- function(v) {
  stopifnot(
    identical(names(v), ARTEFACT_NAMES),

    length(v$print_spec_summary) >= 30L,
    any(grepl("Target Trial Specification", v$print_spec_summary)),
    length(v$print_spec_summary_yaml) > length(v$print_spec_summary),

    length(v$print_target_checklist) >= 250L,
    any(grepl("TARGET CHECKLIST", v$print_target_checklist)),
    any(grepl("^Date: <DATE>", v$print_target_checklist)),
    !any(grepl(format(Sys.Date(), "%Y-%m-%d"), v$print_target_checklist)),
    length(v$print_target_checklist_yaml) > length(v$print_target_checklist),

    length(v$results_summary) >= 10L,
    any(grepl("SKIP: no events", v$results_summary)),

    identical(v$excel_spec_summary$sheet_names, "Study Specification"),
    nrow(v$excel_spec_summary$sheets[[1L]]) >= 30L,
    nrow(v$excel_spec_summary_yaml$sheets[[1L]]) >
      nrow(v$excel_spec_summary$sheets[[1L]]),

    # The four numbered exhibits the manifest declares, and the workbook.
    all(
      c(
        "01_fig1_consort.png",
        "02_table1_01.csv",
        "03_fig3_forest_itt.png",
        "04_fig4_forest_pp.png",
        "tables.xlsx"
      ) %in%
        v$s4_file_inventory
    ),
    length(v$s4_png_md5) >= 4L,
    all(nchar(v$s4_png_md5) == 32L),
    length(v$s4_table1_csv) >= 4L,

    length(v$s4_workbook_sheet_names) >= 12L,
    identical(v$s4_workbook_sheet_names[[1L]], "Provenance"),
    identical(
      names(v$s4_workbook_cells),
      v$s4_workbook_sheet_names
    ),
    all(vapply(v$s4_workbook_cells, function(d) nrow(d) > 0L, logical(1)))
  )
  invisible(TRUE)
}

# One printable digest per artefact, so a compare report names WHICH artefact
# moved rather than only that something did.
digest_of <- function(nm, x) {
  if (is.character(x) && is.null(names(x))) {
    return(sprintf(
      "%d lines, md5 %s",
      length(x),
      substr(digest_chr(x), 1L, 12L)
    ))
  }
  if (identical(nm, "s4_png_md5")) {
    return(paste(sprintf("%s=%s", names(x), substr(x, 1L, 8L)), collapse = " "))
  }
  if (is.list(x) && !is.null(x$sheet_names)) {
    return(sprintf(
      "%d sheets: %s",
      length(x$sheet_names),
      paste(x$sheet_names, collapse = ", ")
    ))
  }
  if (is.list(x)) {
    return(sprintf(
      "%d sheets, md5 %s",
      length(x),
      substr(
        digest_chr(unlist(lapply(x, function(d) as.character(unlist(d))))),
        1L,
        12L
      )
    ))
  }
  "<no digest>"
}

# An md5 over character content, with no dependency on a hashing package.
digest_chr <- function(x) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeLines(as.character(x), tf, useBytes = TRUE)
  unname(tools::md5sum(tf))
}

show_artefacts <- function(v) {
  for (nm in ARTEFACT_NAMES) {
    cat(sprintf("  %-30s %s\n", nm, digest_of(nm, v[[nm]])))
  }
}

# ---------------------------------------------------------------------------
# Modes
# ---------------------------------------------------------------------------

if (identical(mode, "capture")) {
  v <- capture_artefacts()
  assert_non_degenerate(v)
  saveRDS(v, path)
  cat(sprintf("CAPTURE: wrote %s\n", path))
  show_artefacts(v)
  quit(status = 0L)
}

baseline <- readRDS(path)
assert_non_degenerate(baseline)
current <- capture_artefacts()
assert_non_degenerate(current)

cat(sprintf("COMPARE: baseline %s\n", path))
cat(sprintf("%-30s %s\n", "ARTEFACT", "IDENTICAL"))
ok <- TRUE
for (nm in ARTEFACT_NAMES) {
  same <- identical(baseline[[nm]], current[[nm]])
  ok <- ok && same
  cat(sprintf("%-30s %s\n", nm, same))
}

if (!ok) {
  cat("\nDIVERGENT ARTEFACTS\n")
  for (nm in ARTEFACT_NAMES) {
    b <- baseline[[nm]]
    c_ <- current[[nm]]
    if (identical(b, c_)) {
      next
    }
    cat(sprintf("  %s\n", nm))
    cat(sprintf("    baseline: %s\n", digest_of(nm, b)))
    cat(sprintf("    current : %s\n", digest_of(nm, c_)))
    if (is.character(b) && is.character(c_) && is.null(names(b))) {
      n <- max(length(b), length(c_))
      bb <- c(b, rep(NA_character_, n - length(b)))
      cc <- c(c_, rep(NA_character_, n - length(c_)))
      first <- which(!identical(bb, cc) & (is.na(bb) | is.na(cc) | bb != cc))
      if (length(first) > 0L) {
        i <- first[[1L]]
        cat(sprintf("    first divergent line %d\n", i))
        cat(sprintf("      baseline: %s\n", bb[[i]]))
        cat(sprintf("      current : %s\n", cc[[i]]))
      }
    }
  }
}

cat(sprintf("\nEQUIVALENCE: all twelve artefacts identical = %s\n", ok))
quit(status = if (ok) 0L else 1L)
