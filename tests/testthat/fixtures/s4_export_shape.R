# =============================================================================
# An export script, in the shape a study writes one
# =============================================================================
# `test-s4_export_script.R` evaluates this file from top to bottom. No package
# function sources it.
#
# WHY A SCRIPT, AND NOT A FUNCTION LIBRARY
# Before this file, nothing in the suite ran a whole export script. Every check
# drove one internal function on its own. A script that leaves out a call stays
# invisible to a check of that kind. The test evaluates this file, so a missing
# call turns the test red.
#
# THE TWO EXPORTS, AND WHY THERE ARE TWO
#   1. `$export()` writes the numbered manuscript exhibits. Each study chooses
#      what goes into its own paper.
#   2. `$export_tables()` writes the supplement workbook and its sidecar
#      images. Every study shares that one workbook.
# Order does not matter between the two calls. `$s3_analyze()` computes the
# risk difference for every emulated trial, so neither export computes it.
#
# THE ONE INPUT
# The caller MUST set `s4_dir_out` to a directory that exists. Every file goes
# there. A production script reads that directory off its plan instead.
# =============================================================================

stopifnot(exists("s4_dir_out"), dir.exists(s4_dir_out))

plan <- .xp_plan("new")
plan$output_dir <- s4_dir_out

# The emulated trials the manuscript figures report, label to id. The workbook
# needs one of them for the `Target trial protocol` sheet.
featured_etts <- list(
  "Contrast one" = "ETT00001",
  "Contrast two" = "ETT00003"
)

# --- Refresh the baseline panels first ---------------------------------------
# Both exports below read Table 1 through `$get_baselines()`. That accessor
# returns whatever the analysis stage cached. The refresh is its own step, and
# not a side effect of whichever export runs first.
#
# This plan holds no analysis file on disk. `$recompute_baselines()` therefore
# warns once per enrollment and leaves each panel unchanged. `suppressWarnings()`
# keeps that expected warning out of the test report.
suppressWarnings(plan$recompute_baselines(output_dir = s4_dir_out))

# --- Manuscript exhibit set ---------------------------------------------------
# The exhibits, in order: participant flow, Table 1, then one forest figure per
# estimand. Every entry writes `NN_<label>`, and `NN` is its position here.
#
# The manifest carries no `survival` entry. That figure needs the count of
# people at risk in each arm, and this plan stores no such count.
#
# `risk_difference = TRUE` is a DISPLAY switch. It adds two columns to the
# figure and computes nothing.
MANUSCRIPT <- list(
  list(type = "consort", label = "fig1_consort", enrollment = "01"),
  list(type = "table1", label = "table1", enrollment = "01"),
  list(
    type = "forest",
    label = "fig3_forest",
    estimands = "itt",
    exposures = featured_etts,
    label_format = "{outcome_name}",
    role_headers = list(
      primary = "Primary outcome",
      secondary = "Secondary outcomes"
    ),
    risk_difference = TRUE
  ),
  list(
    type = "forest",
    label = "fig4_forest",
    estimands = "pp",
    exposures = featured_etts,
    label_format = "{outcome_name}",
    role_headers = list(
      primary = "Primary outcome",
      secondary = "Secondary outcomes"
    ),
    risk_difference = TRUE
  )
)

plan$export(MANUSCRIPT, dir = s4_dir_out)

# --- Workbook -----------------------------------------------------------------
# The supplement. It carries no forest figure, because the `PP results` and
# `ITT results` sheets already report every emulated trial.
#
# `featured_etts` reaches the manuscript exhibits only. The workbook needs one
# id, for the emulated trial the `Target trial protocol` sheet describes.
plan$export_tables(
  path = file.path(s4_dir_out, "tables.xlsx"),
  table1_enrollment = "01",
  protocol_ett_id = unlist(featured_etts, use.names = FALSE)[1]
)
