# Generate the no-rule reference for test-s1b-sparse-rule.R.
#
# The reference is what s1 wrote BEFORE the rare-condition rule existed, for
# the fixture plan of tests/testthat/helper-sparse_rule.R without a
# rare_condition_rule block. It was generated from commit 4a9889c0, the
# phase-11 rename, which runs the 27.2.0 s1 under the renamed stage names:
#
#   mkdir /tmp/swereg-head
#   git archive 4a9889c0 | tar -x -C /tmp/swereg-head
#   Rscript tests/testthat/fixtures/sparse-rule-27.2.0/make.R /tmp/swereg-head
#
# Run it from the repository root, so the two helper files below are the
# current ones. Its one output, reference.rds, holds plain data.frames, so a
# test compares it with identical() and no data.table attribute interferes.

args <- commandArgs(trailingOnly = TRUE)
stopifnot("make.R needs the path of the 27.2.0 source tree" = length(args) == 1L)
pkgload::load_all(args[[1L]], quiet = TRUE)
library(testthat)
library(data.table)
options(swereg.warn_prevalent_user = FALSE)
source("tests/testthat/helper-tteplan_truth.R")
source("tests/testthat/helper-sparse_rule.R")

r <- (function() spr_run_s1(rule = NULL))()
plain <- function(x) {
  return(lapply(x, function(ec) lapply(ec, as.data.frame)))
}
ref <- list(
  enrollment_counts = plain(r$plan$enrollment_counts),
  confounder_vars = r$plan$ett$confounder_vars,
  tuple_cols = lapply(r$pre, function(p) names(p$tuples)),
  attrition_cols = lapply(r$pre, function(p) names(p$attrition))
)
saveRDS(ref, "tests/testthat/fixtures/sparse-rule-27.2.0/reference.rds")
