# Generate the schema-26.14.0 fixtures for test-schema-refusal.R.
#
# Run this script ONCE, with the INSTALLED swereg 26.14.0, in its own
# process:
#
#   Rscript tests/testthat/fixtures/schema-26.14.0/make.R
#
# It MUST NOT run under pkgload::load_all(). The point of the fixtures is
# that every object carries the method bodies 26.14.0 serialised with it, and
# only the installed 26.14.0 namespace gives those. The script stops under any
# other version.
#
# The synthetic data comes from tests/testthat/helper-tteplan_truth.R as it
# stood at the 26.14.0 release commit (0eafa97e), read with `git show`, so a
# later edit to the helper cannot change what this script builds.
#
# Outputs, all in this directory:
#   tteplan.qs2     the plan after s1, s2 and s3, as `$save()` wrote it
#   <file_raw>      the s1 raw enrollment file of enrollment 01
#   <file_imp>      the s1 imputed enrollment file of enrollment 01
#   expected.qs2    what 26.14.0 computed: get_curves(), get_attrition(),
#                   get_estimates(), the stored risk-difference rows and
#                   curves, the IRR rows, the rates and the baseline counts

library(swereg)
library(data.table)
stopifnot(
  "make.R needs the installed swereg 26.14.0" = identical(
    as.character(utils::packageVersion("swereg")),
    "26.14.0"
  ),
  "make.R MUST NOT run under pkgload::load_all()" = is.null(
    swereg:::.swereg_dev_path()
  )
)

release_sha <- "0eafa97eff58199410819cd5dc0b230bc6549095"
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", grep("^--file=", args, value = TRUE)))
out_dir <- dirname(script)
repo <- normalizePath(file.path(out_dir, "..", "..", "..", ".."))

helper <- tempfile(fileext = ".R")
status <- system2(
  "git",
  c(
    "-C",
    repo,
    "show",
    paste0(release_sha, ":tests/testthat/helper-tteplan_truth.R")
  ),
  stdout = helper
)
stopifnot(identical(status, 0L))
source(helper)

root <- file.path(path.expand("~/scratch"), "schema-26.14.0-make")
unlink(root, recursive = TRUE)
dir.create(root, mode = "0700")
for (d in c("spec", "tteplan", "results", "meta")) {
  dir.create(file.path(root, d))
}

sk <- ttm_skeleton(
  "B",
  n_persons = 1500L,
  loss = "independent",
  disc_hazard = 0.02
)
skel_path <- file.path(root, "tteplan", "skel_a.qs2")
qs2::qs_save(sk, skel_path)
spec_path <- file.path(root, "spec", "spec_v001.yaml")
ttm_write_spec(spec_path, "schema", c("rd_age_continuous", "ri_highrisk"))

plan <- tteplan_from_spec_and_registrystudy(
  study = list(
    skeleton_files = skel_path,
    data_meta_dir = file.path(root, "meta")
  ),
  candidate_dir_spec = file.path(root, "spec"),
  candidate_dir_tteplan = file.path(root, "tteplan"),
  candidate_dir_results = file.path(root, "results"),
  spec_version = "v001",
  global_max_isoyearweek = sk[, max(isoyearweek)],
  check_skeletons = FALSE
)
plan$s1_generate_enrollments_and_ipw(
  n_workers = 1L,
  swereg_dev_path = NULL,
  check_skeletons = FALSE
)
plan$s2_generate_analysis_files_and_ipcw_pp(
  n_workers = 1L,
  swereg_dev_path = NULL
)
plan$s3_analyze(n_workers = 1L, swereg_dev_path = NULL)
plan$save()

ett <- plan$ett
stopifnot(nrow(ett) == 1L)
res <- plan$results_ett[[ett$ett_id]]
expected <- list(
  swereg_version = as.character(utils::packageVersion("swereg")),
  release_sha = release_sha,
  ett = data.table::copy(ett),
  curves = plan$get_curves(),
  attrition = plan$get_attrition(),
  estimates = plan$get_estimates(),
  results_ett = res,
  results_enrollment = plan$results_enrollment
)

files <- c("tteplan.qs2", ett$file_raw, ett$file_imp)
ok <- file.copy(
  file.path(plan$dir_tteplan, files),
  file.path(out_dir, files),
  overwrite = TRUE
)
stopifnot(all(ok))
qs2::qs_save(expected, file.path(out_dir, "expected.qs2"))
unlink(root, recursive = TRUE)

cat("Wrote", c(files, "expected.qs2"), sep = "\n  ")
