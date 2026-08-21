#!/usr/bin/env Rscript

# Equivalence capture for the RegistryStudy code-identity code.
#
# Run it from the package root:
#   Rscript dev/equivalence/p9-code-identity.R capture /tmp/p9-before.rds
#   Rscript dev/equivalence/p9-code-identity.R compare /tmp/p9-before.rds
#
# `capture` builds a fixture study under /tmp. It records five code-identity
# values and writes them to the output file. `compare` rebuilds the same
# fixture and reports identical() against the stored baseline, one value at a
# time. It exits with status 1 when any value differs.
#
# Each of the five values is a digest that a stored skeleton compares itself
# against on the next run. A changed digest rebuilds every batch, and the
# package reports that as ordinary work rather than as an error. So a refactor
# of the code-identity code MUST leave all five values byte-identical.
#
# The fixture writes only under /tmp. It deletes its own directory on exit.

.libPaths(c("/tmp/plan-baseline-lib", .libPaths()))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !args[[1]] %in% c("capture", "compare")) {
  stop(
    "usage: p9-code-identity.R capture <out.rds> | compare <baseline.rds>",
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

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# The progress bars carry no information here, and they make the compare
# report hard to read.
progressr::handlers("void")

# ---------------------------------------------------------------------------
# Fixture
# ---------------------------------------------------------------------------

# The registered closures. Their bodies and formals feed every digest below,
# so they MUST stay byte-identical for a baseline to keep its meaning.

.p9_framework <- function(batch_data, config) {
  data.table::data.table(
    id = batch_data[["ids"]]$lopnr,
    isoyear = 2020L,
    isoyearweek = "2020-01",
    is_isoyear = FALSE
  )
}

.p9_trim <- function(skeleton, batch_data, config) {
  invisible(skeleton)
}

.p9_code_fn <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := TRUE]
  }
  invisible(skeleton)
}

.p9_randvars_fn <- function(skeleton, batch_data, config) {
  skeleton[, rv_a := 1L]
  invisible(skeleton)
}

# Two rawbatch groups, six persons, batch size three. That gives two batches,
# so the sidecar reconstruction reads more than one meta file.
build_study <- function(dir) {
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("ids", "codes"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  study$save_rawbatch("ids", data.table::data.table(lopnr = 1:6, val = "a"))
  study$save_rawbatch("codes", data.table::data.table(lopnr = 1:6, code = "X"))
  study$register_framework(.p9_framework)
  study$register_trim(.p9_trim)
  study$register_codes(
    codes = list(p9_one = "X"),
    fn = .p9_code_fn,
    groups = list("codes"),
    label = "p9one"
  )
  study$register_codes(
    codes = list(p9_two = "Y"),
    fn = .p9_code_fn,
    groups = list("codes"),
    label = "p9two"
  )
  study$register_randvars("rv_a", .p9_randvars_fn)
  study
}

# ---------------------------------------------------------------------------
# The five values
# ---------------------------------------------------------------------------

VALUE_NAMES <- c(
  "fingerprints",
  "randvars_hashes",
  "study_pipeline_hash",
  "skeleton_pipeline_hash",
  "sidecar_pipeline_hash"
)

capture_values <- function() {
  dir <- tempfile(pattern = "p9-equiv-", tmpdir = "/tmp")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  invisible(utils::capture.output(
    suppressMessages({
      study <- build_study(dir)
      study$process_skeletons(
        batches = seq_len(study$n_batches),
        n_workers = 1L
      )
      sk <- study$load_skeleton(1L)
      ph <- study$skeleton_pipeline_hashes()
    }),
    type = "output"
  ))

  list(
    fingerprints = study$code_registry_fingerprints(),
    randvars_hashes = study$randvars_hashes(),
    study_pipeline_hash = study$pipeline_hash(),
    skeleton_pipeline_hash = sk$pipeline_hash(),
    sidecar_pipeline_hash = sort(unique(ph$pipeline_hash))
  )
}

# A capture of NA values, or of the wrong length, compares identical against
# another one just like it. The assertion below stops that from becoming a
# baseline.
assert_non_degenerate <- function(v) {
  stopifnot(
    identical(names(v), VALUE_NAMES),

    is.character(v$fingerprints),
    length(v$fingerprints) == 2L,
    !anyNA(v$fingerprints),
    all(nzchar(v$fingerprints)),
    !identical(v$fingerprints[[1]], v$fingerprints[[2]]),

    is.character(v$randvars_hashes),
    length(v$randvars_hashes) == 1L,
    identical(names(v$randvars_hashes), "rv_a"),
    !anyNA(v$randvars_hashes),
    all(nzchar(v$randvars_hashes)),

    is.character(v$study_pipeline_hash),
    length(v$study_pipeline_hash) == 1L,
    !is.na(v$study_pipeline_hash),
    nzchar(v$study_pipeline_hash),

    is.character(v$skeleton_pipeline_hash),
    length(v$skeleton_pipeline_hash) == 1L,
    !is.na(v$skeleton_pipeline_hash),
    nzchar(v$skeleton_pipeline_hash),

    is.character(v$sidecar_pipeline_hash),
    length(v$sidecar_pipeline_hash) == 1L,
    !is.na(v$sidecar_pipeline_hash),
    nzchar(v$sidecar_pipeline_hash)
  )
  invisible(TRUE)
}

# The three surfaces agree on a freshly processed store. Report it rather than
# stop on it, so a compare run shows which surface moved.
report_parity <- function(v) {
  study_vs_skeleton <- identical(
    v$study_pipeline_hash,
    v$skeleton_pipeline_hash
  )
  study_vs_sidecar <- identical(v$study_pipeline_hash, v$sidecar_pipeline_hash)
  cat(sprintf(
    "PARITY: study == skeleton is %s, study == sidecar is %s\n",
    study_vs_skeleton,
    study_vs_sidecar
  ))
  study_vs_skeleton && study_vs_sidecar
}

show_values <- function(v) {
  for (nm in VALUE_NAMES) {
    x <- v[[nm]]
    cat(sprintf(
      "  %-24s %s\n",
      nm,
      paste(sprintf("%s=%s", names(x) %||% seq_along(x), x), collapse = " ")
    ))
  }
}

# ---------------------------------------------------------------------------
# Modes
# ---------------------------------------------------------------------------

if (identical(mode, "capture")) {
  v <- capture_values()
  assert_non_degenerate(v)
  parity <- report_parity(v)
  saveRDS(v, path)
  cat(sprintf("CAPTURE: wrote %s\n", path))
  show_values(v)
  quit(status = if (parity) 0L else 1L)
}

baseline <- readRDS(path)
assert_non_degenerate(baseline)
current <- capture_values()
assert_non_degenerate(current)
parity <- report_parity(current)

cat(sprintf("COMPARE: baseline %s\n", path))
cat(sprintf(
  "%-24s %-10s %-7s %s\n",
  "VALUE",
  "IDENTICAL",
  "NAMES",
  "ATTRIBUTES"
))
ok <- TRUE
for (nm in VALUE_NAMES) {
  b <- baseline[[nm]]
  c_ <- current[[nm]]
  same_value <- identical(b, c_)
  same_names <- identical(names(b), names(c_))
  same_attrs <- identical(attributes(b), attributes(c_))
  ok <- ok && same_value && same_names && same_attrs
  cat(sprintf(
    "%-24s %-10s %-7s %s\n",
    nm,
    same_value,
    same_names,
    same_attrs
  ))
}

if (!ok) {
  cat("\nDIVERGENT VALUES\n")
  for (nm in VALUE_NAMES) {
    b <- baseline[[nm]]
    c_ <- current[[nm]]
    if (identical(b, c_)) {
      next
    }
    cat(sprintf("  %s\n", nm))
    cat(sprintf("    baseline: %s\n", paste(b, collapse = " ")))
    cat(sprintf("    current : %s\n", paste(c_, collapse = " ")))
  }
}

cat(sprintf("\nEQUIVALENCE: all five values identical = %s\n", ok))
quit(status = if (ok && parity) 0L else 1L)
