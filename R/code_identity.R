# The identity of a pipeline: what makes a stored skeleton match the study that wrote it.

# Compute a stable-across-sessions xxhash64 digest of a function's body and
# formal arguments. Used by RegistryStudy$process_skeletons() to detect
# edits to the framework_fn / trim_fn / randvars_fns closures, and by
# .fingerprint_entry() for a code entry's fn, so each phase re-runs on
# exactly the batches that need it.
#
# We deliberately hash only list(body(fn), formals(fn)) and not fn itself,
# because the full function object includes its enclosing environment,
# which varies across R sessions and would make hashes non-deterministic.
.hash_function <- function(fn) {
  stopifnot(is.function(fn))

  # Strip the srcref FIRST. body() carries a srcref when the function was
  # parsed with keep.source = TRUE, which is the interactive/RStudio
  # default, and does not under Rscript, which is the default there. So
  # the same function hashed in the two sessions gave two digests, and a
  # framework registered in RStudio rebuilt every batch when the pipeline
  # then ran under Rscript. removeSource() is a no-op on a primitive.
  fn <- utils::removeSource(fn)

  digest::digest(
    list(body = body(fn), formals = formals(fn)),
    algo = "xxhash64"
  )
}

# Stored in place of a trim hash when a study registers no trim function.
# It MUST NOT be NULL. A skeleton or a meta written before the trim phase
# existed carries NULL. "This study registers no trim" MUST NOT compare
# equal to that. If both were NULL, adding a trim to an existing study
# would rebuild nothing.
.TRIM_NONE <- "__swereg_no_trim__"

# The current trim identity for a study. `fn` is RegistryStudy$trim_fn.
.trim_hash <- function(fn) {
  if (is.null(fn)) .TRIM_NONE else .hash_function(fn)
}

# The phase order this swereg runs. `$process_skeletons()` reads it once and
# passes it down, exactly as it passes `framework_hash`. `.process_one_batch()`
# compares it at the rebuild gate and stamps what it was passed. Nothing else
# reads the constant, so the parent and a worker never disagree about which
# value reached the skeleton.
#
# A skeleton written before this field existed ran the code registry AFTER
# randvars, and it reads NULL. It MUST NOT compare equal. See the
# `phase_order` entry on the Skeleton class for why the answer is a full
# rebuild rather than a replay.
.PHASE_ORDER <- c("framework", "codes", "randvars")

# One-row rendering of a stored phase order, for the `phase_order` column of
# `$skeleton_pipeline_hashes()` and for `Skeleton$print()`. A skeleton written
# before the field existed reads NULL, and `paste(NULL, collapse = " -> ")`
# returns the empty string. The guard returns `empty` instead, so a missing
# order never renders as a blank cell.
.format_phase_order <- function(x, empty = NA_character_) {
  if (is.null(x) || length(x) == 0L) empty else paste(x, collapse = " -> ")
}

# Compute a stable fingerprint for one PRIMARY code_registry entry. Two
# primary entries with identical (codes, label, groups, fn_args,
# combine_as) AND the same `fn` body produce the same fingerprint, and are
# therefore "the same entry" across runs.
#
# Derived entries are NOT fingerprinted via this helper: their fingerprint
# depends on the fingerprints of upstream primary entries (so that edits
# to an upstream primary's `fn_args` or `groups` cascade into a derived
# re-apply), which can only be computed in the two-pass walk inside
# RegistryStudy$code_registry_fingerprints(). Passing a derived entry
# here is a programming error and triggers a loud stop().
#
.fingerprint_entry <- function(reg) {
  kind <- reg$kind %||% "primary"
  if (identical(kind, "derived")) {
    stop(
      "Derived entries must be fingerprinted via ",
      "RegistryStudy$code_registry_fingerprints() so upstream primary ",
      "fingerprints are folded in.",
      call. = FALSE
    )
  }
  # `reg[["fn"]]`, never `reg$fn`. `$` partial-matches on a list, and this
  # entry also carries `fn_args`. An entry missing `fn` would make
  # `reg$fn` return `fn_args`, and the fingerprint would silently hash
  # the wrong thing rather than stop.
  #
  # The fn body is folded in because editing a registered code function
  # changes the column it writes. Without it the fingerprint held still,
  # nothing re-applied, and no randvars step replayed.
  digest::digest(
    list(
      codes = reg$codes,
      label = reg$label,
      groups = reg$groups,
      fn_args = reg$fn_args,
      combine_as = reg$combine_as,
      fn = .hash_function(reg[["fn"]])
    ),
    algo = "xxhash64"
  )
}
