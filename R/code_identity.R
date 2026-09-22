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

  return(digest::digest(
    list(body = body(fn), formals = formals(fn)),
    algo = "xxhash64"
  ))
}

# Stored in place of a trim hash when a study registers no trim function.
# It MUST NOT be NULL. A skeleton or a meta written before the trim phase
# existed carries NULL. "This study registers no trim" MUST NOT compare
# equal to that. If both were NULL, adding a trim to an existing study
# would rebuild nothing.
.TRIM_NONE <- "__swereg_no_trim__"

# The current trim identity for a study. `fn` is RegistryStudy$trim_fn.
.trim_hash <- function(fn) {
  if (is.null(fn)) return(.TRIM_NONE) else return(.hash_function(fn))
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
  if (is.null(x) || length(x) == 0L) return(empty) else return(paste(x, collapse = " -> "))
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
# .code_registry_fingerprints(). Passing a derived entry
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
  return(digest::digest(
    list(
      codes = reg$codes,
      label = reg$label,
      groups = reg$groups,
      fn_args = reg$fn_args,
      combine_as = reg$combine_as,
      fn = .hash_function(reg[["fn"]])
    ),
    algo = "xxhash64"
  ))
}

# The fingerprint of every entry in a study's code registry, in registry
# order. `code_registry` is `RegistryStudy$code_registry`, and
# `RegistryStudy$code_registry_fingerprints()` is a one-call delegate to this
# function.
#
# Two passes. Pass 1 fingerprints each primary entry through
# .fingerprint_entry(). Pass 2 fingerprints each derived entry, and folds in
# the pass-1 fingerprint of every upstream primary entry whose output prefix
# the derived entry reads. That cascade re-applies a derived entry after an
# edit to an upstream primary's `fn_args`, `groups` or `codes`.
.code_registry_fingerprints <- function(code_registry) {
  n <- length(code_registry)
  if (n == 0L) {
    return(character(0))
  }

  fps <- character(n)
  # Pass 1: primary fingerprints.
  for (i in seq_len(n)) {
    reg <- code_registry[[i]]
    if (!identical(reg$kind %||% "primary", "derived")) {
      fps[i] <- .fingerprint_entry(reg)
    }
  }
  # Pass 2: derived fingerprints, folding in upstream primary fps.
  for (i in seq_len(n)) {
    reg <- code_registry[[i]]
    if (!identical(reg$kind %||% "primary", "derived")) {
      next
    }
    upstream <- character()
    for (j in seq_len(i - 1L)) {
      pri <- code_registry[[j]]
      if (identical(pri$kind %||% "primary", "derived")) {
        next
      }
      prefixes <- c(names(pri$groups), pri$combine_as)
      prefixes <- prefixes[!is.null(prefixes) & nzchar(prefixes)]
      if (any(prefixes %in% reg$from)) {
        upstream <- c(upstream, fps[j])
      }
    }
    fps[i] <- digest::digest(
      list(
        kind = "derived",
        codes = reg$codes,
        from = reg$from,
        as = reg$as,
        upstream = upstream
      ),
      algo = "xxhash64"
    )
  }
  return(fps)
}

# One hash per registered phase-3 step, named by step name and in registration
# order. `RegistryStudy$randvars_hashes()` is a one-call delegate to this
# function, and that method's roxygen block says what each hash covers.
#
# `fingerprints` is the code registry fingerprint set. R evaluates it only
# where the code below reads it. An empty `randvars_fns` returns first, so a
# caller that passes an expensive expression never runs it.
.randvars_hashes <- function(
  randvars_fns,
  framework_fn,
  trim_fn,
  fingerprints
) {
  if (length(randvars_fns) == 0L) {
    return(character(0))
  }
  framework_hash <- if (is.null(framework_fn)) {
    NA_character_
  } else {
    .hash_function(framework_fn)
  }
  trim_hash <- .trim_hash(trim_fn)
  return(vapply(
    randvars_fns,
    function(fn) {
      return(digest::digest(
        list(
          fn = .hash_function(fn),
          framework = framework_hash,
          trim = trim_hash,
          phase_order = .PHASE_ORDER,
          codes = fingerprints
        ),
        algo = "xxhash64"
      ))
    },
    character(1)
  ))
}

# The identity of a pipeline: the five things that decide what a freshly built
# skeleton looks like, each normalized so that two representations of the SAME
# pipeline compare equal.
#
# `codes` is a SET, and that is the whole point of this function.
# `Skeleton$sync_with_registry()` applies code entries by set difference, so a
# re-applied entry moves to the END of the stored registry while naming an
# unchanged pipeline. Comparing the stored order against the registration order
# therefore reported "obsolete" for a skeleton that was current. Sorting is what
# makes the stored order irrelevant.
#
# Every other component stays ordered, because for those the order IS semantic.
# Randvars replay is "first divergence, then rewind", so the sequence decides
# what replays. `phase_order` names the order the phases ran in.
#
# `method = "radix"` sorts in the C locale. The default method collates by the
# session locale, so the same eight fingerprints could sort two ways on two
# machines and reintroduce the bug this function exists to remove.
#
# `randvars_hashes` and `fingerprints` arrive as values, so this function never
# reaches back into a study object.
.pipeline_identity <- function(
  framework_hash,
  trim_hash,
  phase_order,
  randvars_hashes,
  fingerprints
) {
  return(list(
    framework = framework_hash %||% NA_character_,
    trim = trim_hash %||% NA_character_,
    phase_order = phase_order,
    randvars = randvars_hashes %||% character(0),
    codes = sort(unname(fingerprints %||% character(0)), method = "radix")
  ))
}

# The identity of anything that carries the five stored provenance fields: a
# live `Skeleton`, a skeleton deserialized from disk, or a `meta_*.qs2` sidecar
# list. All three name the fields identically, so one function serves them all
# and there is no second construction of the component list to keep aligned.
#
# It reads FIELDS, never methods, and that is load-bearing. An R6 object
# serializes its methods along with its data, so a skeleton written by an older
# swereg carries that swereg's method bodies. Calling `obj$pipeline_identity()`
# on it either dispatches to a stale body or, after a rename, to NULL --
# "attempt to apply non-function". Fields survive a rename; methods do not.
.stored_pipeline_identity <- function(x) {
  return(.pipeline_identity(
    framework_hash = x$framework_fn_hash,
    trim_hash = x$trim_fn_hash,
    phase_order = x$phase_order,
    randvars_hashes = vapply(
      x$randvars_state %||% list(),
      function(s) s$fn_hash %||% NA_character_,
      character(1)
    ),
    fingerprints = names(x$applied_registry) %||% character(0)
  ))
}

# The component names of a pipeline identity, in comparison order.
.PIPELINE_IDENTITY_COMPONENTS <- c(
  "framework",
  "trim",
  "phase_order",
  "randvars",
  "codes"
)

# One scalar per identity, for grouping batches. It is a digest of the
# NORMALIZED identity, so it answers "are these two skeletons the same
# generation" and nothing else. Nothing decides what to replay from it: that is
# `.process_one_batch()`, which compares the components individually.
.pipeline_identity_hash <- function(identity) {
  return(digest::digest(identity, algo = "xxhash64"))
}

# Name the FIRST component on which two identities differ, or NULL when they
# match. A component name is what a reader can act on; two unrelated-looking
# digests are not.
.first_identity_difference <- function(a, b) {
  for (nm in .PIPELINE_IDENTITY_COMPONENTS) {
    if (!identical(a[[nm]], b[[nm]])) {
      return(nm)
    }
  }
  return(NULL)
}

# Render the difference on one component, for an error message. `a` is the
# skeleton side and `b` the study side.
.describe_identity_difference <- function(component, a, b) {
  fmt <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      return("(none)")
    }
    if (length(x) == 1L && is.null(names(x))) {
      return(as.character(x))
    }
    nms <- names(x)
    if (is.null(nms)) {
      return(paste(x, collapse = ", "))
    }
    return(paste0(nms, "=", x, collapse = ", "))
  }
  return(sprintf(
    "%s differs\n  on disk: %s\n  current: %s",
    component,
    fmt(a[[component]]),
    fmt(b[[component]])
  ))
}
