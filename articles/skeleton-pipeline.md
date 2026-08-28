# The skeleton pipeline

## The skeleton pipeline

This vignette describes how `swereg` builds and incrementally rebuilds
per-batch skeleton files in production. If you just want to create a
small skeleton and attach some diagnoses by hand, read
[`vignette("skeleton-create")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-create.md)
first. This vignette is for people running full-registry pipelines where
total build time is measured in days and every iteration matters.

### The problem

A production swereg pipeline on Swedish registry data typically has two
dominant costs:

1.  **Rawbatch creation**: split the raw registry files into
    per-person-batch subsets, save as `.qs2`. For national-registry
    scale this is typically many hours of I/O.
2.  **Skeleton processing**: for each batch, build a person-week time
    grid, apply censoring, add demographics from LISA, classify exposure
    from LMED, and match ICD-10 / ATC / operation codes across the
    subset. Typically dominates total pipeline time.

Together these add up to a multi-day first build for national-scale
studies. Before incremental invalidation landed, an edit to one ICD-10
code triggered the full rebuild. The skeleton was saved as a bare
`data.table`, with no provenance to diff against.

The goal of the phased pipeline is to make that 62h cost *proportional
to the size of the change*. An edit re-runs only the phases it
invalidates.

### Four phases, and the order they run in

Per batch, `RegistryStudy$process_skeletons()` runs four phases:

    framework  ->  trim  ->  codes  ->  randvars

Each phase carries its own invalidation rule, so an edit re-runs only
what it affects. The sections below take the phases in that order.

#### Phase 1 – framework

A single user-supplied function, registered via
`$register_framework(fn)`, with signature
`function(batch_data, config)`. It returns a fresh `data.table`
containing the base time grid plus structural censoring (immigration,
emigration, death). Nothing downstream can exist without it.

**Invalidation:** full rebuild whenever
`digest::digest(list(body(fn), formals(fn)), "xxhash64")` changes.
Framework edits are expected to be rare (you don’t change the time grid
on a whim), so a conservative “always rebuild on hash change” is fine.
When phase 1 re-runs, swereg discards every later phase and re-applies
it from scratch on the fresh base.

#### Phase 1b – trim

At most one user-supplied function, registered via `$register_trim(fn)`,
with signature `function(skeleton, batch_data, config)`. It returns a
`data.table`, and `$process_skeletons()` rebinds the skeleton to what it
returns. A study that registers no trim skips this phase.

**This is the one place in the pipeline that MAY delete skeleton rows.**
A code entry or a randvars step that changes the row count stops the
run, and the error names the registration to edit. Phase 1b runs before
the code registry, so every later phase sees the rows the trim leaves
behind.

``` r
study$register_trim(function(skeleton, batch_data, config) {
  skeleton[isoyear >= 2010]
})
```

**Invalidation:** full rebuild whenever the trim’s identity changes.
Three changes rebuild the base of every batch: a new trim, a removed
trim, and an edit to the trim’s body. A deletion cannot be rewound, so a
fresh base is the only correct answer.

The trim runs on a rebuild and at no other time. It therefore always
reads a fresh base, so it MAY delete a fixed count or a fraction rather
than a predicate. An edit to a code entry or to a randvars step rebuilds
nothing and re-runs no trim.

A `Skeleton` records the trim in `trim_fn_hash`, not in `phase_order`.
`phase_order` names the three phases that a swereg release can reorder.
The trim is pinned immediately after the framework.

`$register_trim()` stops on a second call. A study has one framework by
construction, so `$register_framework()` overwrites instead. Two trim
registrations are a script that means to delete rows in two places.

#### Phase 2 – code registry

A list of code registrations, each added via `$register_codes()`
(primary) or `$register_derived_codes()` (derived). Each entry
contributes one or more columns to the skeleton; the shape of those
columns is predicted from metadata, not observed from running `fn`.

**Invalidation:** per-entry fingerprint diff.
`Skeleton$sync_with_registry()` computes
`to_drop = stored_fingerprints - current_fingerprints` and
`to_add = current_fingerprints - stored_fingerprints`, drops stale
entries’ columns, then re-applies new entries in registration order.
When you change one code in one entry, every other entry’s fingerprint
is unchanged, so only that one entry re-runs.

That is true of the code registry alone. A code entry edit also replays
every phase-3 step.

#### Phase 3 – randvars

An ordered named list of user-supplied functions, each registered via
`$register_randvars(name, fn)`. Each has signature
`function(skeleton, batch_data, config)`. A step adds columns to the
skeleton. It MUST NOT modify or delete an existing column: the rewind
tracks only the columns a step records.

**A randvars step MAY read a code registry column**, because phase 2
already ran. That capability is why the code registry runs first. A step
MUST NOT change the row count. Rewind-and-replay drops columns, and it
cannot restore rows. Register a row filter as the phase-1b trim instead.

Registration order is execution order. An edit to one step replays that
step and everything downstream of it. It leaves upstream steps
untouched.

**Invalidation:** *divergence-point rewind-and-replay*. swereg scans the
stored `(name, hash)` sequence against the current one, finds the first
position where either the name or the hash differs, drops the stored
`added_columns` of every step from that point forward, and then replays
current steps from that point. Add, remove, edit, and reorder are all
handled uniformly because any of those operations changes either the
name sequence or the hash sequence.

Each step’s hash folds in the framework hash, the trim identity and
every code registry fingerprint. So a code edit moves every step’s hash.
The divergence point lands at step 1, and the whole sequence replays
against the freshly-applied code columns.

A typical phase 3 registration block looks like:

``` r
study$register_randvars("demographics",        rv_demographics)
study$register_randvars("exposure",            rv_exposure)
study$register_randvars("baseline_exclusion",  rv_baseline_exclusion)
```

If `baseline_exclusion` reads a column produced by `exposure` (a common
pattern: exclusion depends on exposure classification), then editing
`exposure` triggers the divergence point at step 2 and both step 2 and
step 3 replay. That dependency is implicit in the registration order –
there is no explicit dep graph.

### The `Skeleton` R6 class

Each per-batch file on disk is a serialized `Skeleton` object, not a
bare `data.table`. A `Skeleton` carries its own provenance:

``` r
sk <- study$load_skeleton(batch_number = 1L)
sk
#> <Skeleton batch 1>
#>   rows:             8,234,112
#>   cols:             287
#>   framework_hash:   abcd1234efgh
#>   trim_hash:        9f3c77aa1122
#>   phase_order:      framework -> codes -> randvars
#>   randvars steps:   3
#>   applied codes:    127
#>   pipeline_hash:    4d8f99af7b2c

sk$data                          # the underlying data.table
sk$framework_fn_hash             # phase-1 hash
sk$trim_fn_hash                  # phase-1b trim identity
sk$phase_order                   # the order the phases ran in
names(sk$randvars_state)         # applied phase-3 step names in order
length(sk$applied_registry)      # phase-2 entries currently materialized
sk$pipeline_hash()               # rolled-up provenance scalar
```

#### Why R6 and not a bare `data.table`?

Because the provenance IS the interesting state. Without it, you can’t
answer “is this skeleton still valid for the current pipeline?” without
either rebuilding from scratch or trusting a separate sidecar file. With
it, every batch file can answer the question locally via
`sk$pipeline_hash() == study$pipeline_hash()`.

#### The qs2 over-allocation gotcha

`data.table` keeps over-allocated pointer slots (“truelength”) so that
an in-place `:=` does not have to reallocate. qs2 does not keep those
slots. A table read back from disk therefore has a `truelength` of 0,
and the next `:=` inside a function reallocates it. The function then
writes to a shallow copy of its own, and the caller never sees the new
column. data.table reports nothing.

[`swereg::qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
repairs this, and every swereg load path gets the repair.
`RegistryStudy$load_skeleton()` therefore hands you a `Skeleton` whose
`$data` is already over-allocated:

``` r
sk <- study$load_skeleton(1L)
data.table::truelength(sk$data) > ncol(sk$data)   # TRUE
```

The repair reaches a data.table nested inside a list or inside an R6
field, not only a bare one.
[`?qs2_read`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
states exactly what it reaches and what it leaves alone.

Read your own loader’s files with
[`swereg::qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
rather than
[`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html), and it
gets the same repair.

### The code registry: primary and derived entries

#### Primary entries via `register_codes()`

A primary entry ties a code list to a `fn` (e.g. `add_diagnoses`,
`add_cods`, `add_rx`) and a set of raw-data groups to apply it to.
Optional `combine_as` runs `fn` once more on the rbind of all groups to
produce a combined column.

``` r
study$register_codes(
  codes      = list(e11 = c("E11"), vte = c("I26", "I80")),
  fn         = swereg::add_diagnoses,
  groups     = list(ov = "outpatient", sv = "inpatient"),
  combine_as = "os"
)
```

For each `(group_prefix, code_name)` pair, a column is written:
`ov_e11`, `sv_e11`, `ov_vte`, `sv_vte`. Because `combine_as = "os"` is
set, two additional columns are produced: `os_e11 = ov_e11 | sv_e11` and
`os_vte = ov_vte | sv_vte` (computed by re-running `add_diagnoses` on
the rbind of outpatient and inpatient rawbatch data, not by ORing the
columns).

#### The DORS gotcha and `register_derived_codes()`

The pre-derived-codes approach tried to be clever: register ICD-10 codes
once with `groups = list(ov, sv, dors)` and `combine_as = "osd"`, hoping
to get one column covering all three sources. This failed silently
because `add_diagnoses` searches for `hdia`/`dia*`/`ekod*`/`icd*`
columns – none of which exist in the cause-of-death registry. DORS uses
`ulorsak` (underlying cause) and `morsak*` (contributing causes). So
`dors_e11` was always `FALSE` and `osd_e11 = ov_e11 | sv_e11 | FALSE`
was effectively just hospital data. **Deaths never contributed to
outcomes.**

The fix needs two different functions – `add_diagnoses` for hospital,
`add_cods` for DORS – sharing one logical combined column. `combine_as`
can’t express that: it reruns the SAME `fn` on rbind data. So we need a
way to OR together columns that were produced by different
registrations.

Enter `register_derived_codes()`:

``` r
# 1. Hospital (OV + SV) -> ov_*, sv_*, os_*
study$register_codes(
  codes      = ICD10_CODES,
  fn         = swereg::add_diagnoses,
  groups     = list(ov = "outpatient", sv = "inpatient"),
  combine_as = "os"
)

# 2. DORS underlying cause -> dorsu_*
study$register_codes(
  codes   = ICD10_CODES,
  fn      = swereg::add_cods,
  fn_args = list(cod_type = "underlying"),
  groups  = list(dorsu = "dors")
)

# 3. DORS contributing causes -> dorsm_*
study$register_codes(
  codes   = ICD10_CODES,
  fn      = swereg::add_cods,
  fn_args = list(cod_type = "multiple"),
  groups  = list(dorsm = "dors")
)

# 4. Derived: osd_<nm> = os_<nm> | dorsu_<nm> | dorsm_<nm>
study$register_derived_codes(
  codes = ICD10_CODES,
  from  = c("os", "dorsu", "dorsm"),
  as    = "osd"
)
```

A derived entry doesn’t read rawbatch data at all. It iterates its own
`codes` list, looks up `get("<from[1]>_<nm>")`, `get("<from[2]>_<nm>")`,
etc. on the skeleton, ORs them, and writes `<as>_<nm>`. Missing source
columns raise a loud error. Because it runs in registration order during
phase 2 sync, upstream primary entries are guaranteed to have been
applied already.

#### Fingerprint cascade for derived entries

Derived entries fold the fingerprints of their upstream primary entries
into their own fingerprint. That means editing ANY upstream field –
`fn_args`, `groups`, even a single code pattern – cascades into derived
replay without the user having to touch the derived entry. If you flip
`cod_type = "underlying"` to `cod_type = "both"` on the DORS primary,
the next `process_skeletons()` run drops the derived entry’s columns and
rewrites them from the new dorsu output, all on every batch, in order,
without rebuilding anything else.

Cascade is implemented in `code_registry_fingerprints()` as a two-pass
walk:

1.  Compute every primary entry’s fingerprint. It depends on `codes`,
    `label`, `groups`, `fn_args`, `combine_as` and the hash of the
    entry’s `fn`.
2.  For each derived entry, walk entries registered before it, collect
    the primary fingerprints whose `groups` prefixes or `combine_as`
    appear in the derived’s `from`, and hash
    `(kind="derived", codes, from, as, upstream_fps)`.

swereg 26.10.2 added the `fn` hash to that payload, so every primary
fingerprint moves once. Every code entry re-applies on the next run, and
every derived entry cascades with its upstream. This release rebuilds
every skeleton anyway.

### Registering the pipeline

A complete runner script has the shape:

``` r
study <- swereg::RegistryStudy$new(
  data_rawbatch_dir = c("/mnt/shared/rawbatch/", "C:/shared/rawbatch/"),
  data_skeleton_dir = c("/mnt/shared/skeleton/", "C:/shared/skeleton/"),
  data_raw_dir      = c("/mnt/shared/raw/",      "C:/shared/raw/"),
  group_names       = c("lmed", "inpatient", "outpatient",
                        "cancer", "dors", "other")
)

# Phase 1
study$register_framework(my_framework_fn)

# Phase 1b (optional; the only place rows may be deleted)
study$register_trim(my_trim_fn)

# Phase 2: primaries first, then derived
study$register_codes(ICD10_CODES, swereg::add_diagnoses,
  groups = list(ov = "outpatient", sv = "inpatient"), combine_as = "os")
study$register_codes(ICD10_CODES, swereg::add_cods,
  fn_args = list(cod_type = "underlying"),
  groups = list(dorsu = "dors"))
study$register_codes(ICD10_CODES, swereg::add_cods,
  fn_args = list(cod_type = "multiple"),
  groups = list(dorsm = "dors"))
study$register_derived_codes(ICD10_CODES,
  from = c("os", "dorsu", "dorsm"), as = "osd")

# Phase 3 (registration order = execution order)
study$register_randvars("demographics",        rv_demographics)
study$register_randvars("exposure",            rv_exposure)
study$register_randvars("baseline_exclusion",  rv_baseline_exclusion)

# ... rawbatch creation (one-time) ...

study$process_skeletons(n_workers = 4L)
```

The critical invariant: the framework, the trim, the code registry and
the randvars steps must all be registered on `study` BEFORE
`$process_skeletons()` is called. There is no callback argument on
`$process_skeletons()` – the pipeline is declared on the study itself,
not passed in per call.

### The reload-clobber anti-pattern

A subtle failure mode when running the pipeline repeatedly:

``` r
# WRONG
study <- swereg::RegistryStudy$new(...)
study$register_framework(current_framework_fn)     # fresh in-memory
study$register_codes(current_code_list, ...)       # fresh in-memory

if (file.exists(study$meta_file)) {
  study <- swereg::qs2_read(study$meta_file)       # CLOBBER
}
```

The reload path replaces the freshly-registered pipeline with the stale
on-disk version, silently reverting any edits you just made. The fix is
to absorb only the persisted *runtime* state (IDs, batches,
groups_saved) into the fresh study without touching its config:

``` r
# CORRECT
if (file.exists(study$meta_file)) {
  study$adopt_runtime_state_from(swereg::qs2_read(study$meta_file))
}
```

### Provenance verification

Each `Skeleton` can compute its own `pipeline_hash()` from its stored
phase provenance. The study exposes the expected value:

``` r
sk <- study$load_skeleton(1L)
sk$pipeline_hash() == study$pipeline_hash()
# FALSE means this batch is definitely stale
```

Equality is necessary for a synced batch, and it is not sufficient.
`FALSE` means the batch is definitely stale. `TRUE` means only that
nothing changed among the inputs both hashes cover. Those inputs are the
framework function, the trim identity, the phase order, the randvars
sequence and the code registry fingerprints.

Two inputs sit outside both hashes. The rawbatch data is one: nothing
hashes raw content. Whatever a registered function calls or reads is the
other. Each hash covers that function’s own body and formals, and
follows no call into a helper. Change either one and the two hashes stay
equal over a stale skeleton.

Two summary helpers:

``` r
# Per-batch table with framework hash, randvars count, code count, saved_at
study$skeleton_pipeline_hashes()

# Errors loudly if any persisted skeleton is out of sync
study$assert_skeletons_consistent()
```

Downstream TTE consumers call `$assert_skeletons_consistent()`
automatically near the top of
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md),
so you can’t accidentally build a plan against a half-upgraded skeleton
set.

### Cross-host pipeline snapshots

If `data_pipeline_snapshot_dir` is configured at construction time,
`$process_skeletons()` writes one TSV row per host to
`{snapshot_dir}/{host_label}.tsv` after each successful run. The
snapshot file is meant to be `git add`-ed and committed, so the history
of who ran what version of the pipeline on which host lives in
`git log`.

Why per-host files and not a single append-only log: the user typically
runs on 3+ hosts in parallel. A shared append-only file would conflict
on every concurrent run. Per-host files never have the same name, so
concurrent runs produce separate commits that merge without conflict.

A tiny helper shell script like `dev/commit_pipeline_snapshot.sh` (in
your own project repo) can wrap `git add` + `git commit` into a
one-liner the operator runs after the generator finishes. swereg never
runs git commands itself – that would be a side effect the user hasn’t
explicitly asked for.

### Expected re-run costs

After the first full build:

| Edit                                      | Cost                                                                                                                                          |
|-------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| Nothing                                   | Per-batch no-op: load `Skeleton`, see nothing changed, save. Seconds per batch.                                                               |
| One code pattern in one primary entry     | Drop that entry’s columns on all batches and re-apply it: roughly `(phase-2 total) / (N primaries)`. Then replay the whole randvars sequence. |
| One primary’s `fn_args` (e.g. `cod_type`) | Same as above, plus the cascade re-applies any derived entries that depend on it.                                                             |
| One primary’s registered `fn` body        | Same as above. The fingerprint folds in that function’s body and formals.                                                                     |
| One phase-3 step’s body                   | Drop that step’s columns on all batches, replay it and everything downstream of it in the randvars sequence.                                  |
| The trim function’s body                  | Full rebuild from scratch, all four phases.                                                                                                   |
| The framework function’s body             | Full rebuild from scratch, all four phases. Rare.                                                                                             |

An “edit one ICD-10 code” run re-applies that one entry and replays the
whole randvars sequence. It never re-runs the framework, the trim, or
the other code entries. On a national-registry pipeline with a multi-day
first build, that is still far below a full rebuild.

### Summary

- **Per-batch skeleton files are `Skeleton` R6 objects.** Each carries
  the framework hash, the trim identity, the phase order, the ordered
  randvars state, and the applied code registry fingerprint map.
- **Four phases, each with its own invalidation rule**: framework (full
  rebuild on hash change), trim (full rebuild on identity change), codes
  (per-entry fingerprint diff), randvars (divergence-point rewind and
  replay).
- **Only the phase-1b trim may delete rows.** A code entry or a randvars
  step that changes the row count stops the run.
- **Primary and derived code entries** let you combine outputs from
  different registration functions into one logical column. Derived
  entries cascade upstream fingerprint changes automatically.
- **Provenance verification** via `pipeline_hash()` lets any consumer
  check whether a skeleton matches the current pipeline.
- **`adopt_runtime_state_from()`** is the correct reload pattern – never
  clobber the freshly-registered pipeline by reassigning from disk.

For further context:

- [`?RegistryStudy`](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)
  – full method reference
- [`?Skeleton`](https://papadopoulos-lab.github.io/swereg/reference/Skeleton.md)
  – per-batch provenance object
- [`vignette("skeleton-concept")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-concept.md)
  – why the person-week time grid
- [`vignette("rowdep-rowind-concept")`](https://papadopoulos-lab.github.io/swereg/articles/rowdep-rowind-concept.md)
  – variable-type conventions
