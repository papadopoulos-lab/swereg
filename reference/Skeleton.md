# Skeleton: per-batch time grid + derived columns with provenance

A `Skeleton` is a single batch's person-week data.table plus its full
provenance. The provenance is five things:

- the hash of the framework function that built the base time grid

- the identity of the trim function that deleted rows from it

- the phase order that produced it

- an ordered record of every randvars function applied to it

- a fingerprint map of every code_registry entry whose columns live in
  the data

This is the on-disk unit produced by
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$process_skeletons()`.
One file per batch.

`Skeleton` objects are rarely constructed directly. Use
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$load_skeleton(batch_number)`
to read one from disk and
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$save_skeleton(sk)`
to write one back.

## Phase provenance fields

- `framework_fn_hash`:

  xxhash64 of `list(body(fn), formals(fn))` for the framework function
  that built `self$data`. Used by `$process_skeletons()` to decide
  whether to rebuild this batch from scratch (phase 1) when the
  framework code has changed.

- `trim_fn_hash`:

  Identity of the trim function (phase 1b) that ran on `self$data`.
  Three values, and each means something different:

  - An xxhash64 digest: that trim function ran.

  - `"__swereg_no_trim__"`: the study registered no trim, and this
    skeleton was built by a swereg that knows about trims.

  - `NULL`: this skeleton was written before the trim phase existed.
    `$process_skeletons()` rebuilds it once.

  The last two MUST stay distinct. If both were `NULL`, adding a trim to
  an existing study would rebuild nothing.

- `phase_order`:

  Character vector naming the order the phases ran in. This swereg
  writes `c("framework", "codes", "randvars")`. A skeleton written by a
  swereg that ran the code registry after randvars carries `NULL`, and
  `$process_skeletons()` rebuilds it once. The rebuild is the only
  correct answer. A randvars step may read a code column, and no rewind
  can add a value the old order never wrote.

- `applied_registry`:

  Named list keyed by code_registry entry fingerprint. Each value is a
  minimal descriptor sufficient to recompute the entry's column names
  via `.entry_columns()` at drop time, without re-running `fn`:

  - Primary entries (from `$register_codes()`) store
    `list(codes, groups, combine_as, label, fn_args)`.

  - Derived entries (from `$register_derived_codes()`) store
    `list(kind = "derived", codes, from, as, label)`. `.entry_columns()`
    branches on the entry's `kind` field (defaulting to `"primary"` when
    absent) so both shapes produce the right column predictions at drop
    time.

  The entry's `fn` is NOT stored – serializing R function objects
  carries enclosing-environment bloat and we never call `fn` at drop
  time anyway.

- `randvars_state`:

  Named ordered list, one entry per phase-3 step that's been applied.
  Each value is `list(fn_hash = ..., added_columns = ...)`. `fn_hash` is
  the hash of the function that ran; `added_columns` is the character
  vector of column names it wrote, recorded via a before/after diff at
  apply time (since randvars functions are arbitrary user code whose
  outputs can't be predicted from metadata).

## See also

[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)
for the pipeline that produces and consumes `Skeleton` objects;
[CandidatePath](https://papadopoulos-lab.github.io/swereg/reference/CandidatePath.md)
for the directory resolution mechanism behind `study$load_skeleton()` /
`$save_skeleton()`.

Other skeleton_pipeline:
[`RegistryStudy`](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)

## Public fields

- `data`:

  The underlying `data.table` (time grid + derived columns).
  `$apply_code_entry()` and `$sync_randvars()` MAY replace it with a new
  object, because R cannot grow a column list in place. Read `sk$data`
  at the point of use. An alias taken earlier is stale.

- `batch_number`:

  Integer batch index.

- `framework_fn_hash`:

  xxhash64 of the framework function that built `self$data`.

- `trim_fn_hash`:

  Identity of the trim function (phase 1b) that ran on `self$data`. An
  xxhash64 digest, or the sentinel `"__swereg_no_trim__"` when the study
  registers no trim, or `NULL` when this skeleton predates the trim
  phase.

- `phase_order`:

  Character vector naming the order the phases ran in.
  `$process_skeletons()` stamps it on every rebuild, exactly as it
  stamps `framework_fn_hash`. `NULL` on a fresh object, and on a
  skeleton that predates the move of the code registry ahead of
  randvars.

- `applied_registry`:

  Named list (keyed by code_registry entry fingerprint). Each value is a
  minimal descriptor: for primary entries it's
  `list(codes, groups, combine_as, label, fn_args)`; for derived entries
  (from `$register_derived_codes()`) it's
  `list(kind = "derived", codes, from, as, label)`. See the class-level
  "Phase provenance fields" section for why both shapes omit `fn`.

- `randvars_state`:

  Named ordered list, one entry per phase-3 step that's been applied.
  Each value is `list(fn_hash = ..., added_columns = ...)`.

- `created_at`:

  POSIXct timestamp for when this `Skeleton` object was constructed.

## Methods

### Public methods

- [`Skeleton$new()`](#method-Skeleton-initialize)

- [`Skeleton$check_version()`](#method-Skeleton-check_version)

- [`Skeleton$pipeline_hash()`](#method-Skeleton-pipeline_hash)

- [`Skeleton$apply_code_entry()`](#method-Skeleton-apply_code_entry)

- [`Skeleton$refresh_code_entry_counts()`](#method-Skeleton-refresh_code_entry_counts)

- [`Skeleton$drop_code_entry()`](#method-Skeleton-drop_code_entry)

- [`Skeleton$sync_with_registry()`](#method-Skeleton-sync_with_registry)

- [`Skeleton$sync_randvars()`](#method-Skeleton-sync_randvars)

- [`Skeleton$save()`](#method-Skeleton-save)

- [`Skeleton$print()`](#method-Skeleton-print)

- [`Skeleton$clone()`](#method-Skeleton-clone)

------------------------------------------------------------------------

### `Skeleton$new()`

Construct a new `Skeleton` wrapping an existing `data.table`. Typically
called by
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$process_skeletons()`
after the framework function produces the base time grid.

#### Usage

    Skeleton$new(data, batch_number)

#### Arguments

- `data`:

  The base `data.table` to wrap.

- `batch_number`:

  Integer batch index.

------------------------------------------------------------------------

### `Skeleton$check_version()`

Check this object's schema version against the current `Skeleton` schema
version. Errors with an actionable migration message on mismatch.

#### Usage

    Skeleton$check_version()

------------------------------------------------------------------------

### `Skeleton$pipeline_hash()`

Compute this skeleton's total pipeline hash from its own stored
provenance.

`sk$pipeline_hash() == study$pipeline_hash()` is necessary for a synced
skeleton. It is not sufficient. Unequal hashes mean the skeleton is
definitely stale. Equal hashes mean only that nothing changed among the
inputs both hashes cover. Those inputs are the framework function, the
trim identity, the phase order, the randvars sequence and the code
registry fingerprints.

Two inputs sit outside both hashes: the rawbatch data, and whatever a
registered function calls or reads from its environment. A change to
either one leaves the hashes equal over a stale skeleton. See
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$randvars_hashes()`
for why.

A skeleton written before `phase_order` existed carries `NULL` there, so
its hash differs and `$assert_skeletons_consistent()` names it.

#### Usage

    Skeleton$pipeline_hash()

#### Returns

A single character string (xxhash64 digest).

------------------------------------------------------------------------

### `Skeleton$apply_code_entry()`

Apply one code_registry entry to `self$data`. The method also records a
minimal descriptor of the entry under its fingerprint, so a future
`$drop_code_entry(fingerprint)` call knows which columns to remove.

The method assigns the applier's return value to `self$data`. It has to.
data.table cannot grow a column-pointer vector in place, so an entry
that runs out of free column slots writes its columns to a new table.
The applier reserves the slots the entry needs before it runs, which
keeps the common case in place.

A name that pointed at `$data` before the call is stale after a growth.
`$data` holds the new table and the other name holds the old one. Read
`sk$data` each time rather than hold an alias.

On error nothing is recorded, and
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$process_skeletons()`
never writes that batch. The run halts, so no partial state reaches a
skeleton file.

`$data` is unusable after an error. It MAY carry the entry's writes and
it MAY not, because the answer depends on whether the entry had to grow
the table. A write into the table it was given lands on `$data`. A write
into a grown copy does not, because the assignment back to `$data` never
runs. Discard the object and rerun the batch.

The stored descriptor shape depends on `entry$kind`: primary entries
store the `codes/groups/combine_as/label/fn_args` quintuple, derived
entries store `list(kind = "derived", codes, from, as, label)`. For
derived entries, `batch_data` is unused – the apply just ORs
already-existing skeleton columns under new names.

#### Usage

    Skeleton$apply_code_entry(entry, batch_data, id_col, fingerprint)

#### Arguments

- `entry`:

  A code_registry entry (as constructed by
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$register_codes()`
  or
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$register_derived_codes()`).

- `batch_data`:

  Named list of data.tables from
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$load_rawbatch()`.
  Ignored for derived entries.

- `id_col`:

  Character. Person-ID column name.

- `fingerprint`:

  Character. The xxhash64 fingerprint for `entry` (computed by
  [RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$code_registry_fingerprints()`).

------------------------------------------------------------------------

### `Skeleton$refresh_code_entry_counts()`

Recompute the per-column counts of every applied code entry from this
skeleton's current data. Call it after the last phase runs, so the
counts describe the skeleton that gets written.

`$apply_code_entry()` records no counts.
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$save_skeleton()`
is the one site that computes them. It calls this method before it
writes the skeleton file and the meta sidecar, so both files report the
same data.

Column names come from `.entry_columns()` on each stored descriptor,
which is the prediction `$drop_code_entry()` also uses. The method skips
a predicted column that the data does not hold.

#### Usage

    Skeleton$refresh_code_entry_counts()

#### Returns

This `Skeleton`, invisibly.

------------------------------------------------------------------------

### `Skeleton$drop_code_entry()`

Drop every column that the registry entry with the given fingerprint
contributed to `self$data`, and clear its descriptor from
`self$applied_registry`. Columns are computed from the stored descriptor
via `.entry_columns()` – no lookup map, no before/after diff.

Tolerates missing columns (e.g. after a partial-state crash): the column
set is intersected with `names(self$data)` before dropping, so the
method is a safe idempotent operation.

#### Usage

    Skeleton$drop_code_entry(fingerprint)

#### Arguments

- `fingerprint`:

  Character. Fingerprint of the entry to drop.

------------------------------------------------------------------------

### `Skeleton$sync_with_registry()`

Bring this skeleton into sync with the given code registry (phase 2 of
`$process_skeletons()`). Entries in `stored - current` are dropped
(their columns removed via `.entry_columns()` on the stored descriptor).
Entries in `current - stored` are applied via `$apply_code_entry()`.

"Changed" entries – same `label` but different `codes` / `groups` / etc.
– are handled automatically without special casing: their old
fingerprint lives in `stored` (so the old descriptor's columns get
dropped) and their new fingerprint lives in `current` (so the new entry
gets freshly applied).

Rawbatches are loaded lazily via `batch_data_loader`: if no new entries
need to be applied, the loader is never called.

#### Usage

    Skeleton$sync_with_registry(current_fps, registry, batch_data_loader, id_col)

#### Arguments

- `current_fps`:

  Character vector of fingerprints for the current registry, in registry
  order.

- `registry`:

  The current `RegistryStudy$code_registry` list.

- `batch_data_loader`:

  Zero-argument closure returning the rawbatch data for this batch.

- `id_col`:

  Character. Person-ID column name.

------------------------------------------------------------------------

### `Skeleton$sync_randvars()`

Bring this skeleton into sync with the currently- registered phase-3
step sequence (phase 3 of `$process_skeletons()`).

Uses "divergence-point + rewind and replay" semantics:

1.  Scan the stored step sequence (`names(self$randvars_state)` + stored
    `fn_hash`s) against the current sequence (`names(randvars_fns)` +
    `randvars_hashes`). Find the first position where the name or hash
    differs, or where one sequence ends.

2.  Rewind: drop the stored `added_columns` of every step from the
    divergence point forward, in stored order.

3.  Replay: run the current steps from the divergence point forward, in
    current order, recording each step's hash + new `added_columns`.

This handles add, remove, edit, and reorder uniformly because any of
those operations changes either the name sequence or the hash sequence,
and the first mismatch point is the divergence point. When no divergence
exists, the method is a no-op and `batch_data_loader` is never called.

A step MUST NOT change the row count. The method compares `nrow` before
and after each replayed function. It stops when the count moves, and
names the step. Row deletion belongs to the trim registered with
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$register_trim()`,
which runs on a fresh base before the code registry.

#### Usage

    Skeleton$sync_randvars(
      randvars_fns,
      randvars_hashes,
      batch_data_loader,
      config
    )

#### Arguments

- `randvars_fns`:

  Named ordered list of phase-3 functions (from
  `RegistryStudy$randvars_fns`).

- `randvars_hashes`:

  Character vector parallel to `randvars_fns` with the xxhash64 of each
  function's body + formals.

- `batch_data_loader`:

  Zero-argument closure returning the rawbatch data for this batch.

- `config`:

  The owning `RegistryStudy` (passed as the third argument to each
  randvars function).

------------------------------------------------------------------------

### `Skeleton$save()`

Save this `Skeleton` to disk as `skeleton_NNN.qs2` inside `dir`. Prefer
[RegistryStudy](https://papadopoulos-lab.github.io/swereg/reference/RegistryStudy.md)`$save_skeleton(sk)`
which supplies `self$data_skeleton_dir` automatically.

#### Usage

    Skeleton$save(dir)

#### Arguments

- `dir`:

  Character. Destination directory.

#### Returns

The full path the file was written to, invisibly.

------------------------------------------------------------------------

### `Skeleton$print()`

Print a compact summary of this skeleton.

#### Usage

    Skeleton$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `Skeleton$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Skeleton$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load a persisted skeleton from disk and inspect its provenance.
sk <- study$load_skeleton(batch_number = 1L)
sk                              # print summary
sk$data                         # the underlying data.table
sk$framework_fn_hash            # hash of the phase-1 fn that built it
sk$trim_fn_hash                 # identity of the phase-1b trim
sk$phase_order                  # the order the phases ran in
names(sk$randvars_state)        # applied phase-3 steps in order
length(sk$applied_registry)     # applied code registry entries
sk$pipeline_hash()              # rolled-up provenance scalar

# Check consistency with the study's current pipeline.
identical(sk$pipeline_hash(), study$pipeline_hash())

# Write back after manual editing (rare; process_skeletons handles
# this automatically).
study$save_skeleton(sk)
} # }
```
