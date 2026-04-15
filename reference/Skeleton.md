# Skeleton: per-batch time grid + derived columns with provenance

A \`Skeleton\` is a single batch's person-week data.table plus its full
provenance: the hash of the framework function that built the base time
grid, an ordered record of every randvars function that has been applied
to it, and a fingerprint map of every code_registry entry whose columns
currently live in the data.

This is the on-disk unit produced by
\[RegistryStudy\]\`\$process_skeletons()\`. One file per batch,
replacing the bare data.table format used before the incremental
code-registry migration. Existing bare-dt files are auto-wrapped on
first load for backwards compatibility.

\`Skeleton\` objects are rarely constructed directly. Use
\[RegistryStudy\]\`\$load_skeleton(batch_number)\` to read one from disk
and \[RegistryStudy\]\`\$save_skeleton(sk)\` to write one back.

## Phase provenance fields

- \`framework_fn_hash\`:

  xxhash64 of \`list(body(fn), formals(fn))\` for the framework function
  that built \`self\$data\`. Used by \`\$process_skeletons()\` to decide
  whether to rebuild this batch from scratch (phase 1) when the
  framework code has changed.

- \`applied_registry\`:

  Named list keyed by code_registry entry fingerprint. Each value is a
  minimal descriptor (\`list(codes, groups, combine_as, label,
  fn_args)\`) sufficient to compute the entry's column names via
  \`.entry_columns()\` at drop time. The entry's \`fn\` is NOT stored –
  serializing R function objects carries enclosing-environment bloat and
  we never call \`fn\` at drop time.

- \`randvars_state\`:

  Named ordered list, one entry per phase-3 step that's been applied.
  Each value is \`list(fn_hash = ..., added_columns = ...)\`.
  \`fn_hash\` is the hash of the function that ran; \`added_columns\` is
  the character vector of column names it wrote, recorded via a
  before/after diff at apply time (since randvars functions are
  arbitrary user code whose outputs can't be predicted from metadata).

## See also

\[RegistryStudy\], \[CandidatePath\]

## Public fields

- `data`:

  The underlying \`data.table\` (time grid + derived columns).

- `batch_number`:

  Integer batch index.

- `framework_fn_hash`:

  xxhash64 of the framework function that built \`self\$data\`, or
  \`NULL\` for legacy skeletons.

- `applied_registry`:

  Named list (keyed by code_registry entry fingerprint). Each value is a
  minimal descriptor: \`list(codes, groups, combine_as, label,
  fn_args)\`. See the class documentation for why \`fn\` is
  intentionally excluded.

- `randvars_state`:

  Named ordered list, one entry per phase-3 step that's been applied.
  Each value is \`list(fn_hash = ..., added_columns = ...)\`.

- `created_at`:

  POSIXct timestamp for when this \`Skeleton\` object was constructed.

## Methods

### Public methods

- [`Skeleton$new()`](#method-Skeleton-new)

- [`Skeleton$check_version()`](#method-Skeleton-check_version)

- [`Skeleton$pipeline_hash()`](#method-Skeleton-pipeline_hash)

- [`Skeleton$apply_code_entry()`](#method-Skeleton-apply_code_entry)

- [`Skeleton$drop_code_entry()`](#method-Skeleton-drop_code_entry)

- [`Skeleton$sync_with_registry()`](#method-Skeleton-sync_with_registry)

- [`Skeleton$sync_randvars()`](#method-Skeleton-sync_randvars)

- [`Skeleton$save()`](#method-Skeleton-save)

- [`Skeleton$print()`](#method-Skeleton-print)

- [`Skeleton$clone()`](#method-Skeleton-clone)

------------------------------------------------------------------------

### Method `new()`

Construct a new \`Skeleton\` wrapping an existing \`data.table\`.
Typically called by \[RegistryStudy\]\`\$process_skeletons()\` after the
framework function produces the base time grid, OR by
\`load_skeleton()\` when wrapping a legacy bare-data.table file.

#### Usage

    Skeleton$new(data, batch_number)

#### Arguments

- `data`:

  The base \`data.table\` to wrap.

- `batch_number`:

  Integer batch index.

------------------------------------------------------------------------

### Method `check_version()`

Check this object's schema version against the current \`Skeleton\`
schema version. Errors with an actionable migration message on mismatch.

#### Usage

    Skeleton$check_version()

------------------------------------------------------------------------

### Method `pipeline_hash()`

Compute this skeleton's total pipeline hash from its own stored
provenance. Invariant: \`sk\$pipeline_hash() == study\$pipeline_hash()\`
iff the skeleton is fully synced with the study's currently-registered
framework + randvars + codes.

#### Usage

    Skeleton$pipeline_hash()

#### Returns

A single character string (xxhash64 digest).

------------------------------------------------------------------------

### Method `apply_code_entry()`

Apply one code_registry entry to \`self\$data\`, mutating it in place,
and record a minimal descriptor of the entry under its fingerprint so a
future \`\$drop_code_entry(fingerprint)\` call knows which columns to
remove.

#### Usage

    Skeleton$apply_code_entry(entry, batch_data, id_col, fingerprint)

#### Arguments

- `entry`:

  A code_registry entry (as constructed by
  \[RegistryStudy\]\`\$register_codes()\`).

- `batch_data`:

  Named list of data.tables from \[RegistryStudy\]\`\$load_rawbatch()\`.

- `id_col`:

  Character. Person-ID column name.

- `fingerprint`:

  Character. The xxhash64 fingerprint for \`entry\` (computed by
  \[RegistryStudy\]\`\$code_registry_fingerprints()\`).

------------------------------------------------------------------------

### Method `drop_code_entry()`

Drop every column that the registry entry with the given fingerprint
contributed to \`self\$data\`, and clear its descriptor from
\`self\$applied_registry\`. Columns are computed from the stored
descriptor via \`.entry_columns()\` – no lookup map, no before/after
diff.

Tolerates missing columns (e.g. after a partial-state crash): the column
set is intersected with \`names(self\$data)\` before dropping, so the
method is a safe idempotent operation.

#### Usage

    Skeleton$drop_code_entry(fingerprint)

#### Arguments

- `fingerprint`:

  Character. Fingerprint of the entry to drop.

------------------------------------------------------------------------

### Method `sync_with_registry()`

Bring this skeleton into sync with the given code registry (phase 2 of
\`\$process_skeletons()\`). Entries in \`stored - current\` are dropped
(their columns removed via \`.entry_columns()\` on the stored
descriptor). Entries in \`current - stored\` are applied via
\`\$apply_code_entry()\`.

"Changed" entries – same \`label\` but different \`codes\` / \`groups\`
/ etc. – are handled automatically without special casing: their old
fingerprint lives in \`stored\` (so the old descriptor's columns get
dropped) and their new fingerprint lives in \`current\` (so the new
entry gets freshly applied).

Rawbatches are loaded lazily via \`batch_data_loader\`: if no new
entries need to be applied, the loader is never called.

#### Usage

    Skeleton$sync_with_registry(current_fps, registry, batch_data_loader, id_col)

#### Arguments

- `current_fps`:

  Character vector of fingerprints for the current registry, in registry
  order.

- `registry`:

  The current \`RegistryStudy\$code_registry\` list.

- `batch_data_loader`:

  Zero-argument closure returning the rawbatch data for this batch.

- `id_col`:

  Character. Person-ID column name.

------------------------------------------------------------------------

### Method `sync_randvars()`

Bring this skeleton into sync with the currently- registered phase-3
step sequence (phase 3 of \`\$process_skeletons()\`).

Uses "divergence-point + rewind and replay" semantics: 1. Scan the
stored step sequence (\`names(self\$randvars_state)\` + stored
\`fn_hash\`s) against the current sequence (\`names(randvars_fns)\` +
\`randvars_hashes\`). Find the first position where the name or hash
differs, or where one sequence ends. 2. Rewind: drop the stored
\`added_columns\` of every step from the divergence point forward, in
stored order. 3. Replay: run the current steps from the divergence point
forward, in current order, recording each step's hash + new
\`added_columns\`.

This handles add, remove, edit, and reorder uniformly because any of
those operations changes either the name sequence or the hash sequence,
and the first mismatch point is the divergence point. When no divergence
exists, the method is a no-op and \`batch_data_loader\` is never called.

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
  \`RegistryStudy\$randvars_fns\`).

- `randvars_hashes`:

  Character vector parallel to \`randvars_fns\` with the xxhash64 of
  each function's body + formals.

- `batch_data_loader`:

  Zero-argument closure returning the rawbatch data for this batch.

- `config`:

  The owning \`RegistryStudy\` (passed as the third argument to each
  randvars function).

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save this \`Skeleton\` to disk as \`skeleton_NNN.qs2\` inside \`dir\`.
Prefer \[RegistryStudy\]\`\$save_skeleton(sk)\` which supplies
\`self\$data_skeleton_dir\` automatically.

#### Usage

    Skeleton$save(dir)

#### Arguments

- `dir`:

  Character. Destination directory.

#### Returns

The full path the file was written to, invisibly.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a compact summary of this skeleton.

#### Usage

    Skeleton$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Skeleton$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
