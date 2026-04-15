# RegistryStudy: Unified R6 class for skeleton pipeline

RegistryStudy: Unified R6 class for skeleton pipeline

RegistryStudy: Unified R6 class for skeleton pipeline

## Details

Manages the full skeleton pipeline lifecycle: portable batch
directories, batch splitting, raw registry loading, the declarative code
registry, and the three-phase orchestrated per-batch processing
(framework -\> randvars -\> codes) that produces one \[Skeleton\] file
per batch with incremental invalidation.

## Portable Directory Resolution

Directories are stored as candidate path vectors and resolved lazily via
\[CandidatePath\] active bindings. The first existing directory wins and
is cached. If the cached path becomes invalid (e.g. after moving to a
different machine), the binding automatically re-resolves from the
candidate list.

## Three-phase pipeline

\`\$process_skeletons()\` runs three phases per batch, with per-phase
incremental invalidation so editing one step only re-runs what it
affects:

- Phase 1 – framework:

  A single user function registered via \`\$register_framework(fn)\`,
  signature \`(batch_data, config)\`, returns a fresh base
  \`data.table\` (time grid + structural censoring). Full rebuild on
  \`body(fn)\` / \`formals(fn)\` hash change.

- Phase 3 – randvars:

  An ordered named list of user functions registered via
  \`\$register_randvars(name, fn)\`, each signature \`(skeleton,
  batch_data, config)\`. Divergence-point rewind-and-replay
  invalidation: the first step whose name or hash differs from the
  stored sequence triggers a drop of its columns and replay of it plus
  everything downstream of it. Add/remove/edit/reorder all handled
  uniformly.

- Phase 2 – codes:

  The declarative code registry, built via \`\$register_codes()\`
  (primary) and \`\$register_derived_codes()\` (derived). Per-entry
  fingerprint diff: entries no longer present are dropped, new or
  modified entries are freshly applied. Derived entry fingerprints fold
  in their upstream primary fingerprints so upstream behavior edits
  cascade correctly.

Phase 2 runs AFTER phase 3, so phase-3 steps cannot read phase-2
columns. See the \[Skeleton\] class for the on-disk provenance format.

## Code Registry

Primary entries are registered via \`\$register_codes()\`, which
declares codes, the function to apply them (e.g. \`add_diagnoses\`,
\`add_cods\`), which rawbatch groups to use, and optional
prefixing/combining. Derived entries are registered via
\`\$register_derived_codes()\` and OR together already-existing skeleton
columns from upstream primary entries – useful when the combined column
needs to draw from registrations that use DIFFERENT \`fn\`s (something
\`combine_as\` can't express because it re-runs the same \`fn\` on
rbound data).

## See also

\[Skeleton\] for the per-batch on-disk format and provenance fields;
\[CandidatePath\] for the multi-host directory resolution mechanism;
\[add_diagnoses\], \[add_cods\], \[add_rx\] for common \`fn\` choices in
\`\$register_codes()\`.

Other skeleton_pipeline:
[`Skeleton`](https://papadopoulos-lab.github.io/swereg/reference/Skeleton.md)

## Public fields

- `group_names`:

  Character vector. Names of rawbatch groups.

- `batch_size`:

  Integer. Number of IDs per batch.

- `seed`:

  Integer. Shuffle seed for reproducibility.

- `id_col`:

  Character. Person ID column name.

- `n_ids`:

  Integer. Total number of IDs across all batches.

- `n_batches`:

  Integer. Number of batches.

- `batch_id_list`:

  List of ID vectors, one per batch.

- `groups_saved`:

  Character vector of rawbatch groups saved to disk.

- `code_registry`:

  List of code registration entries, appended to by
  \`\$register_codes()\` and \`\$register_derived_codes()\`. Primary
  entries (from \`\$register_codes()\`) are plain lists with: \`codes,
  fn, fn_args, groups, combine_as, label\`. Derived entries (from
  \`\$register_derived_codes()\`) are tagged with \`kind = "derived"\`
  and hold \`codes, from, as, label\` instead – no \`fn\`, no
  \`groups\`, no raw data access. The dispatcher
  \`.apply_code_entry_impl()\` branches on the entry's \`kind\` field,
  defaulting to \`"primary"\` when absent.

- `created_at`:

  POSIXct. Timestamp when this study was created.

- `data_rawbatch_cp`:

  \[CandidatePath\] for the rawbatch directory.

- `data_skeleton_cp`:

  \[CandidatePath\] for the skeleton directory.

- `data_raw_cp`:

  \[CandidatePath\] for the raw-registry directory, or NULL if not
  configured.

- `data_pipeline_snapshot_cp`:

  \[CandidatePath\] for the pipeline-snapshot directory (one TSV file
  per host, git-tracked), or NULL if the feature is not configured. When
  NULL, \`\$write_pipeline_snapshot()\` is a silent no-op.

- `framework_fn`:

  Function of signature \`(batch_data, config)\` returning a fresh base
  skeleton \`data.table\` (phase 1). Set via \`\$register_framework()\`.
  \`\$process_skeletons()\` re-runs this function per batch when its
  body/formals hash changes.

- `randvars_fns`:

  Named ordered list of phase-3 functions, each with signature
  \`(skeleton, batch_data, config)\`. Populated via
  \`\$register_randvars(name, fn)\`. Registration order is execution
  order. \`\$process_skeletons()\` uses \`Skeleton\$sync_randvars()\`'s
  divergence-point rewind-and-replay to apply changes incrementally.

- `host_label`:

  Optional character scalar. Overrides \`Sys.info()\[\["nodename"\]\]\`
  when naming the per-host pipeline snapshot file. Useful when hostnames
  are ambiguous or overly dynamic.

## Active bindings

- `data_rawbatch_dir`:

  Character (read-only). Resolved rawbatch directory for the current
  host. Lazily resolved from \`self\$data_rawbatch_cp\`.

- `data_skeleton_dir`:

  Character (read-only). Resolved skeleton directory for the current
  host.

- `data_raw_dir`:

  Character or NULL (read-only). Resolved raw-registry directory, or
  NULL if not configured.

- `data_pipeline_snapshot_dir`:

  Character or NULL (read-only). Resolved pipeline-snapshot directory
  for the current host, or NULL if not configured (snapshot feature
  disabled).

- `skeleton_files`:

  Character vector (read-only). Skeleton output file paths detected on
  disk. Scans \`skeleton_dir\` on each access.

- `expected_skeleton_file_count`:

  Integer (read-only). Expected number of skeleton files (one per
  batch).

- `meta_file`:

  Character. Path to the on-disk metadata file (\`registrystudy.qs2\`)
  inside the rawbatch directory.

## Methods

### Public methods

- [`RegistryStudy$new()`](#method-RegistryStudy-new)

- [`RegistryStudy$check_version()`](#method-RegistryStudy-check_version)

- [`RegistryStudy$register_framework()`](#method-RegistryStudy-register_framework)

- [`RegistryStudy$register_randvars()`](#method-RegistryStudy-register_randvars)

- [`RegistryStudy$code_registry_fingerprints()`](#method-RegistryStudy-code_registry_fingerprints)

- [`RegistryStudy$pipeline_hash()`](#method-RegistryStudy-pipeline_hash)

- [`RegistryStudy$adopt_runtime_state_from()`](#method-RegistryStudy-adopt_runtime_state_from)

- [`RegistryStudy$register_codes()`](#method-RegistryStudy-register_codes)

- [`RegistryStudy$register_derived_codes()`](#method-RegistryStudy-register_derived_codes)

- [`RegistryStudy$describe_codes()`](#method-RegistryStudy-describe_codes)

- [`RegistryStudy$summary_table()`](#method-RegistryStudy-summary_table)

- [`RegistryStudy$apply_codes_to_skeleton()`](#method-RegistryStudy-apply_codes_to_skeleton)

- [`RegistryStudy$set_ids()`](#method-RegistryStudy-set_ids)

- [`RegistryStudy$save_rawbatch()`](#method-RegistryStudy-save_rawbatch)

- [`RegistryStudy$load_rawbatch()`](#method-RegistryStudy-load_rawbatch)

- [`RegistryStudy$load_skeleton()`](#method-RegistryStudy-load_skeleton)

- [`RegistryStudy$save_skeleton()`](#method-RegistryStudy-save_skeleton)

- [`RegistryStudy$skeleton_pipeline_hashes()`](#method-RegistryStudy-skeleton_pipeline_hashes)

- [`RegistryStudy$assert_skeletons_consistent()`](#method-RegistryStudy-assert_skeletons_consistent)

- [`RegistryStudy$write_pipeline_snapshot()`](#method-RegistryStudy-write_pipeline_snapshot)

- [`RegistryStudy$process_skeletons()`](#method-RegistryStudy-process_skeletons)

- [`RegistryStudy$compute_population()`](#method-RegistryStudy-compute_population)

- [`RegistryStudy$delete_rawbatches()`](#method-RegistryStudy-delete_rawbatches)

- [`RegistryStudy$delete_skeletons()`](#method-RegistryStudy-delete_skeletons)

- [`RegistryStudy$delete_meta_file()`](#method-RegistryStudy-delete_meta_file)

- [`RegistryStudy$save_meta()`](#method-RegistryStudy-save_meta)

- [`RegistryStudy$print()`](#method-RegistryStudy-print)

- [`RegistryStudy$clone()`](#method-RegistryStudy-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new RegistryStudy object.

#### Usage

    RegistryStudy$new(
      data_rawbatch_dir,
      group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors", "other"),
      data_skeleton_dir = data_rawbatch_dir,
      data_raw_dir = NULL,
      data_pipeline_snapshot_dir = NULL,
      batch_size = 1000L,
      seed = 4L,
      id_col = "lopnr"
    )

#### Arguments

- `data_rawbatch_dir`:

  Character vector of candidate paths for rawbatch files. The first
  existing path is used; a single non-existing path is created
  automatically.

- `group_names`:

  Character vector of rawbatch group names.

- `data_skeleton_dir`:

  Character vector of candidate paths for skeleton output. Defaults to
  same candidates as \`data_rawbatch_dir\`.

- `data_raw_dir`:

  Character vector of candidate paths for raw registry files (optional).
  NULL if raw data paths are managed externally.

- `data_pipeline_snapshot_dir`:

  Optional character vector of candidate paths for a git-tracked
  pipeline-snapshot directory (one TSV per host). When NULL (default),
  the snapshot feature is disabled and \`\$write_pipeline_snapshot()\`
  is a no-op.

- `batch_size`:

  Integer. Number of IDs per batch. Default: 1000L.

- `seed`:

  Integer. Shuffle seed.

- `id_col`:

  Character. Person ID column name.

------------------------------------------------------------------------

### Method `check_version()`

Check if this object's schema version matches the current class version.
Errors if the object was saved with an older schema.

#### Usage

    RegistryStudy$check_version()

#### Returns

\`invisible(TRUE)\` if versions match. Errors otherwise with an
actionable migration message.

------------------------------------------------------------------------

### Method `register_framework()`

Register the framework function (phase 1). Called once per batch at the
start of \`\$process_skeletons()\`, with signature
\`function(batch_data, config)\`, returns a fresh \`data.table\`
containing the base time grid + censoring. Everything downstream builds
on this output. A change to the function body or formals triggers a full
rebuild of every batch on the next \`\$process_skeletons()\` run.

#### Usage

    RegistryStudy$register_framework(fn)

#### Arguments

- `fn`:

  A function of signature \`(batch_data, config)\` returning a
  \`data.table\`.

#### Returns

\`invisible(self)\`.

------------------------------------------------------------------------

### Method `register_randvars()`

Register one phase-3 "random variables" step. Phase 3 is an ordered
sequence of user-supplied functions; each call to
\`\$register_randvars()\` appends one step to the end of the sequence.
Registration order is execution order at \`\$process_skeletons()\` time.

Signature of \`fn\`: \`function(skeleton, batch_data, config)\`. It
mutates \`skeleton\` in place and must ONLY ADD columns (never modifying
or deleting existing ones – the drop-and-replay tracking depends on this
invariant).

Editing \`fn\`'s body (keeping the same \`name\`) changes the hash and
triggers a re-run of this step and everything downstream of it in the
sequence.

#### Usage

    RegistryStudy$register_randvars(name, fn)

#### Arguments

- `name`:

  Character scalar. The user-facing step name. Used as the key in
  \`Skeleton\$randvars_state\` and in the divergence-point comparison.

- `fn`:

  A function of signature \`(skeleton, batch_data, config)\`.

#### Returns

\`invisible(self)\`.

------------------------------------------------------------------------

### Method `code_registry_fingerprints()`

Return the xxhash64 fingerprint of every entry in
\`self\$code_registry\`, in registry order.

Primary entries: fingerprint depends on \`(codes, label, groups,
fn_args, combine_as)\` – two primary entries with identical config
produce the same fingerprint and are therefore treated as "the same
entry" across runs.

Derived entries: fingerprint depends on \`(codes, from, as)\` PLUS the
fingerprints of every upstream primary entry whose output prefix is
referenced in \`from\`. This cascades invalidation when an upstream
primary's \`fn_args\` / \`groups\` / \`codes\` change, without requiring
the user to touch the derived entry. Computed in a two-pass walk:
primary fingerprints first, then derived fingerprints using the
already-computed upstream fingerprints.

Used by \`Skeleton\$sync_with_registry()\` for incremental per-entry
add/drop.

#### Usage

    RegistryStudy$code_registry_fingerprints()

#### Returns

Character vector of fingerprints.

------------------------------------------------------------------------

### Method `pipeline_hash()`

Compute this study's current total pipeline hash from the registered
framework, randvars sequence, and code registry. Answer to "what would a
freshly-built skeleton look like?"

Invariant: \`sk\$pipeline_hash() == study\$pipeline_hash()\` iff the
skeleton is fully synced with the study's current registered framework +
randvars + codes.

#### Usage

    RegistryStudy$pipeline_hash()

#### Returns

A single character string (xxhash64 digest).

------------------------------------------------------------------------

### Method `adopt_runtime_state_from()`

Copy runtime state (IDs, batch list, saved groups) from another
\`RegistryStudy\` into this one, WITHOUT touching config fields
(group_names, code_registry, directory candidates, framework/randvars
registration, schema version, etc.).

Use case: in \`run_generic_create_datasets_v2.R\`, the generator script
constructs a fresh study every run with the current in-memory config,
then on re-runs calls
\`\$adopt_runtime_state_from(qs2_read(self\$meta_file))\` to pick up
batch ids and saved-group state without silently adopting a stale code
registry or group name list.

#### Usage

    RegistryStudy$adopt_runtime_state_from(other)

#### Arguments

- `other`:

  Another \`RegistryStudy\` to copy runtime state from.

#### Returns

\`invisible(self)\`.

------------------------------------------------------------------------

### Method `register_codes()`

Register code definitions for the code registry.

Each call declares codes, the function to apply them, which batch data
groups to use, and optional prefixing/combining. Appends to
\`self\$code_registry\`.

#### Usage

    RegistryStudy$register_codes(
      codes,
      fn,
      groups,
      fn_args = list(),
      combine_as = NULL,
      label = NULL
    )

#### Arguments

- `codes`:

  Named list of code vectors (e.g. ICD-10, ATC, operation codes).

- `fn`:

  Function to call (e.g. \`add_diagnoses\`, \`add_rx\`).

- `groups`:

  Named list mapping prefixes to group names. Unnamed elements get no
  prefix. Each element is a character vector of group names to rbindlist
  before calling \`fn\`.

- `fn_args`:

  Named list of extra arguments to pass to \`fn\` (e.g. \`list(source =
  "atc")\`).

- `combine_as`:

  Character or NULL. If non-NULL, also run \`fn\` on all groups
  combined, using this as the prefix.

- `label`:

  Character. Human-readable label for describe_codes() output. Defaults
  to deparse(substitute(fn)).

------------------------------------------------------------------------

### Method `register_derived_codes()`

Register a derived code entry: one that doesn't read rawbatch data, but
instead ORs together already-existing skeleton columns from earlier
primary entries.

For each name \`\<nm\>\` in \`codes\`, a new column \`\<as\>\_\<nm\>\`
is written as \`Reduce("\|", list(get("\<from\[1\]\>\_\<nm\>"), ...))\`.
The \`codes\` list pattern values are ignored at apply time but DO
participate in the fingerprint, so editing the code list triggers
replay. The fingerprint also folds in the fingerprints of every upstream
primary entry whose output prefix appears in \`from\`, so upstream
behavior edits (e.g. \`cod_type\` on an \`add_cods\` primary) cascade
into derived replay automatically.

The derived entry runs in registration order during phase-2 sync, so any
primary registrations whose output columns it references MUST be
registered BEFORE this call.

#### Usage

    RegistryStudy$register_derived_codes(codes, from, as)

#### Arguments

- `codes`:

  Named list. Keys name the output columns' suffixes; the pattern values
  are ignored at apply time.

- `from`:

  Character vector of source prefixes (e.g. \`c("os", "dorsu",
  "dorsm")\`).

- `as`:

  Character scalar: the output column prefix.

------------------------------------------------------------------------

### Method `describe_codes()`

Print human-readable description of all registered codes.

#### Usage

    RegistryStudy$describe_codes()

------------------------------------------------------------------------

### Method `summary_table()`

Return a data.table summarizing all registered codes.

#### Usage

    RegistryStudy$summary_table()

#### Returns

data.table with columns: name, codes, label, generated_columns.

------------------------------------------------------------------------

### Method `apply_codes_to_skeleton()`

Apply all registered codes to a skeleton data.table. Thin loop over
\`self\$code_registry\` that delegates per-entry work to the file-level
\`.apply_code_entry_impl()\` helper. Kept for backwards-compatible
"apply everything at once" callers; the incremental code-registry sync
inside the Skeleton R6 class calls \`.apply_code_entry_impl()\` directly
on one entry at a time.

#### Usage

    RegistryStudy$apply_codes_to_skeleton(skeleton, batch_data)

#### Arguments

- `skeleton`:

  data.table. The person-week skeleton to modify in place.

- `batch_data`:

  Named list of data.tables from load_rawbatch().

------------------------------------------------------------------------

### Method `set_ids()`

Set IDs and split into batches.

#### Usage

    RegistryStudy$set_ids(ids)

#### Arguments

- `ids`:

  Vector of person IDs.

------------------------------------------------------------------------

### Method `save_rawbatch()`

Save rawbatch files for one group.

#### Usage

    RegistryStudy$save_rawbatch(group, data)

#### Arguments

- `group`:

  Character. Group name (must be in group_names).

- `data`:

  data.table or named list of data.tables.

------------------------------------------------------------------------

### Method `load_rawbatch()`

Load rawbatch files for a single batch.

#### Usage

    RegistryStudy$load_rawbatch(batch_number)

#### Arguments

- `batch_number`:

  Integer. 1-indexed batch number.

#### Returns

Named list of data.tables.

------------------------------------------------------------------------

### Method `load_skeleton()`

Load a skeleton file for \`batch_number\` as a \[Skeleton\] R6 object.
Returns \`NULL\` if the file is missing (caller rebuilds from scratch).

Legacy bare-\`data.table\` files (from before the Skeleton R6 migration)
are auto-wrapped in a new \`Skeleton\` with empty \`framework_fn_hash\`
/ \`applied_registry\` / \`randvars_state\`. The next
\`\$process_skeletons()\` call will observe the empty framework hash,
mismatch the current one, and trigger a full rebuild of that batch – the
safe fallback for files predating the provenance tracking.

#### Usage

    RegistryStudy$load_skeleton(batch_number)

#### Arguments

- `batch_number`:

  Integer batch index.

#### Returns

A \[Skeleton\], or \`NULL\` if the file is missing.

------------------------------------------------------------------------

### Method `save_skeleton()`

Save a \[Skeleton\] to this study's skeleton directory. Thin wrapper
around \`sk\$save(self\$data_skeleton_dir)\` so callers never have to
know or pass the directory explicitly.

#### Usage

    RegistryStudy$save_skeleton(sk)

#### Arguments

- `sk`:

  A \[Skeleton\] to persist.

#### Returns

The full path the file was written to, invisibly.

------------------------------------------------------------------------

### Method `skeleton_pipeline_hashes()`

Summary of per-batch pipeline hashes across all currently-persisted
skeleton files in \`self\$data_skeleton_dir\`. Use this to spot batches
out of sync with each other or with \`self\$pipeline_hash()\`.

Legacy bare-\`data.table\` files surface as rows with \`NA\`
\`pipeline_hash\` and \`NA\` \`framework_fn_hash\`.

#### Usage

    RegistryStudy$skeleton_pipeline_hashes()

#### Returns

A \`data.table\` with columns: batch, pipeline_hash, framework_fn_hash,
n_randvars, n_code_entries, saved_at.

------------------------------------------------------------------------

### Method `assert_skeletons_consistent()`

Assert that every persisted skeleton file has the same pipeline hash AND
that it matches this study's current pipeline hash. Errors loudly with
an actionable message if not.

Intended as a pre-flight check at the top of downstream consumers like
\`tteplan_from_spec_and_registrystudy()\`, so partial-rebuild stragglers
or config drift never silently flow into a TTE plan.

#### Usage

    RegistryStudy$assert_skeletons_consistent()

#### Returns

The single pipeline hash on success, invisibly.

------------------------------------------------------------------------

### Method `write_pipeline_snapshot()`

Write a one-row TSV snapshot of this host's current pipeline state to
\`data_pipeline_snapshot_dir\` / \`host_label.tsv\` (one file per host).
The file is OVERWRITTEN (not appended) on each call, so concurrent runs
from different hosts never conflict in git. The chronological audit
trail is \`git log -p dev/pipeline_snapshots/your_host.tsv\`.

Silently skipped when \`self\$data_pipeline_snapshot_cp\` is NULL
(feature not configured) or when the candidate directory does not exist
on the current host (e.g. hosts without the git repo mounted).

The \`host_label\` defaults to \`Sys.info()\[\["nodename"\]\]\` but can
be overridden by setting \`self\$host_label\` when hostnames are
ambiguous.

#### Usage

    RegistryStudy$write_pipeline_snapshot()

#### Returns

Invisibly: the written path, or NULL if skipped.

------------------------------------------------------------------------

### Method `process_skeletons()`

Orchestrate the three-phase skeleton pipeline per batch.

Reads \`self\$framework_fn\` (phase 1), \`self\$randvars_fns\` (phase
3), and \`self\$code_registry\` (phase 2) from the study and applies
them via the incremental logic on \[Skeleton\]. Exact per-batch work:

1\. Load existing skeleton via \`self\$load_skeleton(i)\`. If missing OR
its \`framework_fn_hash\` doesn't match the current framework's hash,
rebuild the base skeleton from scratch by calling
\`self\$framework_fn(batch_data, self)\` and wrapping in a fresh
\[Skeleton\]. (Phase 1.) 2. Call \`sk\$sync_randvars()\` with the
current ordered \`self\$randvars_fns\` and their body/formals hashes.
Divergence- point rewind-and-replay semantics drop and re-run the
affected phase-3 steps only. (Phase 3.) 3. Call
\`sk\$sync_with_registry()\` with
\`self\$code_registry_fingerprints()\`. Entries present on disk but not
in the current registry are dropped (via \`.entry_columns()\` on the
stored descriptor); entries present in the current registry but not on
disk are applied fresh. (Phase 2.) 4. Save via
\`self\$save_skeleton(sk)\`.

\`batch_data\` is loaded lazily – exactly once per batch, by whichever
phase needs it first. If no phase needs it (everything already in sync),
the rawbatch read is skipped entirely and the per-batch work is just
load → save.

At the end of the full batch loop, \`self\$write_pipeline_snapshot()\`
is called (silently no-ops when \`data_pipeline_snapshot_cp\` is NULL).

#### Usage

    RegistryStudy$process_skeletons(batches = NULL, n_workers = 1L, ...)

#### Arguments

- `batches`:

  Integer vector of batch indices to process, or \`NULL\` (default) for
  all batches in \`self\$batch_id_list\`.

- `n_workers`:

  Integer. Number of parallel workers (1 = sequential). When \`\> 1\`,
  each batch runs in a fresh callr subprocess.

- `...`:

  Additional arguments (unused; reserved for future use).

#### Returns

\`invisible(self)\`.

------------------------------------------------------------------------

### Method `compute_population()`

Compute a population table from saved skeleton files.

Loads each skeleton file, counts unique persons per `isoyear` and
user-specified structural variables, and returns a complete grid with
all combinations (missing cells filled with zero).

Both annual (`is_isoyear == TRUE`) and weekly (`is_isoyear == FALSE`)
rows are handled: each person is counted once per `isoyear` per unique
combination of `by` variables via `uniqueN(id)`.

#### Usage

    RegistryStudy$compute_population(by, batches = NULL)

#### Arguments

- `by`:

  Character vector of column names to group by in addition to `isoyear`.
  For example, `c("saab", "age")` for sex by 1-year age groups.

- `batches`:

  Integer vector of batch numbers to include. Default `NULL` uses all
  available skeleton files.

#### Returns

A data.table with columns: `isoyear`, the `by` columns, and `n` (person
count). Also saved as `population.qs2` in the skeleton directory.

------------------------------------------------------------------------

### Method `delete_rawbatches()`

Delete all rawbatch files from disk.

#### Usage

    RegistryStudy$delete_rawbatches()

------------------------------------------------------------------------

### Method `delete_skeletons()`

Delete all skeleton output files from disk.

#### Usage

    RegistryStudy$delete_skeletons()

------------------------------------------------------------------------

### Method `delete_meta_file()`

Delete the metadata file from disk.

#### Usage

    RegistryStudy$delete_meta_file()

------------------------------------------------------------------------

### Method `save_meta()`

Save this study object as metadata. Captures the destination path first,
then clears host-specific \[CandidatePath\] caches before writing, so
the on-disk file never carries a resolved path from the saving host.

#### Usage

    RegistryStudy$save_meta()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for RegistryStudy.

#### Usage

    RegistryStudy$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RegistryStudy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
study <- RegistryStudy$new(
  data_rawbatch_dir = c("/linux/.../rawbatch/", "C:/win/.../rawbatch/"),
  data_skeleton_dir = c("/linux/.../skeleton/", "C:/win/.../skeleton/"),
  data_raw_dir      = c("/linux/.../raw/",      "C:/win/.../raw/"),
  group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors")
)

# Phase 1: framework (structural time grid + censoring)
study$register_framework(my_framework_fn)

# Phase 3: randvars (ordered user steps; order = execution order)
study$register_randvars("demographics", my_demographics_fn)
study$register_randvars("exposure",     my_exposure_fn)

# Phase 2: codes. Primary entries first, derived entries after.
study$register_codes(
  codes      = list(f20 = c("F20"), vte = c("I26", "I80")),
  fn         = swereg::add_diagnoses,
  groups     = list(ov = "outpatient", sv = "inpatient"),
  combine_as = "os"
)
study$register_codes(
  codes   = list(f20 = c("F20"), vte = c("I26", "I80")),
  fn      = swereg::add_cods,
  fn_args = list(cod_type = "underlying"),
  groups  = list(dorsu = "dors")
)
study$register_codes(
  codes   = list(f20 = c("F20"), vte = c("I26", "I80")),
  fn      = swereg::add_cods,
  fn_args = list(cod_type = "multiple"),
  groups  = list(dorsm = "dors")
)
# Build osd_f20 = os_f20 | dorsu_f20 | dorsm_f20 (same codes list
# shared by reference so an edit in one place cascades to all four)
study$register_derived_codes(
  codes = list(f20 = c("F20"), vte = c("I26", "I80")),
  from  = c("os", "dorsu", "dorsm"),
  as    = "osd"
)

study$set_ids(ids)
study$save_rawbatch("lmed", lmed_data)
study$describe_codes()
study$process_skeletons(n_workers = 4L)

# Per-batch provenance and cross-batch consistency check
sk <- study$load_skeleton(1L)
sk$pipeline_hash() == study$pipeline_hash()  # TRUE iff in sync
study$assert_skeletons_consistent()          # errors on mixed state
} # }
```
