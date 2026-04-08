# RegistryStudy: Unified R6 class for skeleton pipeline

RegistryStudy: Unified R6 class for skeleton pipeline

RegistryStudy: Unified R6 class for skeleton pipeline

## Details

Manages batch directories, batch splitting, code registries, and
skeleton processing.

## Portable Directory Resolution

Directories are stored as candidate path vectors and resolved lazily via
active bindings. The first existing directory wins and is cached. If the
cached path becomes invalid (e.g. after moving to a different machine),
the binding automatically re-resolves from the candidate list.

## Code Registry

Code registrations are declarative. Each \`register_codes()\` call
specifies codes, the function to apply them (e.g. \`add_diagnoses\`),
which data groups to use, and optional prefixing/combining. This
replaces the old system of separate fields per code type.

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

  List of code registration entries. Each entry is a list with: codes,
  fn, fn_args, groups, combine_as, label.

- `created_at`:

  POSIXct. Timestamp when this study was created.

## Active bindings

- `data_rawbatch_dir`:

  Character (read-only). Resolved path for rawbatch files. Lazily
  resolved from candidates.

- `data_skeleton_dir`:

  Character (read-only). Resolved path for skeleton output files.

- `data_raw_dir`:

  Character or NULL (read-only). Resolved path for raw registry files.
  NULL if not configured.

- `skeleton_files`:

  Character vector (read-only). Skeleton output file paths detected on
  disk. Scans \`skeleton_dir\` on each access.

- `expected_skeleton_file_count`:

  Integer (read-only). Expected number of skeleton files (one per
  batch).

- `meta_file`:

  Character. Path to the metadata file.

## Methods

### Public methods

- [`RegistryStudy$new()`](#method-RegistryStudy-new)

- [`RegistryStudy$check_version()`](#method-RegistryStudy-check_version)

- [`RegistryStudy$register_codes()`](#method-RegistryStudy-register_codes)

- [`RegistryStudy$describe_codes()`](#method-RegistryStudy-describe_codes)

- [`RegistryStudy$summary_table()`](#method-RegistryStudy-summary_table)

- [`RegistryStudy$apply_codes_to_skeleton()`](#method-RegistryStudy-apply_codes_to_skeleton)

- [`RegistryStudy$set_ids()`](#method-RegistryStudy-set_ids)

- [`RegistryStudy$save_rawbatch()`](#method-RegistryStudy-save_rawbatch)

- [`RegistryStudy$load_rawbatch()`](#method-RegistryStudy-load_rawbatch)

- [`RegistryStudy$process_skeletons()`](#method-RegistryStudy-process_skeletons)

- [`RegistryStudy$compute_population()`](#method-RegistryStudy-compute_population)

- [`RegistryStudy$delete_rawbatches()`](#method-RegistryStudy-delete_rawbatches)

- [`RegistryStudy$delete_skeletons()`](#method-RegistryStudy-delete_skeletons)

- [`RegistryStudy$reset()`](#method-RegistryStudy-reset)

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

- `batch_size`:

  Integer. Number of IDs per batch. Default: 1000L.

- `seed`:

  Integer. Shuffle seed.

- `id_col`:

  Character. Person ID column name.

------------------------------------------------------------------------

### Method `check_version()`

Check if this object's schema version matches the current class version.
Warns if the object was saved with an older schema version.

#### Usage

    RegistryStudy$check_version()

#### Returns

\`invisible(TRUE)\` if versions match, \`invisible(FALSE)\` otherwise.

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

Apply all registered codes to a skeleton data.table.

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

### Method `process_skeletons()`

Process batches through a user-defined function.

#### Usage

    RegistryStudy$process_skeletons(
      process_fn,
      batches = NULL,
      n_workers = 1L,
      ...
    )

#### Arguments

- `process_fn`:

  Function with signature \`function(batch_data, batch_number,
  config)\`.

- `batches`:

  Integer vector of batch indices, or NULL for all.

- `n_workers`:

  Integer. Number of parallel workers (1 = sequential).

- `...`:

  Additional arguments (unused).

#### Returns

List with \`study\` (self, updated) and \`results\`.

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

### Method `reset()`

Reset the pipeline: delete rawbatches, skeletons, and meta file.

#### Usage

    RegistryStudy$reset()

------------------------------------------------------------------------

### Method `save_meta()`

Save this study object as metadata.

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
  data_rawbatch_dir = c("/linux/path/2026/rawbatch/", "C:/win/2026/rawbatch/"),
  data_skeleton_dir = c("/linux/path/2026/skeleton/", "C:/win/2026/skeleton/"),
  data_raw_dir = c("/linux/path/raw/", "C:/windows/path/raw/"),
  group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors", "other")
)
study$data_rawbatch_dir   # e.g. /linux/path/2026/rawbatch
study$data_skeleton_dir   # e.g. /linux/path/2026/skeleton
study$register_codes(
  codes = list("stroke_any" = c("I60", "I61", "I63")),
  fn = add_diagnoses,
  groups = list(ov = "outpatient", sv = "inpatient", dors = "dors", can = "cancer"),
  combine_as = "osdc"
)
study$register_codes(
  codes = list("rx_n05a" = c("N05A")),
  fn = add_rx,
  fn_args = list(source = "atc"),
  groups = list("lmed")
)
study$set_ids(ids)
study$save_rawbatch("lmed", lmed_data)
study$describe_codes()
result <- study$process_skeletons(my_fn, n_workers = 4L)
} # }
```
