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

Four named lists define diagnosis/medication/operation codes:

- icd10_codes:

  ICD-10 codes applied to 4 registries (ov, sv, dors, can). Generates 5
  columns per entry: \`ov\_\`, \`sv\_\`, \`dors\_\`, \`can\_\`,
  \`osdc\_\` (all combined).

- rx_atc_codes:

  ATC codes applied to lmed. 1 column per entry, no prefix.

- rx_produkt_codes:

  Product names applied to lmed. 1 column per entry, no prefix.

- operation_codes:

  Operation codes applied to sv+ov combined. 1 column per entry, no
  prefix.

## Public fields

- `group_names`:

  Character vector. Names of rawbatch groups.

- `batch_sizes`:

  Integer vector. IDs per batch (first = dev, second = rest).

- `seed`:

  Integer. Shuffle seed for reproducibility.

- `id_col`:

  Character. Person ID column name.

- `ids_per_skeleton_file`:

  Integer. IDs per skeleton sub-file.

- `n_ids`:

  Integer. Total number of IDs across all batches.

- `n_batches`:

  Integer. Number of batches.

- `batch_id_list`:

  List of ID vectors, one per batch.

- `groups_saved`:

  Character vector of rawbatch groups saved to disk.

- `icd10_codes`:

  Named list of character vectors. ICD-10 codes applied to 4 diagnosis
  registries (ov, sv, dors, can) + combined (osdc).

- `rx_atc_codes`:

  Named list of character vectors. ATC codes applied to lmed.

- `rx_produkt_codes`:

  Named list of character vectors. Product names applied to lmed.

- `operation_codes`:

  Named list of character vectors. Operation codes applied to sv+ov.

- `icdo3_codes`:

  Named list of character vectors. ICD-O-3 topography codes applied to
  the cancer registry.

- `created_at`:

  POSIXct. Timestamp when this study was created.

## Active bindings

- `data_generic_dir`:

  Character (read-only). Resolved path for rawbatch and (by default)
  skeleton files. Lazily resolved from candidates.

- `skeleton_dir`:

  Character (read-only). Resolved path for skeleton output.

- `data_raw_dir`:

  Character or NULL (read-only). Resolved path for raw registry files.
  NULL if not configured.

- `skeleton_files`:

  Character vector (read-only). Skeleton output file paths detected on
  disk. Scans \`skeleton_dir\` on each access.

- `expected_skeleton_file_count`:

  Integer (read-only). Expected number of skeleton files based on batch
  configuration.

- `meta_file`:

  Character. Path to the metadata file.

## Methods

### Public methods

- [`RegistryStudy$new()`](#method-RegistryStudy-new)

- [`RegistryStudy$register_codes()`](#method-RegistryStudy-register_codes)

- [`RegistryStudy$describe_codes()`](#method-RegistryStudy-describe_codes)

- [`RegistryStudy$summary_table()`](#method-RegistryStudy-summary_table)

- [`RegistryStudy$apply_codes_to_skeleton()`](#method-RegistryStudy-apply_codes_to_skeleton)

- [`RegistryStudy$set_ids()`](#method-RegistryStudy-set_ids)

- [`RegistryStudy$save_rawbatch()`](#method-RegistryStudy-save_rawbatch)

- [`RegistryStudy$load_rawbatch()`](#method-RegistryStudy-load_rawbatch)

- [`RegistryStudy$process_skeletons()`](#method-RegistryStudy-process_skeletons)

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
      data_generic_dir,
      group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors", "other"),
      skeleton_dir = data_generic_dir,
      data_raw_dir = NULL,
      batch_sizes = c(1000L, 10000L),
      seed = 4L,
      id_col = "lopnr",
      ids_per_skeleton_file = 1000L
    )

#### Arguments

- `data_generic_dir`:

  Character vector of candidate paths for rawbatch and (by default)
  skeleton files. Resolved lazily.

- `group_names`:

  Character vector of rawbatch group names.

- `skeleton_dir`:

  Character vector of candidate paths for skeleton output. Defaults to
  same candidates as \`data_generic_dir\`.

- `data_raw_dir`:

  Character vector of candidate paths for raw registry files (optional).
  NULL if raw data paths are managed externally.

- `batch_sizes`:

  Integer vector. First = dev batch size, second = rest.

- `seed`:

  Integer. Shuffle seed.

- `id_col`:

  Character. Person ID column name.

- `ids_per_skeleton_file`:

  Integer. IDs per skeleton sub-file.

------------------------------------------------------------------------

### Method `register_codes()`

Register code definitions for the code registry.

#### Usage

    RegistryStudy$register_codes(
      icd10_codes = NULL,
      icdo3_codes = NULL,
      operation_codes = NULL,
      rx_atc_codes = NULL,
      rx_produkt_codes = NULL
    )

#### Arguments

- `icd10_codes`:

  Named list of ICD-10 code vectors (optional).

- `icdo3_codes`:

  Named list of ICD-O-3 code vectors (optional).

- `operation_codes`:

  Named list of operation code vectors (optional).

- `rx_atc_codes`:

  Named list of ATC code vectors (optional).

- `rx_produkt_codes`:

  Named list of product name vectors (optional).

------------------------------------------------------------------------

### Method `describe_codes()`

Print human-readable description of all registered codes.

#### Usage

    RegistryStudy$describe_codes(type = NULL)

#### Arguments

- `type`:

  Optional character. Filter to one type: "icd10", "rx_atc",
  "rx_produkt", "operation", "icdo3". NULL shows all.

------------------------------------------------------------------------

### Method `summary_table()`

Return a data.table summarizing all registered codes.

#### Usage

    RegistryStudy$summary_table(type = NULL)

#### Arguments

- `type`:

  Optional character filter (see describe_codes).

#### Returns

data.table with columns: name, codes, type, generated_columns.

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
  data_generic_dir = c("/linux/path/generic/", "C:/windows/path/generic/"),
  data_raw_dir = c("/linux/path/raw/", "C:/windows/path/raw/"),
  group_names = c("lmed", "inpatient", "outpatient", "cancer", "dors", "other")
)
study$icd10_codes <- list("stroke_any" = c("I60", "I61", "I63"))
study$rx_atc_codes <- list("rx_n05a" = c("N05A"))
study$set_ids(ids)
study$save_rawbatch("lmed", lmed_data)
study$describe_codes()
result <- study$process_skeletons(my_fn, n_workers = 4L)
} # }
```
