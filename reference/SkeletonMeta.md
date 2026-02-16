# SkeletonMeta class for skeleton pipeline

Holds a \[SkeletonConfig\] plus runtime state: batched ID lists and disk
file tracking. Analogous to \[TTETrial\] – carries data through a
method-chaining workflow.

## Usage

``` r
SkeletonMeta(
  config = SkeletonConfig(),
  n_ids = integer(0),
  n_batches = integer(0),
  batch_id_list = list(),
  groups_saved = character(0),
  skeleton_files = character(0)
)
```

## Arguments

- config:

  A \[SkeletonConfig\] object.

- n_ids:

  Integer, total number of IDs across all batches.

- n_batches:

  Integer, number of batches.

- batch_id_list:

  List of ID vectors, one per batch.

- groups_saved:

  Character vector of rawbatch group names that exist on disk.

- skeleton_files:

  Character vector of skeleton output file paths detected on disk.

## See also

\[skeleton_meta()\] for creating objects, \[skeleton_save_rawbatch()\],
\[skeleton_load_rawbatch()\], \[skeleton_process()\]

Other skeleton_classes:
[`SkeletonConfig()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonConfig.md),
[`skeleton_config()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_config.md),
[`skeleton_meta()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_meta.md)
