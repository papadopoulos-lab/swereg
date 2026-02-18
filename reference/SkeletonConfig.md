# SkeletonConfig class for skeleton pipeline

Holds directory paths, group names, batch sizing, and other
configuration for the rawbatch/skeleton pipeline. Analogous to
\[TTEDesign\] – specifies the "schema" without holding data.

## Usage

``` r
SkeletonConfig(
  rawbatch_dir = character(0),
  skeleton_dir = character(0),
  group_names = character(0),
  batch_sizes = integer(0),
  seed = integer(0),
  id_col = character(0),
  ids_per_skeleton_file = integer(0)
)
```

## Arguments

- rawbatch_dir:

  Character, resolved single path for rawbatch files.

- skeleton_dir:

  Character, resolved single path for skeleton output.

- group_names:

  Character vector, names of rawbatch groups (e.g., \`c("lmed",
  "diagnoses_and_operations", "other")\`).

- batch_sizes:

  Integer vector, IDs per batch. First element is used for batch 1
  (development batch), second element for all remaining batches.
  Default: \`c(1000L, 10000L)\`.

- seed:

  Integer, shuffle seed for reproducibility. Default: \`4L\`.

- id_col:

  Character, name of the person ID column. Default: \`"lopnr"\`.

- ids_per_skeleton_file:

  Integer, number of IDs per skeleton sub-file. Default: \`1000L\`.

## Computed properties

- meta_file:

  Character (read-only). Full path to the \`skeleton_meta.qs2\` file,
  derived from \`rawbatch_dir\`.

## See also

\[skeleton_meta()\] for creating runtime objects

Other skeleton_classes:
[`SkeletonMeta()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonMeta.md),
[`skeleton_config()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_config.md),
[`skeleton_meta()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_meta.md)

## Examples

``` r
if (FALSE) { # \dontrun{
config <- skeleton_config(
  rawbatch_dir = c(
    "/data/argos/Bronze/Embla_data/_MHT/2026/generic/",
    "C:/Users/isbeihm/Uppsala/argos/Bronze/Embla_data/_MHT/2026/generic/"
  ),
  group_names = c("lmed", "diagnoses_and_operations", "other")
)
} # }
```
