# Create a skeleton pipeline configuration

Constructor function for \[SkeletonConfig\] objects. Accepts multiple
candidate paths (like \[org::initialize_project()\]) and resolves each
to the first existing directory at runtime.

## Usage

``` r
skeleton_config(
  rawbatch_dir,
  group_names,
  skeleton_dir = rawbatch_dir,
  batch_sizes = c(1000L, 10000L),
  seed = 4L,
  id_col = "lopnr",
  ids_per_skeleton_file = 1000L
)
```

## Arguments

- rawbatch_dir:

  Character vector of candidate paths for rawbatch files. The first
  existing directory is used. If only one path is given and it doesn't
  exist, it is accepted as-is (will be created).

- group_names:

  Character vector, names of rawbatch groups.

- skeleton_dir:

  Character vector of candidate paths for skeleton output. Defaults to
  the same resolved value as \`rawbatch_dir\`.

- batch_sizes:

  Integer vector. First element = IDs in batch 1 (dev), second = IDs in
  remaining batches. Default: \`c(1000L, 10000L)\`.

- seed:

  Integer, shuffle seed. Default: \`4L\`.

- id_col:

  Character, person ID column name. Default: \`"lopnr"\`.

- ids_per_skeleton_file:

  Integer, IDs per skeleton sub-file. Default: \`1000L\`.

## Value

A \[SkeletonConfig\] object.

## See also

\[SkeletonConfig\] for class details

Other skeleton_classes:
[`SkeletonConfig()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonConfig.md),
[`SkeletonMeta()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonMeta.md),
[`skeleton_meta()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_meta.md)

## Examples

``` r
if (FALSE) { # \dontrun{
config <- skeleton_config(
  rawbatch_dir = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Embla_data/_MHT/2026/generic/",
    "/data/argos/Bronze/Embla_data/_MHT/2026/generic/",
    "C:/Users/isbeihm/Uppsala/argos/Bronze/Embla_data/_MHT/2026/generic/"
  ),
  group_names = c("lmed", "diagnoses_and_operations", "other")
)
} # }
```
