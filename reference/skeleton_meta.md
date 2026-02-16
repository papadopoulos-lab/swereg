# Create a skeleton set from config and IDs

Constructor function for \[SkeletonMeta\] objects. Shuffles IDs using
the config seed, splits into batches per \`config@batch_sizes\`, and
scans disk for existing rawbatch and skeleton files.

## Usage

``` r
skeleton_meta(config, ids)
```

## Arguments

- config:

  A \[SkeletonConfig\] object.

- ids:

  Vector of person IDs to split into batches.

## Value

A \[SkeletonMeta\] object.

## See also

\[SkeletonConfig\], \[skeleton_save_rawbatch()\]

Other skeleton_classes:
[`SkeletonConfig()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonConfig.md),
[`SkeletonMeta()`](https://papadopoulos-lab.github.io/swereg/reference/SkeletonMeta.md),
[`skeleton_config()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_config.md)

## Examples

``` r
if (FALSE) { # \dontrun{
config <- skeleton_config(
  rawbatch_dir = "/data/processed/",
  group_names = c("lmed", "diagnoses_and_operations", "other")
)
ids <- unique(grunduppgifter$lopnr)
skel_meta <- skeleton_meta(config, ids)
} # }
```
