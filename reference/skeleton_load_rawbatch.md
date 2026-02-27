# Load rawbatch files for a single batch

Reads all group files for one batch and returns a named list. Group
files that are themselves named lists (e.g., "other") are flattened into
the result. No project-specific exclusions are applied – the user can
filter after loading.

## Usage

``` r
skeleton_load_rawbatch(skeleton_meta, ...)
```

## Arguments

- skeleton_meta:

  A \[SkeletonMeta\] object.

- ...:

  Method dispatch. Pass \`batch_number\` (integer, 1-indexed).

## Value

Named list of data.tables (one entry per group, with list-type groups
flattened).

## See also

Other skeleton_methods:
[`skeleton_delete_rawbatches()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_rawbatches.md),
[`skeleton_delete_skeletons()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_skeletons.md),
[`skeleton_process()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_process.md),
[`skeleton_reset()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_reset.md),
[`skeleton_save_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_save_rawbatch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
batch_data <- skeleton_load_rawbatch(skel_meta, batch_number = 1)
# Apply project-specific exclusions
batch_data[["lmed"]] <- batch_data[["lmed"]][!produkt %in% excluded]
} # }
```
