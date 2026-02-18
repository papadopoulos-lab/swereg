# Save rawbatch files for one group

Splits \`data\` by batch IDs from the \[SkeletonMeta\] and saves as
\`BBB_rawbatch_group.qs2\` files. Skips if all files for this group
already exist on disk.

## Usage

``` r
skeleton_save_rawbatch(skeleton_meta, ...)
```

## Arguments

- skeleton_meta:

  A \[SkeletonMeta\] object.

- ...:

  Method dispatch. Pass \`group\` (character, group name) and \`data\`
  (data.table or named list of data.tables).

## Value

The \[SkeletonMeta\] with updated \`groups_saved\`.

## See also

Other skeleton_methods:
[`skeleton_delete_rawbatches()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_rawbatches.md),
[`skeleton_delete_skeletons()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_skeletons.md),
[`skeleton_load_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_load_rawbatch.md),
[`skeleton_process()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_process.md)

## Examples

``` r
if (FALSE) { # \dontrun{
lmed <- data.table::fread("lmed.csv")
skel_meta <- skeleton_save_rawbatch(skel_meta, "lmed", lmed)
rm(lmed); gc()
} # }
```
