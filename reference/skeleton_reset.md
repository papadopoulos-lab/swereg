# Reset the skeleton pipeline by deleting all generated files

Deletes rawbatch files, skeleton output files, and the
\`skeleton_meta.qs2\` metadata file. After calling this, re-run the full
pipeline to regenerate everything from raw registry data.

## Usage

``` r
skeleton_reset(x, ...)
```

## Arguments

- x:

  A \[SkeletonMeta\] or \[SkeletonConfig\] object.

- ...:

  Not used.

## Value

Invisibly returns \`NULL\`.

## Details

Can operate on a \[SkeletonMeta\] object or a \[SkeletonConfig\] (useful
when \`skeleton_meta.qs2\` doesn't exist or is corrupt).

## See also

Other skeleton_methods:
[`skeleton_delete_rawbatches()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_rawbatches.md),
[`skeleton_delete_skeletons()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_delete_skeletons.md),
[`skeleton_load_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_load_rawbatch.md),
[`skeleton_process()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_process.md),
[`skeleton_save_rawbatch()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_save_rawbatch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Reset from a loaded skeleton_meta
skeleton_reset(skel_meta)

# Reset from just a config (no skeleton_meta.qs2 needed)
skeleton_reset(config)
} # }
```
