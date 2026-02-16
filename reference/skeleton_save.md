# Save skeleton output as sub-files split by ID count

Splits a skeleton data.table into sub-files of \`ids_per_file\` unique
IDs each and saves them as \`skeleton_BBB_SS.qs\` files. This keeps
individual files small enough for fast loading downstream.

## Usage

``` r
skeleton_save(
  dt,
  batch_number,
  output_dir,
  ids_per_file = 1000L,
  id_col = "id"
)
```

## Arguments

- dt:

  A data.table of skeleton data to save.

- batch_number:

  Integer batch number (used in file naming).

- output_dir:

  Character, directory for output files.

- ids_per_file:

  Integer, number of unique IDs per sub-file. Default: \`1000L\`.

- id_col:

  Character, name of the ID column in \`dt\`. Default: \`"id"\`.

## Value

Character vector of file paths created.

## See also

Other skeleton_utils:
[`skeleton_checkpoint()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_checkpoint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
files <- skeleton_save(
  skeleton,
  batch_number = 1,
  output_dir = config@skeleton_dir,
  ids_per_file = config@ids_per_skeleton_file,
  id_col = "id"
)
# → skeleton_001_01.qs, skeleton_001_02.qs, ...
} # }
```
