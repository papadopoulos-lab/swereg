# Save a skeleton batch to disk

Saves a skeleton data.table as a single `skeleton_{BBB}.qs2` file.

## Usage

``` r
skeleton_save(dt, batch_number, output_dir)
```

## Arguments

- dt:

  A data.table of skeleton data to save.

- batch_number:

  Integer batch number (used in file naming).

- output_dir:

  Character, directory for output files.

## Value

The file path created (length-1 character).

## See also

Other skeleton_utils:
[`skeleton_checkpoint()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_checkpoint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- skeleton_save(
  skeleton,
  batch_number = 1,
  output_dir = study$skeleton_dir
)
# → skeleton_001.qs2
} # }
```
