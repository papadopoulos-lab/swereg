# Read a raw registry file with fread, then lowercase names

Convenience wrapper around
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
followed by
[`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md).

## Usage

``` r
fread_raw(file, ..., date_columns = NULL, verbose = FALSE)
```

## Arguments

- file:

  Path to the file (passed to
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)).

- ...:

  Extra arguments forwarded to
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
  (e.g. `encoding`, `select`).

- date_columns:

  Passed to
  [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md).

- verbose:

  If `TRUE`, prints a timestamped message with the file path.

## Value

A `data.table` with lowercase column names.
