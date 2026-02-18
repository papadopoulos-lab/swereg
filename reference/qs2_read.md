# Read a qs2 file (auto-detecting format)

Reads files saved with either \`qs2::qd_save\` (qdata format) or
\`qs2::qs_save\` (standard format). Tries qdata first, falls back to
standard.

## Usage

``` r
qs2_read(file, nthreads = 1L)
```

## Arguments

- file:

  Path to the .qs2 file.

- nthreads:

  Number of threads for decompression.

## Value

The deserialized R object.
