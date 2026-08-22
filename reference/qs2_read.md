# Read a standard-format qs2 file

Reads a file written in standard qs2 format, that is, one saved with
[`qs2::qs_save`](https://rdrr.io/pkg/qs2/man/qs_save.html) or with
`qs2_write_atomic`. The call goes straight to
[`qs2::qs_read`](https://rdrr.io/pkg/qs2/man/qs_read.html).

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

## Details

Files in the qdata format
([`qs2::qd_save`](https://rdrr.io/pkg/qs2/man/qd_save.html)) are no
longer readable through this function. An earlier version tried
[`qs2::qd_read`](https://rdrr.io/pkg/qs2/man/qd_read.html) first and
fell back to the standard reader; that attempt is gone, so a qdata file
now raises the underlying qs2 error
`qdata format detected, use qs2::qd_read`. swereg has never written
qdata files itself.
