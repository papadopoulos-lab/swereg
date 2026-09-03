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

## data.table over-allocation

The reader restores data.table over-allocation before it returns. qs2
does not keep the over-allocated column slots, so a table read from disk
has a
[`truelength()`](https://rdrr.io/pkg/data.table/man/truelength.html) of
0. The first `:=` on that table inside a function writes to a shallow
copy. The caller then never sees the new column, and data.table reports
nothing.

`qs2_read()` calls
[`data.table::setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html)
on every data.table it reaches. It reaches the top-level object, every
element of a plain list at any depth, and every field of an R6 object,
public or private. There is no depth limit.

It enters nothing else. It returns everything it does not enter
unchanged, so a data.table held inside one of these keeps a
[`truelength()`](https://rdrr.io/pkg/data.table/man/truelength.html) of
0:

- an active binding, because reading one runs user code

- a function, because its enclosure is an environment

- a classed list, a `data.frame` included, because `[[<-` dispatches
  there

- an environment that is not an R6 object, because a binding in one can
  hold a package namespace

- any other object, an S4 object included

The walker visits a self-referential R6 object once. A plain list cannot
refer to itself, because R copies a list on assignment.

Two figures, and which table gets which:

- A SKELETON gets 4096 free column slots, held in `R/dt_alloc.R` as
  `.DT_ALLOC_SPARE_SLOTS`. A skeleton is the top-level object when that
  is a data.table, and the `$data` field of a
  [Skeleton](https://papadopoulos-lab.github.io/swereg/reference/Skeleton.md).
  The `add_*` functions reserve the same number.

- Every OTHER data.table gets 1024, which is data.table's own default. A
  plan can hold thousands of small result tables, and one free slot
  costs 16 bytes whether it is used or not.

The reader does not read `options(datatable.alloccol)`. Set that option
and it changes what `[.data.table` reserves, not what this reader
restores.

The repair is cheap.
[`setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html)
allocates a new column-pointer header and shares the column data by
reference. It costs a few bytes per free slot, not a copy of the table.
