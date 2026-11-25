# Add PRE2DUP drug use periods to skeleton

Runs PRE2DUP on the LMED dispensings that match each code list, and adds
one logical column per code list to the skeleton. A column is TRUE on
every week or annual row that overlaps a drug use period, and FALSE
elsewhere.

## Usage

``` r
add_rxpre2dup(
  skeleton,
  lmed,
  hosp = NULL,
  id_name = "lopnr",
  codes,
  design,
  atc_parameters = swereg::pre2dup_atc_parameters,
  ...
)
```

## Arguments

- skeleton:

  A data.table created by
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md).

- lmed:

  A data.table of LMED dispensings, with the columns `id_name`, `edatum`
  (Date), `atc`, `produkt`, `lnmn`, `antal` and `fddd`. `antal` is
  LMED's `ANTAL`, the number of packs of the dispensing. A missing
  `antal` column is an error.

- hosp:

  A data.table of hospital stays, with the columns `id_name`, `indatum`
  (Date) and `utdatum` (Date), or NULL. Stays with `utdatum <= indatum`
  are dropped.

- id_name:

  The name of the person id column in `lmed` and `hosp`.

- codes:

  A named list of ATC patterns, as in
  [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  with `source = "atc"`. Every name MUST start with `rxpre2dup_`. A
  pattern matches every ATC code that starts with it. A pattern that
  starts with `"!"` excludes the codes that start with the rest of it.

- design:

  A data.table returned by
  [`rxpre2dup_read_design()`](https://papadopoulos-lab.github.io/swereg/reference/rxpre2dup_read_design.md).

- atc_parameters:

  The ATC-level parameters of PRE2DUP, with the columns of
  [pre2dup_atc_parameters](https://papadopoulos-lab.github.io/swereg/reference/pre2dup_atc_parameters.md).

- ...:

  The global arguments of PRE2DUP, passed to the PRE2DUP run:
  `global_gap_max`, `global_min`, `global_max`, `global_max_single`,
  `global_ddd_high`, `global_hosp_max`, `days_covered`, `weight_past`,
  `weight_current`, `weight_next`, `weight_first_last` and
  `post_process_perc`.

## Value

The skeleton, invisibly. The function adds the columns by reference, and
it grows the column allocation as
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
does.

The attribute `package_coverage` of the skeleton is a data.table with
one row per code list and ATC code. Its columns are `name`, `atc`,
`n_purchases` and `share_without_package`. The share counts the
purchases with no design row, after PRE2DUP adds the dispensings of one
package on one day together. One message line shows each share.

## Dispensings

In LMED, `antal` is the number of packs of a dispensing, and `fddd` is
the DDDs of all its packs. A dispensing matches a design row when `atc`,
`produkt` and `tolower(trimws(lnmn))` are equal and
`abs(fddd / antal - pack_ddd) < 0.005`. Every dispensing enters PRE2DUP
with `ratio = antal` and `ddd = fddd`. PRE2DUP multiplies `dur_min`,
`dur_usual` and `dur_max` of the design row, or the duration limits of
the ATC-level rule, by `ratio`. A dispensing with no design row uses the
ATC-level rule of PRE2DUP, with the parameters of `atc_parameters`.

The function drops the rows of `lmed` whose `fddd` or `antal` is
missing, not finite, 0 or negative. One message line gives the number of
dropped rows per reason. Each dropped row counts under one reason, and a
reason of `fddd` comes before a reason of `antal`.

## Periods on the skeleton

Each code list gets its own PRE2DUP run, on the dispensings whose ATC
code matches it. Each period becomes the closed interval from its start
date to its end date.
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
then maps these intervals to the skeleton, with the period dates as
`start_date` and `stop_date`. Weekly and annual rows therefore behave
exactly as in
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md).

## See also

[`rxpre2dup_read_design()`](https://papadopoulos-lab.github.io/swereg/reference/rxpre2dup_read_design.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[pre2dup_atc_parameters](https://papadopoulos-lab.github.io/swereg/reference/pre2dup_atc_parameters.md)
