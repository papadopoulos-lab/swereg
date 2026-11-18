# Read a PRE2DUP package design file

Reads the `packages` sheet of a package design file and checks every row
against the design file format.
[`add_rxpre2dup()`](https://papadopoulos-lab.github.io/swereg/reference/add_rxpre2dup.md)
matches LMED dispensings to the rows it returns.

## Usage

``` r
rxpre2dup_read_design(path)
```

## Arguments

- path:

  The path to the `.xlsx` design file.

## Value

A data.table with one row per package and 14 columns. The first column
is an integer `package_id`, 1 to N in sheet order. The 11 columns of the
sheet follow it. The last two columns are
`ddd_usual = pack_ddd / dur_usual` and `ddd_low = pack_ddd / dur_max`.

## Design file format

The file is an `.xlsx` workbook with one sheet, `packages`, and one row
per package. The columns are, in order:

- `atc`: the full ATC code.

- `produkt`: the product name, as LMED writes it.

- `lnmn`: the product name and strength, matched after
  `tolower(trimws())`.

- `pack_ddd`: the DDDs that one dispensing of this package holds, as
  LMED's `fddd`. It MUST be greater than 0.

- `form`: the dose form, as free text, for example `"tablet"`.

- `dur_min`, `dur_usual`, `dur_max`: durations of one dispensing, in
  days. Each MUST be greater than 0, and
  `dur_min <= dur_usual <= dur_max` MUST hold.

- `status`: `"auto"` or `"approved"`.

- `source`, `note`: text. Both MAY be empty.

Every column except `source` and `note` MUST hold a value on every row.
Two rows with the same `atc`, `produkt` and normalised `lnmn` MUST have
values of `pack_ddd` that differ by 0.01 or more. One dispensing
therefore matches at most one row.

The reader stops on the first rule that a row breaks, and names every
sheet row that breaks it. Row 1 of the sheet is the header. The reader
ignores columns outside the list above, and it does not check their
order.

## See also

[`add_rxpre2dup()`](https://papadopoulos-lab.github.io/swereg/reference/add_rxpre2dup.md)
