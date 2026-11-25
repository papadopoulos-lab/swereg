# Read a PRE2DUP package design file

Reads the `packages` sheet of a package design file and checks every row
against the design file format.
[`add_rxpre2dup()`](https://papadopoulos-lab.github.io/swereg/reference/add_rxpre2dup.md)
matches LMED dispensings to the rows it returns.

## Usage

``` r
rxpre2dup_read_design(path, as_of = NULL)
```

## Arguments

- path:

  The path to the `.xlsx` design file.

- as_of:

  NULL, or one date that
  [`base::as.Date()`](https://rdrr.io/r/base/as.Date.html) reads. When
  it is not NULL, the reader drops every row with `design_date` after
  `as_of`. It MUST be NULL for a file without the column `design_date`.

## Value

A data.table with one row per package that the reader keeps. The first
column is an integer `package_id`, 1 to N in sheet order. The 11 columns
of the format follow it. The next two columns are
`ddd_usual = pack_ddd / dur_usual` and `ddd_low = pack_ddd / dur_max`. A
file with `design_date` gives a 15th column, `design_date`, of class
`Date`.

## Design file format

The file is an `.xlsx` workbook with one sheet, `packages`, and one row
per package. The columns are, in order:

- `atc`: the full ATC code.

- `produkt`: the product name, as LMED writes it.

- `lnmn`: the product name and strength, matched after
  `tolower(trimws())`.

- `pack_ddd`: the DDDs that one pack of this package holds. A dispensing
  of `antal` packs has `fddd = antal * pack_ddd`. It MUST be greater
  than 0.

- `form`: the dose form, as free text, for example `"tablet"`.

- `dur_min`, `dur_usual`, `dur_max`: durations of one pack, in days.
  Each MUST be greater than 0, and `dur_min <= dur_usual <= dur_max`
  MUST hold.

- `status`: `"auto"` or `"approved"`.

- `source`, `note`: text. Both MAY be empty.

Every column except `source` and `note` MUST hold a value on every row.
Two rows with the same `atc`, `produkt` and normalised `lnmn` MUST have
values of `pack_ddd` that differ by 0.01 or more. One dispensing
therefore matches at most one row.

The reader stops on the first rule that a row breaks, and names every
sheet row that breaks it. Row 1 of the sheet is the header. The reader
ignores every column that is not in the list above or in the section
"Design dates". It does not check the column order.

## Design dates

The sheet MAY have one more column, `design_date`. The workbook can then
grow by appended rows. Each row MUST hold the date of its design, as
text in the form `YYYY-MM-DD`. An Excel date cell reads as a number, and
the reader refuses it.

The reader first drops the rows with `design_date` after `as_of`. Then,
for each `atc`, it keeps only the rows with the latest remaining date of
that `atc`. An `atc` that a later date leaves out keeps the rows of its
own latest date. The reader stops when it keeps no row.

The rules of the list above apply only to the rows that the reader
keeps. A dropped row therefore takes no part in the duplicate-key rule.
A refusal names the sheet rows of the file.

## See also

[`add_rxpre2dup()`](https://papadopoulos-lab.github.io/swereg/reference/add_rxpre2dup.md)
