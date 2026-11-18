# ATC-level parameters of PRE2DUP

The ATC-level parameters that PRE2DUP-R ships as `ATC_parameters`, with
unchanged values. PRE2DUP uses the row of the longest `partial_atc` that
starts the ATC code of a purchase.

## Usage

``` r
pre2dup_atc_parameters
```

## Format

A data frame with 175 rows and 5 columns:

- partial_atc:

  A full or a partial ATC code.

- lower_ddd_atc:

  The lowest usual daily dose, in DDDs.

- usual_ddd_atc:

  The usual daily dose, in DDDs.

- minimum_dur_atc:

  The minimum duration of one purchase, in days.

- maximum_dur_atc:

  The maximum duration of one purchase, in days.

## Source

PRE2DUP-R 0.2.2, <https://github.com/piavat/PRE2DUP-R>, commit 43b6d89.
MIT licence, copyright 2025 PRE2DUPR authors. The file `COPYRIGHTS` of
the installed package names the ported code.
