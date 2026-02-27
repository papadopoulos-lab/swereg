# Check eligibility based on no events ever (person-level, before and after baseline)

Adds a logical column indicating whether the person has NO TRUE values
in the specified event variable at ANY time point. Unlike
\[tte_eligible_no_events_in_window_excluding_wk0()\] which checks a
rolling window relative to each week, this is a person-level flag: if
the event is TRUE at any row for a person, ALL rows for that person are
marked ineligible.

## Usage

``` r
tte_eligible_no_events_lifetime_before_and_after_baseline(
  dt,
  event_var,
  col_name = NULL
)
```

## Arguments

- dt:

  A data.table with an \`id\` column and the specified event variable.

- event_var:

  Character. Name of a logical column indicating event occurrence.

- col_name:

  Character or NULL. Name of the eligibility column to create. If NULL,
  auto-generates as
  "eligible_no\_\<event_var\>\_lifetime_before_and_after_baseline".

## Value

The input data.table (invisibly), modified by reference with the new
eligibility column.

## Details

Use case: excluding people with a condition diagnosed at any time (e.g.,
gender dysphoria F64) where future diagnoses also indicate the person
should never have been eligible.

## See also

\[tte_eligible_no_events_in_window_excluding_wk0()\] for time-relative
eligibility, \[tte_eligible_combine()\] to combine criteria

Other tte_eligibility:
[`tte_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_age_range.md),
[`tte_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_combine.md),
[`tte_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_isoyears.md),
[`tte_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_in_window_excluding_wk0.md),
[`tte_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_observation_in_window_excluding_wk0.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Exclude anyone who EVER has gender dysphoria diagnosis
temp |>
  tte_eligible_no_events_lifetime_before_and_after_baseline("icd10_f64")
} # }
```
