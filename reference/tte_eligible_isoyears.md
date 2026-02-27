# Check eligibility based on ISO years

Adds a logical column indicating whether each row falls within the
specified ISO years. Typically used to restrict analysis to a calendar
period.

## Usage

``` r
tte_eligible_isoyears(dt, isoyears, col_name = "eligible_isoyears")
```

## Arguments

- dt:

  A data.table with an \`isoyear\` column.

- isoyears:

  Integer vector of eligible ISO years (e.g., 2007:2020).

- col_name:

  Character. Name of the eligibility column to create. Default:
  "eligible_isoyears".

## Value

The input data.table (invisibly), modified by reference with the new
eligibility column.

## See also

\[tte_eligible_combine()\] to combine multiple eligibility criteria

Other tte_eligibility:
[`tte_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_age_range.md),
[`tte_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_combine.md),
[`tte_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_in_window_excluding_wk0.md),
[`tte_eligible_no_events_lifetime_before_and_after_baseline()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_lifetime_before_and_after_baseline.md),
[`tte_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_observation_in_window_excluding_wk0.md)

## Examples

``` r
if (FALSE) { # \dontrun{
temp |>
  tte_eligible_isoyears(2007:2020)
} # }
```
