# Combine multiple eligibility criteria

Combine multiple eligibility criteria

## Usage

``` r
skeleton_eligible_combine(dt, eligible_cols, col_name = "eligible")
```

## Arguments

- dt:

  A data.table with the specified eligibility columns.

- eligible_cols:

  Character vector of eligibility column names.

- col_name:

  Character. Default: "eligible".

## Value

The input data.table (invisibly), modified by reference.

## See also

\[skeleton_eligible_isoyears()\], \[skeleton_eligible_age_range()\],
\[skeleton_eligible_no_events_in_window_excluding_wk0()\],
\[skeleton_eligible_no_observation_in_window_excluding_wk0()\]

Other skeleton_eligibility:
[`skeleton_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_age_range.md),
[`skeleton_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_isoyears.md),
[`skeleton_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_events_in_window_excluding_wk0.md),
[`skeleton_eligible_no_events_lifetime_before_and_after_baseline()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_events_lifetime_before_and_after_baseline.md),
[`skeleton_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_observation_in_window_excluding_wk0.md)
