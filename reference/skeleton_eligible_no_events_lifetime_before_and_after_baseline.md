# Check eligibility based on no events ever (person-level, before and after baseline)

Check eligibility based on no events ever (person-level, before and
after baseline)

## Usage

``` r
skeleton_eligible_no_events_lifetime_before_and_after_baseline(
  dt,
  event_var,
  col_name = NULL
)
```

## Arguments

- dt:

  A data.table with an \`id\` column and the specified event variable.

- event_var:

  Character. Name of a logical column.

- col_name:

  Character or NULL.

## Value

The input data.table (invisibly), modified by reference.

## See also

\[skeleton_eligible_no_events_in_window_excluding_wk0()\],
\[skeleton_eligible_combine()\]

Other skeleton_eligibility:
[`skeleton_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_age_range.md),
[`skeleton_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_combine.md),
[`skeleton_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_isoyears.md),
[`skeleton_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_events_in_window_excluding_wk0.md),
[`skeleton_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_observation_in_window_excluding_wk0.md)
