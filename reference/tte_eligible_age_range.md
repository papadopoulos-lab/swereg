# Check eligibility based on age range

Adds a logical column indicating whether each row's age falls within the
specified range (inclusive on both ends).

## Usage

``` r
tte_eligible_age_range(
  dt,
  age_var,
  min_age,
  max_age,
  col_name = "eligible_age"
)
```

## Arguments

- dt:

  A data.table with the specified age variable.

- age_var:

  Character. Name of the column containing continuous age.

- min_age:

  Numeric. Minimum eligible age (inclusive).

- max_age:

  Numeric. Maximum eligible age (inclusive).

- col_name:

  Character. Name of the eligibility column to create. Default:
  "eligible_age".

## Value

The input data.table (invisibly), modified by reference with the new
eligibility column.

## See also

\[tte_eligible_combine()\] to combine multiple eligibility criteria

Other tte_eligibility:
[`tte_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_combine.md),
[`tte_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_isoyears.md),
[`tte_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_in_window_excluding_wk0.md),
[`tte_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_observation_in_window_excluding_wk0.md)

## Examples

``` r
if (FALSE) { # \dontrun{
temp |>
  tte_eligible_age_range("rd_age_continuous", min_age = 50, max_age = 60)
} # }
```
