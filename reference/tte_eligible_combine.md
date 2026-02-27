# Combine multiple eligibility criteria

Combines multiple eligibility columns into a single logical column using
AND logic (all criteria must be TRUE for overall eligibility).

## Usage

``` r
tte_eligible_combine(dt, eligible_cols, col_name = "eligible")
```

## Arguments

- dt:

  A data.table with the specified eligibility columns.

- eligible_cols:

  Character vector. Names of the eligibility columns to combine.

- col_name:

  Character. Name of the combined eligibility column to create. Default:
  "eligible".

## Value

The input data.table (invisibly), modified by reference with the new
combined eligibility column.

## See also

\[tte_eligible_isoyears()\], \[tte_eligible_age_range()\],
\[tte_eligible_no_events_in_window_excluding_wk0()\],
\[tte_eligible_no_observation_in_window_excluding_wk0()\]

Other tte_eligibility:
[`tte_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_age_range.md),
[`tte_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_isoyears.md),
[`tte_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_in_window_excluding_wk0.md),
[`tte_eligible_no_events_lifetime_before_and_after_baseline()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_lifetime_before_and_after_baseline.md),
[`tte_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_observation_in_window_excluding_wk0.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# First define individual criteria
temp |>
  tte_eligible_isoyears(2007:2020) |>
  tte_eligible_age_range("rd_age_continuous", min_age = 50, max_age = 60) |>
  tte_eligible_no_events_in_window_excluding_wk0("icd10_f20_f29", window = Inf) |>
  tte_eligible_no_observation_in_window_excluding_wk0(
    "rd_approach1_single", "systemic_mht", window = Inf
  ) |>
  tte_eligible_no_events_in_window_excluding_wk0("rx_n05a", window = 52) |>
  tte_eligible_no_events_in_window_excluding_wk0("rx_g03aa", window = 52) |>
  tte_eligible_combine(c(
    "eligible_isoyears",
    "eligible_age",
    "eligible_no_icd10_f20_f29_ever",
    "eligible_no_rd_approach1_single_ever",
    "eligible_no_rx_n05a_52wk",
    "eligible_no_rx_g03aa_52wk"
  ))
} # }
```
