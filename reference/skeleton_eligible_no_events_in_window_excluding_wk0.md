# Check eligibility based on no events in prior window (excluding baseline week)

Adds a logical column indicating whether there were NO TRUE values in
the specified event variable within the prior window, EXCLUDING the
current (baseline) week. The window counts ISO weeks from `isoyearweek`,
as
[`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)
describes.

## Usage

``` r
skeleton_eligible_no_events_in_window_excluding_wk0(
  dt,
  event_var,
  window = 52,
  col_name = NULL
)
```

## Arguments

- dt:

  A data.table with `id`, `isoyearweek` and the specified event
  variable. Within each `id`, the rows MUST be in calendar order.

- event_var:

  Character. Name of a logical column indicating event occurrence.

- window:

  Number of prior ISO weeks to check. 99999 or more, `Inf` included,
  means every earlier row. Default: 52.

- col_name:

  Character or NULL. Name of the eligibility column to create.

## Value

The input data.table (invisibly), modified by reference.

## See also

[`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md),
[`skeleton_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_combine.md)

Other skeleton_eligibility:
[`skeleton_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_age_range.md),
[`skeleton_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_combine.md),
[`skeleton_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_isoyears.md),
[`skeleton_eligible_no_events_lifetime_before_and_after_baseline()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_events_lifetime_before_and_after_baseline.md),
[`skeleton_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_observation_in_window_excluding_wk0.md)

## Examples

``` r
if (FALSE) { # \dontrun{
temp |>
  skeleton_eligible_no_events_in_window_excluding_wk0("icd10_e10_e14", window = Inf)
} # }
```
