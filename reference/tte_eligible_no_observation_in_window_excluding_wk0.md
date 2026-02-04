# Check eligibility based on no observation of a specific value (excluding baseline week)

Adds a logical column indicating whether a specific value was NOT
observed in the specified variable within the prior window, EXCLUDING
the current (baseline) week. Useful for categorical variables where you
want to exclude people who had a specific value (e.g., "systemic_mht")
in their history.

## Usage

``` r
tte_eligible_no_observation_in_window_excluding_wk0(
  dt,
  var,
  value,
  window = Inf,
  col_name = NULL
)
```

## Arguments

- dt:

  A data.table with the specified variable. Must be grouped by person ID
  for proper calculation.

- var:

  Character. Name of the column to check.

- value:

  The specific value to look for (will be compared with \`==\`).

- window:

  Integer or Inf. Number of prior weeks to check (excluding current
  week). Use \`Inf\` to check entire lifetime history. Default: Inf.

- col_name:

  Character or NULL. Name of the eligibility column to create. If NULL,
  auto-generates as "eligible_no\_\<var\>\_\<window\>wk" or
  "eligible_no\_\<var\>\_ever" for Inf window.

## Value

The input data.table (invisibly), modified by reference with the new
eligibility column.

## Details

This is a convenience wrapper around
\[tte_eligible_no_events_in_window_excluding_wk0()\] that first creates
a temporary logical column for whether the value was observed.

## See also

\[tte_eligible_no_events_in_window_excluding_wk0()\] for the underlying
logic, \[tte_eligible_combine()\] to combine multiple eligibility
criteria

Other tte_eligibility:
[`tte_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_age_range.md),
[`tte_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_combine.md),
[`tte_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_isoyears.md),
[`tte_eligible_no_events_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_in_window_excluding_wk0.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# No prior systemic MHT use (excluding baseline week)
temp |>
  tte_eligible_no_observation_in_window_excluding_wk0(
    "rd_approach1_single", "systemic_mht", window = Inf
  )
} # }
```
