# Check eligibility based on no events in prior window (excluding baseline week)

Adds a logical column indicating whether there were NO TRUE values in
the specified event variable within the prior window, EXCLUDING the
current (baseline) week. This is the correct semantic for trial
eligibility: we want to know if someone had the event BEFORE they could
potentially enter the trial.

## Usage

``` r
tte_eligible_no_events_in_window_excluding_wk0(
  dt,
  event_var,
  window = 52,
  col_name = NULL
)
```

## Arguments

- dt:

  A data.table with the specified event variable. Must be grouped by
  person ID for proper calculation.

- event_var:

  Character. Name of a logical column indicating event occurrence.

- window:

  Integer or Inf. Number of prior weeks to check (excluding current
  week). Use \`Inf\` to check entire lifetime history. Default: 52 (one
  year).

- col_name:

  Character or NULL. Name of the eligibility column to create. If NULL,
  auto-generates as "eligible_no\_\<event_var\>\_\<window\>wk" or
  "eligible_no\_\<event_var\>\_ever" for Inf window.

## Value

The input data.table (invisibly), modified by reference with the new
eligibility column.

## Details

Uses \[any_events_prior_to()\] internally with \`.after = -1L\`, which
always excludes the current row.

\*\*Why exclude baseline week?\*\*

In target trial emulation, eligibility is assessed at the moment of
potential trial entry. An event occurring IN the baseline week cannot be
used to determine eligibility because it happens simultaneously with (or
after) the eligibility assessment. The semantic is: "Did this person
have the event BEFORE this week?"

Using \`cumsum(x) == 0\` is INCORRECT because it INCLUDES the current
week. This function correctly excludes the current week.

## See also

\[any_events_prior_to()\] for the underlying function,
\[tte_eligible_combine()\] to combine multiple eligibility criteria

Other tte_eligibility:
[`tte_eligible_age_range()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_age_range.md),
[`tte_eligible_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_combine.md),
[`tte_eligible_isoyears()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_isoyears.md),
[`tte_eligible_no_events_lifetime_before_and_after_baseline()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_events_lifetime_before_and_after_baseline.md),
[`tte_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/tte_eligible_no_observation_in_window_excluding_wk0.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# No psychosis diagnosis ever (excluding baseline week)
temp |>
  tte_eligible_no_events_in_window_excluding_wk0("icd10_f20_f29", window = Inf)

# No antipsychotic prescriptions in prior year (excluding baseline week)
temp |>
  tte_eligible_no_events_in_window_excluding_wk0("rx_n05a", window = 52)
} # }
```
