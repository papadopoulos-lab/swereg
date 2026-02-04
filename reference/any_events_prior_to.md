# Check for any TRUE values in a prior window

Checks if any TRUE values exist in the preceding window, excluding the
current row. Useful for determining if an event occurred in a prior time
period.

## Usage

``` r
any_events_prior_to(x, window_excluding_wk0 = 104L)
```

## Arguments

- x:

  Logical vector

- window_excluding_wk0:

  Number of prior weeks to check, excluding current week (default 104,
  ~2 years). Use a large number (e.g., 99999) for lifetime history.

## Value

Logical vector indicating if any TRUE in prior window

## Details

For infinite/large windows (\>= 99999), uses O(n) cumsum approach. For
finite windows, uses O(n) data.table::frollsum. Both exclude the current
row (week 0).

## See also

\[steps_to_first()\] for counting steps until first event

Other survival_analysis:
[`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
