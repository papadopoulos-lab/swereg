# Calculate steps until first TRUE in a forward window

Slides forward through a logical vector to find the number of steps
until the first TRUE value. Useful for time-to-event calculations in
longitudinal registry data.

## Usage

``` r
steps_to_first(x, window_including_wk0 = 104L)
```

## Arguments

- x:

  Logical vector

- window_including_wk0:

  Total window size including current week (default 104, ~2 years)

## Value

Integer vector of steps until first TRUE, NA if none in window

## See also

\[any_events_prior_to()\] for checking if events occurred in prior
window

Other survival_analysis:
[`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)
