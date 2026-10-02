# Check for any TRUE values in a prior calendar window

Returns, for each row, whether an earlier row is TRUE inside the N ISO
weeks before the first week of that row. The window counts calendar
weeks from `isoyearweek`, not rows.

## Usage

``` r
any_events_prior_to(x, window_excluding_wk0 = 104L, isoyearweek)
```

## Arguments

- x:

  Logical vector, one element per row.

- window_excluding_wk0:

  Number of ISO weeks to look back, excluding the current row (default
  104). A value of 99999 or more, `Inf` included, means every earlier
  row.

- isoyearweek:

  Character vector of the same length as `x`. Each element is an ISO
  week (`"2008-01"`) or an ISO year (`"2004-**"`). The rows MUST be in
  strictly increasing calendar order and MUST NOT overlap. An annual row
  and a weekly row of the same year overlap.

## Value

Logical vector of the same length as `x`.

## Details

A weekly row covers its own week. An annual row covers every ISO week of
its year, week 53 included where the year has one. The window of an
annual row ends at the week before the first week of its year. An
earlier row is inside the window when any of its weeks is inside the
window. The current row is never inside its own window.

Missing values follow [`any()`](https://rdrr.io/r/base/any.html). The
result is TRUE if a row in the window is TRUE. Otherwise it is NA if a
row in the window is NA, and FALSE if not.

The function stops when:

- `isoyearweek` is missing.

- `x` and `isoyearweek` differ in length.

- a week is not in
  [`cstime::dates_by_isoyearweek`](https://rdrr.io/pkg/cstime/man/dates_by_isoyearweek.html).

- the rows are out of calendar order.

## See also

[`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
for counting steps until first event

Other survival_analysis:
[`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)

## Examples

``` r
any_events_prior_to(
  c(TRUE, FALSE, FALSE),
  window_excluding_wk0 = 1,
  isoyearweek = c("2008-01", "2008-02", "2008-03")
)
#> [1] FALSE  TRUE FALSE
```
