# Create longitudinal data skeleton

Creates a longitudinal data skeleton with individual IDs and time
periods (both ISO years and ISO year-weeks) for Swedish registry data
analysis. The skeleton provides the framework for merging various
registry datasets with consistent time structure.

## Usage

``` r
create_skeleton(ids, isoyear_min, isoyearweek_max, ...)
```

## Arguments

- ids:

  Vector of individual IDs to include in the skeleton

- isoyear_min:

  ISO year, one whole number, whose week 01 starts the weekly rows.
  Every ISO year from 1900 to `isoyear_min - 1` gets one annual row.

- isoyearweek_max:

  ISO week (`"YYYY-WW"`) of the last weekly row. It MUST NOT be before
  `"<isoyear_min>-01"`.

- ...:

  Retired arguments. `date_min` and `date_max` stop with directions:
  pass `isoyear_min` and
  `isoyearweek_max = cstime::date_to_isoyearweek_c(date_max)` instead.

## Value

A data.table skeleton with columns:

- id: Individual identifier

- isoyear: ISO year (integer)

- isoyearweek: ISO year-week (character, format "YYYY-WW" or "YYYY-\*\*"
  for annual rows)

- is_isoyear: Logical indicating if row represents annual (TRUE) or
  weekly (FALSE) data

- isoyearweeksun: Date representing the Sunday (last day) of the ISO
  week/year

- personyears: Person-time contribution (1 for annual rows, 1/52.25 for
  weekly rows)

## Details

No ISO year holds both an annual row and a weekly row. An annual row
therefore always stands for a whole ISO year, and the person-years of a
year sum to 1.

## See also

[`add_onetime`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
for demographic data,
[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for diagnosis codes,
[`add_rx`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
for prescription data,
[`add_operations`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md)
for surgical procedures

## Examples

``` r
# Load fake data
data("fake_person_ids", package = "swereg")

# Annual rows up to 2019, weekly rows from 2020-01 to 2022-52
skeleton <- create_skeleton(
  ids = fake_person_ids[1:10],
  isoyear_min = 2020,
  isoyearweek_max = "2022-52"
)
utils::head(skeleton)
#>       id isoyear isoyearweek is_isoyear isoyearweeksun personyears
#>    <int>   <int>      <char>     <lgcl>         <Date>       <num>
#> 1:     1    1900     1900-**       TRUE     1900-07-01           1
#> 2:     1    1901     1901-**       TRUE     1901-06-30           1
#> 3:     1    1902     1902-**       TRUE     1902-06-29           1
#> 4:     1    1903     1903-**       TRUE     1903-06-28           1
#> 5:     1    1904     1904-**       TRUE     1904-07-03           1
#> 6:     1    1905     1905-**       TRUE     1905-07-02           1

# Check structure
utils::str(skeleton)
#> Classes ‘data.table’ and 'data.frame':   2770 obs. of  6 variables:
#>  $ id            : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ isoyear       : int  1900 1901 1902 1903 1904 1905 1906 1907 1908 1909 ...
#>  $ isoyearweek   : chr  "1900-**" "1901-**" "1902-**" "1903-**" ...
#>  $ is_isoyear    : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ isoyearweeksun: Date, format: "1900-07-01" "1901-06-30" ...
#>  $ personyears   : num  1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, ".internal.selfref")=<pointer: 0x55c6dd4b2a10> 
```
