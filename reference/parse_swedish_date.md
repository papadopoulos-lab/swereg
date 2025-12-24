# Parse Swedish registry dates

Parses Swedish registry dates that may have varying precision (year
only, year-month, or full date) and converts them to proper Date
objects. Handles common Swedish registry date formats and missing date
patterns.

## Usage

``` r
parse_swedish_date(
  date_string,
  default_month_day = "0701",
  default_day = "15",
  na_strings = c("", "NA", "9999", "99999999")
)
```

## Arguments

- date_string:

  Character vector of dates in Swedish registry format

- default_month_day:

  Character string for default month-day when only year is provided
  (default: "0701" for July 1st)

- default_day:

  Character string for default day when only year-month is provided
  (default: "15" for 15th)

- na_strings:

  Character vector of strings to treat as NA (default: c("", "NA",
  "9999", "99999999"))

## Value

Date vector with parsed dates

## Details

Swedish registry dates often come in different formats:

- 4 characters (YYYY): Only year known - adds default_month_day

- 6 characters (YYYYMM): Year and month known - adds default_day

- 8 characters (YYYYMMDD): Full date known - uses as-is

Special handling:

- "0000" endings are replaced with default_month_day

- "00" endings are replaced with default_day

- Invalid dates return NA with warnings

## Examples

``` r
# Different date formats
dates <- c("2020", "202003", "20200315", "19990000", "199900", "19990600", "1989-01-01", "")
parse_swedish_date(dates)
#> [1] "2020-07-01" "2020-03-15" "2020-03-15" "1999-07-01" "1999-07-01"
#> [6] "1999-06-15" "1989-01-01" NA          

# Custom defaults
parse_swedish_date(dates, default_month_day = "0101", default_day = "01")
#> [1] "2020-01-01" "2020-03-01" "2020-03-15" "1999-01-01" "1999-01-01"
#> [6] "1999-06-01" "1989-01-01" NA          
```
