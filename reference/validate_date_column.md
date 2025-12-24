# Validate date column exists and is proper format

Validate date column exists and is proper format

## Usage

``` r
validate_date_column(data, date_col, data_type = "data")
```

## Arguments

- data:

  A data.table containing the data

- date_col:

  Character string with the date column name

- data_type:

  Character string describing the data type (for error messages)

## Value

Nothing if valid, stops with error if invalid
