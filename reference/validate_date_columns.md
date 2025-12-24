# Validate date columns exist and are proper format

Validate date columns exist and are proper format

## Usage

``` r
validate_date_columns(data, expected_date_cols, data_type = "data")
```

## Arguments

- data:

  A data.table containing the data

- expected_date_cols:

  Character vector of expected date column names

- data_type:

  Character string describing the data type (for error messages)

## Value

Nothing if valid, stops with error if invalid
