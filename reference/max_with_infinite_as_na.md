# Calculate maximum while treating infinite values as NA

Computes the maximum value of a numeric vector, converting any infinite
values to NA. Useful for robust statistical calculations.

## Usage

``` r
max_with_infinite_as_na(x, na.rm = T)
```

## Arguments

- x:

  Numeric vector

- na.rm:

  Logical, whether to remove NA values before calculation (default:
  TRUE)

## Value

Maximum value with infinite values converted to NA

## Examples

``` r
x <- c(1, 2, 4)
max_with_infinite_as_na(x)  # Returns 4
#> [1] 4

y <- c(1, 2, Inf, 4, -Inf)
max_with_infinite_as_na(y)  # Returns NA (because of infinite values)
#> [1] NA
```
