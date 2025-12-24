# Calculate minimum while treating infinite values as NA

Computes the minimum value of a numeric vector, converting any infinite
values to NA. Useful for robust statistical calculations.

## Usage

``` r
min_with_infinite_as_na(x, na.rm = T)
```

## Arguments

- x:

  Numeric vector

- na.rm:

  Logical, whether to remove NA values before calculation (default:
  TRUE)

## Value

Minimum value with infinite values converted to NA

## Examples

``` r
x <- c(1, 2, 4)
min_with_infinite_as_na(x)  # Returns 1
#> [1] 1

y <- c(1, 2, Inf, 4, -Inf)
min_with_infinite_as_na(y)  # Returns NA (because of infinite values)
#> [1] NA
```
