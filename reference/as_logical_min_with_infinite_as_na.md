# Convert minimum to logical while treating infinite values as NA

Computes the minimum value and converts it to logical, treating infinite
values as NA. Useful for aggregating boolean data.

## Usage

``` r
as_logical_min_with_infinite_as_na(x, na.rm = T)
```

## Arguments

- x:

  Numeric vector

- na.rm:

  Logical, whether to remove NA values before calculation (default:
  TRUE)

## Value

Logical value (minimum converted to logical) with infinite values as NA

## See also

[`min_with_infinite_as_na`](https://papadopoulos-lab.github.io/swereg/reference/min_with_infinite_as_na.md)
