# Convert maximum to logical while treating infinite values as NA

Computes the maximum value and converts it to logical, treating infinite
values as NA. Useful for aggregating boolean data.

## Usage

``` r
as_logical_max_with_infinite_as_na(x, na.rm = T)
```

## Arguments

- x:

  Numeric vector

- na.rm:

  Logical, whether to remove NA values before calculation (default:
  TRUE)

## Value

Logical value (maximum converted to logical) with infinite values as NA

## See also

[`max_with_infinite_as_na`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
