# Get last non-NA value from vector

Returns the last non-missing value from a vector, useful for data
cleaning and summarization tasks.

## Usage

``` r
last_non_na(x)
```

## Arguments

- x:

  Vector of any type

## Value

Last non-NA value in the vector

## Examples

``` r
x <- c(1, 2, 3, NA, NA)
last_non_na(x)  # Returns 3
#> [1] 3
#> attr(,"na.action")
#> [1] 4 5
#> attr(,"class")
#> [1] "omit"
```
