# Get first non-NA value from vector

Returns the first non-missing value from a vector, useful for data
cleaning and summarization tasks.

## Usage

``` r
first_non_na(x)
```

## Arguments

- x:

  Vector of any type

## Value

First non-NA value in the vector

## Examples

``` r
x <- c(NA, NA, 3, 4, 5)
first_non_na(x)  # Returns 3
#> [1] 3
#> attr(,"na.action")
#> [1] 1 2
#> attr(,"class")
#> [1] "omit"
```
