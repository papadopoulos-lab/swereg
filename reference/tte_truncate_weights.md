# Truncate extreme inverse probability weights

Truncates weights at specified quantiles to reduce variance inflation
from extreme values. This is a common practice in causal inference to
balance the bias-variance tradeoff when using inverse probability
weights.

## Usage

``` r
tte_truncate_weights(
  data,
  weight_cols,
  lower = 0.01,
  upper = 0.99,
  suffix = "_trunc"
)
```

## Arguments

- data:

  A data.table containing the weight columns to truncate.

- weight_cols:

  Character vector of column names containing weights to truncate.

- lower:

  Numeric, lower quantile cutoff (default: 0.01 for 1st percentile).

- upper:

  Numeric, upper quantile cutoff (default: 0.99 for 99th percentile).

- suffix:

  Character, suffix to append to column names for truncated weights
  (default: "\_trunc").

## Value

The input data.table with added columns for truncated weights. Column
names follow the pattern `original_col + suffix`.

## Details

Weight truncation addresses the variance inflation that can occur with
inverse probability weights. Extreme weights (very large or very small)
can dramatically increase the variance of effect estimates. Truncating
at the 1st and 99th percentiles is a common choice that typically
introduces minimal bias while substantially reducing variance.

The function calculates quantiles across all non-NA values in each
column, then clips values outside these bounds to the boundary values.

## See also

[`tte_calculate_ipw`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md),
[`tte_calculate_ipcw`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md)

Other tte_weights:
[`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md),
[`tte_calculate_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md),
[`tte_combine_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md),
[`tte_identify_censoring()`](https://papadopoulos-lab.github.io/swereg/reference/tte_identify_censoring.md)

## Examples

``` r
library(data.table)
dt <- data.table(
  id = 1:100,
  ipw = c(0.1, rep(1, 98), 10),  # Extreme values at ends
  analysis_weight_pp = runif(100, 0.5, 2)
)
result <- tte_truncate_weights(dt, weight_cols = c("ipw", "analysis_weight_pp"))
# Creates ipw_trunc and analysis_weight_pp_trunc columns
```
