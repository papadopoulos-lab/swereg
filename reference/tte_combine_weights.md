# Combine IPW and IPCW-PP weights for per-protocol analysis

Creates combined inverse probability weights by multiplying IPW
(baseline confounding adjustment) and IPCW-PP (censoring adjustment for
per-protocol). The combined weight is used for per-protocol effect
estimation in target trial emulation.

## Usage

``` r
tte_combine_weights(
  data,
  ipw_col = "ipw",
  ipcw_col = "ipcw_pp",
  output_col = "analysis_weight_pp"
)
```

## Arguments

- data:

  A data.table containing both IPW and IPCW-PP columns.

- ipw_col:

  Character, name of the IPW column (default: "ipw").

- ipcw_col:

  Character, name of the IPCW-PP column (default: "ipcw_pp").

- output_col:

  Character, name for the combined weight column (default:
  "analysis_weight_pp").

## Value

The input data.table with an added column for combined weights.

## Details

The per-protocol weight combines two adjustments:

- \*\*IPW\*\*: Adjusts for baseline confounding (who gets treated)

- \*\*IPCW-PP\*\*: Adjusts for time-varying censoring (who
  deviates/drops out)

The combined weight is simply: analysis_weight_pp = IPW x IPCW-PP

This creates a pseudo-population where both treatment assignment and
censoring are independent of measured confounders, enabling unbiased
estimation of the per-protocol effect.

## See also

[`tte_calculate_ipw`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md),
[`tte_calculate_ipcw`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md)

Other tte_weights:
[`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md),
[`tte_calculate_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipw.md),
[`tte_identify_censoring()`](https://papadopoulos-lab.github.io/swereg/reference/tte_identify_censoring.md),
[`tte_truncate_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate_weights.md)

## Examples

``` r
library(data.table)
dt <- data.table(
  id = 1:100,
  ipw = runif(100, 0.8, 1.2),
  ipcw_pp = runif(100, 0.9, 1.1)
)
result <- tte_combine_weights(dt)
# Creates analysis_weight_pp = ipw * ipcw_pp
```
