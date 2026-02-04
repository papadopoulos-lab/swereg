# Calculate inverse probability of treatment weights (IPW)

Calculates stabilized inverse probability of treatment weights to adjust
for baseline confounding in target trial emulation. IPW reweights the
sample so that confounders are balanced across treatment groups,
creating a pseudo-population where treatment is independent of measured
confounders.

## Usage

``` r
tte_calculate_ipw(
  data,
  exposure_var,
  confounder_vars,
  id_var = "trial_id",
  stabilize = TRUE
)
```

## Arguments

- data:

  A data.table with one row per trial at baseline (typically where
  \`tstart == 0\`).

- exposure_var:

  Character, name of the binary exposure/treatment column.

- confounder_vars:

  Character vector of confounder variable names for the propensity score
  model.

- id_var:

  Character, name of the trial identifier column (default: "trial_id").

- stabilize:

  Logical, whether to use stabilized weights (recommended, default:
  TRUE). Stabilized weights have the marginal probability in the
  numerator, reducing variance while maintaining valid inference.

## Value

The input data.table with added columns:

- ps:

  Propensity score: P(exposure \| confounders)

- ipw:

  Inverse probability weight (stabilized if \`stabilize = TRUE\`)

## Details

IPW addresses confounding by creating a pseudo-population where
treatment assignment is independent of confounders. The propensity score
P(A\|L) is estimated using logistic regression, where A is treatment and
L represents confounders.

\*\*Stabilized weights (recommended):\*\* \$\$IPW =
\frac{P(A)}{P(A\|L)}\$\$ for exposed, and \$\$IPW =
\frac{1-P(A)}{1-P(A\|L)}\$\$ for unexposed.

\*\*Unstabilized weights:\*\* \$\$IPW = \frac{1}{P(A\|L)}\$\$ for
exposed, and \$\$IPW = \frac{1}{1-P(A\|L)}\$\$ for unexposed.

Stabilized weights are generally preferred because they have lower
variance and the pseudo-population has the same expected size as the
original sample.

## See also

[`tte_calculate_ipcw`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md),
[`tte_combine_weights`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md)

Other tte_weights:
[`tte_calculate_ipcw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_calculate_ipcw.md),
[`tte_combine_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_combine_weights.md),
[`tte_identify_censoring()`](https://papadopoulos-lab.github.io/swereg/reference/tte_identify_censoring.md),
[`tte_truncate_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate_weights.md)

## Examples

``` r
library(data.table)
# Create example baseline data
set.seed(42)
baseline <- data.table(
  trial_id = 1:1000,
  exposed = rbinom(1000, 1, 0.3),
  age_cat = sample(1:4, 1000, replace = TRUE),
  education = sample(1:3, 1000, replace = TRUE)
)
# Calculate IPW
result <- tte_calculate_ipw(
  data = baseline,
  exposure_var = "exposed",
  confounder_vars = c("age_cat", "education"),
  id_var = "trial_id"
)
summary(result$ipw)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.9127  0.9733  0.9946  1.0000  1.0211  1.1045 
```
