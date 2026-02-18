# Fit Kaplan-Meier curves and optionally plot

S7 method that fits weighted Kaplan-Meier curves by exposure group and
optionally creates a step plot. Uses IPW only (not IPCW) because IPCW is
time-varying and cannot be directly applied to KM estimation.

## Usage

``` r
tte_km(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object with trial level data.

- ...:

  Method arguments:

  ipw_col

  :   Character, required. Column name for IPW weights.

  save_path

  :   Character or NULL. If specified, saves the plot to this path.

  title

  :   Character or NULL. Plot title. If NULL, no title is shown.

## Value

A svykm object (invisibly if save_path is specified).

## Details

This method requires \`data_level == "trial"\`.

The method extracts the last row per trial (final follow-up status) and
creates a survey design using the specified IPW column. The svykm object
contains survival curves for each exposure group.

If \`save_path\` is specified, creates a step plot with: - Exposed group
in red, Unexposed group in blue - Y-axis scaled to 0.99-1.0 (appropriate
for rare events) - X-axis showing time in weeks

Note: We use IPW only (not weight_pp = IPW × IPCW-PP) because IPCW-PP is
time-varying and cannot be directly applied to KM estimation. The IPW
adjusts for baseline confounding, providing an ITT-like visualization of
event-free survival by treatment group.

## See also

\[survey::svykm()\]

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
[`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
[`tte_prepare_outcome()`](https://papadopoulos-lab.github.io/swereg/reference/tte_prepare_outcome.md),
[`tte_rates()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates.md),
[`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md),
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
[`tte_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_summary.md),
[`tte_table1()`](https://papadopoulos-lab.github.io/swereg/reference/tte_table1.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Just fit the KM curves
km <- trial |> tte_km(ipw_col = "ipw_trunc")

# Fit and save plot
km <- trial |> tte_km(
  ipw_col = "ipw_trunc",
  save_path = "results/km_plot.png",
  title = "Event-free survival by treatment group"
)
} # }
```
