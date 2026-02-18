# Summarize trial data statistics

Returns key statistics about a \[TTETrial\] object: row count,
person-weeks, number of trials, individuals, events, and memory size.

## Usage

``` r
tte_summary(trial, ...)
```

## Arguments

- trial:

  A \[TTETrial\] object.

- ...:

  Method arguments: \`pretty\` (logical, default FALSE). If TRUE, prints
  a formatted summary to the console instead of returning a list.

## Value

If \`pretty = FALSE\` (default), returns a list with:

- n_rows:

  Number of rows in the data

- person_weeks:

  Total person-weeks (sum of person_weeks column, or NA if not
  available)

- n_trials:

  Number of unique trials

- n_individuals:

  Number of unique individuals (or NA if person_id_var not set)

- n_events:

  Number of events (sum of event column, or NA if not available)

- size_mb:

  Object size in megabytes

If \`pretty = TRUE\`, prints formatted output and invisibly returns the
list.

## See also

Other tte_methods:
[`tte_collapse()`](https://papadopoulos-lab.github.io/swereg/reference/tte_collapse.md),
[`tte_enroll()`](https://papadopoulos-lab.github.io/swereg/reference/tte_enroll.md),
[`tte_extract()`](https://papadopoulos-lab.github.io/swereg/reference/tte_extract.md),
[`tte_ipcw_pp()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipcw_pp.md),
[`tte_ipw()`](https://papadopoulos-lab.github.io/swereg/reference/tte_ipw.md),
[`tte_irr()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr.md),
[`tte_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_irr_combine.md),
[`tte_km()`](https://papadopoulos-lab.github.io/swereg/reference/tte_km.md),
[`tte_prepare_outcome()`](https://papadopoulos-lab.github.io/swereg/reference/tte_prepare_outcome.md),
[`tte_rates()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates.md),
[`tte_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rates_combine.md),
[`tte_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tte_rbind.md),
[`tte_table1()`](https://papadopoulos-lab.github.io/swereg/reference/tte_table1.md),
[`tte_truncate()`](https://papadopoulos-lab.github.io/swereg/reference/tte_truncate.md),
[`tte_weight_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weight_summary.md),
[`tte_weights()`](https://papadopoulos-lab.github.io/swereg/reference/tte_weights.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get statistics as a list
stats <- trial |> tte_summary()
cat("Rows:", stats$n_rows, "\n")

# Print formatted summary
trial |> tte_summary(pretty = TRUE)
} # }
```
