# Impute missing confounders by sampling from observed values

Thin standalone wrapper that delegates to
\`trial\$impute_confounders()\`. Exists as a standalone function so it
can be used as the default \`impute_fn\` callback in
\`\$generate_enrollments_and_ipw()\`.

## Usage

``` r
tte_impute_confounders(trial, confounder_vars, seed = 4L)
```

## Arguments

- trial:

  A \[TTETrial\] object.

- confounder_vars:

  Character vector of confounder column names to impute.

- seed:

  Integer seed for reproducibility (default: 4L).

## Value

The modified \[TTETrial\] object (invisibly).
