# Report what the follow-up confounder fill changed

The function subtracts the aggregates of the filled panel from the
aggregates of the raw panel. The paper reports the result.

## Usage

``` r
tteenrollment_fill_summary(raw, imp)
```

## Arguments

- raw:

  A
  [TTEEnrollment](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
  object before the fill, or its aggregates table from
  [`tteenrollment_fill_aggregates()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_aggregates.md).

- imp:

  A
  [TTEEnrollment](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
  object after the fill, or its aggregates table.

## Value

A data.table, one row per confounder, with columns:

- `confounder`:

  The confounder name.

- `rows_n`:

  Rows in the panel.

- `rows_filled_n`:

  Rows the fill gave a value.

- `trials_n`:

  Person-trials in the panel.

- `trials_filled_n`:

  Person-trials the fill left with no `NA`.

- `trials_entry_imputed_n`:

  Person-trials whose snapshot value was `NA` in the raw panel.

- `rows_from_imputed_entry_n`:

  Rows the imputed snapshot value reaches.

## Details

Both arguments MUST describe the same panel. The function compares the
`key_digest` of the two tables and stops when they differ. It also stops
when the two confounder sets differ. It matches the rows of `imp` to the
rows of `raw` by confounder name. The two tables MAY therefore list the
same confounders in a different order.

An `impute_fn` passed to `$s1_generate_enrollments_and_ipw()` MUST NOT
change the row set. s1d takes the raw aggregates before it calls the
callback, and this function after. A callback that adds a row or drops a
row moves the `key_digest`. A change to an `id_var`, `tstart_var` or
`tstop_var` value moves it too, and s1d stops here. A reorder is safe,
because the digest sorts the three key columns first.

## See also

[`tteenrollment_fill_aggregates()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_aggregates.md)
for the input tables.

Other tte_methods:
[`tteenrollment_combined_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_combined_combine.md),
[`tteenrollment_fill_aggregates()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_aggregates.md),
[`tteenrollment_fill_followup_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_followup_confounders.md),
[`tteenrollment_irr_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_irr_combine.md),
[`tteenrollment_rates_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rates_combine.md),
[`tteenrollment_rbind()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_rbind.md)

## Examples

``` r
d <- data.table::data.table(
  enrollment_person_trial_id = rep(c("a", "b"), each = 3L),
  tstart = rep(c(0L, 4L, 8L), 2L),
  tstop = rep(c(4L, 8L, 12L), 2L),
  exposed = rep(c(TRUE, FALSE), each = 3L),
  event = 0L,
  income = c(NA, "mid", NA, NA, NA, "high"),
  .tte_entry__income = rep(c("low", "mid"), each = 3L)
)
design <- TTEDesign$new(
  treatment_var = "exposed",
  outcome_vars = "event",
  confounder_vars = "income",
  follow_up_time = 12L
)
trial <- TTEEnrollment$new(d, design)
raw <- tteenrollment_fill_aggregates(trial)
tteenrollment_fill_followup_confounders(trial)
tteenrollment_fill_summary(raw, trial)
#>    confounder rows_n rows_filled_n trials_n trials_filled_n
#>        <char>  <int>         <int>    <int>           <int>
#> 1:     income      6             4        2               2
#>    trials_entry_imputed_n rows_from_imputed_entry_n
#>                     <int>                     <int>
#> 1:                      0                         0
```
