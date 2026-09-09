# Count the missing follow-up confounder values of a trial panel

The function returns one row per confounder that carries a
`.tte_entry__` snapshot column. Run it once before the fill and once
after it, then pass both tables to
[`tteenrollment_fill_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_summary.md).

## Usage

``` r
tteenrollment_fill_aggregates(trial)
```

## Arguments

- trial:

  A
  [TTEEnrollment](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
  object at trial level.

## Value

A data.table of class `tteenrollment_fill_aggregates`, with columns:

- `confounder`:

  The confounder name.

- `rows_n`:

  Rows in the panel.

- `trials_n`:

  Person-trials in the panel.

- `rows_na_n`:

  Rows whose plain value is `NA`.

- `trials_na_n`:

  Person-trials with at least one `NA` plain value.

- `trials_entry_na_n`:

  Person-trials whose snapshot value is `NA`.

- `rows_leading_na_in_entry_na_trials_n`:

  Rows the imputed snapshot value reaches. Within each person-trial
  whose snapshot is `NA`, it is the length of the `NA` run at the start
  of that person-trial.

- `key_digest`:

  A digest of the ordered `id_var`, `tstart_var` and `tstop_var`
  columns. It is the same string on every row.

## Details

The function orders a local copy of the key columns. It never reorders
`trial$data`.

## See also

[`tteenrollment_fill_followup_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_followup_confounders.md)
for the fill itself.

Other tte_methods:
[`tteenrollment_combined_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_combined_combine.md),
[`tteenrollment_fill_followup_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_followup_confounders.md),
[`tteenrollment_fill_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_summary.md),
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
tteenrollment_fill_aggregates(TTEEnrollment$new(d, design))
#>    confounder rows_n trials_n rows_na_n trials_na_n trials_entry_na_n
#>        <char>  <int>    <int>     <int>       <int>             <int>
#> 1:     income      6        2         4           2                 0
#>    rows_leading_na_in_entry_na_trials_n                       key_digest
#>                                   <int>                           <char>
#> 1:                                    0 d68f95b906d66b2ea4d45ff0afe6a07c
```
