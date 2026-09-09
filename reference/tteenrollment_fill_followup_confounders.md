# Carry the last observed confounder value forward through follow-up

The censoring model needs a value on every follow-up row. This function
fills each plain confounder column of a trial panel. It carries the last
observed value forward within each person-trial.

## Usage

``` r
tteenrollment_fill_followup_confounders(trial)
```

## Arguments

- trial:

  A
  [TTEEnrollment](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
  object at trial level.

## Value

The
[TTEEnrollment](https://papadopoulos-lab.github.io/swereg/reference/TTEEnrollment.md)
object, invisibly.

## Details

The carry starts from the entry-window snapshot, so the first row of a
person-trial is never left `NA`. `$s1_impute_confounders()` supplies
that snapshot value for a person-trial with no observed value before
follow-up.

The function reads `trial$design` for every column name. It fills only
the confounders that carry a `.tte_entry__` snapshot column. A panel
with no snapshot column at all comes back untouched.

The function sorts `trial$data` by `id_var`, then `tstart_var`, then
`tstop_var`. It modifies `trial$data` by reference. It appends
`"fill_followup"` to `trial$steps_completed` once. A second call appends
no second token and changes no value.

The function stops when a filled row would still hold `NA`. That happens
when the snapshot is `NA` and no earlier row of the same person-trial
holds a value.

## See also

[`tteenrollment_fill_aggregates()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_aggregates.md)
for the counts the fill changes.

Other tte_methods:
[`tteenrollment_combined_combine()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_combined_combine.md),
[`tteenrollment_fill_aggregates()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_aggregates.md),
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
trial <- TTEEnrollment$new(d, design)
tteenrollment_fill_followup_confounders(trial)
trial$data$income
#> [1] "low"  "mid"  "mid"  "mid"  "mid"  "high"
```
