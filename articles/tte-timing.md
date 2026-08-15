# TTE timing: intervals, the landmark, and the estimand

## The interval convention

Read this section before any other. Every worked example below depends
on it, and a reader who takes the other convention misreads all of them.

Every interval in swereg is `[tstart, tstop)`. The stop is
**exclusive**. The person holds no part of the week that `tstop` names.

Every duration is `person_weeks = tstop - tstart`. It **never** adds
one.

Three four-week bands span `[0, 12)`. That is 12 person-weeks, and the
three bands bill 4, 4 and 4:

    band 1   [0,  4)   ->  4 - 0  = 4 weeks
    band 2   [4,  8)   ->  8 - 4  = 4 weeks
    band 3   [8, 12)   -> 12 - 8  = 4 weeks
                              total = 12 weeks

The `+ 1` form is the **inclusive** convention. Under it, weeks 1
through 4 is `4 - 1 + 1 = 4`. The same three bands then bill 5, 5 and 5,
for a total of 15. Both are correct arithmetic. The two differ in
whether the stop belongs to the interval.

Mixing them is how an off-by-one becomes a silently wrong denominator.
Nothing errors. A rate simply divides by 15 person-weeks where the panel
holds 12.

### A boundary is not a duration

A **boundary** is a week index on the follow-up scale. A **duration** is
a count of weeks. The two corrections are not interchangeable.

`weeks_to_admin_end` needed a `+ 1L`, because
[`difftime()`](https://rdrr.io/r/base/difftime.html) counts the whole
weeks BETWEEN the baseline week and the administrative week. The person
is under study to the end of the administrative week itself, so the
boundary sits one week after that count.

`person_weeks` sits beside it in the same function and MUST NOT add one.
It is a duration, and the two rows that reach that boundary already bill
exactly the weeks between them.

### The five boundary quantities

| Column                        | What it names                                  |
|-------------------------------|------------------------------------------------|
| `weeks_to_event`              | the week the first outcome occurrence stops in |
| `weeks_to_protocol_deviation` | the week per-protocol follow-up stops in       |
| `weeks_to_loss`               | the week observation stops in                  |
| `weeks_to_admin_end`          | the week the administrative end stops in       |
| `weeks_to_record_end`         | the week the person’s record stops in          |

Each of the five is an exclusive stop on the follow-up scale.
`tests/testthat/test-interval-convention.R` pins each one with its own
test. Every fixture there places its boundary where the exclusive and
the inclusive reading disagree, and each test asserts both numbers.

## The worked skeleton

Every example below runs on one small skeleton. It holds 16 consecutive
ISO year-weeks, `period_width = 4` and `follow_up_time = 12`. The first
four weeks are the entry band. The remaining twelve are three follow-up
bands.

``` r
library(swereg)
library(data.table)

PW <- 4L # period_width
FU <- 12L # follow_up_time

# Sixteen consecutive weeks, starting on a band boundary.
tte_weeks <- function(n = 16L) {
  wk <- copy(cstime::dates_by_isoyearweek[, list(isoyearweek)])
  wk[, idx := .I]
  s <- wk[isoyearweek >= "2020-01" & (idx - 1L) %% PW == 0L]$idx[1]
  wk$isoyearweek[s:(s + n - 1L)]
}
weeks <- tte_weeks()

# One woman, one row per week.
#
# `arm_weeks` is the treatment column, week by week. `TRUE` is the
# intervention, `FALSE` is the comparator, and `NA` is neither arm.
# `discordant_fu`, `event_fu` and `absent_fu` name 1-indexed FOLLOW-UP weeks.
# Follow-up week `f` is the interval `[f - 1, f)`, so its right edge is `f`.
tte_person <- function(id, arm_weeks, discordant_fu = integer(0),
                       event_fu = integer(0), event_entry = integer(0),
                       absent_fu = integer(0)) {
  n <- length(weeks)
  fu <- seq_len(n) - PW
  assigned <- any(arm_weeks[seq_len(PW)] %in% TRUE)
  on_tx <- rep(assigned, n)
  on_tx[fu %in% discordant_fu] <- !assigned
  d <- data.table(
    id = id,
    isoyearweek = weeks,
    exposed = arm_weeks,
    eligible = seq_len(n) <= PW,
    died = (fu %in% event_fu) | (seq_len(n) %in% event_entry),
    on_tx = on_tx,
    age = 50 + seq_len(n)
  )
  d[!(fu %in% absent_fu)]
}

# Concordant fillers, so the propensity model has rows to fit.
tte_fillers <- function(ni = 8L, nc = 12L) {
  n <- length(weeks)
  rbindlist(list(
    rbindlist(lapply(seq_len(ni), function(i) {
      tte_person(paste0("FI", i), rep(TRUE, n))
    })),
    rbindlist(lapply(seq_len(nc), function(i) {
      tte_person(paste0("FC", i), rep(FALSE, n))
    }))
  ))
}

tte_design <- function(ik = 0L, ck = 0L, admin = NULL) {
  TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "age",
    follow_up_time = FU,
    period_width = PW,
    intervention_tolerance_weeks = ik,
    comparator_tolerance_weeks = ck,
    admin_censor_isoyearweek = admin
  )
}

tte_enroll <- function(d, design, ratio = 2, seed = 4) {
  TTEEnrollment$new(
    data = copy(d), design = design,
    ratio = ratio, seed = seed, extra_cols = "isoyearweek"
  )
}

# The fixtures are small, so the censoring model separates and warns. That
# warning is about the toy data and not about a boundary.
tte_prepare <- function(trial, estimand = "pp") {
  suppressWarnings({
    trial$s2_ipw(stabilize = TRUE)
    trial$s4_prepare_for_analysis(
      outcome = "died", follow_up = FU, estimand = estimand,
      estimate_ipcw_pp_with_gam = FALSE
    )
  })
  trial$data
}
```

A woman with no boundary of any kind bills three whole bands:

``` r
whole <- rbindlist(list(tte_person("WHOLE", rep(TRUE, 16L)), tte_fillers()))
tte_enroll(whole, tte_design())$data[
  id == "WHOLE"
][order(tstart), .(entry_band_id, trial_id, tstart, tstop, person_weeks)]
#>    entry_band_id trial_id tstart tstop person_weeks
#>            <int>    <int>  <int> <int>        <int>
#> 1:          1566     1567      0     4            4
#> 2:          1566     1568      4     8            4
#> 3:          1566     1569      8    12            4
```

## Three instants, and they are three different weeks

A person-trial has three instants, and each answers a different
question.

The table shows the first two bands of the worked skeleton below.

| week index        | 6264  | 6265  | 6266  | 6267  | 6268     | 6269     | 6270     | 6271     |
|-------------------|-------|-------|-------|-------|----------|----------|----------|----------|
| band              | 1566  | 1566  | 1566  | 1566  | 1567     | 1567     | 1567     | 1567     |
| role              | entry | entry | entry | entry | landmark | n/a      | n/a      | n/a      |
| `[tstart, tstop)` | n/a   | n/a   | n/a   | n/a   | `[0, 1)` | `[1, 2)` | `[2, 3)` | `[3, 4)` |

1.  The **recruiting week** is the earliest week of the entry band that
    is both eligible and in an arm. swereg reads the confounders there.
2.  The **entry band** is the window that classifies the arm. It
    contributes no follow-up.
3.  The **landmark** is the week that closes the entry band. Follow-up
    opens there, at `tstart == 0`.

Band `b` covers week indices `b * period_width` to
`(b + 1) * period_width - 1`. Its landmark sits at week index
`(b + 1) * period_width`, which is the first week of band `b + 1`. Week
indices are positions in
[`cstime::dates_by_isoyearweek`](https://rdrr.io/pkg/cstime/man/dates_by_isoyearweek.html),
minus one. That is the scale `trial_id` reads.

`entry_band_id` names the trial. `trial_id` names the follow-up band, so
the first row of every person-trial holds
`trial_id == entry_band_id + 1` and `tstart == 0`.

### The recruiting week is not the first week of the entry band

EARLY is on the intervention in every week. LATE holds `NA` in weeks 1
and 2, then starts in week 3. `age` rises by one each week, so the
recruiting week is visible in the value swereg reads.

``` r
n <- length(weeks)
three <- rbindlist(list(
  tte_person("EARLY", rep(TRUE, n)),
  tte_person("LATE", c(NA, NA, rep(TRUE, n - 2L))),
  tte_person("CMP1", rep(FALSE, n)),
  tte_fillers()
))
tte_enroll(three, tte_design())$data[
  id %chin% c("EARLY", "LATE", "CMP1") & tstart == 0
][order(id), .(id, exposed, entry_band_id, trial_id, .tte_entry__age, age)]
#> Key: <id, trial_id>
#>        id exposed entry_band_id trial_id .tte_entry__age   age
#>    <char>  <lgcl>         <int>    <int>           <num> <num>
#> 1:   CMP1   FALSE          1566     1567              51    55
#> 2:  EARLY    TRUE          1566     1567              51    55
#> 3:   LATE    TRUE          1566     1567              53    55
```

`.tte_entry__age` holds 51 for EARLY and 53 for LATE. Week 1 gives 51
and week 3 gives 53, so LATE is recruited in the week her treatment
starts. The plain `age` column holds the time-updated value of the
follow-up band, which is 55.

Entry covariates are therefore read in the **same** week treatment
starts for an initiator, and not strictly before it. The rule is
symmetric across the arms: no rule keyed to initiation can be, because a
comparator never initiates. `$s2_ipw()` and `$table1()` read
`.tte_entry__<v>`. `$s6_ipcw_pp()` reads the time-updated `<v>`, because
censoring depends on what is true during follow-up.

### Band boundaries are anchored to the calendar

`trial_id` is `(week_index - 1) %/% period_width` over
[`cstime::dates_by_isoyearweek`](https://rdrr.io/pkg/cstime/man/dates_by_isoyearweek.html),
which starts at ISO week `1900-01`. Two studies with different start
dates share the same boundaries.

``` r
band_of <- function(w) {
  (match(w, cstime::dates_by_isoyearweek$isoyearweek) - 1L) %/% PW
}
data.table(
  isoyearweek = weeks[1:8],
  study_a_from_week_1 = band_of(weeks[1:8]),
  study_b_from_week_3 = c(NA, NA, band_of(weeks[3:8]))
)
#>    isoyearweek study_a_from_week_1 study_b_from_week_3
#>         <char>               <int>               <int>
#> 1:     2020-04                1566                  NA
#> 2:     2020-05                1566                  NA
#> 3:     2020-06                1566                1566
#> 4:     2020-07                1566                1566
#> 5:     2020-08                1567                1567
#> 6:     2020-09                1567                1567
#> 7:     2020-10                1567                1567
#> 8:     2020-11                1567                1567
```

Study B starts two weeks later and still breaks at the same week. Its
first band holds two weeks rather than four.

## The qualified population

A person-band reaches the candidate table only when both statements hold
at its landmark.

1.  The person is under observation at the landmark.
2.  No outcome occurrence stops at or before the landmark.

A week is a half-open interval, so an occurrence in week `w` stops at
`w + 1`. Statement 2 therefore covers every week of the entry band, and
every week before it.

Statement 2 reads **every** column in `design$outcome_vars`, and not the
one outcome a later step analyses. One enrollment serves several
outcomes, so one enrolled set has to be event-free for all of them.

Eligibility stays a baseline property. `.band_baseline_treatment()`
assesses it on the entry band, and no step reads it again at the
landmark. Reading it there would empty the intervention arm, because a
new-user exclusion turns `eligible` off from the week after initiation.

### The row-presence assertion, and what it assumes

`observed_var` decides statement 1, and it takes one of two forms.

- `observed_var: {column: rd_observed}` names a real logical person-week
  column.
- `observed_var: {sentinel: row_presence}` asserts that the caller
  already deleted every unobserved person-week.

The sentinel is an assertion about the caller’s skeleton, and swereg
cannot check it. Under it, a row exists if and only if the person was
under observation that week. Use it only when the skeleton already
deletes every person-week the person was not observed in.

A skeleton that already trims to observed weeks cannot carry a useful
`observed` column. Every retained row would hold `TRUE`, so the column
could not represent an absent week. The `NEWS.md` entry for 26.9.0 names
the trimming rules of the production skeleton.

Reading row presence as observation WITHOUT declaring the sentinel stays
forbidden, and
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops on a spec that declares neither form.

### An outcome inside the entry band

W has the outcome in week 2 of her entry band and starts treatment in
week 4 of the same band. K is the same woman without the outcome.

``` r
qual <- rbindlist(list(
  tte_person("W", c(NA, NA, NA, rep(TRUE, 13L)), event_entry = 2L),
  tte_person("K", c(NA, NA, NA, rep(TRUE, 13L))),
  tte_fillers()
))
tq <- tte_enroll(qual, tte_design())
sort(unique(tq$data[id %chin% c("W", "K")]$id))
#> [1] "K"
tq$landmark_attrition[is.na(trial_id)]
#>    trial_id n_persons n_person_trials n_intervention n_comparator
#>       <int>     <int>           <int>          <int>        <int>
#> 1:       NA        22              22             10           12
#> 2:       NA        22              22             10           12
#> 3:       NA        21              21              9           12
#>              criterion
#>                 <char>
#> 1: landmark_candidates
#> 2:   landmark_observed
#> 3: landmark_event_free
```

W does not enroll. Her event sits at `tstart = 0` under the old time
origin, with the initiation behind it. That is immortal-time
attribution, and removing it is why the step exists.

The cascade names the reason. Three criteria join the CONSORT attrition
table: `landmark_candidates`, `landmark_observed` and
`landmark_event_free`. Each count is cumulative, and each row splits
into the two arms.

### Matching happens after qualification

Qualification runs after the arm classification and before the
comparator draw. The position is part of the rule. It runs after the
classification, so attrition reports both arms. It runs before the draw,
so sampling refills the ratio from qualified comparators alone.

An unqualified comparator therefore cannot shrink the matched set.

``` r
one_int <- rbindlist(list(
  tte_person("I1", rep(TRUE, n)),
  tte_person("CBAD", rep(FALSE, n), event_entry = 2L),
  tte_fillers(ni = 0L, nc = 2L)
))
m <- tte_enroll(one_int, tte_design(), ratio = 2)
m$data[tstart == 0, .N, by = exposed][order(exposed)]
#>    exposed     N
#>     <lgcl> <int>
#> 1:   FALSE     2
#> 2:    TRUE     1
sort(unique(m$data$id))
#> [1] "FC1" "FC2" "I1"
```

CBAD has the outcome in week 2 of her entry band, so she cannot qualify.
The draw returns two comparators for one initiator, and it takes both
from the qualified pool. CBAD is not among them.

## Protocol deviation is read weekly

Deviation used to be decided from the band-collapsed treatment value,
which is the LAST week of the band. A woman’s verdict followed where her
weeks fell against the calendar grid, and not what she did.

`enroll()` now reads the weekly sequence itself, and writes one exact
boundary into `weeks_to_protocol_deviation`.

An assessment is discordant when `time_treatment_var` does not hold the
assigned arm of that person-trial. `NA` is discordant in both arms.

A tolerance is the number of CONSECUTIVE discordant assessments an arm
allows. A concordant assessment resets the run. For tolerance `k`,
follow-up stops at the right edge of the `(k + 1)`th consecutive
discordant week.

### Switch and return

Four women, each on the intervention, differing only in which follow-up
weeks are discordant.

``` r
dev <- rbindlist(list(
  tte_person("SWITCH_3_4", rep(TRUE, n), discordant_fu = c(3L, 4L)),
  tte_person("RETURN_2", rep(TRUE, n), discordant_fu = 2L),
  tte_person("RETURN_2_3", rep(TRUE, n), discordant_fu = c(2L, 3L)),
  tte_person("LATE_9", rep(TRUE, n), discordant_fu = 9L),
  tte_fillers()
))
boundary <- function(ik) {
  unique(tte_enroll(dev, tte_design(ik = ik))$data[
    id %like% "_",
  ][!id %like% "^F", .(id, weeks_to_protocol_deviation)])
}
merge(boundary(0L), boundary(1L), by = "id", suffixes = c("_tol0", "_tol1"))
#> Key: <id>
#>            id weeks_to_protocol_deviation_tol0 weeks_to_protocol_deviation_tol1
#>        <char>                            <int>                            <int>
#> 1:     LATE_9                                9                               NA
#> 2:   RETURN_2                                2                               NA
#> 3: RETURN_2_3                                2                                3
#> 4: SWITCH_3_4                                3                                4
```

RETURN_2 is discordant in one week and returns. Tolerance 0 stops her at
week 2. Tolerance 1 allows the single week, so she reaches the end of
follow-up and her boundary is `NA`.

RETURN_2_3 is discordant in two consecutive weeks. Tolerance 1 allows
the first and stops her at the right edge of the second, which is week
3.

LATE_9 is discordant in week 9 only. Tolerance 1 allows it, and no
second discordant week follows, so she is never censored for deviation.
A run that never reaches `k + 1` weeks does not censor, including a run
that the end of follow-up cuts short.

### The two tolerances are not symmetric

`intervention_tolerance_weeks` and `comparator_tolerance_weeks` are
separate fields, and each applies to its own arm. They are not symmetric
because the two arms are not the same object.

An initiator’s discordant week is a treatment gap, and a short gap is
often a dispensing artefact rather than a stop. A comparator’s
discordant week is an initiation, and an initiation is the very event
that defines the other arm.

Set them from the protocol, and never from each other. The two women
below share one discordant pattern and get opposite verdicts.

``` r
asym <- rbindlist(list(
  tte_person("INT", rep(TRUE, n), discordant_fu = c(2L, 3L)),
  tte_person("CMP", rep(FALSE, n), discordant_fu = c(2L, 3L)),
  tte_fillers()
))
unique(tte_enroll(asym, tte_design(ik = 0L, ck = 3L))$data[
  id %chin% c("INT", "CMP"), .(id, exposed, weeks_to_protocol_deviation)
])
#>        id exposed weeks_to_protocol_deviation
#>    <char>  <lgcl>                       <int>
#> 1:    CMP   FALSE                          NA
#> 2:    INT    TRUE                           2
```

### Loss of observation is never tolerated

An internal gap in the weekly sequence stops follow-up at the first
absent week. No tolerance applies, because loss of observation is not
discordance. The person may return in a later week. She is still
censored at the gap.

``` r
gap <- rbindlist(list(
  tte_person("GAP", rep(TRUE, n), absent_fu = 5L), tte_fillers()
))
unique(tte_enroll(gap, tte_design(ik = 3L))$data[
  id == "GAP", .(id, weeks_to_protocol_deviation, weeks_to_record_end)
])
#>        id weeks_to_protocol_deviation weeks_to_record_end
#>    <char>                       <int>               <int>
#> 1:    GAP                           4                  NA
```

GAP is missing follow-up week 5 and holds every other week. Under a
tolerance of 3 she still stops at week 4, the left edge of the absent
week. Her `weeks_to_record_end` is `NA`, because her record reaches the
end of the panel.

## The terminal band is clipped, and it is kept

The panel is one row per person-trial-band. Nothing is expanded weekly.
Every band before the boundary is complete follow-up, and the band that
reaches the boundary carries the censoring.

`s5_prepare_outcome()` clips that band at the exact boundary and sets
`person_weeks` to `tstop - tstart`. The row stays in the analysis data.
It carries the exposure before the boundary and nothing after it.

``` r
clip <- rbindlist(list(tte_person("EVENT7", rep(TRUE, n), event_fu = 7L), tte_fillers()))
tte_prepare(tte_enroll(clip, tte_design()))[
  id == "EVENT7"
][order(tstart), .(tstart, tstop, person_weeks, event, weeks_to_event)]
#>    tstart tstop person_weeks event weeks_to_event
#>     <int> <int>        <int> <int>          <int>
#> 1:      0     4            4     0              7
#> 2:      4     7            3     1              7
```

EVENT7 has the outcome in follow-up week 7, which is the interval
`[6, 7)`. Week 7 falls inside the band `[4, 8)`, so her terminal row
stops at 7 and bills three weeks. The two rows bill 7 weeks in total,
which is the boundary itself. Reading the boundary at the band stop
would bill 8.

### A record that ends inside a band bills only the weeks present

``` r
gone <- rbindlist(list(tte_person("GONE", rep(TRUE, n), absent_fu = 11:12), tte_fillers()))
tte_prepare(tte_enroll(gone, tte_design()))[
  id == "GONE"
][order(tstart), .(tstart, tstop, person_weeks, event, weeks_to_loss)]
#>    tstart tstop person_weeks event weeks_to_loss
#>     <int> <int>        <int> <int>         <int>
#> 1:      0     4            4     0            10
#> 2:      4     8            4     0            10
#> 3:      8    10            2     0            10
```

GONE holds ten follow-up weeks and bills ten. The band-level read
credited her with twelve, because it read the stop of the last band.

### The tie, and the priority order

Follow-up stops at the earliest of five events, and priority runs in
three levels.

1.  The first outcome event beats everything.
2.  A protocol deviation and an observed loss come next.
3.  An administrative end and a requested follow-up end come last.

An event that stops in the deviation band wins that band. The row counts
as an event and not as a censoring, and the deviation does not clip it.

``` r
tie <- rbindlist(list(
  tte_person("TIE", rep(TRUE, n), discordant_fu = 6L, event_fu = 6L),
  tte_fillers()
))
tte_prepare(tte_enroll(tie, tte_design(ik = 0L)))[
  id == "TIE"
][order(tstart), .(tstart, tstop, person_weeks, event, censor_this_period)]
#>    tstart tstop person_weeks event censor_this_period
#>     <int> <int>        <int> <int>              <int>
#> 1:      0     4            4     0                  0
#> 2:      4     6            2     1                  0
```

TIE has both boundaries at week 6. She exits as an event, her terminal
row stops at 6, and `censor_this_period` reads 0.

### The administrative end, exactly

``` r
adm <- rbindlist(list(tte_person("ADM", rep(TRUE, n)), tte_fillers()))
tte_prepare(tte_enroll(adm, tte_design(admin = weeks[10])))[
  id == "ADM"
][order(tstart), .(tstart, tstop, person_weeks, weeks_to_admin_end)]
#>    tstart tstop person_weeks weeks_to_admin_end
#>     <int> <int>        <int>              <int>
#> 1:      0     4            4                  6
#> 2:      4     6            2                  6
```

ADM enters at week 5 of the skeleton and the administrative week is week
10. [`difftime()`](https://rdrr.io/r/base/difftime.html) counts 5 whole
weeks between them, and the `+ 1L` makes the boundary 6. Her two rows
bill 4 and 2, which is 6. The boundary took the `+ 1L` and the durations
beside it did not.

Neither the administrative end nor the requested follow-up end is
rounded to a band boundary. A six-week requested follow-up stops at week
six, and a woman in a four-week band keeps the two weeks that used to
disappear.

## The estimand

### The qualified population

The population is every person-band that reaches its landmark `L` under
observation and free of every enrollment outcome through `L`. The whole
entry band must also classify it into an arm.

Matching happens after that. The comparator draw runs on the qualified
candidates, so the ratio counts qualified people.

### Intention-to-treat

Among people who reach `L` alive, event-free and under observation,
compare those the entry band classified as initiators against those it
classified as comparators. Follow-up runs from `L`. Later treatment
changes do not enter.

### Per-protocol

Among the same population, compare sustained treatment against sustained
non-treatment. Follow-up runs from `L` and stops at the first deviation
that exceeds the arm’s tolerance. The censoring weight adjusts for the
selection that this censoring introduces.

``` r
sw <- rbindlist(list(
  tte_person("SWITCH", rep(TRUE, n), discordant_fu = 5:12), tte_fillers()
))
rbind(
  data.table(estimand = "pp", tte_prepare(tte_enroll(sw, tte_design()), "pp")[
    id == "SWITCH", .(tstart, tstop, person_weeks)
  ]),
  data.table(estimand = "itt", tte_prepare(tte_enroll(sw, tte_design()), "itt")[
    id == "SWITCH", .(tstart, tstop, person_weeks)
  ])
)
#>    estimand tstart tstop person_weeks
#>      <char>  <int> <int>        <int>
#> 1:       pp      0     4            4
#> 2:       pp      4     5            1
#> 3:      itt      0     4            4
#> 4:      itt      4     8            4
#> 5:      itt      8    12            4
```

SWITCH is discordant from follow-up week 5 onward. Per-protocol bills 5
weeks. Intention-to-treat bills the whole 12.

### Two spec versions are not comparable

A result built on a spec version before swereg 26.9.0, for example
`v011`, and a result built on 26.9.0, for example `v012`, answer
different questions.

The difference is a combined change of five things at once: the
population, the time origin, the censoring rule, the weighting and the
estimator. It MUST NOT be described as a correction to the same
estimand. It MUST NOT be attributed to immortal time alone.

Report the two as separate analyses with separate protocols. A
difference between them decomposes into no single named cause, because
no single change was made in isolation.

## Limitations

- **The censoring approximation is compressed.** The panel is one row
  per band, so the censoring model reads a band and not a week. A
  boundary inside a band is exact in the person-time, and the censoring
  probability that covers it is still a band-level quantity.
- **Within-week ordering is unidentifiable.** A week is the finest
  resolution the source data carries. Two things in one week have no
  order, and swereg resolves them at the weekly right boundary by
  convention.
- **Timing is weekly, and not date-level.** A registry date is known to
  the day. The skeleton is a person-week grid, so every boundary rounds
  to a week.
- **Trials still open every `period_width` weeks.** The landmark removes
  the within-band immortal time. It does not make the trial grid finer,
  so a person who becomes eligible mid-band waits for the next band.
- **The estimand is now landmark-survivor.** It conditions on reaching
  `L`. People who die or have the outcome inside the entry band are not
  in the population. The estimate says nothing about them (Dafni 2011).
- **The censoring model carries no adherence history.** It reads the
  time-updated confounders, the band start and the trial index. It
  carries no lagged treatment term, so a person whose adherence has been
  failing for months looks like one who fails for the first time.
- **swereg implements no grace period.** A grace period allows
  initiation within a fixed window after assignment without counting it
  as a deviation, and it requires cloning, censoring and weighting
  (Hernán 2018; Maringe 2020). `period_width` gives within-band slack
  for the timing of initiation at enrollment, and nothing else.

## References

The passage quoted for each reference is from its abstract.

**Hernán MA, Alonso A, Logan R, et al. Observational studies analyzed
like randomized experiments: an application to postmenopausal hormone
therapy and coronary heart disease. Epidemiology. 2008;19(6):766-779.**
DOI 10.1097/EDE.0b013e3181875e61. PMID 18854702. The origin of this
construction. “The observational study was conceptualized as a sequence
of ‘trials,’ in which eligible women were classified as initiators or
noninitiators of estrogen/progestin therapy.”

**Hernán MA, Robins JM. Using big data to emulate a target trial when a
randomized trial is not available. Am J Epidemiol.
2016;183(8):758-764.** DOI 10.1093/aje/kwv254. PMID 26994063. “Causal
inference from large observational databases (big data) can be viewed as
an attempt to emulate a randomized experiment: the target experiment or
target trial: that would answer the question of interest.”

**Danaei G, García Rodríguez LA, Cantero OF, Logan RW, Hernán MA.
Observational data for comparative effectiveness research: an emulation
of randomised trials of statins and primary prevention of coronary heart
disease. Stat Methods Med Res. 2013;22(1):70-96.** DOI
10.1177/0962280211403603. PMID 22016461. The source of the two estimands
and of the weighting. “We also explain two approaches to conduct the
analogues of per-protocol and as-treated analyses after further
adjusting for measured time-varying confounding and selection bias using
inverse-probability weighting.”

**Caniglia EC, Zash R, Swanson SA, et al. Emulating target trials to
avoid immortal time bias: an application to antibiotic initiation and
preterm delivery. Epidemiology. 2023;34(3):430-438.** DOI
10.1097/EDE.0000000000001601. PMID 36805380. The reason time zero cannot
precede classification. “Defining exposure as antibiotic initiation at
any time during follow-up after time zero resulted in substantial
immortal time bias, making antibiotics appear protective against preterm
delivery.”

**Cain LE, Robins JM, Lanoy E, Logan R, Costagliola D, Hernán MA. When
to start treatment? A systematic approach to the comparison of dynamic
regimes using observational data. Int J Biostat. 2010;6(2):Article 18.**
DOI 10.2202/1557-4679.1212. PMID 21972433. The regime swereg does not
implement. It compares regimes of the form “initiate treatment within a
certain time period of some time-varying covariate first crossing a
particular threshold”.

**Anderson JR, Cain KC, Gelber RD. Analysis of survival by tumor
response. J Clin Oncol. 1983;1(11):710-719.** DOI
10.1200/JCO.1983.1.11.710. PMID 6668489. The origin of the landmark
method. “The usual method of comparing responders and nonresponders is
biased in favor of responders, and these results are frequently
misinterpreted as providing evidence that response prolongs survival.”

**Dafni U. Landmark analysis at the 25-year landmark point. Circ
Cardiovasc Qual Outcomes. 2011;4(3):363-371.** DOI
10.1161/CIRCOUTCOMES.110.957951. PMID 21586725. The conditional reading
of a landmark estimand. “The goal of the landmark method is to estimate
in an unbiased way the time-to-event probabilities in each group
conditional on the group membership of patients at a specific time
point, the landmark time.”

**Maringe C, Benitez Majano S, Exarchakou A, et al. Reflection on modern
methods: trial emulation in the presence of immortal-time bias. Int J
Epidemiol. 2020;49(5):1719-1729.** DOI 10.1093/ije/dyaa057. PMID
32386426. The cloning route swereg does not take. “The steps consist in:
(i) specifying the target trial and inclusion criteria; (ii) cloning
patients; (iii) defining censoring and survival times; (iv) estimating
the weights to account for informative censoring introduced by design;
and (v) analysing these data.”

**Hernán MA. How to estimate the effect of treatment duration on
survival outcomes using observational data. BMJ. 2018;360:k182.** DOI
10.1136/bmj.k182. PMID 29419381. The three steps a grace period needs.
“The first step is cloning people to assign them to multiple treatment
strategies. The second step is censoring clones when they deviate from
their assigned treatment strategy. The third step is performing inverse
probability weighting.”

## See also

- [`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md)
  for the pipeline that runs this.
- [`vignette("tte-methods")`](https://papadopoulos-lab.github.io/swereg/articles/tte-methods.md)
  for the statistical analysis plan and the validation evidence.
- [`vignette("tte-nomenclature")`](https://papadopoulos-lab.github.io/swereg/articles/tte-nomenclature.md)
  for the vocabulary and the file layout.
