# Target trial emulation on a matched-set study

This vignette explains what a target trial emulation (TTE) does when one
person has more than one copy in the skeleton files. That happens on a
study built with `$set_matched_sets()`, and on a study built with
`$set_batches()` that puts one ID in two batches.

The short version:

- The TTE enrolls a person at most once per enrollment period. A person
  MAY be enrolled in many periods.
- All the enrollments of one person are clustered on that person.
- To get both, s1 reads one copy of each person and drops the other
  copies before it computes anything.

Every number below comes from running swereg on a small study that this
vignette builds.

## Memberships and copies

A **matched set** is one case and the controls matched to it. The
**roster** is the table you give to `$set_matched_sets()`. It has one
row per **membership**, which is one person in one matched set. A person
who is a control in two sets has two memberships.

Each membership has its own **skeleton ID**. `$save_rawbatch()` writes
the person’s whole registry history once per skeleton ID, with the
skeleton ID in the ID column. So a person with two memberships has two
**copies** in the skeleton files. A copy is the rows of one membership:
one (batch, skeleton ID).

There are two ways to get a second copy:

- Two memberships in one batch. The person has two skeleton IDs in one
  skeleton file.
- Two memberships in two batches. The person has a copy in each of the
  two skeleton files, under the same skeleton ID or under two.

`$set_batches()` gives the second way when an ID is in two batches.

Every copy of a person holds the same registry history, because each one
is written from the same rawbatch rows.

## How the TTE enrolls

The TTE is a sequence of trials, one per enrollment period of
`period_width` weeks. In each period:

1.  Every eligible person who starts the intervention is enrolled.
2.  Comparators are sampled from the eligible comparators of that
    period, `comparator_to_intervention_ratio` times the number of
    interventions, with the spec’s `seed`.

A comparator who stays eligible can be sampled again in a later period.
So one person can be enrolled in several periods. Each enrollment is a
**person-trial**, with its own time zero. The analysis clusters on the
person: `survey::svydesign(ids = ~id)` puts all the person-trials of one
ID in one cluster.

Both rules need the ID to be the person. A second copy breaks them:

- Each copy is eligible on its own, so a person is enrolled twice in one
  period, or is in the comparator pool twice.
- Two skeleton IDs of one person are two clusters, so the analysis
  treats one person as two independent persons.

## One copy per person

s1 keeps the first copy of each person by (batch, skeleton ID), among
the skeleton files the run reads. s1a drops the rows of every other copy
straight after it loads a skeleton, before the eligibility rules run.
Then:

- The attrition counts, the tuples, the rare-condition counts, the
  comparator draw and the panels hold each person once.
- Each person is left with one skeleton ID, the one of the kept copy. A
  skeleton ID belongs to one person, so the clustering on `id` is a
  clustering on the person. The kept ID need not equal the person ID.

A copy that is dropped is not an exclusion. The person is still there,
as the kept copy. So no attrition or CONSORT step counts the dropped
copies. The s1 log prints their number.

On a study where no person has two copies, s1 drops nothing and runs as
it did before.

## A worked example

The study below has 24 persons in two batches. Persons 1 to 4 start the
intervention in different periods. Everyone else is a comparator in
every week.

``` r
library(swereg)
#> swereg 27.3.14
#> https://papadopoulos-lab.github.io/swereg/
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%

set.seed(1)
sk <- create_skeleton(1:24, isoyear_min = 2019, isoyearweek_max = "2019-52")
sk <- sk[is_isoyear == FALSE]
weeks <- sort(unique(sk$isoyearweek))
start <- c(5L, 13L, 21L, 29L)
sk[, week := match(isoyearweek, weeks)]
sk[, rd_tx := "control"]
for (p in 1:4) {
  sk[id == p, rd_tx := fifelse(week >= start[p], "treated", "none")]
}
sk[, rd_age_continuous := 50 + id / 2]
sk[, osd_a := rbinom(.N, 1L, 0.005)]
sk[, week := NULL]
setkey(sk, id, isoyearweek)
```

The roster gives three persons a second copy:

- Person 1, who starts the intervention, has a second membership in
  batch 1, under skeleton ID 101.
- Person 5, a comparator, has a second membership in batch 1, under
  skeleton ID 105.
- Person 6, a comparator, has a second membership in batch 2, under the
  same skeleton ID 6.

``` r
roster <- list(
  data.table(lopnr = c(1:12, 1L, 5L), skeleton_id = c(1:12, 101L, 105L)),
  data.table(lopnr = c(13:24, 6L), skeleton_id = c(13:24, 6L))
)
roster[[1]][, `:=`(set_id = 1L, role = "member")]
roster[[2]][, `:=`(set_id = 2L, role = "member")]
roster[[1]][lopnr %in% c(1L, 5L)]
#>    lopnr skeleton_id set_id   role
#>    <int>       <int>  <int> <char>
#> 1:     1           1      1 member
#> 2:     5           5      1 member
#> 3:     1         101      1 member
#> 4:     5         105      1 member
```

`$save_rawbatch()` writes one copy per skeleton ID. The code below
writes the same skeleton files by hand. The second study holds the same
24 persons once each.

``` r
copy_of <- function(p, sid) {
  x <- copy(sk[id == p])
  x[, id := sid]
  return(x)
}
write_study <- function(dir, with_copies) {
  for (d in c("spec", "tteplan", "results", "meta")) {
    dir.create(file.path(dir, d), recursive = TRUE, showWarnings = FALSE)
  }
  b1 <- sk[id <= 12]
  b2 <- sk[id > 12]
  if (with_copies) {
    b1 <- rbind(b1, copy_of(1L, 101L), copy_of(5L, 105L))
    b2 <- rbind(b2, copy_of(6L, 6L))
  }
  setkey(b1, id, isoyearweek)
  setkey(b2, id, isoyearweek)
  files <- file.path(dir, "tteplan", sprintf("skeleton_%05d.qs2", 1:2))
  qs2::qs_save(b1, files[1])
  qs2::qs_save(b2, files[2])
  study <- list(
    skeleton_files = files,
    data_meta_dir = file.path(dir, "meta"),
    load_skeleton = function(b) list(data = qs2_read(files[b]))
  )
  if (with_copies) {
    study$matched_set_roster <- roster
    study$id_col <- "lopnr"
  } else {
    study$batch_id_list <- list(1:12, 13:24)
  }
  return(study)
}
```

A real study is a `RegistryStudy`. The plan reads three of its fields:
`skeleton_files`, `matched_set_roster` and `id_col`. This example passes
them in a list.

The spec has one enrollment, treated against control, with a new-user
rule and an age range.

``` r
run_s1 <- function(with_copies) {
  dir <- tempfile("ms_")
  study <- write_study(dir, with_copies)
  write_spec(file.path(dir, "spec", "spec_v001.yaml"))
  invisible(capture.output(suppressMessages(
    plan <- tteplan_from_spec_and_registrystudy(
      study = study,
      candidate_dir_spec = file.path(dir, "spec"),
      candidate_dir_tteplan = file.path(dir, "tteplan"),
      candidate_dir_results = file.path(dir, "results"),
      spec_version = "v001",
      check_skeletons = FALSE
    )
  )))
  log <- capture.output(suppressWarnings(
    plan$s1_generate_enrollments_and_ipw(
      n_workers = 1L,
      swereg_dev_path = swereg:::.swereg_dev_path(),
      check_skeletons = FALSE
    )
  ))
  raw <- qs2_read(file.path(plan$dir_tteplan, plan$ett$file_raw[1]))
  return(list(plan = plan, log = log, panel = raw$data))
}
with_copies <- run_s1(TRUE)
without <- run_s1(FALSE)
```

### Which copies s1 drops

``` r
files <- with_copies$plan$skeleton_files
swereg:::.tte_copies_to_drop(with_copies$plan$registrystudy, files)
#> $drop
#> $drop$skeleton_00001.qs2
#> [1] 101 105
#> 
#> $drop$skeleton_00002.qs2
#> [1] 6
#> 
#> 
#> $n_dropped
#> [1] 3
grep("copies of a person", with_copies$log, value = TRUE)
#> [1] "      copies of a person dropped before eligibility: 3"
```

s1 drops skeleton IDs 101 and 105 from batch 1, and ID 6 from batch 2.
Person 6 keeps the copy in batch 1, because batch 1 comes first.

### One enrollment per person per period

The study with copies gives exactly the result of the study without
them: the same attrition, the same comparator draw and the same panels.

``` r
identical(with_copies$plan$get_attrition(), without$plan$get_attrition())
#> [1] TRUE
identical(with_copies$plan$get_matching(), without$plan$get_matching())
#> [1] TRUE
identical(with_copies$panel, without$panel)
#> [1] TRUE
```

No skeleton ID of a dropped copy is in the panel:

``` r
intersect(c(101L, 105L), with_copies$panel$id)
#> integer(0)
```

The attrition counts each person once. The first step counts 24 persons,
not 27:

``` r
att <- with_copies$plan$get_attrition()
att[is.na(enrollment_period_id), .(step_name, n_persons, n_person_trials)][1:3]
#>                   step_name n_persons n_person_trials
#>                      <char>     <num>           <num>
#> 1:        before_exclusions        24             336
#> 2: eligible_valid_treatment        24             320
#> 3:        eligible_isoyears        24             320
```

### Several periods, one cluster

Person 5 is a comparator in every week, so the draw can pick them in
more than one period. Each enrollment is one person-trial, and every one
carries the ID 5:

``` r
trials <- unique(with_copies$panel[,
  .(id, enrollment_period_id, enrollment_person_trial_id, baseline_intervention)
])
trials[id == 5L]
#>       id enrollment_period_id enrollment_person_trial_id baseline_intervention
#>    <int>                <int>                     <char>                <lgcl>
#> 1:     5                 1555                  01.5.1555                 FALSE
#> 2:     5                 1559                  01.5.1559                 FALSE
```

Clustering on `id` gives one cluster per person, so the number of
clusters is the number of enrolled persons:

``` r
trials[, .(person_trials = .N, clusters = uniqueN(id))]
#>    person_trials clusters
#>            <int>    <int>
#> 1:            12       11
```

Person 1 starts the intervention once, and is enrolled once as an
intervention:

``` r
trials[id == 1L]
#>       id enrollment_period_id enrollment_person_trial_id baseline_intervention
#>    <int>                <int>                     <char>                <lgcl>
#> 1:     1                 1553                  01.1.1553                  TRUE
```

## The s0 check

Dropping a copy is safe only when the copies agree on everything the
spec reads. They do when every column comes from the rawbatch rows. A
phase function can break that, because it reads `batch_data[["roster"]]`
and can give each membership its own values.

So
[`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md)
checks the copies before s1:

- It loads the skeleton files that hold the first person with more than
  one copy.
- For every person whose copies are all in those files, every copy MUST
  hold the same weeks.
- Every column the spec reads MUST hold the same value in every copy,
  week by week.

Otherwise it stops and names the column. Here the second copy of person
5 gets another age, and the spec reads the age:

``` r
dir <- tempfile("ms_bad_")
study <- write_study(dir, TRUE)
b1 <- qs2_read(study$skeleton_files[1])
b1[id == 105L, rd_age_continuous := rd_age_continuous + 1]
qs2::qs_save(b1, study$skeleton_files[1])
write_spec(file.path(dir, "spec", "spec_v001.yaml"))
msg <- tryCatch(
  suppressMessages(capture.output(tteplan_from_spec_and_registrystudy(
    study = study,
    candidate_dir_spec = file.path(dir, "spec"),
    candidate_dir_tteplan = file.path(dir, "tteplan"),
    candidate_dir_results = file.path(dir, "results"),
    spec_version = "v001",
    check_skeletons = FALSE
  ))),
  error = conditionMessage
)
writeLines(strwrap(msg, 60))
#> tteplan_from_spec_and_registrystudy(): the spec reads
#> column 'rd_age_continuous', and the copies of person 5 hold
#> different values in it. s1 reads one copy per person, so a
#> column the spec reads MUST be the same in every copy.
```

What the check does not prove:

- It reads only the files that hold the first person with two copies.
  Copies in other batches are not compared.
- A column the spec does not read MAY differ between copies. The TTE
  never sees it, because s1a keeps only the columns the spec reads.

The check runs in s0 and takes seconds. It runs before s1 for that
reason: s1 to s3 run for days, and they never stop on a copy.

## Counts

- `plan$expected_n_ids`, printed as “Individuals” in the spec summary,
  counts distinct persons. On a matched-set study it counts the persons
  of the roster.
- `study$n_ids` counts distinct IDs across the batches. After
  `$set_matched_sets()` these are skeleton IDs, so a person with two
  memberships counts twice there.
- The CONSORT diagram and the attrition tables count the kept copy only,
  so each person once.

``` r
with_copies$plan$expected_n_ids
#> [1] 24
```
