# Writing your own add\_\* function

``` r
library(data.table)
library(swereg)
```

## Who this is for

swereg’s built-in `add_*` functions cover the common Swedish registries
(NPR, LMED, cause of death, quality registries, SCB one-time and
annual). They do not cover everything. You will need a custom `add_*` if
you work with:

- **Non-Swedish registries**: Norwegian NPR, Danish LPR, FinDep, etc.
  They follow similar longitudinal patterns but use different column
  names and coding systems.
- **Swedish registries that swereg has no built-in support for**:
  SWEDEHEART, RIKSHIP, regional cohorts, research registries.
- **In-house data**: payer claims, EHR extracts, institutional biobanks
  with time-stamped events.

swereg cannot ship code for every registry, so it defines a contract
that your own functions follow.

## Two ways to use a custom `add_*`

1.  **Register it via `RegistryStudy$register_codes()`.** The pipeline
    validates the contract on every call, fingerprints your function,
    and replays it when it changes, for incremental rebuilds. Use this
    path for production work.
2.  **Call it directly as a free function** in the manual workflow.
    Nothing validates it, so only your own tests catch mistakes. This is
    acceptable for one-off exploration and risky at scale.

The rest of this vignette is about path (1), because swereg enforces the
contract for you there. The function body is the same either way.

## The `add_*` contract

An `add_*` function that meets the contract:

1.  **Takes `skeleton` as its first argument** and mutates it in place
    using `skeleton[data, on = ..., := ...]`. No
    [`merge()`](https://rdrr.io/r/base/merge.html), no `left_join()`, no
    `skeleton <- ...` reassignment.
2.  **Preserves row count.** If `nrow(skeleton)` changes, the pipeline
    stops the run. Phase 1b, the trim registered with
    `RegistryStudy$register_trim()`, is the one phase that MAY delete
    skeleton rows.
3.  **Preserves the four structural columns** (`id`, `isoyear`,
    `isoyearweek`, `is_isoyear`). Read-only.
4.  **Adds one column per entry in the `codes` list**, named exactly as
    the list is named. If the caller passes
    `codes = list(flu_vax = ..., covid_vax = ...)`, the skeleton gains
    columns `flu_vax` and `covid_vax`.
5.  **Takes `id_name` as an explicit argument.** Different registries
    use different ID column names, so do not hard-code one.
6.  **Accepts `codes` as a named list.** It receives any extra
    configuration through positional / named arguments that the caller
    passes via `fn_args` (see below).

## Column-slot headroom

A table’s headroom is its count of free column slots. data.table
over-allocates the list that holds a table’s columns, and `:=` writes a
new column into a free slot, in place. Past the last free slot
data.table allocates a longer list. A longer list is a new R object, so
`[.data.table` rebinds the grown table in the frame it ran in.

Inside your `add_*` that frame belongs to your function. The caller
keeps the old table and never gets the new columns. data.table 1.18.4
reports nothing: no error, no warning.

The failure is silent where it happens and loud somewhere else. The
column count does not move, every call returns, and the run stops later
on a name that is missing. Read the free slot count first when a column
you asked for is not there.

data.table gives a new table 1024 free slots on top of its column count,
and restores 1024 whenever it grows one. qs2 keeps none of them, so
[`swereg::qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md)
restores them: 4096 for a skeleton, 1024 for every other table. So the
hazard needs a table that spent its slack, or a table that reached you
from [`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html) rather
than from
[`swereg::qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md).

Every shipped `add_*` grows the list first, writes the grown table back
to its caller where it can, and returns it either way.
`.ensure_dt_alloc()` in `R/dt_alloc.R` does that. A function you write
yourself carries the hazard.

The rest of this section is about a function you call yourself. A
function registered with `RegistryStudy$register_codes()` runs on slots
the pipeline has already reserved.

### Reproducing it

Two steps set a table to an exact free slot count.
[`data.table::setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html)
never shrinks, so it cannot do it alone.

``` r
fresh <- data.table(a = 1:3)
truelength(fresh) - ncol(fresh)
#> [1] 1024
truelength(setalloccol(fresh, 3L)) - ncol(fresh)
#> [1] 1024
```

Serialization drops the over-allocation, which is what a `.qs2` file on
disk does to a table.
[`setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html)
then sets the exact number back. Its `n` is the free slot count on top
of [`ncol()`](https://rdrr.io/r/base/nrow.html), not the total.

``` r
no_headroom <- function(dt, free = 0L) {
  return(setalloccol(unserialize(serialize(dt, NULL)), free))
}
```

Now write an `add_*` with no guard, and give it a skeleton with no free
slots.

``` r
add_flag <- function(skeleton, dataset, id_name, codes = list()) {
  for (nm in names(codes)) skeleton[, (nm) := FALSE]
  return(invisible(NULL))
}

sk_tight <- no_headroom(
  create_skeleton(fake_person_ids[1:3], 2020, "2020-53")
)
truelength(sk_tight) - ncol(sk_tight)
#> [1] 0

add_flag(sk_tight, vax_batch, "id", codes = list(flu_vax = TRUE))
"flu_vax" %in% names(sk_tight)
#> [1] FALSE
```

The call returns normally and the column is missing.

### Reserve the slots in the caller

`.ensure_dt_alloc()` is internal, so your own function cannot call it.
Your tool is
[`data.table::setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html),
and where you call it decides whether it works.

[`setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html)
rebinds in its own calling frame. Called inside your `add_*`, it grows
that function’s local and leaves the caller’s table alone.

``` r
inside_only <- function(skeleton, new_col) {
  setalloccol(skeleton, 1000L)
  skeleton[, (new_col) := TRUE]
  return(invisible(NULL))
}

sk_inside <- no_headroom(
  create_skeleton(fake_person_ids[1:3], 2020, "2020-53")
)
inside_only(sk_inside, "flu_vax")
"flu_vax" %in% names(sk_inside)
#> [1] FALSE
```

Call it in the frame that owns the table, before the add call.

``` r
sk_caller <- no_headroom(
  create_skeleton(fake_person_ids[1:3], 2020, "2020-53")
)
sk_caller <- setalloccol(sk_caller, 1000L)

add_flag(sk_caller, vax_batch, "id", codes = list(flu_vax = TRUE))
"flu_vax" %in% names(sk_caller)
#> [1] TRUE
```

Reserve generously. One free slot costs 16 bytes, measured on R 4.5.2
and data.table 1.18.4 at 3, 10,000 and 200,000 rows alike. A thousand
free slots therefore cost 16 KB per table.

**The return value is the other remedy, and it covers every shipped
function.** All eight exported `add_*` return the skeleton, invisibly.
So do
[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
and the six `skeleton_eligible_*()` functions. If you write
`sk <- add_diagnoses(sk, ...)`, you do not need the reservation. swereg
exports no helper for the reservation itself, because
[`data.table::setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html)
already is one.

Pass a variable, never an expression. The guard writes the grown table
back to a name, or to a `$`, `[[` or `@` chain that ends at a name. An
expression gives it no binding to write to, so it warns instead.

``` r
tight <- function() {
  return(no_headroom(
    create_skeleton(fake_person_ids[1:3], 2020, "2020-53")
  ))
}

add_annual(
  tight(),
  data.table(lopnr = fake_person_ids[1:3], lisa_x = 1:3),
  id_name = "lopnr",
  isoyear = 2020
)
#> Warning: add_annual() cannot write its new columns back to the table it was
#> given. New columns: 1. Free column slots: 0. data.table cannot grow a
#> column-pointer vector in place, so the table you passed does not get the new
#> columns. Use the table this call returns. Or assign that table to a variable,
#> then pass the variable.
```

### Count the temporaries

[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
needs two free slots, not one. It writes a `temp` column, then the new
column, then deletes `temp`. Both columns sit on the table at the same
time, so one free slot is not enough. At one free slot the caller keeps
the orphan `temp` and never gets the new column. The next call then
collides with the leftover `temp`.
[`skeleton_eligible_no_observation_in_window_excluding_wk0()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_eligible_no_observation_in_window_excluding_wk0.md)
holds a `.temp_obs_*` column the same way.

Any scratch column you build carries the same cost. This function needs
two free slots for one output column:

``` r
add_first_year <- function(skeleton, flag_col, new_var) {
  skeleton[get(flag_col) == TRUE, temp := isoyear]
  skeleton[, (new_var) := first_non_na(temp), by = .(id)]
  skeleton[, temp := NULL]
  return(invisible(NULL))
}

build_hits <- function(free) {
  sk <- create_skeleton(fake_person_ids[1:3], 2019, "2020-53")
  sk[, hit := isoyear == 2020]
  return(no_headroom(sk, free))
}

one_slot <- build_hits(1L)
add_first_year(one_slot, "hit", "ri_year_first_hit")
c(
  temp = "temp" %in% names(one_slot),
  new = "ri_year_first_hit" %in% names(one_slot)
)
#>  temp   new 
#>  TRUE FALSE
```

Two free slots reach the caller with the output column and no orphan.

``` r
two_slots <- build_hits(2L)
add_first_year(two_slots, "hit", "ri_year_first_hit")
c(
  temp = "temp" %in% names(two_slots),
  new = "ri_year_first_hit" %in% names(two_slots)
)
#>  temp   new 
#> FALSE  TRUE
unique(two_slots$ri_year_first_hit)
#> [1] 2020
```

### The rebind reaches one frame

A helper that wraps an `add_*` call keeps the grown table in its own
local. The helper’s own caller sees nothing, and nothing warns, because
the rebind succeeded. data.table 1.18.4 behaves this way.

``` r
via_helper <- function(skeleton) {
  swereg::skeleton_eligible_isoyears(skeleton, 2020)
  return("eligible_isoyears" %in% names(skeleton))
}

sk_wrapped <- no_headroom(
  create_skeleton(fake_person_ids[1:3], 2020, "2020-53")
)
via_helper(sk_wrapped)
#> [1] TRUE
"eligible_isoyears" %in% names(sk_wrapped)
#> [1] FALSE
```

A helper like `via_helper()` MUST reserve the slots itself. Its own
caller MAY reserve them instead, before the helper runs.

## How the pipeline enforces the contract

When you pass a function to
`RegistryStudy$register_codes(..., fn = my_add)`, the pipeline reserves
the column slots your codes need. It then wraps the call with a check
before and after. Suppose your function writes into the table it was
given, leaves the structural columns alone, and adds every expected new
column. Then you see nothing. If it breaks the contract,
`$process_skeletons()` (or `$apply_codes_to_skeleton()` directly) stops
with an error that names the registration:

    $register_codes(my_add): did not add the expected columns: covid_vax.
    Check that your loop over `names(codes)` actually writes to the
    skeleton (e.g. `skeleton[..., (nm) := TRUE]`).

You do not call any validation helpers yourself. Write the function and
register it.

## Common case: reusing a built-in via registration

Most “custom” work uses a built-in through the registration path, so
that the pipeline’s hash tracking and incremental rebuild apply. Your
registry can look like Swedish NPR data, with columns for `hdia`,
`dia*`, `ekod*`, etc. after
[`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md).
Then you do not need your own function:

``` r
study$register_codes(
  codes  = list("depression" = c("F32", "F33"), "anxiety" = c("F40", "F41")),
  fn     = swereg::add_diagnoses,
  groups = list("npr" = c("inpatient", "outpatient")),
  label  = "add_diagnoses_mood"
)
```

If the built-in takes extra arguments, pass them through `fn_args`. For
example, `diag_type = "main"` searches only the primary diagnosis:

``` r
study$register_codes(
  codes   = list("primary_mi" = "I21"),
  fn      = swereg::add_diagnoses,
  groups  = list("npr" = c("inpatient")),
  fn_args = list(diag_type = "main"),
  label   = "add_diagnoses_mi_primary"
)
```

`fn_args` holds every argument that `fn` accepts beyond the four
mandatory arguments (`skeleton`, `data`, `id_name`, `codes`). It is part
of the entry’s fingerprint, so a change from `diag_type = "main"` to
`diag_type = "both"` triggers replay.

Write a custom `add_*` only when the built-ins cannot express what you
need (different column names, different matching semantics, a non-ICD
coding system, etc.).

## A complete worked example: `add_vaccinations()`

Suppose you have a regional vaccination registry from any country. Each
row is one dose, with columns `personnummer` (person ID), `vac_date`
(Date), and `atc_vac` (vaccine ATC code). You want one boolean column
per user-supplied ATC prefix, `TRUE` on every week a person received a
matching dose.

### Design decisions

- **Point events, not intervals**: a vaccine dose is a single date.
  Convert `vac_date` to an `isoyearweek` and write `TRUE` on that week.
- **Collision policy**: overwrite. Do what
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  does: set the columns to `FALSE`, then set `TRUE` on matching weeks.
- **Pattern semantics**: ATC prefixes (same as
  `add_rx(source = "atc")`). Use
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html) for speed.

### The function

``` r
add_vaccinations <- function(
  skeleton,
  dataset,
  id_name,
  codes = list()
) {
  stopifnot(
    data.table::is.data.table(dataset),
    id_name %in% names(dataset),
    "vac_date" %in% names(dataset),
    inherits(dataset$vac_date, "Date"),
    "atc_vac" %in% names(dataset),
    is.list(codes),
    length(codes) > 0L,
    !is.null(names(codes))
  )

  # Initialise output columns to FALSE so non-matching weeks aren't NA.
  for (nm in names(codes)) skeleton[, (nm) := FALSE]

  # Work on a local projection so we don't mutate the caller's dataset.
  local_ds <- dataset[, .(
    id  = get(id_name),
    iyw = cstime::date_to_isoyearweek_c(vac_date),
    atc = atc_vac
  )]

  for (nm in names(codes)) {
    patterns <- codes[[nm]]
    hits <- Reduce(`|`, lapply(patterns, function(p) startsWith(local_ds$atc, p)))
    matches <- unique(local_ds[which(hits), .(id, isoyearweek = iyw)])
    skeleton[matches, on = .(id, isoyearweek), (nm) := TRUE]
  }
}
```

### Registering it with a `RegistryStudy`

A minimal `RegistryStudy` in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html), so the vignette
needs no other files:

``` r
study <- RegistryStudy$new(
  data_rawbatch_dir = tempfile("raw"),
  data_skeleton_dir = tempfile("skel"),
  group_names       = c("vax_registry"),
  id_col            = "id"
)

study$register_codes(
  codes  = list("flu_vax" = "J07BB", "covid_vax" = "J07BX"),
  fn     = add_vaccinations,
  groups = list("vax_registry"),
  label  = "add_vaccinations"
)
```

### Applying it

``` r
data("fake_person_ids", package = "swereg")

skeleton <- create_skeleton(fake_person_ids[1:3], 2020, "2020-53")

vax_batch <- data.table::data.table(
  id       = rep(fake_person_ids[1:3], each = 2),
  vac_date = as.Date(c(
    "2020-03-12", "2020-10-05",
    "2020-04-20", "2020-11-02",
    "2020-06-15", "2020-12-14"
  )),
  atc_vac  = c("J07BB02", "J07BX03", "J07BB02", "J07BB02", "J07CA02", "J07BX03")
)

batch_data <- list(vax_registry = vax_batch)

study$apply_codes_to_skeleton(skeleton, batch_data)

skeleton[flu_vax | covid_vax, .(id, isoyearweek, flu_vax, covid_vax)] |> head()
#>       id isoyearweek flu_vax covid_vax
#>    <int>      <char>  <lgcl>    <lgcl>
#> 1:     1     2020-11    TRUE     FALSE
#> 2:     1     2020-41   FALSE      TRUE
#> 3:     2     2020-17    TRUE     FALSE
#> 4:     2     2020-45    TRUE     FALSE
#> 5:     3     2020-51   FALSE      TRUE
```

The user code has no snapshot or validate call. The wrapper runs inside
`$apply_codes_to_skeleton()`, and a contract violation stops this call
with an error.

### The wrapper on a broken function

This broken `add_*` does not create the requested columns:

``` r
broken_add_vax <- function(skeleton, dataset, id_name, codes = list()) {
  # BUG: loops but writes to a typo'd column name
  for (nm in names(codes)) skeleton[, (paste0(nm, "_typo")) := FALSE]
}

study2 <- RegistryStudy$new(
  data_rawbatch_dir = tempfile("raw"),
  data_skeleton_dir = tempfile("skel"),
  group_names       = c("vax_registry"),
  id_col            = "id"
)

study2$register_codes(
  codes  = list("flu_vax" = "J07BB"),
  fn     = broken_add_vax,
  groups = list("vax_registry"),
  label  = "broken_add_vax"
)

sk2 <- create_skeleton(fake_person_ids[1:3], 2020, "2020-53")

tryCatch(
  study2$apply_codes_to_skeleton(sk2, batch_data),
  error = function(e) cat("ERROR caught:\n", conditionMessage(e), "\n")
)
#> ERROR caught:
#>  $register_codes(broken_add_vax) did not add the expected columns: flu_vax. Check that your loop over `names(codes)` actually writes to the skeleton (e.g. `skeleton[..., (nm) := TRUE]`). A function that runs out of free column slots and returns a NEW table loses its columns: write into the skeleton it was given.
```

The error names the registration (`broken_add_vax`) and the missing
column (`flu_vax`), and says what to look for. The wrapper gives an
error in the same way for each of these:

- a changed row count
- a changed row order
- a dropped column
- a returned table that is not the one the wrapper passed in

### Registration removes the hazard

`RegistryStudy$register_codes()` reserves the slots for you, before your
function runs. Your function then writes in place, and the pipeline
keeps what it wrote. A function that needs more slots than the
reservation still works: the pipeline compares column vectors, so it can
tell a growth from a replacement.

Here is a skeleton with no free slot, the shape that lost its column
above, applied through the registry:

``` r
sk_reg <- no_headroom(
  create_skeleton(fake_person_ids[1:3], 2020, "2020-53")
)
truelength(sk_reg) - ncol(sk_reg)
#> [1] 0

study$apply_codes_to_skeleton(sk_reg, batch_data)
c(flu = "flu_vax" %in% names(sk_reg), covid = "covid_vax" %in% names(sk_reg))
#>   flu covid 
#>  TRUE  TRUE
```

Both columns are there, from a table that started with no free slot.

## Failure modes the wrapper catches

Each of these is a real bug in a first custom `add_*`:

- **Building a new table instead of writing into the one you were
  given.** Each of
  [`copy()`](https://rdrr.io/pkg/data.table/man/copy.html),
  [`merge()`](https://rdrr.io/r/base/merge.html),
  [`rbind()`](https://rdrr.io/r/base/cbind.html),
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  and a `.()` projection builds new column vectors. The wrapper records
  the address of every column vector before your function runs and
  compares them after. It refuses a replacement however that replacement
  reaches the pipeline.
- **Writing your columns onto a table you built, then returning it.**
  The wrapper ignores that table, so the skeleton never gets the columns
  and the expected-column check reports them missing.
- **Non-equi joins multiplying rows**. Joining on ATC prefix without
  [`unique()`](https://rdrr.io/r/base/unique.html)-ing the matches can
  insert duplicate `(id, isoyearweek)` pairs. The wrapper catches the
  row-count change.
- **Writing to a structural column.** The wrapper compares `id`,
  `isoyear`, `isoyearweek` and `is_isoyear` by value against the ones it
  took before the call. That catches a row swap that keeps the row
  count, and a `skeleton[i, isoyear := x]` that keeps the type.
- **Looping over `names(codes)` but writing to a typo’d column name**.
  As demonstrated above.
- **Dropping any column**, structural or not. The wrapper checks that
  every column present before the call is present after it.
- **Running out of column slots and returning `NULL`.** The grown table
  is then the only one with your columns, and you threw it away. Return
  the skeleton.

The wrapper does **not** catch one failure mode: columns set to `NA`
instead of `FALSE`. The column exists, so the wrapper accepts it, but
downstream code such as `sum(col)` or `col & other_col` propagates `NA`.
Always set new columns to `FALSE` (or another non-`NA` sentinel that
suits your type).

## What the wrapper guarantees, and what it cannot

It guarantees three things after your function runs:

1.  Every column your `codes` list names is present.
2.  `id`, `isoyear`, `isoyearweek` and `is_isoyear` satisfy
    [`identical()`](https://rdrr.io/r/base/identical.html) against the
    ones your function started with, attributes included.
3.  No column that was there before is dropped, and none is replaced by
    a new vector.

**It cannot see a write INTO an existing column vector.** Your function
receives the skeleton by reference, so
`set(skeleton, i, "an_old_column", value)` changes that column and moves
nothing the wrapper measures. A new table header that shares the
original vectors passes the same way. Write only the columns your
`codes` list names.

An error from the wrapper stops the run and records nothing.
`$process_skeletons()` never writes that batch to disk. Treat the
in-memory skeleton as unusable afterwards. It MAY carry your writes and
it MAY not: the answer depends on whether the entry grew the table.

A table you return is ignored unless it is the skeleton itself. That
means it holds every column the skeleton held before, each at the same
vector address. A table the pipeline grew for you satisfies that. A
table you built does not.

## Design cheat sheet

The built-in `add_*` functions follow these patterns. Copy them when in
doubt.

### Do

- **Use update-by-reference joins.**
  `skeleton[data, on = c("id==<id_name>", "isoyearweek"), (cols) := ...]`
  is the standard pattern. Never
  [`merge()`](https://rdrr.io/r/base/merge.html), never `left_join()`.
- **Initialise new columns to `FALSE`** (or another non-`NA` sentinel)
  before the join.
- **Respect `is_isoyear`.** The skeleton has both weekly and annual
  rows. Decide explicitly whether each new column belongs on weekly
  rows, annual rows, or both.
- **Use [`startsWith()`](https://rdrr.io/r/base/startsWith.html) over
  regex for prefix matching.** It is about 5x faster.
- **Take `id_name` as a parameter.** Do not hard-code `"lopnr"`.
- **Warn on partial ID matches.** If most skeleton IDs are not in the
  registry data, the user probably made a mistake.
- **Count every column your function holds at once**, temporaries
  included. See the section “Column-slot headroom”.

### Don’t

- **Do not reassign `skeleton`.** Any line that looks like
  `skeleton <- something` inside an `add_*` is a bug.
- **Do not assume the user ran
  [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md).**
  Check. Stop with an error that says which required columns are
  missing.
- **Do not silently drop rows with `NA` in critical columns.** Warn with
  a count. (See
  [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  for the right pattern.)

### Judgment call: input-data mutation

Changes to the input `data.table` (scratch columns, date format
coercion) are **acceptable** in the `RegistryStudy` batched pipeline,
because the pipeline discards the input after each call. They cause
**problems** in manual or interactive workflows where the caller reuses
the dataset variable. Some built-ins change their input and some do not.
If the cost of a copy is acceptable, copy the input. If the input is
huge and you write for the pipeline, do not copy it.

## Summary

1.  Check whether a built-in already does what you need, and wrap *that*
    in `register_codes(codes = ..., fn = swereg::add_diagnoses, ...)`
    instead of writing a custom function.
2.  If you need custom code, write it to the 6-point contract in the
    section “The `add_*` contract”.
3.  Register it via
    `RegistryStudy$register_codes(codes = ..., fn = my_add, groups = ...)`.
    The pipeline validates the contract on every call and replays your
    function when the registration changes.
4.  Read the source of any built-in `add_*` for a reference
    implementation.

See
[`vignette("builtin-add-functions")`](https://papadopoulos-lab.github.io/swereg/articles/builtin-add-functions.md)
for every shipped `add_*`, and
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)
for the full four-phase pipeline.
