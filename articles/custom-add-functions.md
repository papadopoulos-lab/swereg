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

- **Non-Swedish registries** — Norwegian NPR, Danish LPR, FinDep, etc.
  They follow similar longitudinal patterns but use different column
  names and coding systems.
- **Swedish registries swereg doesn’t ship hard-coded support for** —
  SWEDEHEART, RIKSHIP, regional cohorts, research registries.
- **In-house data** — payer claims, EHR extracts, institutional biobanks
  with time-stamped events.

The mechanism is open-ended by design. You cannot write code for every
registry anyone might want to ingest; the right answer is a clean
contract that user-written functions can plug into.

## Two ways to use a custom `add_*`

1.  **Register it via `RegistryStudy$register_codes()`.** The pipeline
    auto-validates the contract on every call, fingerprints your
    function, and replays it on change for incremental rebuilds. This is
    the recommended path for any production-style use.
2.  **Call it directly as a free function** in the manual workflow. No
    automatic validation — you catch mistakes through your own tests.
    Fine for one-off exploration, risky at scale.

The rest of this vignette focuses on path (1), because swereg enforces
the contract for you there. The function body is the same either way.

## The `add_*` contract

A well-behaved `add_*` function:

1.  **Takes `skeleton` as its first argument** and mutates it in place
    using `skeleton[data, on = ..., := ...]`. No
    [`merge()`](https://rdrr.io/r/base/merge.html), no `left_join()`, no
    `skeleton <- ...` reassignment.
2.  **Preserves row count.** If `nrow(skeleton)` changes, something went
    wrong.
3.  **Preserves the four structural columns** (`id`, `isoyear`,
    `isoyearweek`, `is_isoyear`). Read-only.
4.  **Adds one column per entry in the `codes` list**, named exactly as
    the list is named. If the caller passes
    `codes = list(flu_vax = ..., covid_vax = ...)`, the skeleton gains
    columns `flu_vax` and `covid_vax`.
5.  **Takes `id_name` as an explicit argument.** Different registries
    use different ID column names; don’t hard-code.
6.  **Accepts `codes` as a named list** and receives any extra
    configuration through positional / named arguments the caller passes
    via `fn_args` (see below).

## How the pipeline enforces the contract

When you pass a function to
`RegistryStudy$register_codes(..., fn = my_add)`, the pipeline wraps
every call with a pre/post check. If your function preserves row count,
keeps the structural columns, and adds every expected new column, you
see nothing. If it misbehaves, `$process_skeletons()` (or
`$apply_codes_to_skeleton()` directly) errors with a pointer back to the
registration:

    $register_codes(my_add): did not add the expected columns: covid_vax.
    Check that your loop over `names(codes)` actually writes to the
    skeleton (e.g. `skeleton[..., (nm) := TRUE]`).

You don’t call any validation helpers yourself. Just write the function
and register it.

## Common case: reusing a built-in via registration

Most “custom” work isn’t actually custom code — it’s using a built-in
through the registration path so the pipeline’s hash tracking and
incremental rebuild apply. If the registry you’re ingesting looks like
Swedish NPR data (columns for `hdia`, `dia*`, `ekod*`, etc. after
[`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)),
you don’t need your own function at all:

``` r
study$register_codes(
  codes  = list("depression" = c("F32", "F33"), "anxiety" = c("F40", "F41")),
  fn     = swereg::add_diagnoses,
  groups = list("npr" = c("inpatient", "outpatient")),
  label  = "add_diagnoses_mood"
)
```

If the built-in takes extra knobs — e.g. `diag_type = "main"` to search
only the primary diagnosis — pass them through `fn_args`:

``` r
study$register_codes(
  codes   = list("primary_mi" = "I21"),
  fn      = swereg::add_diagnoses,
  groups  = list("npr" = c("inpatient")),
  fn_args = list(diag_type = "main"),
  label   = "add_diagnoses_mi_primary"
)
```

`fn_args` is the bucket for anything the fn accepts beyond the four
mandatory arguments (`skeleton`, `data`, `id_name`, `codes`). It
participates in the entry’s fingerprint, so changing
`diag_type = "main"` to `diag_type = "both"` triggers replay.

Reach for a custom `add_*` only when the built-ins genuinely can’t
express what you need (different column names, different matching
semantics, a non-ICD coding system, etc.).

## A complete worked example: `add_vaccinations()`

Suppose you have a regional vaccination registry (Swedish, Norwegian, or
other — doesn’t matter). Each row is one dose, with columns
`personnummer` (person ID), `vac_date` (Date), and `atc_vac` (vaccine
ATC code). You want one boolean column per user-supplied ATC prefix,
`TRUE` on every week a person received a matching dose.

### Design decisions

- **Point events, not intervals**: a vaccine dose is a single date. Snap
  `vac_date` to an `isoyearweek` and write `TRUE` on that week.
- **Collision policy**: overwrite. Match
  [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  — init columns to `FALSE`, then set `TRUE` on matching weeks.
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

A minimal `RegistryStudy` scoped to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) so the vignette is
self-contained:

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

skeleton <- create_skeleton(fake_person_ids[1:3], "2020-01-01", "2020-12-31")

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

Note there is no explicit snapshot/validate call in user code. The
wrapper runs automatically inside `$apply_codes_to_skeleton()` — any
contract violation would have errored here.

### Seeing the wrapper fire on a broken function

Let’s deliberately write a broken `add_*` that forgets to create the
requested columns and see what happens:

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

sk2 <- create_skeleton(fake_person_ids[1:3], "2020-01-01", "2020-12-31")

tryCatch(
  study2$apply_codes_to_skeleton(sk2, batch_data),
  error = function(e) cat("ERROR caught:\n", conditionMessage(e), "\n")
)
#> ERROR caught:
#>  $register_codes(broken_add_vax) did not add the expected columns: flu_vax. Check that your loop over `names(codes)` actually writes to the skeleton (e.g. `skeleton[..., (nm) := TRUE]`).
```

The wrapper named the registration (`broken_add_vax`) and the missing
column (`flu_vax`) and told us what to look for. Similar errors fire for
row-count changes, dropped structural columns, and skeleton
reassignment.

## Failure modes the wrapper catches

All of these come from real bugs people hit writing their first custom
`add_*`:

- **Accidentally reassigning `skeleton`** (e.g.
  `skeleton <- merge(skeleton, matches, ...)`). Reassignment changes
  `skeleton` in your local env but the caller’s binding still points at
  the original. The wrapper notices the expected columns are missing on
  the caller’s object.
- **Non-equi joins multiplying rows**. Joining on ATC prefix without
  [`unique()`](https://rdrr.io/r/base/unique.html)-ing the matches can
  insert duplicate `(id, isoyearweek)` pairs. The wrapper catches the
  row-count change.
- **Looping over `names(codes)` but writing to a typo’d column name**.
  As demonstrated above.
- **Dropping a structural column**. Any line that `NULL`s out `id`,
  `isoyear`, `isoyearweek`, or `is_isoyear` fails the post-check.

One failure mode the wrapper does **not** catch: initialising to `NA`
instead of `FALSE`. The column exists, so the wrapper is happy, but
downstream code that does `sum(col)` or `col & other_col` will propagate
`NA`. Always initialise to `FALSE` (or whatever non-`NA` sentinel makes
sense for your type).

## Design cheat sheet

Patterns the built-in `add_*` functions have converged on (sometimes
after making the mistake first). Copy them when in doubt.

### Do

- **Use update-by-reference joins.**
  `skeleton[data, on = c("id==<id_name>", "isoyearweek"), (cols) := ...]`
  is the canonical pattern. Never
  [`merge()`](https://rdrr.io/r/base/merge.html), never `left_join()`.
- **Initialise new columns to `FALSE`** (or another non-`NA` sentinel)
  before the join.
- **Respect `is_isoyear`.** The skeleton has both weekly and annual
  rows. Decide explicitly whether each new column belongs on weekly
  rows, annual rows, or both.
- **Use [`startsWith()`](https://rdrr.io/r/base/startsWith.html) over
  regex for prefix matching.** ~5x faster.
- **Take `id_name` as a parameter.** Don’t hard-code `"lopnr"`.
- **Warn on partial ID matches.** If most skeleton IDs aren’t in the
  registry data, the user probably made a mistake.

### Don’t

- **Don’t reassign `skeleton`.** Any line that looks like
  `skeleton <- something` inside an `add_*` is a bug.
- **Don’t assume the user ran
  [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md).**
  Check. Fail with an actionable error if required columns are missing.
- **Don’t silently drop rows with `NA` in critical columns.** Warn with
  a count. (See
  [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  for the right pattern.)

### Judgment call: input-data mutation

Mutating the input `data.table` (adding scratch columns, coercing date
formats) is **fine** in the `RegistryStudy` batched pipeline — the input
is discarded after each call. It’s **messy** in manual or interactive
workflows where the caller reuses the dataset variable. Several
built-ins mutate; several don’t. If copy-cost is tolerable, prefer the
cleaner approach. If the input is huge and you’re writing for the
pipeline, don’t bother.

## Summary

1.  Check whether a built-in already does what you need, and wrap *that*
    in `register_codes(codes = ..., fn = swereg::add_diagnoses, ...)`
    instead of writing a custom function.
2.  If you genuinely need custom code, write it following the 6-point
    contract above.
3.  Register it via
    `RegistryStudy$register_codes(codes = ..., fn = my_add, groups = ...)`.
    The pipeline validates the contract on every call and replays your
    function when the registration changes.
4.  Read the source of any built-in `add_*` for a battle-tested
    reference implementation.

See
[`vignette("builtin-add-functions")`](https://papadopoulos-lab.github.io/swereg/articles/builtin-add-functions.md)
for every shipped `add_*`, and
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)
for the full three-phase pipeline.
