# Check a YAML study specification and list every problem

Reads one specification file and returns a table with one row per
problem. It never stops on a bad specification, so a caller can check
many files in one pass.

## Usage

``` r
tteplan_check_spec(spec_path)
```

## Arguments

- spec_path:

  Character scalar, the path of the YAML specification file.

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with one row per problem, and three character columns:

- `path`:

  The normalised key path of the problem, for example
  `$/study/implementation/version`. A problem with the whole file has
  the path `$`.

- `kind`:

  The problem kind. The Details section lists the twelve kinds.

- `message`:

  What is wrong, and the repair where swereg knows it.

The table has zero rows when the checker finds no problem.

## Details

The checker reports twelve kinds of problem:

- `unreadable`:

  The file is missing, is not valid UTF-8, or is not valid YAML. This
  row is the only row, because the checker has no parsed specification
  to check.

- `undeclared_key`:

  The schema does not declare the key path. The message lists the keys
  that the context accepts.

- `retired_key`:

  The schema refuses the key path. The message names the replacement.

- `invalid_enrollment_id`:

  An enrollment has no `id`, or its `id` is not one non-empty character
  string or one number. The checker refuses a sequence, a mapping, and
  an empty or whitespace-only string. It writes one row for each such
  enrollment.

- `duplicate_enrollment_id`:

  Two or more enrollments carry the same `id`. The checker writes one
  row for each repeated id. It compares only the ids that give no
  `invalid_enrollment_id` row.

- `version_mismatch`:

  The file name has the form `spec_vNNN.yaml`, and
  `study$implementation$version` is missing or is not `vNNN`. The
  checker skips this check for every other file name.

- `outcome_selecting_exclusion`:

  An exclusion criterion has the window
  `lifetime_before_and_after_baseline` on a variable of an outcome. That
  window reads the weeks after time zero, so the criterion selects on
  the follow-up outcome. The checker writes one row for each criterion
  and outcome. The checker reads the global `exclusion_criteria` and
  each enrollment's `additional_exclusion`.

- `enrollment_family`:

  An enrollment's `family` is not one non-empty character string. Or
  some enrollments set `family` and others do not. When one enrollment
  sets `family`, every enrollment MUST set it. The checker writes one
  row for each bad value, and one row that names the enrollments without
  a `family`.

- `enrollment_rule_exempt`:

  An enrollment sets `minimum_events_rule_exempt` to a value that is not
  one logical, `true` or `false`. The checker writes one row for each
  such enrollment, and the message names the enrollment id.

- `feasibility_follow_up_weeks`:

  The top-level key `feasibility_follow_up_weeks` is not one positive
  whole number of weeks. The checker writes one row, and the message
  names the key.

- `follow_up_only_enrollments`:

  A `follow_up` entry sets `only_enrollments` to a bad value. The value
  MUST be a non-empty list of enrollment ids. A single id is a list of
  one. The list MUST NOT repeat an id, and each string MUST be the `id`
  of an enrollment. The checker writes one row for each bad entry. The
  message names the follow-up label and the bad value.

- `read_error`:

  [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
  stops after its key gate. The message is the message of that error.

The checker reports every undeclared key path and every retired key
path.
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
reports them too, but stops after them. The checker calls
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
only when no key path is undeclared or retired, because the key gate
stops it first.
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
stops at its first problem after the key gate, so a table holds at most
one `read_error` row.

A warning from
[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md),
for example on an open question, reaches the caller unchanged.

## See also

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md),
which stops at the first problem, and
[`vignette("tte-spec-schema", package = "swereg")`](https://papadopoulos-lab.github.io/swereg/articles/tte-spec-schema.md)
for the key paths the schema declares.

## Examples

``` r
spec_dir <- tempfile()
dir.create(spec_dir)
spec_path <- file.path(spec_dir, "spec_v002.yaml")
writeLines(
  c(
    "study:",
    "  colour: blue",
    "  implementation:",
    "    project_prefix: example",
    "    version: v001"
  ),
  spec_path
)
tteplan_check_spec(spec_path)
#>                              path             kind
#>                            <char>           <char>
#> 1:                 $/study/colour   undeclared_key
#> 2: $/study/implementation/version version_mismatch
#>                                                                                                       message
#>                                                                                                        <char>
#> 1: Unknown key 'colour'. $/study accepts: description, design, implementation, principal_investigator, title.
#> 2:                     study$implementation$version is 'v001', and the file name spec_v002.yaml gives 'v002'.
unlink(spec_dir, recursive = TRUE)
```
