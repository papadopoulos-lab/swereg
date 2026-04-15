# Transform a row-dependent variable to a row-independent variable using first occurrence

Creates a row-independent (\`ri\_\`) variable by finding the first
occurrence where a condition is TRUE and extracting the corresponding
value. This is a common pattern in longitudinal registry data analysis
for creating stable person-level characteristics from time-varying
skeleton columns.

## Usage

``` r
make_rowind_first_occurrence(dt, condition, value_var, new_var)
```

## Arguments

- dt:

  A data.table with longitudinal data

- condition:

  Character string representing the logical condition to evaluate

- value_var:

  Character string naming the column to extract values from

- new_var:

  Character string naming the new \`ri\_\*\` variable to create

## Value

The data.table is modified by reference (invisibly returned)

## Details

swereg distinguishes two variable shapes in longitudinal skeleton data:

- \*\*row-dependent\*\* (prefix \`rd\_\`):

  Values that can change over time for a person. Examples:
  \`rd_age_continuous\`, \`rd_education\`,
  \`rd_income_inflation_adjusted\`.

- \*\*row-independent\*\* (prefix \`ri\_\`):

  Values that are fixed person-level. Examples: \`ri_birthcountry\`,
  \`ri_age_first_diagnosis\`, \`ri_isoyear_first_diagnosis\`,
  \`ri_register_tag\`.

This function automates the common \`rd\_\` -\> \`ri\_\` transformation
of capturing "the value at the first time something became true". The
transformation follows these steps: 1. Create a temporary column where
\`condition\` is TRUE 2. Use \`first_non_na()\` to find the first
occurrence for each person 3. Clean up the temporary column
automatically

Equivalent to the manual pattern: `dt[condition, temp := value_var]`
`dt[, new_var := first_non_na(temp), by = .(id)]` `dt[, temp := NULL]`

## See also

[`first_non_na`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md)
for the aggregation function used internally

## Examples

``` r
if (FALSE) { # \dontrun{
# Create example skeleton with diagnosis data
skeleton <- create_skeleton(c(1,2,3), "2020-01-01", "2020-12-31")

# Add some example diagnosis data
add_diagnoses(skeleton, diagnosis_data, "lopnr",
              codes = list("example_diag" = "^F64"))

# Transform: age at first example diagnosis
make_rowind_first_occurrence(skeleton,
                             condition = "example_diag == TRUE",
                             value_var = "age",
                             new_var = "ri_age_first_example_diag")
} # }
```
