# Transform rowdep variable to rowind variable using first occurrence

Creates a row-independent (rowind) variable by finding the first
occurrence where a condition is TRUE and extracting the corresponding
value. This is a common pattern in longitudinal registry data analysis
for creating stable person-level characteristics.

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

  Character string naming the new rowind variable to create

## Value

The data.table is modified by reference (invisibly returned)

## Details

This function implements the common pattern of transforming time-varying
(rowdep) variables into time-invariant (rowind) variables by capturing
the value at the first occurrence of a condition.

The transformation follows these steps: 1. Create temporary variable
where condition is TRUE 2. Use first_non_na() to find the first
occurrence for each person 3. Clean up temporary variables automatically

This is equivalent to the manual pattern:
`dt[condition, temp := value_var]`
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
              diags = list("example_diag" = "^F64"))

# Transform: Age at first example diagnosis
make_rowind_first_occurrence(skeleton,
                             condition = "diag_example_diag == TRUE",
                             value_var = "age",
                             new_var = "rowind_age_first_example_diag")
} # }
```
