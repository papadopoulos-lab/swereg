# Add annual data to skeleton

Merges annual data into the main skeleton data structure for a specific
ISO year. This function is used for adding data that is measured or
recorded annually, such as yearly income, employment status, or annual
health assessments.

## Usage

``` r
add_annual(skeleton, data, id_name, isoyear)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure with id and time
  variables

- data:

  A data.table containing the annual data to be merged

- id_name:

  Character string specifying the name of the ID variable in the data

- isoyear:

  Integer specifying the ISO year for which the data applies

## Value

The skeleton data.table is modified by reference with annual data merged
in. Columns from data that already exist in skeleton will be prefixed
with "i."

The function also returns the skeleton, invisibly. The return is the
object the caller passed, while that object has a free column slot for
every new column. Past that point data.table cannot grow the column list
in place, so the new columns land on the returned table alone. A caller
that passes an expression rather than a variable MUST use the return
value.

Any other name that pointed at the same table before the call is stale
afterwards. R cannot grow a list in place. A growth therefore leaves the
caller's binding on a NEW object, and every other name on the old one.
Take an alias after the call, never before it.

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_onetime`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
for one-time data,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_cancer_without_morphology()`](https://papadopoulos-lab.github.io/swereg/reference/add_cancer_without_morphology.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)

## Examples

``` r
# Load fake data
data("fake_person_ids", package = "swereg")
data("fake_annual_family", package = "swereg")
swereg::make_lowercase_names(fake_annual_family)

# Create skeleton
skeleton <- create_skeleton(fake_person_ids[1:5], "2020-01-01", "2022-12-31")

# Add annual family data for 2021
add_annual(skeleton, fake_annual_family, "lopnr", 2021)

# Check data was added only for 2021
skeleton[isoyear == 2021 & is_isoyear == TRUE, .(id, isoyear, famtyp)]
#> Empty data.table (0 rows and 3 cols): id,isoyear,famtyp
```
