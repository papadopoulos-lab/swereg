# Add one-time data to skeleton

Merges one-time data (non-longitudinal) into the main skeleton data
structure. This function is used for adding data that doesn't change
over time, such as demographic information or baseline characteristics.

## Usage

``` r
add_onetime(skeleton, data, id_name)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure with id and time
  variables

- data:

  A data.table containing the one-time data to be merged

- id_name:

  Character string specifying the name of the ID variable in the data

## Value

The skeleton data.table is modified by reference with one-time data
merged in. Columns from data that already exist in skeleton will be
prefixed with "i."

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
[`add_annual`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md)
for annual data,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cancer_without_morphology()`](https://papadopoulos-lab.github.io/swereg/reference/add_cancer_without_morphology.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)

## Examples

``` r
# Load fake data
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
swereg::make_lowercase_names(fake_demographics)
#> Found potential date columns: fodelseman. Consider adding them to date_columns parameter for automatic date parsing.

# Create skeleton
skeleton <- create_skeleton(fake_person_ids[1:5], "2020-01-01", "2020-12-31")

# Add demographic data
add_onetime(skeleton, fake_demographics, "lopnr")

# Check added variables
names(skeleton)
#> [1] "id"             "isoyear"        "isoyearweek"    "is_isoyear"    
#> [5] "isoyearweeksun" "personyears"    "fodelseman"     "doddatum"      
```
