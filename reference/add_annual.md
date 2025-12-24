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

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_onetime`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
for one-time data,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

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
#>          id isoyear isoyearweek is_isoyear isoyearweeksun personyears famtyp
#>       <int>   <int>      <char>     <lgcl>         <Date>       <num> <char>
#>    1:     1    1900     1900-**       TRUE     1900-07-01  1.00000000   <NA>
#>    2:     1    1901     1901-**       TRUE     1901-06-30  1.00000000   <NA>
#>    3:     1    1902     1902-**       TRUE     1902-06-29  1.00000000   <NA>
#>    4:     1    1903     1903-**       TRUE     1903-06-28  1.00000000   <NA>
#>    5:     1    1904     1904-**       TRUE     1904-07-03  1.00000000   <NA>
#>   ---                                                                       
#> 1386:     5    2022     2022-48      FALSE     2022-12-04  0.01913876   <NA>
#> 1387:     5    2022     2022-49      FALSE     2022-12-11  0.01913876   <NA>
#> 1388:     5    2022     2022-50      FALSE     2022-12-18  0.01913876   <NA>
#> 1389:     5    2022     2022-51      FALSE     2022-12-25  0.01913876   <NA>
#> 1390:     5    2022     2022-52      FALSE     2023-01-01  0.01913876   <NA>

# Check data was added only for 2021
skeleton[isoyear == 2021 & is_isoyear == TRUE, .(id, isoyear, famtyp)]
#> Empty data.table (0 rows and 3 cols): id,isoyear,famtyp
```
