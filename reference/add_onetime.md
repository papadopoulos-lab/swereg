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

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_annual`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md)
for annual data,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

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
#>         id isoyear isoyearweek is_isoyear isoyearweeksun personyears fodelseman
#>      <int>   <int>      <char>     <lgcl>         <Date>       <num>     <char>
#>   1:     1    1900     1900-**       TRUE     1900-07-01  1.00000000       2004
#>   2:     1    1901     1901-**       TRUE     1901-06-30  1.00000000       2004
#>   3:     1    1902     1902-**       TRUE     1902-06-29  1.00000000       2004
#>   4:     1    1903     1903-**       TRUE     1903-06-28  1.00000000       2004
#>   5:     1    1904     1904-**       TRUE     1904-07-03  1.00000000       2004
#>  ---                                                                           
#> 866:     5    2020     2020-49      FALSE     2020-12-06  0.01913876       1990
#> 867:     5    2020     2020-50      FALSE     2020-12-13  0.01913876       1990
#> 868:     5    2020     2020-51      FALSE     2020-12-20  0.01913876       1990
#> 869:     5    2020     2020-52      FALSE     2020-12-27  0.01913876       1990
#> 870:     5    2020     2020-53      FALSE     2021-01-03  0.01913876       1990
#>      doddatum
#>        <char>
#>   1:     2008
#>   2:     2008
#>   3:     2008
#>   4:     2008
#>   5:     2008
#>  ---         
#> 866:         
#> 867:         
#> 868:         
#> 869:         
#> 870:         

# Check added variables
names(skeleton)
#> [1] "id"             "isoyear"        "isoyearweek"    "is_isoyear"    
#> [5] "isoyearweeksun" "personyears"    "fodelseman"     "doddatum"      
```
