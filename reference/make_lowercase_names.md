# Convert column names to lowercase and optionally clean date columns

Generic function to convert all column names in an object to lowercase.
Optionally parses and cleans specified date columns using Swedish
registry date format handling. Methods are provided for data.table and
default objects.

## Usage

``` r
make_lowercase_names(x, date_columns = NULL, ...)

# Default S3 method
make_lowercase_names(x, date_columns = NULL, ...)

# S3 method for class 'data.table'
make_lowercase_names(x, date_columns = NULL, ...)
```

## Arguments

- x:

  An object with named columns (data.frame, data.table, etc.)

- date_columns:

  Character vector specifying the names of date columns to clean. Should
  use lowercase names since column names are converted to lowercase
  first. If uppercase names are provided, a warning is issued and
  lowercase versions are used. If provided, these columns will be parsed
  using Swedish date format handling and converted to Date class in
  place (keeping original column names). If NULL, the function will
  suggest commonly found Swedish registry date columns.

- ...:

  Additional arguments for date parsing (default_month_day, default_day,
  na_strings)

## Value

The object with all column names converted to lowercase, and optionally
cleaned date columns if date_columns was specified (converted to Date
class in place)

## Details

The function automatically detects common Swedish registry date columns
(indatum, utdatum, edatum, dodsdat, fodelseman) and provides helpful
messages suggesting their inclusion in the date_columns parameter.

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_onetime`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
for merging data,
[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for diagnosis data,
[`parse_swedish_date`](https://papadopoulos-lab.github.io/swereg/reference/parse_swedish_date.md)
for date parsing details

## Examples

``` r
# Load fake data
data("fake_demographics", package = "swereg")

# Basic usage - convert column names to lowercase
# This will show a message suggesting to include 'fodelseman' in date_columns
swereg::make_lowercase_names(fake_demographics)
#> Found potential date columns: fodelseman. Consider adding them to date_columns parameter for automatic date parsing.

# With date cleaning - clean birth dates (use lowercase column names)
swereg::make_lowercase_names(fake_demographics, date_columns = "fodelseman")

# Check that fodelseman column was converted to Date class
head(fake_demographics$fodelseman)
#> [1] "2004-07-01" "1955-07-01" "1954-07-01" "1974-07-01" "1990-07-01"
#> [6] "1984-07-01"

# For diagnosis data with multiple date columns (use lowercase column names)
data("fake_inpatient_diagnoses", package = "swereg")
swereg::make_lowercase_names(fake_inpatient_diagnoses, date_columns = c("indatum", "utdatum"))

# The function suggests missing date columns
swereg::make_lowercase_names(fake_inpatient_diagnoses, date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.
# Message: "Found additional date columns not in date_columns: utdatum"
```
