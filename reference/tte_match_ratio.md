# Match unexposed to exposed at a specified ratio

Samples unexposed individuals to achieve a target ratio relative to
exposed individuals. This is commonly used in target trial emulation to
increase statistical efficiency while maintaining reasonable sample
sizes.

## Usage

``` r
tte_match_ratio(
  data,
  exposure_var,
  eligible_var = NULL,
  ratio = 2,
  id_var = NULL,
  seed = NULL,
  mark_unsampled = "na"
)
```

## Arguments

- data:

  A data.table containing the study population.

- exposure_var:

  Character, name of the binary exposure column.

- eligible_var:

  Character, name of the eligibility indicator column (default: NULL,
  meaning all rows are eligible).

- ratio:

  Numeric, target ratio of unexposed to exposed (default: 2, meaning 2
  unexposed for every 1 exposed).

- id_var:

  Character, name of the ID column for reporting (default: NULL).

- seed:

  Integer, random seed for reproducibility (default: NULL).

- mark_unsampled:

  Character, how to mark unsampled unexposed individuals: "na" sets
  exposure to NA, "drop" removes rows, "flag" adds a column (default:
  "na").

## Value

The input data.table, modified:

- If \`mark_unsampled = "na"\`: unsampled unexposed have exposure set to
  NA

- If \`mark_unsampled = "drop"\`: unsampled unexposed rows are removed

- If \`mark_unsampled = "flag"\`: adds \`sampled\` column (TRUE/FALSE)

## Details

In observational studies, there are often many more unexposed than
exposed individuals. Including all unexposed can be computationally
expensive and adds little statistical power beyond a certain ratio.
Common choices are 1:1, 2:1, or 4:1 matching.

The function:

1.  Counts exposed individuals among eligible rows

2.  Randomly samples \`ratio \* n_exposed\` from eligible unexposed

3.  Marks or removes unsampled unexposed individuals

If there are fewer unexposed than \`ratio \* n_exposed\`, all unexposed
are kept.

## Examples

``` r
library(data.table)
set.seed(42)
dt <- data.table(
  id = 1:1000,
  eligible = TRUE,
  exposed = c(rep(TRUE, 100), rep(FALSE, 900))
)
# 2:1 matching: keep 200 unexposed for 100 exposed
result <- tte_match_ratio(dt, "exposed", "eligible", ratio = 2, seed = 123)
table(result$exposed, useNA = "always")
#> 
#> FALSE  TRUE  <NA> 
#>   200   100   700 
```
