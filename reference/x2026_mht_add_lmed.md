# Add 2023 MHT-specific prescription data to skeleton

Processes Swedish prescription registry data (LMED) specifically for the
2023 Menopausal Hormone Therapy (MHT) study. Categorizes medications
into predefined groups, applies duration fixes, and creates treatment
approach variables.

## Usage

``` r
x2026_mht_add_lmed(skeleton, lmed, verbose = TRUE)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure

- lmed:

  A data.table containing prescription registry data with columns: lopnr
  (ID), produkt (product name), edatum (dispensing date), fddd (duration
  in days), atc (ATC code)

- verbose:

  Logical. If TRUE (default), print progress messages to stderr.

## Value

The skeleton data.table is modified by reference with MHT treatment
variables and approach categorizations added. Returns the modified
skeleton invisibly.

## Details

This function performs several steps:

- Restricts LMED data to individuals in the skeleton

- Categorizes products into MHT groups (A1-I2) based on product names

- Applies duration fixes for specific products (IUDs, minimum doses)

- Creates treatment approach variables based on predefined logic

- Handles treatment gaps and overlapping prescriptions

## Note

This function is specific to the 2023 MHT study and uses study-specific
categorizations and approaches defined in the package data dictionary.
