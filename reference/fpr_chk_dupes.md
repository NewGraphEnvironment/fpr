# Check for Duplicate Values in a Column

This function checks for duplicate values in a specified column of a
dataset and provides a warning if duplicates are found.

## Usage

``` r
fpr_chk_dupes(dat = NULL, col_screen = NULL)
```

## Arguments

- dat:

  \[data.frame\] A data frame to be checked for duplicates.

- col_screen:

  A column in the data frame to check for duplicates. Must be specified
  as a tidy evaluation argument.

## Value

The function does not return a value but provides a warning if
duplicates are found.

## Details

The function identifies duplicate values in the column and provides a
warning with a list of the duplicate values if any are found.
