# Function to process site ID value from pscis_crossing_id and my_crossing_reference

Intended for processing field data from digital form in which only one
value should be provided. It checks to confirm the dataframe has one and
only one of either a \`pscis_crossing_id\` or a
\`my_crossing_reference\`. It also uses \[fpr_chk_dupes()\] to issue a
warning if there are duplicate values of \`pscis_crossing_id\`,
\`my_crossing_reference\` or \`site_id\`

## Usage

``` r
fpr_tidy_assign_site_id(dat = NULL)
```

## Arguments

- dat:

  A dataframe to process. Default is NULL.

## Value

A processed dataframe.

## Examples
