# Trim up an excel worksheet - usually during import

Select column names based on first row with inputs all across. Trim up
the dataframe based on whether there are events in the first column.
This is a \`date\` in the pscis inputs and \`reference_number\` in the
fish submission template

## Usage

``` r
fpr_sheet_trim(
  dat,
  col_filter_na = FALSE,
  col_filter_na_num = 2,
  row_empty_remove = F
)
```

## Arguments

- dat:

  Dataframe

- col_filter_na:

  Logical whether to remove rows if \`NA\` is present in a specified
  column. Defaults to false.

- col_filter_na_num:

  Integer specifying column to filter if \`NA\`s are present. Defaults
  to 2 because we use that in the fish submission form since
  \`gazetted_name\` is required and because first column of location
  sheet (\`step_1_ref_and_loc_info') is populated to row 1500 out of
  box. Note that this !is.na for col 2 results in some trimming of a
  couple events on the admin_only_pick_lists.csv

- row_empty_remove:

  Logical on whether to remove empty dataframe rows until the first
  complete one is found in order to select as rownames. Needs to be
  turned to TRUE for \`fpr_import_hab_con\` to work.

## Value

A dataframe
