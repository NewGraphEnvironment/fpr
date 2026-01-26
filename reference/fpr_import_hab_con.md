# Import provincial fish data submission template

Includes habitat data and fish sampling information. Can back up all
sheets to csvs. Suggest updateing git index for submission template to
to assume-unchanged so that a version of the spreadsheet is stored in
the repo but can be updated when it is actually necessary.

## Usage

``` r
fpr_import_hab_con(
  path = "data/habitat_confirmations.xls",
  backup = TRUE,
  path_backup = "data/backup/",
  ...
)
```

## Arguments

- path:

  String location of submission template file populated from
  https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/fish-and-fish-habitat-data-information/fish-data-submission/submit-fish-data#submitfish

- backup:

  Logical whether to backup all sheets as csvs or not. Defaults to true

- path_backup:

  String indicating directory to create and backup to. Defaults to
  'data/backup/'

- ...:

  Not used. For passing arguments from
  [fpr_sheet_trim](http://www.newgraphenvironment.com/fpr/reference/fpr_sheet_trim.md).
  For fpr_import_hab_con we pass the switch row_empty_remove = T because
  we have not yet assigned column types. We can also pass "true" or
  "false" to col_filter_na and col_filter_na_num to change which column
  we use to drop off rows

## See also

[fpr_sheet_trim](http://www.newgraphenvironment.com/fpr/reference/fpr_sheet_trim.md)
