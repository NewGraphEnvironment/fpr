# Import the pscis template

Import the pscis template. Assigns the column types based on the
fpr_xref_pscis data object. Extracts a time_start and a date_time_start
of the survey from the comments if it is sritten in 24hr format after
the last period of the comments column. Assigns the camera tot hte first
person listed in the crew memebers column. Creates a site_id as either
the PSCIS id or the my crossing reference This is a helper for
fpr_import_pscis_all.

## Usage

``` r
fpr_import_pscis(workbook_name = "pscis_phase1.xlsm", dir_root = "data", ...)
```

## Arguments

- workbook_name:

  string value for name of worksheet

- dir_root:

  String indicating name of directory to look for pscis submission file

- ...:

  Unused - Pass through another param to
  [fpr_sheet_trim](http://www.newgraphenvironment.com/fpr/reference/fpr_sheet_trim.md)

## Value

A dataframe

## See also

Other import:
[`fpr_pscis_wkb_paths()`](http://www.newgraphenvironment.com/fpr/reference/fpr_pscis_wkb_paths.md)
