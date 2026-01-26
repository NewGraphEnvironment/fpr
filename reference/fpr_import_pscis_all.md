# Import All PSCIS Sheets at Once and backup

Import All PSCIS Sheets at Once and backup

## Usage

``` r
fpr_import_pscis_all(backup = TRUE, path_backup = "data/backup/", ...)
```

## Arguments

- backup:

  Logical whether to backup all sheets as a combined csvs or not.
  Defaults to true

- path_backup:

  String indicating directory to create (if not exists) and backup to.
  Defaults to 'data/backup/'

- ...:

  Unused - Pass through another param to
  [fpr_pscis_wkb_paths](http://www.newgraphenvironment.com/fpr/reference/fpr_pscis_wkb_paths.md)

## Value

list of tibbles and a csv that backs up a combined tibble of list
components
