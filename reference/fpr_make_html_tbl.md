# Make html table from dataframe

Usually just a one row tibble is transformed as this is helper function
for making kml files from PSCIS spreadsheets. Other functions for the
kml are in the 0110-make-kml.R file in the
fish_passage_skeena_2021_reporting repo for now.

## Usage

``` r
fpr_make_html_tbl(dat)
```

## Arguments

- dat:

  Dataframe or tibble which usually only has one row

## Value

html object

## See also

Other spatial:
[`fpr_sp_wshd_stats()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_wshd_stats.md)
