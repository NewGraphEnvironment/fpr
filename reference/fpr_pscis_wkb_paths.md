# Get Names of PSCIS Files

Reads filenames in your data file for files with the 'pscis' string in
their name. Ignores temp files that are there when

## Usage

``` r
fpr_pscis_wkb_paths(path_ls_files = "data")
```

## Arguments

- path_ls_files:

  String indicating directory to look for pscis submission files.
  Defaults to 'data'.

## Value

String of pull path name of file by that pattern

## See also

Other import:
[`fpr_import_pscis()`](http://www.newgraphenvironment.com/fpr/reference/fpr_import_pscis.md)
