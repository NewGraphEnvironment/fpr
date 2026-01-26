# Summary table of bcfishpass modelling outputs

Built from bcfishpass with the names of the left hand side of the table
and 'potential' habitat calculated

## Usage

``` r
fpr_table_bcfp(
  dat = bcfishpass,
  xref_table = xref_bcfishpass_names,
  site = my_site,
  col = stream_crossing_id,
  ...
)
```

## Arguments

- dat:

  Dataframe with one row. Defaults to bcfishpass

- xref_table:

  Table of column names from bcfishpass with reader friendly versions
  and columns indicating order and location (left or right) for how to
  present the information.

- site:

  Integer identifying which site to filter the bcfishpass dataframe

- col:

  Column to filter bcfishpass. Defaults to stream_crossing_id

- ...:

  Not use. Pass param to
  [fpr_kable](http://www.newgraphenvironment.com/fpr/reference/fpr_kable.md)
