# Filter table by a value. Commopnly used to filter bcfishpass.crossings table export by PSCIS stream_crossing_id

Filter table by a value. Commopnly used to filter bcfishpass.crossings
table export by PSCIS stream_crossing_id

## Usage

``` r
fpr_my_bcfishpass(
  dat = bcfishpass,
  site = my_site,
  col_filter = stream_crossing_id,
  col_pull = stream_order,
  round_dig = 0
)
```

## Arguments

- dat:

  Dataframe. Defaults to bcfishpass filtered by Phase 2 sites only

- site:

  Numeric Value to filter \`col_filter\` on. Usually the PSCIS
  stream_crossing_id. Defaults to value defined by my_site

- col_filter:

  String. Tidyselect format. Column name for information you want to
  filter on

- col_pull:

  String. Tidyselect format. Column name for information you want to
  pull.

- round_dig:

  Integer for how many numbers to round to. Defaults to 0.

- col_to_filter:

  String value of column to filter

- col_to_pull:

  String vlaue of column to pull

## Value

Vector
