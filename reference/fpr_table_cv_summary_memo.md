# Summary table of culvert info with photo to place in memos.

Summary table of culvert info with photo to place in memos.

## Usage

``` r
fpr_table_cv_summary_memo(
  dat = pscis_phase2,
  site = my_site,
  site_photo_id = my_site,
  font = 11,
  col_filter = pscis_crossing_id
)
```

## Arguments

- dat:

  Dataframe. Defaults to pscis_phase2

- site:

  Integer. Name of PSCIS site to filter pscis_phase2. Defaults to
  my_site

- site_photo_id:

  Integer. Name of PSCIS site to source photo. Defaults to my_site

- font:

  Integer. Size of font.

- col_filter:

  String unquoted. Name of column to filter site from. Defaults to
  pscis_crossing_id

## Value

html object
