# Extact cost to fix site

Extact cost to fix site

## Usage

``` r
fpr_my_cost_estimate(
  dat = tab_cost_est_phase2,
  col_filter = pscis_crossing_id,
  site = my_site,
  col_pull = cost_est
)
```

## Arguments

- dat:

  Dataframe with pscis_crossing_id and cost_est_1000s columns

- col_filter:

  String (unqouted) name of column to filter. Defaults to
  pscis_crossing_id

- site:

  Integer. Corresponds to the PSCIS site id. Defaults to my_site defined
  in environment

## Value

String. Formatted number
