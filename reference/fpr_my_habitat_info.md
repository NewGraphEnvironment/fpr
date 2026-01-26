# Extract habitat details from hab_site dataframe

Extract habitat details from hab_site dataframe

## Usage

``` r
fpr_my_habitat_info(
  dat = hab_site,
  sit = my_site,
  loc = "us",
  col_pull = "avg_channel_width_m"
)
```

## Arguments

- dat:

  Dataframe. Defaults to hab_site.

- sit:

  Integer. Corresponds to the PSCIS site id. Defaults to my_site defined
  in envrionment

- loc:

  String. Either 'us' (upstream) or 'ds' (downstream). Use quotes.

- row:

  String in quotes indicating name of column (actually a row at this
  point) to pull out.

## Value

Sring or number from hab data
