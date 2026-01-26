# Extract info from habitat_confirmations_priorities spreadsheet

Extract info from habitat_confirmations_priorities spreadsheet

## Usage

``` r
fpr_my_priority_info(
  dat = habitat_confirmations_priorities,
  sit = my_site,
  loc = "us",
  col_pull = hab_value
)
```

## Arguments

- dat:

  Dataframe. Defaults to habitat_confirmations_priorities

- sit:

  Integer. Corresponds to the PSCIS site id. Defaults to my_site defined
  in envrionment

- loc:

  String. Either 'us' (upstream) or 'ds' (downstream). Used quotes.

- col_pull:

  String. Tidyselect format. Column name for information you want to
  pull.

## Value

String
