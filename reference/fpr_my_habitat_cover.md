# Pull out dominant and subdominant cover types

Pull out dominant and subdominant cover types

## Usage

``` r
fpr_my_habitat_cover(
  dat = hab_site,
  sit = my_site,
  loc = "us",
  cover_type = "dominant"
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

- cover_type:

  Cover type from the fish submission spreadsheet

## Value

string value of cover as per submission template dropdown options
