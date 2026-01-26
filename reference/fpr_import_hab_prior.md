# Import habitat priorities csv and assign aggregated crossing id

Import habitat priorities csv and assign aggregated crossing id

## Usage

``` r
fpr_import_hab_prior(
  input = "data/habitat_confirmations_priorities.csv",
  filter_ef_s2 = TRUE
)
```

## Arguments

- input:

  csv to input which has site as a column

- filter_ef_s2:

  Boolean - Do we filter out electrofishing sites, feature_record_only
  ie.falls, LWD jams, and sites with multiple cards for different
  sections surveyed?

## Value

dataframe of the priorities with an aggregated_crossings_id added
