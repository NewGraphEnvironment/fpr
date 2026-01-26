# Pull info about a site

Pull info about a site

## Usage

``` r
fpr_my_pscis_info(
  dat = pscis_phase2,
  col_filter = pscis_crossing_id,
  site = my_site,
  col_pull = stream_name
)
```

## Arguments

- dat:

  Dataframe to pull info from. Defaults to object pscis_phase2 built in
  tables.R

- col_filter:

  String name of column on which to filter dat. Defaults to
  \`pscis_crossing_id\`.

- site:

  Numeric value of site of which to filter dat. Defaults to object
  \`my_site\`.

- col_pull:

  String name of column of which to pull from filtered \`dat\`. Defaults
  to \`stream_name\`
