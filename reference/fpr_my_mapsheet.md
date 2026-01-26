# Print link to map based on watershed name and 50k grid found in bcfishpass

Print link to map based on watershed name and 50k grid found in
bcfishpass

## Usage

``` r
fpr_my_mapsheet(wshd = "bulkley", archive_date = "2022-05-02", ...)
```

## Arguments

- wshd:

  String identifying the watershed. Not standardized. Must match folders
  at https://hillcrestgeo.ca/outgoing/fishpassage/projects/

- archive_date:

  string quoted. Date in format YYYY-MM-DD corresponding to the archive
  date of the mapdeck

- ...:

  unused. placed to pass arguments to fpr_my_bcfishpass

## Value

Stirng with the url for the map where the crossing is located
