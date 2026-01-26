# Make Geopackage from an dataframe with UTM coordinates

Creates a directory (if it doesn't exist) and writes a layer to a
GeoPackage using the name of the input \`dat\` object as the layer name.

## Usage

``` r
fpr_make_geopackage(
  dat,
  dir = "data/fishpass_mapping/",
  gpkg_name = "fishpass_mapping",
  ...
)
```

## Arguments

- dat:

  \[data.frame\] A data frame with coordinates to burn to a GeoPackage.

- dir:

  \[character\] A string representing the path to the output directory.
  Default is "data/fishpass_mapping/".

- gpkg_name:

  \[character\] A string representing the name of the GeoPackage file.
  Default is "fishpass_mapping".

- ...:

  Additional arguments passed to \[fpr_sp_assign_sf_from_utm()\].

## Value

The written GeoPackage layer, invisibly.

## Details

This function internally calls \[fpr_sp_assign_sf_from_utm()\] to
convert the input data frame to an \[sf::sf\] object. You can supply UTM
zone and coordinate column names via the \`...\` arguments: -
\`col_utm_zone\`: A string for the UTM zone column (default is
"utm_zone") - \`col_easting\`: A string for the easting (X) column
(e.g., "utm_easting") - \`col_northing\`: A string for the northing (Y)
column (e.g., "utm_northing") - \`crs_return\`: An integer for the
intermediate CRS to assign before transformation

## See also

\[fpr_sp_assign_sf_from_utm()\], \[sf::st_write()\]

## Examples

``` r
dat <- data.frame(
  utm_zone = 10,
  utm_easting = c(610000, 611000),
  utm_northing = c(5600000, 5601000)
)
fpr_make_geopackage(
  dat = dat,
  col_easting = "utm_easting",
  col_northing = "utm_northing",
  crs_return = 4326
)
#> Writing layer `dat' to data source 
#>   `data/fishpass_mapping/fishpass_mapping.gpkg' using driver `GPKG'
#> Writing 2 features with 3 fields and geometry type Point.
```
