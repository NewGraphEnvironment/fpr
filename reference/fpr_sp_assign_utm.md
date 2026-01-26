# Import sf object (point) and assign utm zone, easting and northing

Import sf object (point) and assign utm zone, easting and northing

## Usage

``` r
fpr_sp_assign_utm(
  dat = NULL,
  col_zone = "utm_zone",
  col_easting = "easting",
  col_northing = "northing",
  sig_dig = 0
)
```

## Arguments

- dat:

  sf dataframe of type \`Geometry type: POINT\`. Default is \`NULL\`.

- col_zone:

  Quoted string defining the name of the column where the utm zone will
  be stored. Default is \`'utm_zone'\`.

- col_easting:

  Quoted string defining the name of the column where the utm easting
  will be stored. Default is \`'easting'\`.

- col_northing:

  Quoted string defining the name of the column where the utm northing
  will be stored. Default is \`'northing'\`.

- sig_dig:

  Number of significant digits to round the easting and northing to.
  Default is \`0\`.

## Value

An sf dataframe with assigned utm zone, easting and northing.

## See also

Other spatial operations:
[`fpr_sp_assign_latlong()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_latlong.md),
[`fpr_sp_assign_sf_from_utm()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_sf_from_utm.md),
[`fpr_sp_gpkg_backup()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_gpkg_backup.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sf_df <- data.frame(
  lon = c(-128.5, -123.5, -118.5),  # Longitudes for UTM zones 9, 10, and 11
  lat = c(45.5, 45.5, 45.5)  # Same latitude for simplicity
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Use the function
result <- fpr_sp_assign_utm(dat = sf_df)
} # }
```
