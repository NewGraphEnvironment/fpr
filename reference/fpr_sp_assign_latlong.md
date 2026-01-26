# Extract latitude and longitude from sf object

Extract latitude and longitude (WSG84 CRS 4326 from sf object and add as
columns.)

## Usage

``` r
fpr_sp_assign_latlong(dat = NULL, col_lat = "lat", col_lon = "lon")
```

## Arguments

- dat:

  sf dataframe of type \`Geometry type: POINT\`. Default is \`NULL\`.

- col_lat:

  Quoted character string defining the name of the column where the
  northing will be stored.

- col_lon:

  Quoted character string defining the name of the column where the
  easting will be stored.

## Value

sf object in equivalent CRS as input with lat and lon columns appended.

## See also

Other spatial operations:
[`fpr_sp_assign_sf_from_utm()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_sf_from_utm.md),
[`fpr_sp_assign_utm()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_utm.md),
[`fpr_sp_gpkg_backup()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_gpkg_backup.md)
