# Assign a spatial data frame from UTM coordinates

This function is designed to work on a dataframe that contains columns
with details of UTM zone, easting and northing from a NAD 83 UTM Zone
coordinate systems. It prepares a spatial data frame by grouping based
on a specified column (UTM zone), converting to a spatial data frame,
transforming the coordinate reference system, and then binding the rows
back together. The returned dataframe must be in a Geographic Coordinate
System (ex. BC Albers or WSG84 vs a Spatial coordinate system like
UTM.).

## Usage

``` r
fpr_sp_assign_sf_from_utm(
  dat = NULL,
  col_utm_zone = "utm_zone",
  col_easting = "easting",
  col_northing = "northing",
  crs_return = 3005
)
```

## Arguments

- dat:

  A data frame.

- col_utm_zone:

  The name of the column to group by. Default is "utm_zone".

- col_easting:

  The name of the easting coordinate column. Default is "easting".

- col_northing:

  The name of the northing coordinate column. Default is "northing".

- crs_return:

  Numeric. The EPSG code to transform the coordinate reference system
  to. Must be Geographic (ex. Albers - 3005 or WSG84 - 4326). Default is
  BC Albers.

## Value

A spatial data frame.

## Details

The reason the returned dataframe must be in a Geographic Coordinate
System is to avoid issues when input points are located in different
spatial zones.

## See also

Other spatial operations:
[`fpr_sp_assign_latlong()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_latlong.md),
[`fpr_sp_assign_utm()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_utm.md),
[`fpr_sp_gpkg_backup()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_gpkg_backup.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fpr_sp_assign_sf_from_utm(pscis_all)
} # }
```
