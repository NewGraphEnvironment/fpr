# Backup a geopackage point file to a .csv and .RData file

This function reads a geopackage file, updates its UTM coordinate column
values, and optionally writes it back to the original path, a backup
directory as a .csv and .RData file. This function wraps
[fpr_sp_assign_utm](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_utm.md)
facilitating updating utm zone, easting and northing columns once
geometries have been altered in QGIS.

## Usage

``` r
fpr_sp_gpkg_backup(
  path_gpkg = NULL,
  dir_backup = "data/backup/",
  update_utm = TRUE,
  update_site_id = FALSE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = FALSE,
  ...
)
```

## Arguments

- path_gpkg:

  A character string specifying the path to the geopackage file
  including only one layer. Default is NULL.

- dir_backup:

  A character string specifying the directory for backup. Default is
  "data/backup/".

- update_utm:

  A logical value indicating whether to update the UTM coordinate
  columns with
  [fpr_sp_assign_utm](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_utm.md).
  Default is TRUE.

- update_site_id:

  A logical value indicating whether to update the site_id column with
  [fpr_tidy_assign_site_id](http://www.newgraphenvironment.com/fpr/reference/fpr_tidy_assign_site_id.md).
  Default is TRUE.

- write_back_to_path:

  A logical value indicating whether to write the updated geopackage
  back to the original path. Will overwrite existing gpkg if it is
  present and the layer is the same as the basename of the geopackage
  (ex. a layer named form_pscis_2023 will be written back to
  form_pscis_2023.gpkg). Default is FALSE.

- write_to_csv:

  A logical value indicating whether to write csv version of gpkg to
  file for versioning. Default is TRUE

- write_to_rdata:

  A logical value indicating whether to write .RData object
  representation gpkg to file. Reason for storing the geopackage in this
  format is that is preserves the object exactly where as recreating the
  gpkg from csv will introduce small changes which appear to be only the
  removal of spaces. Default is TRUE

- return_object:

  A logical value indicating whether to return the updated sf object.
  Default is FALSE.

- ...:

  Not used. Can be used to pass additional arguments to
  fpr::fpr_sp_assign_utm.

## Value

Depending on write and return switches write a backup of the geopackage
to the specified directory as a .RData file containing an object named
as the basename (without extension) of the input gpkg. Also a .csv file
containing the gpkg represented as a data frame with the geometry column
removed. The updated sf object can be returned to the global
environment. Switch can be used to overwrite the existing geopackage it
is reading if UTM coordinate columns and or site_id is updated updated.

## See also

Other spatial operations:
[`fpr_sp_assign_latlong()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_latlong.md),
[`fpr_sp_assign_sf_from_utm()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_sf_from_utm.md),
[`fpr_sp_assign_utm()`](http://www.newgraphenvironment.com/fpr/reference/fpr_sp_assign_utm.md)
