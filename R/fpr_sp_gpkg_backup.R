#' Backup a geopackage point file to a .csv and .RData file
#'
#' This function reads a geopackage file, checks and updates its UTM coordinates, and writes it back to the original path
#' and/or a backup directory as a .csv and .RData file. This function wraps \link{fpr_sp_assign_utm} facilitating
#' updating utm zone, easting and northing columns once geometries have been altered in QGIS.
#'
#' @param path_gpkg A character string specifying the path to the geopackage file including only one layer. Default is NULL.
#' @param dir_backup A character string specifying the directory for backup. Default is "data/backup/".
#' @param write_back_to_path A logical value indicating whether to write the updated geopackage back to the original
#' path. Will overwrite existing gpkg if it is present and the layer is the same as the basename of the geopackage
#' (ex. a layer named form_pscis_2023 will be written back to form_pscis_2023.gpkg).  Default is FALSE.
#' @param write_backup A logical value indicating whether to write a backup of the geopackage. Default is TRUE.
#' @param return_object A logical value indicating whether to return the updated sf object. Default is FALSE.
#' @param ... Not used.  Can be used to pass additional arguments to \link{fpr::fpr_sp_assign_utm}.
#'
#' @return Depending on write switches write a backup of the geopackage to the specified directory as a .RData
#' file containing an object named gpkg and a .csv file containing the data frame with the geometry column removed.
#' Switch can be used to overwrite the existing geopackage it is reading from because UTM coordinate columns will be
#' updated based on the geometry column.
#'
#' @importFrom chk chk_file chk_dir chk_logical
#' @importFrom sf st_read st_write st_drop_geometry
#' @importFrom poisutils ps_error
#' @importFrom readr write_csv
#' @importFrom tools file_path_sans_ext
#' @importFrom fpr fpr_sp_assign_utm
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Don't write any files or update the geopackage file in QGIS - just update the coordinates columns and import
#' # the updated sf object to the global environment and compare to a raw version of the file.
#' gpkg_raw <- sf::st_read(
#'   dsn = "~/Projects/gis/sern_peace_fwcp_2023/data_field/2023/form_fiss_site_2023.gpkg",
#'   quiet = TRUE
#' ) %>% slice(1:5)
#'
#' gpkg_utms_updated <- fpr_sp_gpkg_backup(
#'   path_gpkg = "~/Projects/gis/sern_peace_fwcp_2023/data_field/2023/form_fiss_site_2023.gpkg",
#'   write_backup = FALSE,
#'   col_easting = "utm_easting",
#'   col_northing = "utm_northing",
#'   sig_dit = 3,
#'   return_object = TRUE
#' ) %>% slice(1:5)
#'
#' waldo::compare(gpkg_raw, gpkg_utms_updated)
#' }
fpr_sp_gpkg_backup <- function(
    path_gpkg =  NULL,
    dir_backup =  "data/backup/",
    write_back_to_path = FALSE,
    write_backup = TRUE,
    return_object = FALSE,
    ...
){
  # Check inputs
  chk::chk_file(path_gpkg)
  chk::chk_dir(dir_backup)
  chk::chk_logical(write_back_to_path)
  chk::chk_logical(write_backup)

  # Read geopackage
  dat <- sf::st_read(dsn = path_gpkg, quiet = TRUE)

  # Check data
  if (is.null(dat))
    poisutils::ps_error('please provide "dat" (sf point dataframe) object')
  if (!is.data.frame(dat))
    poisutils::ps_error('"dat" must be a data.frame')
  if (class(dat)[1]!="sf")
    poisutils::ps_error('"dat" must be a sf object')
  if (!attributes(sf::st_geometry(dat))$class[1] != "sfc_point")
    poisutils::ps_error('"dat" must be a point simple feature object')

  # Update coordinates
  dat2 <-  dat %>%
    fpr::fpr_sp_assign_utm(...)

  if(write_back_to_path){
    dat2 %>%
      sf::st_write(path_gpkg, append = FALSE, delete_dsn = TRUE)
  }

  if(write_backup){
    dir.create(dir_backup, showWarnings = FALSE)
    dat2 %>%
      sf::st_drop_geometry() %>%
      readr::write_csv(paste0(
        dir_backup, tools::file_path_sans_ext(basename(path_gpkg)), ".csv"), na = "")
    # we rename the object just to get an informative name for the object saved in the .RData file
    gpkg <- dat2
    save(gpkg,
         file = paste0(dir_backup, tools::file_path_sans_ext(basename(path_gpkg)),".RData"))
  }
  if(return_object){
    dat2
  }
}
