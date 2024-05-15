#' Backup a geopackage point file to a .csv and .RData file
#'
#' This function reads a geopackage file, updates its UTM coordinate column values, and optionally writes it back to the
#' original path, a backup directory as a .csv and .RData file. This function wraps \link{fpr_sp_assign_utm} facilitating
#' updating utm zone, easting and northing columns once geometries have been altered in QGIS.
#'
#' @param path_gpkg A character string specifying the path to the geopackage file including only one layer. Default is NULL.
#' @param dir_backup A character string specifying the directory for backup. Default is "data/backup/".
#' @param write_back_to_path A logical value indicating whether to write the updated geopackage back to the original
#' path. Will overwrite existing gpkg if it is present and the layer is the same as the basename of the geopackage
#' (ex. a layer named form_pscis_2023 will be written back to form_pscis_2023.gpkg).  Default is FALSE.
#' @param return_object A logical value indicating whether to return the updated sf object. Default is FALSE.
#' @param ... Not used.  Can be used to pass additional arguments to \link{fpr::fpr_sp_assign_utm}.
#' @param write_to_csv  A logical value indicating whether to write csv version of gpkg to file for versioning. Default is TRUE
#' @param write_to_rdata A logical value indicating whether to write .RData object representation gpkg to file. Reason
#' for storing the geopackage in this format is that is preserves the object exactly where as recreating the gpkg
#' from csv will introduce small changes which appear to be only the removal of spaces. Default is TRUE
#' @param update_utm A logical value indicating whether to update the UTM coordinate columns with \link{fpr_sp_assign_utm}.
#' Default is TRUE.
#' @param update_site_id A logical value indicating whether to update the site_id column with \link{fpr_tidy_assign_site_id}.
#' Default is TRUE.
#'
#' @return Depending on write and return switches write a backup of the geopackage to the specified directory as a .RData
#' file containing an object named as the basename (without extension) of the input gpkg.  Also a .csv file containing
#' the gpkg represented as a data frame with the geometry column removed.  The updated sf object can be returned to the
#' global environment. Switch can be used to overwrite the existing geopackage it is reading if UTM coordinate columns
#' and or site_id is updated updated.
#'
#' @importFrom chk chk_file chk_dir chk_logical
#' @importFrom sf st_read st_write st_drop_geometry
#' @importFrom readr write_csv
#' @importFrom tools file_path_sans_ext
#' @importFrom fpr fpr_sp_assign_utm
#' @importFrom cli cli_abort
#'
#' @family spatial operations
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
#'   write_csv = FALSE,
#'   write_rdata = FALSE,
#'   return_object = TRUE,
#'   # these are passed to fpr_sp_assign_utm
#'   col_easting = "utm_easting",
#'   col_northing = "utm_northing",
#'   sig_dig = 3
#' ) %>% slice(1:5)
#'
# waldo::compare(gpkg_raw, gpkg_utms_updated)
# }

fpr_sp_gpkg_backup <- function(
    path_gpkg =  NULL,
    dir_backup =  "data/backup/",
    update_utm = TRUE,
    update_site_id = FALSE,
    write_back_to_path = FALSE,
    write_to_csv = TRUE,
    write_to_rdata = TRUE,
    return_object = FALSE,
    ...
){
  # Check inputs
  chk::chk_file(path_gpkg)
  chk::chk_string(dir_backup)
  chk::chk_logical(update_utm)
  chk::chk_logical(update_site_id)
  chk::chk_logical(write_back_to_path)
  chk::chk_logical(write_to_csv)
  chk::chk_logical(write_to_rdata)
  chk::chk_logical(return_object)

  # Read geopackage
  dat <- sf::st_read(dsn = path_gpkg, quiet = TRUE)

  # Check data
  if (is.null(dat))
    cli::cli_abort('please provide "dat" (sf point dataframe) object')
  if (!is.data.frame(dat))
    cli::cli_abort('"dat" must be a data.frame')
  if (class(dat)[1]!="sf")
    cli::cli_abort('"dat" must be a sf object')
  if (!attributes(sf::st_geometry(dat))$class[1] != "sfc_point")
    cli::cli_abort('"dat" must be a point simple feature object')

  if(update_utm){
    dat <-  dat %>%
      fpr_sp_assign_utm(...)
  }

  if(update_site_id){
    dat <-  dat %>%
      fpr_tidy_assign_site_id()
  }

  if(write_back_to_path){
    dat %>%
      sf::st_write(path_gpkg, append = FALSE, delete_dsn = TRUE)
  }

  if(write_to_csv){
    dir.create(dir_backup, showWarnings = FALSE)
    dat %>%
      sf::st_drop_geometry() %>%
      readr::write_csv(paste0(
        dir_backup, tools::file_path_sans_ext(basename(path_gpkg)), ".csv"), na = "")
  }

  if(write_to_rdata){
    # we rename the object just to get an informative name for the object saved in the .RData file
    # apparently not good practice to use assign() within a function because it make code difficult to debug

    # Create a dynamic object name
    obj_name <- tools::file_path_sans_ext(basename(path_gpkg))

    # Assign the value of dat to the new object in the function environment
    assign(obj_name, dat, envir = environment())

    # Save the new object
    save(list = obj_name,
         file = paste0(dir_backup, obj_name, ".RData"))

    # Remove the temporary object from the function environment
    rm(list = obj_name, envir = environment())
  }

  if(return_object){
    dat
  }
}
