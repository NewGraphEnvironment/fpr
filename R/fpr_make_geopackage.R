#' Make Geopackage from an dataframe with UTM coordinates
#'
#' Creates a directory (if it doesn't exist) and writes a layer to a GeoPackage
#' using the name of the input `dat` object as the layer name.
#'
#' This function internally calls [fpr_sp_assign_sf_from_utm()] to convert the input
#' data frame to an [sf::sf] object. You can supply UTM zone and coordinate column names
#' via the `...` arguments:
#' - `col_utm_zone`: A string for the UTM zone column (default is "utm_zone")
#' - `col_easting`: A string for the easting (X) column (e.g., "utm_easting")
#' - `col_northing`: A string for the northing (Y) column (e.g., "utm_northing")
#' - `crs_return`: An integer for the intermediate CRS to assign before transformation
#'
#' @param dat [data.frame] A data frame with coordinates to burn to a GeoPackage.
#' @param dir [character] A string representing the path to the output directory. Default is "data/fishpass_mapping/".
#' @param gpkg_name [character] A string representing the name of the GeoPackage file. Default is "fishpass_mapping".
#' @param crs [integer] Coordinate reference system to transform to. Default is `4326`.
#' @param ... Additional arguments passed to [fpr_sp_assign_sf_from_utm()].
#'
#' @return The written GeoPackage layer, invisibly.
#'
#' @seealso [fpr_sp_assign_sf_from_utm()], [sf::st_write()]
#'
#' @importFrom sf st_transform st_write
#' @importFrom fs dir_create path
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   utm_zone = 10,
#'   utm_easting = c(610000, 611000),
#'   utm_northing = c(5600000, 5601000)
#' )
#' fpr_make_geopackage(
#'   dat = dat,
#'   col_easting = "utm_easting",
#'   col_northing = "utm_northing"
#' )
fpr_make_geopackage <- function(dat,
                                dir = "data/fishpass_mapping/",
                                gpkg_name = "fishpass_mapping",
                                crs = 4326,
                                ...) {
  if (is.null(dat))
  cli::cli_abort('please provide "dat" object')
  if (!is.data.frame(dat))
    cli::cli_abort('"dat" must be a data.frame')
  chk::chk_string(dir)
  chk::chk_string(gpkg_name)
  chk::chk_number(crs)

  fs::dir_create(dir)
  nm <- deparse(substitute(dat))

  dat |>
    fpr_sp_assign_sf_from_utm(...) |>
    sf::st_transform(crs = crs) |>
    sf::st_write(fs::path(dir, paste0(gpkg_name, ".gpkg")), nm, delete_layer = TRUE) |>
    invisible()
}


