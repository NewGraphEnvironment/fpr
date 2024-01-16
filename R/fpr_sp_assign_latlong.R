#' Extract latitude and longitude from sf object
#'
#'
#' Extract latitude and longitude (WSG84 CRS 4326 from sf object and add as columns.)
#'
#' @inheritParams fpr_sp_assign_utm
#' @param dat
#' @param col_lat Quoted character string defining the name of the column where the northing will be stored.
#' @param col_lon Quoted character string defining the name of the column where the easting will be stored.
#'
#' @family spatial operations
#'
#' @return sf object in equivalent CRS as input with lat and lon columns appended.
#' @export
#'
#' @examples
fpr_sp_assign_latlong <- function(dat = NULL,
                                  col_lat = 'lat',
                                  col_lon = 'lon'){

  if (is.null(dat))
    poisutils::ps_error('please provide "dat" (sf point dataframe) object')
  if (!is.data.frame(dat))
    poisutils::ps_error('"dat" must be a data.frame')
  if (class(dat)[1]!="sf")
    poisutils::ps_error('"dat" must be a sf object')
  if (!attributes(sf::st_geometry(dat))$class[1] != "sfc_point")
    poisutils::ps_error('"dat" must be a point simple feature object')
  chk::chk_string(col_lon)
  chk::chk_string(col_lat)

  # capture the original crs
  crs_og <- sf::st_crs(dat, parameters = T)$epsg

  dat2 <- dat %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(
      {{ col_lon }} := sf::st_coordinates(.)[,1],
      {{ col_lat }} := sf::st_coordinates(.)[,2]) %>%
    sf::st_transform(crs = crs_og)
}
