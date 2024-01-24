#' Assign a spatial data frame from UTM coordinates
#'
#' This function is designed to work on a dataframe that contains columns with details of UTM zone,
#' easting and northing from a NAD 83 UTM Zone coordinate systems. It prepares a spatial data frame
#' by grouping based on a specified column (UTM zone), converting to a spatial data frame, transforming
#' the coordinate reference system, and then binding the rows back together. The returned dataframe
#' must be in a Geographic Coordinate System (BC Albers or vs a Spatial coordinate system like UTM.
#'
#' @param dat A data frame.
#' @param utm_zone The name of the column to group by. Default is "utm_zone".
#' @param easting The name of the easting coordinate column. Default is "easting".
#' @param northing The name of the northing coordinate column. Default is "northing".
#' @param crs_return The EPSG code to transform the coordinate reference system to. Must be Geographic (ex. Albers or WSG84). Default is BC Albers - 3005.
#' @return A spatial data frame.
#' @importFrom dplyr group_split bind_rows arrange
#' @importFrom purrr map
#' @importFrom sf st_as_sf st_transform
#' @importFrom rlang sym
#' @export
#' @examples
#' \dontrun{
#' fpr_sp_assign_sf_from_utm(pscis_all)
#' }
fpr_sp_assign_sf_from_utm <- function(
    dat,
    utm_zone = "utm_zone",
    easting = "easting",
    northing = "northing",
    crs_return = 3005
    ) {
  dat %>%
    dplyr::arrange(!!rlang::sym(utm_zone)) %>%
    dplyr::group_split(!!rlang::sym(utm_zone)) %>%
    purrr::map(~ sf::st_as_sf(.x, coords = c(easting, northing),
                              crs = 26900 + unique(.x[[utm_zone]]), remove = FALSE)) %>%
    purrr::map(sf::st_transform, crs = crs_return) %>%
    dplyr::bind_rows()
}
