#' Assign a spatial data frame from UTM coordinates
#'
#' This function is designed to work on a dataframe that contains columns with details of UTM zone,
#' easting and northing from a NAD 83 UTM Zone coordinate systems. It prepares a spatial data frame
#' by grouping based on a specified column (UTM zone), converting to a spatial data frame, transforming
#' the coordinate reference system, and then binding the rows back together. The returned dataframe
#' must be in a Geographic Coordinate System (BC Albers or vs a Spatial coordinate system like UTM.
#'
#' @param dat A data frame.
#' @param col_utm_zone The name of the column to group by. Default is "utm_zone".
#' @param col_easting The name of the easting coordinate column. Default is "easting".
#' @param col_northing The name of the northing coordinate column. Default is "northing".
#' @param crs_return Numeric. The EPSG code to transform the coordinate reference system to. Must be Geographic (ex. Albers or WSG84). Default is BC Albers - 3005.
#' @return A spatial data frame.
#' @importFrom dplyr group_split bind_rows arrange
#' @importFrom purrr map
#' @importFrom sf st_as_sf st_transform
#' @importFrom rlang sym
#'
#' @family spatial operations
#' @export
#' @examples
#' \dontrun{
#' fpr_sp_assign_sf_from_utm(pscis_all)
#' }
fpr_sp_assign_sf_from_utm <- function(
    dat = NULL,
    col_utm_zone = "utm_zone",
    col_easting = "easting",
    col_northing = "northing",
    crs_return = 3005
    ) {

  if (is.null(dat))
    poisutils::ps_error('please provide "dat" (sf point dataframe) object')
  if (!is.data.frame(dat))
    poisutils::ps_error('"dat" must be a data.frame')
  chk::chk_string(col_utm_zone)
  chk::chk_string(col_easting)
  chk::chk_string(col_northing)
  chk::chk_numeric(crs_return)

  dat %>%
    dplyr::arrange(!!rlang::sym(col_utm_zone)) %>%
    dplyr::group_split(!!rlang::sym(col_utm_zone)) %>%
    purrr::map(~ sf::st_as_sf(.x, coords = c(col_easting, col_northing),
                              crs = 26900 + unique(.x[[col_utm_zone]]), remove = FALSE, sf_column_name = "geom")) %>%
    purrr::map(sf::st_transform, crs = crs_return) %>%
    dplyr::bind_rows()
}
