#' Import sf object (point) and assign utm zone, easting and northing
#'
#' @param dat sf dataframe of type `Geometry type: POINT`. Default is `NULL`.
#' @param col_zone Quoted string defining the name of the column where the utm zone will be stored. Default is `'utm_zone'`.
#' @param col_easting Quoted string defining the name of the column where the utm easting will be stored. Default is `'easting'`.
#' @param col_northing Quoted string defining the name of the column where the utm northing will be stored. Default is `'northing'`.
#' @param sig_dig Number of significant digits to round the easting and northing to. Default is `0`.
#'
#' @return An sf dataframe with assigned utm zone, easting and northing.
#'
#' @importFrom sf st_crs st_transform st_coordinates
#' @importFrom dplyr mutate arrange select distinct group_by bind_rows
#' @importFrom purrr map2 map
#' @importFrom chk chk_string
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang sym
#'
#' @family spatial operations
#' @export
#'
#' @examples
#' \dontrun{
#' sf_df <- data.frame(
#'   lon = c(-128.5, -123.5, -118.5),  # Longitudes for UTM zones 9, 10, and 11
#'   lat = c(45.5, 45.5, 45.5)  # Same latitude for simplicity
#' ) %>%
#'   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
#'
#' # Use the function
#' result <- fpr_sp_assign_utm(dat = sf_df)
#' }
fpr_sp_assign_utm <- function(dat = NULL,
                              col_zone = 'utm_zone',
                              col_easting = 'easting',
                              col_northing = 'northing',
                              sig_dig = 0){

  if (is.null(dat))
    cli::cli_abort('please provide "dat" (sf point dataframe) object')
  if (!is.data.frame(dat))
    cli::cli_abort('"dat" must inherit from a data.frame')
  if (class(dat)[1]!="sf")
    cli::cli_abort('"dat" must be a sf object')
  if (!attributes(sf::st_geometry(dat))$class[1] != "sfc_point")
    cli::cli_abort('"dat" must be a point simple feature object')
  chk::chk_string(col_zone)
  chk::chk_string(col_easting)
  chk::chk_string(col_northing)

  # alert user if the default values for col_easting and col_northing are not present in dat and
  # default values are used as param inputs
  if (!col_easting %in% names(dat) | !col_northing %in% names(dat)){
    cli::cli_warn('The values specified for `col_easting` ({col_easting}) and `col_northing` ({col_northing}) are not present in `dat`.
                        Do want to create new columns named {col_easting} and {col_northing} or do
                  you need to provide the existing column names for X and Y coordinates?')
  }

  # capture the original crs
  crs_og <- sf::st_crs(dat, parameters = T)$epsg

  # find the utm zone
  dat2 <- dat %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(
      long = sf::st_coordinates(.)[, 1]) %>%
    # add in utm zone of study area
    dplyr::mutate({{ col_zone }} := floor((long + 180) / 6) + 1) %>%
    dplyr::mutate(epsg = 26900 + !! rlang::sym(col_zone)) %>%
    dplyr::arrange(epsg) %>%
    dplyr::select(-long)

  # grab the crs list to pass to get the utm coords later with map2
  crs_l <- dat2  %>%
    dplyr::distinct(epsg) %>%
    dplyr::pull(epsg)

  # split the sf object row wise after sorting by utm_zone
  form_l <- dat2 %>%
    # important to group by something that will sort the df equivalent to the crs df
    dplyr::group_by(epsg) %>%
    dplyr::group_split()

  # form_l <- split(form_prep1, seq(1:nrow(form_prep1)))

  # # transform to utm crs and extract coords
  dat <- form_l %>%
    purrr::map2(crs_l, sf::st_transform) %>%
    # the above is the same as longer format below
    # form <- form_l %>%
    #   purrr::map2(.x = form_l, .y = crs_l, .f = ~ sf::st_transform(x = .x,
    #                                                                      crs = .y))
    purrr::map(~dplyr::mutate(.,
                              {{ col_easting }} := round(sf::st_coordinates(.)[, 1], sig_dig),
                              {{ col_northing }} := round(sf::st_coordinates(.)[ ,2], sig_dig))
    ) %>%
    # transform back to geodecic crs of choice and recombine
    purrr::map(sf::st_transform, crs = crs_og) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-epsg)

}
