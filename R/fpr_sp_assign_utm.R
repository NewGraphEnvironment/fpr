#' Import sf object (point) and assign utm zone, easting and northing
#'
#'
#' @param dat sf dataframe of type `Geometry type: POINT`
#' @param col_zone Quoted string defining the name of the column where the utm zone will be stored.
#' @param col_easting Quoted string defining the name of the column where the utm easting will be stored.
#' @param col_northing Quoted string defining the name of the column where the utm northing will be stored.
#'
#' @return
#' @export
#'
#' @examples
fpr_sp_assign_utm <- function(dat = NULL,
                               col_zone = 'utm_zone',
                               col_easting = 'utm_easting',
                               col_northing = 'utm_northing'){

  if (is.null(dat))
    poisutils::ps_error('please provide "dat" (sf point dataframe) object')
  if (!is.data.frame(dat))
    poisutils::ps_error('"dat" must inherit from a data.frame')
  if (class(dat)[1]!="sf")
    poisutils::ps_error('"dat" must be a sf object')
  if (!attributes(sf::st_geometry(dat))$class[1] != "sfc_point")
    poisutils::ps_error('"dat" must be a point simple feature object')
  chk::chk_string(col_zone)
  chk::chk_string(col_easting)
  chk::chk_string(col_northing)

  # capture the original crs
  crs_og <- sf::st_crs(dat, parameters = T)$epsg

  # find the utm zone
  dat2 <- dat %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(
      long = sf::st_coordinates(.)[,1]) %>%
    #   lat = sf::st_coordinates(.)[,2]) %>%
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
                              {{ col_easting }} := sf::st_coordinates(.)[,1],
                              {{ col_northing }} := sf::st_coordinates(.)[,2])) %>%
    # transform back to geodecic crs of choice and recombine
    purrr::map(sf::st_transform, crs = crs_og) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-epsg)

}
