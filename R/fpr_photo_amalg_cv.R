#' Amalgamate 6 PSCIS photos into 1
#'
#' Here we stack up and down then side to side for reporting.  There must be all 6 photos
#' present, only 1 of each and within the file names of those photos must be the strings
#' containing upstream, downstream, inlet, outlet, barrel, road.  Before running QA with
#'
#' @param  site_id Numeric value of site corresponding to the directory name that holds the photos
#' which include 'road', 'inlet', 'upstream', 'downstream', 'outlet', barrel' in the filenames.
#' @param size String. Dimensions of individual photos. Defaults to "560x420!" to give oveall photo size of 1120 x 1260.
#' @param dir_photos String quoted for directory where photo directories are kept.  Trailing forward slash required.
#' Defaults to 'data/photos/'.
#' @importFrom magrittr %>%
#' @importFrom stringr str_subset str_detect
#' @importFrom dplyr as_tibble mutate arrange pull
#' @importFrom magick image_read image_append image_scale image_write
#'
#' @return Burned amalgamated crossing_all.JPG which includes 6 PSCIS photos together as one.
#' @family photo
#' @export
fpr_photo_amalg_cv <- function(site_id, dir_photos = 'data/photos/', size = "560x420!"){
  photos_images1 <- list.files(path = paste0(dir_photos, site_id), full.names = TRUE) %>%
    stringr::str_subset(., '_upstream.|_road.|_inlet.') %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(sort = dplyr::case_when(
      stringr::str_detect(value, 'road') ~ 1,
      stringr::str_detect(value, 'inlet') ~ 2,
      stringr::str_detect(value, 'upstream') ~ 3
    )) %>%
    dplyr::arrange(sort) %>%
    dplyr::pull(value) %>%
    magick::image_read()
  photos_images2 <- list.files(path = paste0(dir_photos, site_id), full.names = TRUE) %>%
    stringr::str_subset(., '_barrel.|_outlet.|_downstream.') %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(sort = dplyr::case_when(
      stringr::str_detect(value, 'barrel') ~ 4,
      stringr::str_detect(value, 'outlet') ~ 5,
      stringr::str_detect(value, 'downstream') ~ 6
    )) %>%
    dplyr::arrange(sort) %>%
    dplyr::pull(value) %>%
    magick::image_read()
  photos_stack1 <- magick::image_append(magick::image_scale(photos_images1, size), stack = TRUE)
  photos_stack2 <- magick::image_append(magick::image_scale(photos_images2, size), stack = TRUE)
  photos_stack <- c(photos_stack1, photos_stack2)
  photos_stacked <- magick::image_append(magick::image_scale(photos_stack), stack = FALSE)
  magick::image_write(photos_stacked, path = paste0(dir_photos, site_id, '/crossing_all.JPG'), format = 'jpg')
}
