#' Function to process site ID value from pscis_crossing_id and my_crossing_reference
#'
#' Intended for processing field data from digital form in which only one value should be provided.
#' It checks if the dataframe has both 'pscis_crossing_id' and 'my_crossing_reference' values, or none of them.
#' If any rows violate these rules, it returns an error that prints out a simplified dataframe with violating rows.
#'
#' @param dat A dataframe to process. Default is NULL.
#' @return A processed dataframe.
#' @importFrom dplyr rowwise mutate ungroup filter select
#' @importFrom tibble rowid_to_column
#' @importFrom sf st_drop_geometry
#' @importFrom chk chk_numeric
#' @export
#' @family tidy
#' @examples
#' \dontrun{
#' dat_pass <- tibble(
#' pscis_crossing_id = c(1, 2, NA, 4),
#' my_crossing_reference = c(NA, NA, 3, NA),
#' date_time_start = Sys.time() + 0:3
#' )
#'
#' fpr_t_site_id(dat_pass)
#' }
fpr_t_site_id <- function(dat = NULL) {

  # Sanity checks
  if (is.null(dat))
    stop('please provide "dat" (dataframe) object')
  if (!is.data.frame(dat))
    stop('"dat" must inherit from a data.frame')
  chk::chk_numeric(dat$pscis_crossing_id)
  chk::chk_numeric(dat$my_crossing_reference)

  dat <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(has_both_values = !is.na(pscis_crossing_id) & !is.na(my_crossing_reference),
                  has_no_values = is.na(pscis_crossing_id) & is.na(my_crossing_reference),
                  site_id = ifelse(has_both_values | has_no_values,
                                   NA,
                                   ifelse(!is.na(pscis_crossing_id), pscis_crossing_id, my_crossing_reference))) %>%
    dplyr::ungroup()

  # If any rows violate the rules, return an error that prints out the dataframe
  if (any(dat$has_both_values) | any(dat$has_no_values)) {
    dat <- dat %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(has_both_values | has_no_values) %>%
      dplyr::select(date_time_start, pscis_crossing_id, my_crossing_reference)
    stop("Error: Some rows violate the rules:\n", toString(dat))
  } else {
    dat <- dat %>%
      dplyr::select(-has_both_values, -has_no_values)
  }

  return(dat)
}

