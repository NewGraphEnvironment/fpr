#' Function to process site ID value from pscis_crossing_id and my_crossing_reference
#'
#' Intended for processing field data from digital form in which only one value should be provided.
#' It checks to confirm the dataframe has one and only one of either a `pscis_crossing_id` or a `my_crossing_reference`.
#' It also uses [fpr_chk_dupes()] to issue a warning if there are duplicate values of `pscis_crossing_id`,
#' `my_crossing_reference` or `site_id`
#'
#' @param dat A dataframe to process. Default is NULL.
#' @return A processed dataframe.
#' @importFrom dplyr rowwise mutate ungroup filter select
#' @importFrom tibble rowid_to_column
#' @importFrom sf st_drop_geometry
#' @importFrom chk chk_numeric
#' @importFrom cli cli_abort
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
#' #' dat_warn_dupe <- tibble(
#' pscis_crossing_id = c(1, 3, NA, 4),
#' my_crossing_reference = c(NA, NA, 3, NA),
#' date_time_start = Sys.time() + 0:3
#' )
#'
#' fpr_tidy_assign_site_id(dat_pass)
#' fpr_tidy_assign_site_id(dat_warn_dupe)
#' }
fpr_tidy_assign_site_id <- function(dat = NULL) {

  # Sanity checks
  if (is.null(dat))
    cli::cli_abort('please provide "dat" (dataframe) object')
  if (!is.data.frame(dat))
    cli::cli_abort('"dat" must inherit from a data.frame')
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
    cli::cli_abort("Error: Some rows have either values in both pscis_crossing_id and my_crossing_reference or are missing
                   values in both fields:\n", paste(capture.output(print(dat)), collapse = "\n"))
  } else {
    dat <- dat %>%
      dplyr::select(-has_both_values, -has_no_values)
  }

  # Identify duplicate values in columns if present
  fpr_chk_dupes(dat, my_crossing_reference)
  fpr_chk_dupes(dat, pscis_crossing_id)
  fpr_chk_dupes(dat, site_id)

  return(dat)
}
