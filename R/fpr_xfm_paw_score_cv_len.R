#' Transform PSCIS Assessment Worksheet Data by Calculating Culvert Length Score
#'
#' This function calculates the culvert length score for BC Provincial Stream Crossing Inventory System (PSCIS) data based on the given length or width column.
#'
#' @param dat A dataframe containing the PSCIS data.
#' @param col_length_or_width_meters Column name for length or width, as a string or tidy-select syntax. Default is `length_or_width_meters`.
#'
#' @return A dataframe with `culvert_length_score` column added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   length_or_width_meters = c(10, 20, 35, NA)
#' )
#' fpr_xfm_culvert_length_score(
#'   dat,
#'   col_length_or_width_meters = length_or_width_meters
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @family xfm paw
#' @export
fpr_xfm_paw_score_cv_len <- function(
    dat,
    col_length_or_width_meters = length_or_width_meters
) {
  # Check input validity
  chk::chk_data(dat)

  # Evaluate tidy-select column
  col_length_or_width_meters <- rlang::ensym(col_length_or_width_meters)

  # Check if required column exists in the dataframe
  required_cols <- as.character(col_length_or_width_meters)
  if (!(required_cols %in% colnames(dat))) {
    cli::cli_abort("The required column '{required_cols}' is missing from the dataframe.")
  }

  # Calculate culvert_length_score
  dat <- dat |>
    dplyr::mutate(
      culvert_length_score = dplyr::case_when(
        is.na(!!col_length_or_width_meters) ~ 0,
        !!col_length_or_width_meters < 15 ~ 0,
        !!col_length_or_width_meters >= 15 & !!col_length_or_width_meters < 30 ~ 3,
        !!col_length_or_width_meters >= 30 ~ 6,
        TRUE ~ 0
      )
    )

  return(dat)
}
