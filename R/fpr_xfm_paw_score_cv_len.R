#' Transform PSCIS Assessment Worksheet Data by Calculating Culvert Length Score
#'
#' This function calculates the culvert length score for BC Provincial Stream Crossing Inventory System (PSCIS) data based on the given length or width column.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#' @param col_length_or_width_meters [character] Column name for length or width, as a string or tidy-select syntax. Default is `length_or_width_meters`.
#' @param col_culvert_length_score [character] Column name for the culvert length score, as a string. Default is `culvert_length_score`.
#' @param risk_low_value [numeric] A numeric value representing the lower threshold for culvert length risk. Default is `15`.
#' @param risk_high_value [numeric] A numeric value representing the threshold for culvert length risk. Default is `30`.
#' @param risk_low_score [numeric] A numeric value representing the low risk score. Default is `0`.
#' @param risk_mod_score [numeric] A numeric value representing the moderate risk score. Default is `3`.
#' @param risk_high_score [numeric] A numeric value representing the high risk score. Default is `6`.
#'
#' @return [dataframe] A dataframe with `culvert_length_score` column added or updated.
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
    col_length_or_width_meters = length_or_width_meters,
    col_culvert_length_score = culvert_length_score,
    risk_low_value = 15,
    risk_high_value = 30,
    risk_low_score = 0,
    risk_mod_score = 3,
    risk_high_score = 6
) {
  # Check input validity
  chk::chk_data(dat)

  # Evaluate tidy-select columns
  col_length_or_width_meters <- rlang::ensym(col_length_or_width_meters)
  col_culvert_length_score <- rlang::ensym(col_culvert_length_score)

  # Check if required column exists in the dataframe
  required_cols <- as.character(col_length_or_width_meters)
  if (!(required_cols %in% colnames(dat))) {
    cli::cli_abort("The required column '{required_cols}' is missing from the dataframe.")
  }

  # Calculate culvert_length_score
  dat <- dat |>
    dplyr::mutate(
      !!col_culvert_length_score := dplyr::case_when(
        is.na(!!col_length_or_width_meters) ~ risk_low_score,
        !!col_length_or_width_meters < risk_low_value ~ risk_low_score,
        !!col_length_or_width_meters >= risk_low_value & !!col_length_or_width_meters < risk_high_value ~ risk_mod_score,
        !!col_length_or_width_meters >= risk_high_value ~ risk_high_score,
        TRUE ~ risk_low_score
      )
    )

  return(dat)
}
