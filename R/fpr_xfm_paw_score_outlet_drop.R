#' Transform PSCIS Assessment Worksheet Data by Calculating Outlet Drop Score
#'
#' This function calculates the outlet drop score for PSCIS data based on conditions involving outlet drop meters.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#' @param col_outlet_drop_meters [character] A column name specifying the outlet drop meters, as a string or tidy-select syntax. Default is `outlet_drop_meters`.
#' @param col_outlet_drop_score [character] A column name for the output outlet drop score, as a string or tidy-select syntax. Default is `outlet_drop_score`.
#' @param drop_risk_low [numeric] A numeric value representing the lower threshold for low outlet drop risk. Default is `0.15`.
#' @param drop_risk_high [numeric] A numeric value representing the threshold for high outlet drop risk. Default is `0.30`.
#' @param risk_mod [numeric] A numeric value representing the risk score for moderate outlet drop conditions. Default is `5`.
#' @param risk_high [numeric] A numeric value representing the risk score for high outlet drop conditions. Default is `10`.
#' @param risk_low [numeric] A numeric value representing the risk score for low outlet drop conditions. Default is `0`.
#'
#' @return [dataframe] A dataframe with the specified column for outlet drop score added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   outlet_drop_meters = c(0.1, 0.2, 0.35, NA)
#' )
#' fpr_xfm_paw_score_outlet_drop(
#'   dat
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @importFrom chk chk_data chk_character chk_number
#' @family xfm paw
#' @export
fpr_xfm_paw_score_outlet_drop <- function(
    dat,
    col_outlet_drop_meters = outlet_drop_meters,
    col_outlet_drop_score = outlet_drop_score,
    drop_risk_low = 0.15,
    drop_risk_high = 0.30,
    risk_mod = 5,
    risk_high = 10,
    risk_low = 0
) {
  # Check input validity
  chk::chk_data(dat)
  chk::chk_string(deparse(substitute(col_outlet_drop_meters)))
  chk::chk_string(deparse(substitute(col_outlet_drop_score)))
  chk::chk_number(drop_risk_low)
  chk::chk_number(drop_risk_high)
  chk::chk_number(risk_mod)
  chk::chk_number(risk_high)
  chk::chk_number(risk_low)

  # Evaluate tidy-select columns
  col_outlet_drop_meters <- rlang::ensym(col_outlet_drop_meters)
  col_outlet_drop_score <- rlang::ensym(col_outlet_drop_score)

  # Check if required column exists in the dataframe
  required_cols <- as.character(col_outlet_drop_meters)
  if (!(required_cols %in% colnames(dat))) {
    cli::cli_abort("The required column '{required_cols}' is missing from the dataframe.")
  }

  # Calculate outlet drop score
  dat <- dat |>
    dplyr::mutate(
      !!col_outlet_drop_score := dplyr::case_when(
        !!col_outlet_drop_meters < drop_risk_low ~ risk_low,
        !!col_outlet_drop_meters >= drop_risk_low & !!col_outlet_drop_meters < drop_risk_high ~ risk_mod,
        !!col_outlet_drop_meters >= drop_risk_high ~ risk_high,
        TRUE ~ risk_low
      )
    )

  return(dat)
}
