#' Transform PSCIS Assessment Worksheet Data by Calculating Culvert Slope Score
#'
#' This function calculates the culvert slope score for BC Provincial Stream Crossing Inventory System (PSCIS) data based on conditions involving culvert slope percent.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#' @param col_culvert_slope_percent [character] A column name specifying the culvert slope percent, as a string or tidy-select syntax. Default is `culvert_slope_percent`.
#' @param col_culvert_slope_score [character] A column name for the output culvert slope score, as a string or tidy-select syntax. Default is `culvert_slope_score`.
#' @param slope_risk_low [numeric] A numeric value representing the lower threshold for low culvert slope risk (in percent). Default is `1`.
#' @param slope_risk_high [numeric] A numeric value representing the threshold for high culvert slope risk (in percent). Default is `3`.
#' @param risk_mod [numeric] A numeric value representing the risk score for moderate culvert slope conditions. Default is `5`.
#' @param risk_high [numeric] A numeric value representing the risk score for high culvert slope conditions. Default is `10`.
#' @param risk_low [numeric] A numeric value representing the risk score for low culvert slope conditions. Default is `0`.
#'
#' @return [dataframe] A dataframe with the specified column for culvert slope score added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   culvert_slope_percent = c(0.5, 1.5, 3.5, NA)
#' )
#' fpr_xfm_paw_score_cv_slope(
#'   dat
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @importFrom chk chk_data chk_character chk_number
#' @family xfm paw
#' @export
fpr_xfm_paw_score_cv_slope <- function(
    dat,
    col_culvert_slope_percent = culvert_slope_percent,
    col_culvert_slope_score = culvert_slope_score,
    slope_risk_low = 1,
    slope_risk_high = 3,
    risk_mod = 5,
    risk_high = 10,
    risk_low = 0
) {
  # Check input validity
  chk::chk_data(dat)
  chk::chk_string(deparse(substitute(col_culvert_slope_percent)))
  chk::chk_string(deparse(substitute(col_culvert_slope_score)))
  chk::chk_number(slope_risk_low)
  chk::chk_number(slope_risk_high)
  chk::chk_number(risk_mod)
  chk::chk_number(risk_high)
  chk::chk_number(risk_low)

  # Evaluate tidy-select columns
  col_culvert_slope_percent <- rlang::ensym(col_culvert_slope_percent)
  col_culvert_slope_score <- rlang::ensym(col_culvert_slope_score)

  # Check if required column exists in the dataframe
  required_cols <- as.character(col_culvert_slope_percent)
  if (!(required_cols %in% colnames(dat))) {
    cli::cli_abort("The required column '{required_cols}' is missing from the dataframe.")
  }

  # Calculate culvert slope score
  dat <- dat |>
    dplyr::mutate(
      !!col_culvert_slope_score := dplyr::case_when(
        !!col_culvert_slope_percent < slope_risk_low ~ risk_low,
        !!col_culvert_slope_percent >= slope_risk_low & !!col_culvert_slope_percent < slope_risk_high ~ risk_mod,
        !!col_culvert_slope_percent >= slope_risk_high ~ risk_high,
        TRUE ~ risk_low
      )
    )

  return(dat)
}
