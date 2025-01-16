#' Transform PSCIS Assessment Worksheet Data by Calculating Stream Width Ratio Score
#'
#' This function calculates the stream width ratio (SWR) score for BC Provincial Stream Crossing Inventory System (PSCIS) data based on specified thresholds.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#' @param col_stream_width_ratio [character] A column name specifying the stream width ratio, as a string or tidy-select syntax. Default is `stream_width_ratio`.
#' @param col_stream_width_ratio_score [character] A column name for the output SWR score, as a string or tidy-select syntax. Default is `stream_width_ratio_score`.
#' @param risk_low_value [numeric] A numeric value representing the lower threshold for low stream width ratio risk. Default is `1`.
#' @param risk_high_value [numeric] A numeric value representing the threshold for high stream width ratio risk. Default is `1.3`.
#' @param risk_low_score [numeric] A numeric value representing the risk score for low SWR conditions. Default is `0`.
#' @param risk_mod_score [numeric] A numeric value representing the risk score for moderate SWR conditions. Default is `3`.
#' @param risk_high_score [numeric] A numeric value representing the risk score for high SWR conditions. Default is `6`.
#'
#' @return [dataframe] A dataframe with the specified column for SWR score added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   stream_width_ratio = c(0.8, 1.2, 1.4, NA)
#' )
#' fpr_xfm_paw_score_swr(
#'   dat
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @importFrom chk chk_data chk_character chk_number
#' @family xfm paw
#' @export
fpr_xfm_paw_score_swr <- function(
    dat,
    col_stream_width_ratio = stream_width_ratio,
    col_stream_width_ratio_score = stream_width_ratio_score,
    risk_low_value = 1,
    risk_high_value = 1.3,
    risk_low_score = 0,
    risk_mod_score = 3,
    risk_high_score = 6
) {
  # Check input validity
  chk::chk_data(dat)
  chk::chk_string(deparse(substitute(col_stream_width_ratio)))
  chk::chk_string(deparse(substitute(col_stream_width_ratio_score)))
  chk::chk_number(risk_low_value)
  chk::chk_number(risk_high_value)
  chk::chk_number(risk_low_score)
  chk::chk_number(risk_mod_score)
  chk::chk_number(risk_high_score)

  # Evaluate tidy-select columns
  col_stream_width_ratio <- rlang::ensym(col_stream_width_ratio)
  col_stream_width_ratio_score <- rlang::ensym(col_stream_width_ratio_score)

  # Check if required column exists in the dataframe
  required_cols <- as.character(col_stream_width_ratio)
  if (!(required_cols %in% colnames(dat))) {
    cli::cli_abort("The required column '{required_cols}' is missing from the dataframe.")
  }

  # Calculate SWR score
  dat <- dat |>
    dplyr::mutate(
      !!col_stream_width_ratio_score := dplyr::case_when(
        !!col_stream_width_ratio < risk_low_value ~ risk_low_score,
        !!col_stream_width_ratio >= risk_low_value & !!col_stream_width_ratio < risk_high_value ~ risk_mod_score,
        !!col_stream_width_ratio >= risk_high_value ~ risk_high_score,
        TRUE ~ risk_low_score
      )
    )

  return(dat)
}
