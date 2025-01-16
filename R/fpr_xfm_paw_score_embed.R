#' Transform PSCIS Assessment Worksheet Data by Calculating Embedment Score
#'
#' This function calculates the embedment score for BC Provincial Stream Crossing Inventory System (PSCIS) data based on conditions involving continuous embedment, pipe diameter, and average depth of embedment.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#' @param col_continuous_embeddedment_yes_no [character] A column name specifying continuous embedment ("Yes" or "No"), as a string or tidy-select syntax. Default is `continuous_embeddedment_yes_no`.
#' @param col_diameter_or_span_meters [character] A column name specifying the diameter or span, as a string or tidy-select syntax. Default is `diameter_or_span_meters`.
#' @param col_average_depth_embededdment_meters [character] A column name specifying the average depth of embedment, as a string or tidy-select syntax. Default is `average_depth_embededdment_meters`.
#' @param col_embed_score [character] A column name for the output embedment score, as a string or tidy-select syntax. Default is `embed_score`.
#' @param risk_high [numeric] A numeric value representing the risk score for non-continuous embedment. Default is `10`, aligning with the Pisces assessment worksheet.
#' @param risk_mod [numeric] A numeric value representing the risk score for moderate embedment conditions. Default is `5`, aligning with the Pisces assessment worksheet.
#' @param risk_low [numeric] A numeric value representing the risk score for low-risk embedment conditions. Default is `0`, aligning with the Pisces assessment worksheet.
#'
#' @return [dataframe] A dataframe with the specified column for embedment score added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   continuous_embeddedment_yes_no = c("No", "Yes", "Yes"),
#'   diameter_or_span_meters = c(2, NA, 3),
#'   average_depth_embededdment_meters = c(0.4, 0.1, NA)
#' )
#' fpr_xfm_paw_score_embed(
#'   dat
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @importFrom chk chk_data chk_character chk_number
#' @family xfm paw
#' @export
fpr_xfm_paw_score_embed <- function(
    dat,
    col_continuous_embeddedment_yes_no = continuous_embeddedment_yes_no,
    col_diameter_or_span_meters = diameter_or_span_meters,
    col_average_depth_embededdment_meters = average_depth_embededdment_meters,
    col_embed_score = embed_score,
    risk_high = 10,
    risk_mod = 5,
    risk_low = 0
) {
  # Check input validity
  chk::chk_data(dat)
  chk::chk_string(deparse(substitute(col_continuous_embeddedment_yes_no)))
  chk::chk_string(deparse(substitute(col_diameter_or_span_meters)))
  chk::chk_string(deparse(substitute(col_average_depth_embededdment_meters)))
  chk::chk_string(deparse(substitute(col_embed_score)))
  chk::chk_number(risk_high)
  chk::chk_number(risk_mod)
  chk::chk_number(risk_low)

  # Evaluate tidy-select columns
  col_continuous_embeddedment_yes_no <- rlang::ensym(col_continuous_embeddedment_yes_no)
  col_diameter_or_span_meters <- rlang::ensym(col_diameter_or_span_meters)
  col_average_depth_embededdment_meters <- rlang::ensym(col_average_depth_embededdment_meters)
  col_embed_score <- rlang::ensym(col_embed_score)

  # Check if required columns exist in the dataframe
  required_cols <- c(
    as.character(col_continuous_embeddedment_yes_no),
    as.character(col_diameter_or_span_meters),
    as.character(col_average_depth_embededdment_meters)
  )
  missing_cols <- setdiff(required_cols, colnames(dat))

  if (length(missing_cols) > 0) {
    cli::cli_abort("The following required columns are missing from the dataframe: {paste(missing_cols, collapse = ', ')}")
  }

  # Calculate embedment_score
  dat <- dat |>
    dplyr::mutate(
      !!col_embed_score := dplyr::case_when(
        !!col_continuous_embeddedment_yes_no == "No" ~ risk_high,
        is.na(!!col_diameter_or_span_meters) ~ risk_low,
        is.na(!!col_average_depth_embededdment_meters) ~ risk_low,
        (!!col_average_depth_embededdment_meters / !!col_diameter_or_span_meters <= 0.2 | !!col_average_depth_embededdment_meters < 0.3) ~ risk_mod,
        (!!col_average_depth_embededdment_meters / !!col_diameter_or_span_meters > 0.2 & !!col_average_depth_embededdment_meters >= 0.3) ~ risk_low,
        TRUE ~ risk_low
      )
    )

  return(dat)
}
