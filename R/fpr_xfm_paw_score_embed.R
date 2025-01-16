#' Transform PSCIS Assessment Worksheet Data by Calculating Embedment Score
#'
#' This function calculates the embedment score for PSCIS data based on conditions involving continuous embedment, pipe diameter, and average depth of embedment.
#'
#' @param dat A dataframe containing the PSCIS data.
#' @param col_continuous_embeddedment_yes_no Column name for continuous embedment ("Yes" or "No"), as a string or tidy-select syntax. Default is `continuous_embeddedment_yes_no`.
#' @param col_diameter_or_span_meters Column name for diameter or span, as a string or tidy-select syntax. Default is `diameter_or_span_meters`.
#' @param col_average_depth_embededdment_meters Column name for average depth of embedment, as a string or tidy-select syntax. Default is `average_depth_embededdment_meters`.
#'
#' @return A dataframe with `embedment_score` column added or updated.
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
#' @family xfm paw
#' @export
fpr_xfm_paw_score_embed <- function(
    dat,
    col_continuous_embeddedment_yes_no = continuous_embeddedment_yes_no,
    col_diameter_or_span_meters = diameter_or_span_meters,
    col_average_depth_embededdment_meters = average_depth_embededdment_meters
) {
  # Check input validity
  chk::chk_data(dat)

  # Evaluate tidy-select columns
  col_continuous_embeddedment_yes_no <- rlang::ensym(col_continuous_embeddedment_yes_no)
  col_diameter_or_span_meters <- rlang::ensym(col_diameter_or_span_meters)
  col_average_depth_embededdment_meters <- rlang::ensym(col_average_depth_embededdment_meters)

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
      embedment_score = dplyr::case_when(
        !!col_continuous_embeddedment_yes_no == "No" ~ 10,
        is.na(!!col_diameter_or_span_meters) ~ 0,
        is.na(!!col_average_depth_embededdment_meters) ~ 0,
        (!!col_average_depth_embededdment_meters / !!col_diameter_or_span_meters <= 0.2 | !!col_average_depth_embededdment_meters < 0.3) ~ 5,
        (!!col_average_depth_embededdment_meters / !!col_diameter_or_span_meters > 0.2 & !!col_average_depth_embededdment_meters >= 0.3) ~ 0,
        TRUE ~ 0
      )
    )

  return(dat)
}
