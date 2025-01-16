#' Transform PSCIS Assessment Worksheet Data by Calculating Stream Width Ratio
#'
#' This function calculates the stream width ratio (SWR) for PSCIS data based on the ratio of two specified columns: one for downstream channel width and one for diameter or span.
#'
#' @param dat [data.frame] A dataframe containing the PSCIS data.
#' @param col_downstream_channel_width_meters [character] Column name for downstream channel width, as a string or tidy-select syntax. Default is `downstream_channel_width_meters`.
#' @param col_diameter_or_span_meters [character] Column name for diameter or span, as a string or tidy-select syntax. Default is `diameter_or_span_meters`.
#' @param col_stream_width_ratio [character] Column name for the output stream width ratio, as a string. Default is `stream_width_ratio`.
#' @param digits [numeric] Passed to `[round()]` function to specify the number of decimal places in the output. The default value is `2`.
#'
#' @return [data.frame] A dataframe with the specified `col_stream_width_ratio` column added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   downstream_channel_width_meters = c(5, NA, 10),
#'   diameter_or_span_meters = c(2, 3, NA)
#' )
#' fpr_xfm_paw_score_swr(
#'   dat,
#'   col_downstream_channel_width_meters = downstream_channel_width_meters,
#'   col_diameter_or_span_meters = diameter_or_span_meters
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @family xfm paw
#' @export
fpr_xfm_paw_swr <- function(
    dat,
    col_downstream_channel_width_meters = downstream_channel_width_meters,
    col_diameter_or_span_meters = diameter_or_span_meters,
    col_stream_width_ratio = stream_width_ratio,
    digits = 2
) {
  # Check input validity
  chk::chk_data(dat)

  # Evaluate tidy-select columns
  col_downstream_channel_width_meters <- rlang::ensym(col_downstream_channel_width_meters)
  col_diameter_or_span_meters <- rlang::ensym(col_diameter_or_span_meters)
  col_stream_width_ratio <- rlang::ensym(col_stream_width_ratio)

  # Check if required columns exist in the dataframe
  required_cols <- c(as.character(col_downstream_channel_width_meters), as.character(col_diameter_or_span_meters))
  missing_cols <- setdiff(required_cols, colnames(dat))

  if (length(missing_cols) > 0) {
    cli::cli_abort("The following required columns are missing from the dataframe: {paste(missing_cols, collapse = ', ')}")
  }

  # Calculate stream_width_ratio
  dat <- dat |>
    dplyr::mutate(
      !!col_stream_width_ratio := dplyr::case_when(
        is.na(!!col_downstream_channel_width_meters) ~ 0,
        is.na(!!col_diameter_or_span_meters) | !!col_diameter_or_span_meters <= 0 ~ 0,
        TRUE ~ round((!!col_downstream_channel_width_meters / !!col_diameter_or_span_meters), digits)
      )
    )

  return(dat)
}
