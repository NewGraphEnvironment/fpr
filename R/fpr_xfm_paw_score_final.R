#' Transform PSCIS Assessment Worksheet Data by Calculating Final Score
#'
#' This function calculates the final score for BC Provincial Stream Crossing Inventory System data by summing specified score columns.
#'
#' @param dat [dataframe] A dataframe containing the score data.
#' @param col_final_score [character] A column name for the final score output, as a string or tidy-select syntax. Default is `final_score`.
#' @param col_culvert_length_score [character] A column name for the culvert length score, as a string or tidy-select syntax. Default is `culvert_length_score`.
#' @param col_embed_score [character] A column name for the embed score, as a string or tidy-select syntax. Default is `embed_score`.
#' @param col_outlet_drop_score [character] A column name for the outlet drop score, as a string or tidy-select syntax. Default is `outlet_drop_score`.
#' @param col_culvert_slope_score [character] A column name for the culvert slope score, as a string or tidy-select syntax. Default is `culvert_slope_score`.
#' @param col_stream_width_ratio_score [character] A column name for the stream width ratio score, as a string or tidy-select syntax. Default is `stream_width_ratio_score`.
#'
#' @return [dataframe] A dataframe with the specified column for the final score added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   culvert_length_score = c(1, 2, 3, NA),
#'   embed_score = c(2, 3, NA, 1),
#'   outlet_drop_score = c(3, NA, 2, 1),
#'   culvert_slope_score = c(4, 3, 2, NA),
#'   stream_width_ratio_score = c(5, NA, 4, 3)
#' )
#' fpr_xfm_paw_score_final(
#'   dat
#' )
#'
#' @importFrom dplyr mutate rowwise c_across
#' @importFrom cli cli_abort
#' @importFrom chk chk_data chk_character
#' @family xfm paw
#' @export
fpr_xfm_paw_score_final <- function(
    dat,
    col_final_score = final_score,
    col_culvert_length_score = culvert_length_score,
    col_embed_score = embed_score,
    col_outlet_drop_score = outlet_drop_score,
    col_culvert_slope_score = culvert_slope_score,
    col_stream_width_ratio_score = stream_width_ratio_score
) {
  # Check input validity
  chk::chk_data(dat)
  chk::chk_string(deparse(substitute(col_final_score)))
  chk::chk_string(deparse(substitute(col_culvert_length_score)))
  chk::chk_string(deparse(substitute(col_embed_score)))
  chk::chk_string(deparse(substitute(col_outlet_drop_score)))
  chk::chk_string(deparse(substitute(col_culvert_slope_score)))
  chk::chk_string(deparse(substitute(col_stream_width_ratio_score)))

  # Evaluate tidy-select columns
  col_final_score <- rlang::ensym(col_final_score)
  col_culvert_length_score <- rlang::ensym(col_culvert_length_score)
  col_embed_score <- rlang::ensym(col_embed_score)
  col_outlet_drop_score <- rlang::ensym(col_outlet_drop_score)
  col_culvert_slope_score <- rlang::ensym(col_culvert_slope_score)
  col_stream_width_ratio_score <- rlang::ensym(col_stream_width_ratio_score)

  # Check if required columns exist in the dataframe
  required_cols <- c(
    as.character(col_culvert_length_score),
    as.character(col_embed_score),
    as.character(col_outlet_drop_score),
    as.character(col_culvert_slope_score),
    as.character(col_stream_width_ratio_score)
  )

  missing_cols <- setdiff(required_cols, colnames(dat))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "The following required columns are missing from the dataframe: {missing_cols}"
    )
  }

  # Calculate final score
  dat <- dat |>
    dplyr::rowwise() |>
    dplyr::mutate(
      !!col_final_score := sum(
        dplyr::c_across(
          c(
            !!col_culvert_length_score,
            !!col_embed_score,
            !!col_outlet_drop_score,
            !!col_culvert_slope_score,
            !!col_stream_width_ratio_score
          )
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()

  return(dat)
}
