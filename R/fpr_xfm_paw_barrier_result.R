#' Transform PSCIS Assessment Worksheet Data by Calculating Barrier Result
#'
#' This function calculates the barrier result for PSCIS data based on the crossing type and final score.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#' @param col_crossing_type [character] A column name specifying the crossing type. Default is `crossing_type`.
#' @param col_final_score [character] A column name specifying the final score. Default is `final_score`.
#' @param col_barrier_result [character] A column name for the output barrier result. Default is `barrier_result`.
#' @param risk_low_value [numeric] A numeric value representing the lower threshold for low-risk final scores. Default is `15`.
#' @param risk_high_value [numeric] A numeric value representing the upper threshold for moderate-risk final scores. Default is `19`.
#'
#' @return [dataframe] A dataframe with the specified column for barrier result added or updated.
#'
#' @examples
#' dat <- data.frame(
#'   crossing_type = c("Open Bottom Structure", "Other", NA, "", "Other"),
#'   final_score = c(10, 16, 25, NA, 18)
#' )
#' fpr_xfm_paw_barrier_result(dat)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @importFrom chk chk_data chk_character chk_number
#' @family xfm paw
#' @export
fpr_xfm_paw_barrier_result <- function(
    dat,
    col_crossing_type = crossing_type,
    col_final_score = final_score,
    col_barrier_result = barrier_result,
    risk_low_value = 15,
    risk_high_value = 19
) {
  # Check input validity
  chk::chk_data(dat)
  chk::chk_string(deparse(substitute(col_crossing_type)))
  chk::chk_string(deparse(substitute(col_final_score)))
  chk::chk_string(deparse(substitute(col_barrier_result)))
  chk::chk_number(risk_low_value)
  chk::chk_number(risk_high_value)

  # Evaluate tidy-select columns
  col_crossing_type <- rlang::ensym(col_crossing_type)
  col_final_score <- rlang::ensym(col_final_score)
  col_barrier_result <- rlang::ensym(col_barrier_result)

  # Check if required columns exist in the dataframe
  required_cols <- c(as.character(col_crossing_type), as.character(col_final_score))
  missing_cols <- setdiff(required_cols, colnames(dat))
  if (length(missing_cols) > 0) {
    cli::cli_abort("The following required columns are missing from the dataframe: {missing_cols}")
  }

  # Calculate barrier result
  dat <- dat |>
    dplyr::mutate(
      !!col_barrier_result := dplyr::case_when(
        is.na(!!col_crossing_type) | !!col_crossing_type == "" ~ "",
        !!col_crossing_type == "Open Bottom Structure" ~ "Passable",
        !!col_crossing_type == "Other" ~ "Unknown",
        !!col_final_score < risk_low_value ~ "Passable",
        !!col_final_score >= risk_low_value & !!col_final_score <= risk_high_value ~ "Potential",
        !!col_final_score > risk_high_value ~ "Barrier",
        TRUE ~ ""
      )
    )

  return(dat)
}
