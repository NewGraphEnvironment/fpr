#' Check for Duplicate Values in a Column
#'
#' This function checks for duplicate values in a specified column of a dataset and provides a warning if duplicates are found.
#'
#' @param dat [data.frame] A data frame to be checked for duplicates.
#' @param col_screen A column in the data frame to check for duplicates. Must be specified as a tidy evaluation argument.
#'
#' @details
#' The function identifies duplicate values in the column and provides a warning with a list of the duplicate values if any are found.
#'
#' @importFrom chk chk_not_missing chk_data
#' @importFrom rlang enquo as_name
#' @importFrom dplyr filter group_by n ungroup distinct pull
#' @importFrom cli cli_abort cli_alert_danger
#' @importFrom glue glue_collapse
#'
#' @return The function does not return a value but provides a warning if duplicates are found.
#'
#' @export
fpr_chk_dupes <- function(dat = NULL, col_screen = NULL) {
  # Validate inputs
  chk::chk_not_missing(dat)
  # chk::chk_not_missing({{ col_screen }})
  chk::chk_data(dat)

  # Validate col_screen is a valid column in dat
  col_name <- rlang::as_name(rlang::enquo(col_screen))
  if (!col_name %in% names(dat)) {
    cli::cli_abort("{.val {col_name}} is not a valid column in the dataset.")
  }

  # Find duplicates
  dupes <- dat |>
    dplyr::filter(!is.na({{ col_screen }})) |>
    dplyr::group_by({{ col_screen }}) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(dupes) > 0) {
    # Get distinct duplicate values
    d <- dupes |>
      dplyr::distinct({{ col_screen }}) |>
      dplyr::pull({{ col_screen }})

    # Alert with duplicate values using glue_collapse
    warning(cli::cli_alert_danger("Duplicates found in column {.val {col_name}}: {glue::glue_collapse(d, sep = ', ')}."))
  }
}
