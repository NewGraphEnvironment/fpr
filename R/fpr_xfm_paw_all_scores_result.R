#' Transform PSCIS Assessment Worksheet Data by Calculating All Metrics, Scores, and Results Necessary for Barrier Risk Assessment
#'
#' This function calculates the fall metrics, scores, and results for BC Provincial Stream Crossing Inventory System (PSCIS) data,
#' sums score columns, and assigns `barrier_result`.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#'
#' @return [dataframe] A dataframe with all calculated metrics, scores, and results necessary for barrier risk assessment.
#'
#' @examples
#' path <- system.file("extdata", "pscis_phase1.xlsm", package = "fpr")
#' dat <- fpr_import_pscis(dir_root = fs::path_dir(path))
#' result <- fpr_xfm_paw_all_scores_result(dat)
#' head(result[, grep("score|barrier", names(result), ignore.case = TRUE)])
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#' @importFrom chk chk_data
#'
#' @family xfm paw
#' @export
fpr_xfm_paw_all_scores_result <- function(dat) {
  # Check input validity
  chk::chk_data(dat)

  dat |>
    fpr_xfm_paw_swr() |>
    fpr_xfm_paw_score_cv_len() |>
    fpr_xfm_paw_score_embed() |>
    fpr_xfm_paw_score_outlet_drop() |>
    fpr_xfm_paw_score_cv_slope() |>
    fpr_xfm_paw_score_swr() |>
    fpr_xfm_paw_score_final() |>
    fpr_xfm_paw_barrier_result()

}
