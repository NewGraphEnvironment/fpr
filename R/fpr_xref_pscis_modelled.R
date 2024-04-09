#' Create a Cross-Reference for PSCIS Modelled Data
#'
#' Used after our data is submitted to the province so that we can cross reference our modeled IDs to their new PSCIS identifiers.
#' This function queries geospatial data from the BC Data Catalogue, filters it based on the provided project ID,
#' selects specific columns, cleans the column names, drops unnecessary columns and geometry data, and converts all
#' remaining columns to numeric. The resulting data frame serves as a cross-reference for PSCIS modeled data.
#'
#' @param project_id A character string (quoted) specifying the project ID (or list of project IDS separated by commas).
#' The project_id is detailed as the `FUNDING_PROJECT_NUMBER` in the BC Data Catalougue
#' to filter the data on. Default is NULL.
#' @return A data frame with cleaned names and numeric columns, serving as a cross-reference for PSCIS modeled data.
#' @importFrom bcdata bcdc_query_geodata
#' @importFrom dplyr filter select mutate across everything
#' @importFrom janitor clean_names
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#' fpr_xref_pscis_modelled(project_id = 'peace_2023_Phase1')
#' }
#' @export
fpr_xref_pscis_modelled <- function(
    project_id = NULL){
 x <- bcdata::bcdc_query_geodata("7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881") %>%
    dplyr::filter(FUNDING_PROJECT_NUMBER %in% c(project_id)) %>%
    dplyr::select(EXTERNAL_CROSSING_REFERENCE, STREAM_CROSSING_ID) %>%
    bcdata::collect() %>%
    janitor::clean_names()


 if (nrow(x) == 0) {
   cli::cli_abort("`project_id` (a.k.a `FUNDING_PROJECT_NUMBER`) not found in the BC Data Catalogue. Please check the
                  `project_id` and try again.")

 }


 if (nrow(x) > 0) {
   x %>%
    dplyr::select(-id, -objectid) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
 }
}
