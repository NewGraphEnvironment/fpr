#' Create blank PSCIS submission template
#'
#' For use in prepping digital field data for submission.
#'
#'
#' @return empty tibble with columns and types equivalent to PSCIS submission template.
#' @export
#'
#' @examples example <- fpr_xref_template_pscis()
fpr_xref_template_pscis <- function(){
  template_prep<- fpr::fpr_xref_pscis %>%
    dplyr::filter(!is.na(spdsht))

  d_num = template_prep %>%
    dplyr::filter(type_readxl == 'numeric') %>%
    dplyr::pull(spdsht)

  d_date = template_prep %>%
    dplyr::filter(type_readxl == 'date') %>%
    dplyr::pull(spdsht)

  template <- template_prep %>%
    dplyr::select(spdsht) %>%
    tidyr::pivot_wider(names_from = spdsht, values_from = spdsht) %>%
    dplyr::slice(0) %>%
    # https://stackoverflow.com/questions/65921972/convert-a-vector-of-character-strings-as-symbols
    dplyr::mutate(dplyr::across(c(!!! rlang::syms(d_num)), ~ as.numeric(.x))) %>%
    dplyr::mutate(dplyr::across(c(!!! rlang::syms(d_date)), ~ lubridate::as_date(.x)))
}
