#' Make table with summary of culvert details for results section of report
#'
#' Depends on tab_culvert_prep object and xref_names objects build in the tables.R file
#'
#' @param dat Dataframe built from fpr_import_pscis
#'
#' @return
#' @export
#'
#' @examples
fpr_table_cv_summary <- function(dat){
  tab_culvert_prep <- dat %>%
    select(pscis_crossing_id, continuous_embeddedment_yes_no,
           outlet_drop_meters, diameter_or_span_meters,
           stream_width_ratio, culvert_slope_percent,
           length_or_width_meters,
           final_score, barrier_result)

  names_report <- left_join(
    as_tibble(names(tab_culvert_prep)),
    select(xref_names_pscis, spdsht, report),
    by = c('value' = 'spdsht')
  ) %>%
    pull(report)

  tab_culvert <- tab_culvert_prep %>%
    purrr::set_names(nm = names_report)
}


#' Make two column dataframe of PSCIS info with parameter on left and value on right within each column.
#'
#' @param dat Datframe of PSCIS spreadsheet input data with only 1 row. Split from pscis_phase1, pscis_reasessments or pscis_phase2.
#'
#' @return Two column dataframe with parameter on left and value on right within each column.
#' @export
#'
#' @examples
fpr_table_cv_detailed <- function(dat){
  tab_results_left <- fpr::xref_names_pscis %>%
    filter(id_side == 1)
  ##get the data
  tab_pull_left <- dat %>%
    select(pull(tab_results_left,spdsht)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, fpr::xref_names_pscis, by = c('rowname' = 'spdsht'))

  tab_results_right <- xref_names_pscis %>%
    filter(id_side == 2)

  ##get the data
  tab_pull_right<- dat %>%
    select(pull(tab_results_right,spdsht)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, fpr::xref_names_pscis, by = c('rowname' = 'spdsht'))

  tab_joined <- left_join(
    select(left, report, V1, id_join),
    select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Location and Stream Data', '-', 'Crossing Characteristics', '--'))
  return(tab_joined)
}

####---------------make a table for the comments---------------
#' Make table for the comments to be appended to
#'
#' @param dat Dataframe of PSCIS spreadsheet input data with only 1 row. Split from pscis_phase1, pscis_reasessments or pscis_phase2.
#'
#' @return Dataframe two columns wide with comments: on the left
#' @export
#'
#' @examples
fpr_table_cv_detailed_comments <- function(dat){
  dat %>%
    # sf::st_drop_geometry() %>%
    select(assessment_comment) %>%
    # slice(1) %>%
    set_names('Comment')
}

####--------------phase1 summary tables--------------------------
#' Print out a list of fpr_table_cv_detailed tables showing PSCIS data in untidy table in either gitbook or pdf versions.
#'
#' @param tab_sum list of dataframes formatted by fpr_table_cv_detailed.
#' @param comments list of dataframes formatted by fpr_table_cv_detailed_comments
#' @param photos list of dataframes with just the url of the photo for each site
#' @param gitbook logical based on whether out put is gitbook or pagedown pdf. Defaults to gitbook_on which is
#' defined in the index file as FALSE when the pagedown output is on.  This is when we need the spaces.
#'
#' @return html tables with photo as footnotes
#' @importFrom kableExtra kable_styling add_footnote
#' @importFrom knitr kable
#' @export
#' @family tables
#'
#' @examples
fpr_table_cv_detailed_print <- function(tab_sum,
                                        comments,
                                        photos,
                                        gitbook_switch = gitbook_on){
  output <- knitr::kable(tab_sum, booktabs = T) |>
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) |>
    kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
                                            '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            photos[[1]]), notation = 'none')
  if(gitbook_switch){ output
  }else paste0(output, '<br><br><br><br><br><br><br><br><br><br><br><br><br><br>')
}

# fpr_print_tab_summary_all_pdf <- function(tab_sum, comments, photos){
#   kable(tab_sum, booktabs = T) %>%
#     kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
#     kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
#     kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
#                                             '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
#                                             photos[[1]]), notation = 'none') %>%
#     kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
# }



#' Summary stats for each watershed
#'
#' @param dat Dataframe with stream_crossing_id, area_km
#' @param site_id Numeric id corresponding to stream_crossing_id (psscis id) of site
#'
#' @return
#' @export
#'
#' @examples
fpr_table_wshd_sum <- function(dat = wshds, site_id = NULL){
  wshds_prep <- dat %>%
    sf::st_drop_geometry() %>%
    dplyr::select(site = stream_crossing_id, area_km, contains(c('elev', 'aspect'))) %>%
    purrr::set_names(nm = names(.) %>%
                       janitor::make_clean_names(case = 'title') %>%
                       stringr::str_to_title())
  if(!is.null(site_id)){
    wshds_prep <- wshds_prep %>%
      dplyr::filter(Site == site_id)}
  wshds_prep
}


#' Make html tables that link to interactive map
#'
#' Depends on fpr_table_cv_detailed function and external data xref_names_pscis kept in package.
#'
#' @param site Integer. Site ID for site to be summarized.
#'
#' @return html file representing summary table for culvert details
#' @export
#'
#' @examples
fpr_table_cv_html <- function(site){
  fpr_table_cv_detailed(pscis_all %>% filter(pscis_crossing_id == site)) %>%
    kable(booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 18, html_font = 'helvetica') %>%
    kableExtra::add_footnote(label = paste0('Comments: ', pscis_all %>% filter(pscis_crossing_id == site) %>%
                                              distinct(pscis_crossing_id, .keep_all = T) %>% ##might be my_crossing_refe
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    readr::write_file(., file = paste0("docs/sum/cv/", site, ".html"))
}


#' Summary table of bcfishpass modelling outputs
#'
#' Built from bcfishpass with the names of the left hand side of the table and 'potential' habitat calculated
#'
#' @param dat Dataframe with one row. Defaults to bcfishpass
#' @param xref_table Table of column names from bcfishpass with reader friendly versions and columns
#' indicating order and location (left or right) for how to present the information.
#' @param site Integer identifying which site to filter the bcfishpass dataframe
#' @param col Column to filter bcfishpass.  Defaults to stream_crossing_id
#' @param ... Not use. Pass param to \link{fpr_kable}
#'
#' @return
#' @export
#'
#' @examples
fpr_table_bcfp <- function(dat = bcfishpass,
                                  xref_table = xref_bcfishpass_names,
                                  site = my_site,
                                  col = stream_crossing_id,
                           ...){
  df <- dat %>%
    dplyr::mutate(across(where(is.numeric), round, 1)) %>%
    dplyr::filter({{ col }} == site) %>%
    distinct({{ col }}, .keep_all = T)
  tab_results_left <- xref_table %>%
    dplyr::filter(id_side == 1) %>%
    dplyr::arrange(id_join)
  ##get the data
  tab_pull_left <- df %>%
    select(dplyr::pull(tab_results_left,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_results_right <- xref_table %>%
    dplyr::filter(id_side == 2)

  ##get the data
  tab_pull_right<- df %>%
    dplyr::select(dplyr::pull(tab_results_right,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_joined <- left_join(
    dplyr::select(left, report, V1, id_join),
    dplyr::select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Habitat', 'Potential', 'remove', 'Remediation Gain')) %>%
    dplyr::mutate(Potential = as.numeric(Potential),
           `Remediation Gain` = as.numeric(`Remediation Gain`)) %>%
    dplyr::mutate(`Remediation Gain (%)` = round(`Remediation Gain`/Potential * 100,0),
           Habitat = stringr::str_replace_all(Habitat, 'Ha', '(ha)'),
           Habitat = stringr::str_replace_all(Habitat, 'Km', '(km)'),
           Habitat = stringr::str_replace_all(Habitat, 'Lakereservoir', 'Lake and Reservoir'),
           Habitat = stringr::str_replace_all(Habitat, 'Spawningrearing ', 'Spawning and Rearing ')) %>%
    dplyr::select(-remove)
  tab_joined %>%
    fpr_kable(caption_text = paste0('Summary of fish habitat modelling for PSCIS crossing ', site, '.'),
                   footnote_text = 'Model data is preliminary and subject to adjustments.',
                   ...)
}

#' Write bcfishpass model summaries to html files
#'
#' Tables will be linked to map points
#'
#' @param sites Integer value representing site.
#' @param ... Not used. Pass param to fpr_table_bcfp (or through to fpr_kable)
#'
#' @return
#' @export
#'
#' @examples
fpr_table_bcfp_html <- function(sites, ...){
  fpr_table_bcfp(site = sites, ...) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 18) %>%
    kableExtra::add_footnote(c('Model data is preliminary and subject to adjustments including incorporating area based estimates.',
                             'Modelled rearing habitat estimates include linear lengths of centrelines within wetlands for coho and within lakes >100ha (multiplied by 1.5) for sockeye.',
                             'Remediation Gain is an estimate of the amount of habitat to be gained by providing access above the crossing.  This assumes that all upstream habitat is currently unavailable and that all modelled unassessed crossings located upstream would prevent further passage.'),
                             notation = 'symbol') %>%
    readr::write_file(., file = paste0("docs/sum/bcfp/", sites, ".html"))
}




#' Summary table for fish sampling sites
#'
#' @param dat Dataframe.  Defaults to tab_fish_sites_sum.  Must have columns named site, passes, ef_length_m, ef_width_m, area_m2, enclosure
#' @param sit Integer.  Site ID.  Defaults to my_site
#' @param ... Not used. Open for passing arguments to \link{fpr_kable}
#'
#' @return Dataframe
#' @seealso \link{fpr_kable}
#' @export
#'
#' @examples
fpr_table_fish_site <- function(dat = tab_fish_sites_sum, sit = my_site, ...){
  dat %>%
    tidyr::separate(site, into = c('site_id', 'location', 'ef'), remove = F) %>%
    dplyr::filter(site_id == sit) %>%
    dplyr::select(site, passes, ef_length_m, ef_width_m, area_m2, enclosure) %>%
    fpr_kable(caption_text = paste0('Fish sampling site summary for ', sit, '.'), scroll = F, ...)
}


#' Summary table for fish densities within individual sites
#'
#' @param dat Dataframe.  Defaults to fish_abund. Must have columns site, local_name, species_code,
#' life_stage, catch, density_100m2, nfc_pass
#'
#' @param sit Integer.  Site ID.  Defaults to my_site
#' @param ... Not used. Open for passing arguments to \link{fpr_kable}
#' @seealso \link{fpr_kable}
#'
#' @return
#' @export
#'
#' @examples
fpr_table_fish_density <- function(dat = fish_abund, sit = my_site, ...){
  dat %>%
    dplyr::filter(site == sit) %>%
    dplyr::select(
      local_name,
      species_code,
      life_stage,
      catch,
      density_100m2,
      nfc_pass) %>%
    fpr_kable(caption_text = paste0('Fish sampling density results summary for ', sit, '.'),
              footnote_text = 'nfc_pass FALSE means fish were captured in final pass indicating more fish of this species/lifestage may have remained in site.
              Mark-recaptured required to reduce uncertainties.',
              scroll = F,
              ...)
}

####--------------phase1 summary tables troubleshoot--------------------------
#' Print out test table
#'
#' @param tab_sum Dataframe formatted by fpr_table_cv_detailed.
#' @param comments Dataframe formatted by fpr_table_cv_detailed_comments
#' @param photos test
#' @param gitbook_switch test
#'
#' @return html table with photo as footnotes
#' @export
#'
#' @examples
fpr_test <- function(tab_sum,
                     comments,
                     photos,
                     gitbook_switch = gitbook_on){
  output <- kable(tab_sum, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
                                            '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            photos[[1]]), notation = 'none')
  if(gitbook_switch){output
  }else output %>%
    kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
}
