# functions for using within the text of the report


#' Pull info about a site
#'
#' @param dat Dataframe to pull info from. Defaults to object pscis_phase2 built in tables.R
#' @param col_filter String name of column on which to filter dat. Defaults to `pscis_crossing_id`.
#' @param site Numeric value of site of which to filter dat. Defaults to object `my_site`.
#' @param col_pull String name of column of which to pull from filtered `dat`. Defaults to `stream_name`
#'
#' @return
#' @export
#'
#' @examples
fpr_my_pscis_info <- function(dat = pscis_phase2,
                              col_filter = pscis_crossing_id,
                              site = my_site,
                              col_pull = stream_name){
  dat %>%
    dplyr::mutate(barrier_result = stringr::str_to_lower(barrier_result)) %>%
    dplyr::filter({{ col_filter}} == site) %>%
    dplyr::pull({{ col_pull }})
}

#' Extract info from habitat_confirmations_priorities spreadsheet
#'
#' @param dat Dataframe. Defaults to habitat_confirmations_priorities
#' @param sit Integer. Corresponds to the PSCIS site id. Defaults to my_site defined in envrionment
#' @param loc String. Either 'us' (upstream) or 'ds' (downstream). Used quotes.
#' @param col_pull String. Tidyselect format. Column name for information you want to pull.
#'
#' @return String
#' @export
#'
#' @examples fsr_my_priority_info()
fpr_my_priority_info <- function(dat = habitat_confirmations_priorities,
                                 sit = my_site,
                                 loc = 'us',
                                 col_pull = hab_value){
  dat %>%
    dplyr::filter(site == sit & location == loc) %>%
    dplyr::pull({{ col_pull }})
}


#' Build the title for the appendices
#'
#' @param site Numeric value of site.  Usually PSCIS ID. Defaults to object my_site.
#' @param site2 Numeric value of site 2. Defaults to NULL.
#' @param site3 Numeric value of site 3. Defaults to NULL.
#'
#' @return
#' @export
#'
#' @examples fpr_appendix_title(site = 999)
fpr_appendix_title <- function(site = my_site,
                               site2 = NULL,
                               site3 = NULL){
  if(is.null(site2) & is.null(site3)){
  paste0('# ', fpr_my_pscis_info(), ' - ', site, ' - Appendix {-}')
  }else if(is.null(site3)){ paste0('# ' , fpr_my_pscis_info(), ' - ', site, ' & ', site2, ' - Appendix {-}')
  }else paste0('# ', fpr_my_pscis_info(), ' - ', site, ' & ', site2, ' & ', site3, ' - Appendix {-}')
}

#' Filter the bcfishpass.crossings table export by PSCIS stream_crossing_id
#'
#' @param dat Dataframe. Defaults to bcfishpass filtered by Phase 2 sites only
#' @param site Numeric PSCIS stream_crossing_id. Defaults to value defined by my_site
#' @param round_dig Integer for how many numbers to round to. Defaults to 0.
#' @param col_to_filter String value of column to filter
#' @param col_to_pull String vlaue of column to pull
#'
#' @return Vector
#' @export
#'
#' @examples
fpr_my_bcfishpass <- function(dat = bcfishpass,
                              site = my_site,
                              col_filter = stream_crossing_id,
                              col_pull = stream_order,
                              round_dig = 0){
  dat %>%
    dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, round_dig)) %>%
    dplyr::filter({{col_filter}} == site) %>%
    dplyr::distinct({{col_filter}} , .keep_all = T) %>% #deals with duplicates
    dplyr::pull({{col_pull}})
}


#' PUll out watershed characteristics
#'
#' @param site
#' @param col
#'
#' @return
#' @export
#'
#' @examples
fpr_my_wshd <- function(site = my_site, col = area_km){
  wshds %>%
    filter(stream_crossing_id == site) %>%
    pull({{ col }})
}

#' Pull out fish species long form names from our fish species codes in bcfishpass export
#'
#' @param ... Parameters to pass to fpr_my_bcfishpass
#' @param col_pull Name of column to pull from bcfishpass. Defaults to observedspp_upstr. observedspp_dnstr is the other option
#' @param sp_to_ignore Vector of strings representing species codes to ignore in output. Defaults to SST and TR as these seem a bit ambigous.
#'
#' @return Vector
#' @export
#'
#' @examples
fpr_my_fish_sp <- function(...,
                           col_pull = observedspp_upstr,
                           sp_ignore = c('SST','TR', 'SP')){
  # col_to_pull <-  sym(col_to_pull)
  str_to_pull <- stringr::str_replace_all(
    (fpr_my_bcfishpass(..., col_pull = {{col_pull}})),
    c("\\{" = "","\\}" = "")) %>%
    strsplit(., ",") %>%
    unlist()
  fishbc::freshwaterfish %>%
    dplyr::filter(Code %in% str_to_pull &
                    !Code %in% sp_ignore) %>%
    dplyr::pull(CommonName) %>%
    stringr::str_to_lower() %>%
    knitr::combine_words()
}


#' Print link to map based on watershed name and 50k grid found in bcfishpass
#'
#' @param wshd String identifying the watershed.  Not standardized. Must match folders at https://hillcrestgeo.ca/outgoing/fishpassage/projects/
#'
#' @return Stirng with the url for the map where the crossing is located
#' @export
#'
#' @examples
fpr_my_mapsheet <- function(wshd = 'bulkley'){
  paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/', wshd, '/FishPassage_', fpr_my_bcfishpass(col_pull = dbm_mof_50k_grid), '.pdf')
}




##transpose the data so you can get ranges and filter
#' Extract habitat details from hab_site dataframe
#'
#' @param dat Dataframe. Defaults to hab_site.
#' @param sit Integer. Corresponds to the PSCIS site id. Defaults to my_site defined in envrionment
#' @param loc String. Either 'us' (upstream) or 'ds' (downstream). Use quotes.
#' @param row String in quotes indicating name of column (actually a row at this point) to pull out.
#'
#' @return Sring or number from hab data
#' @export
#'
#' @examples
fpr_my_habitat_info <- function(dat = hab_site,
                                sit = my_site,
                                loc = 'us',
                                col_pull = 'avg_channel_width_m'){
  dat %>%
    filter(site == sit & location == loc) %>%
    select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    # mutate(rowname = stringr::str_replace_all(rowname, '_', ' ')) %>%
    filter(rowname == col_pull) %>%
    pull(V1)
}

#' Pull out dominant and subdominant cover types
#'
#' @param dat Dataframe. Defaults to hab_site.
#' @param sit Integer. Corresponds to the PSCIS site id. Defaults to my_site defined in envrionment
#' @param cover_type
#' @param loc String. Either 'us' (upstream) or 'ds' (downstream). Use quotes.
#'
#' @return
#' @export
#'
#' @examples
fpr_my_habitat_cover <- function(dat = hab_site,
                                 sit = my_site,
                                 loc = 'us',
                                 cover_type = 'dominant'){
  dat2 <- dat %>%
    dplyr::filter(site == sit & location == loc ) %>%
    dplyr::select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%  # as_tibble() %>%
    tibble::rownames_to_column() %>%
    mutate(rowname = stringr::str_replace_all(rowname, '_', ' ')) %>%
    dplyr::filter(V1 == cover_type) %>%
    dplyr::pull(rowname)
  if(identical(cover_type,'sub-dominant')){
    dat2 <- dat2 %>%
      knitr::combine_words()
  }
  dat2
}


#' Sentence to describe survey length of your site.
#'
#' No hard stop at the end of the sentence so we can reference the photos.
#'
#' @param loc String in quotes which is either 'us' upstream or 'ds' downstream.  Defaults to 'us'
#' @param sit Integer. Corresponds to the PSCIS site id. Defaults to my_site defined in environment
#'
#' @return String sentence with the direction and distance surveyed.
#' @export
#'
#' @examples fpr_my_survey_dist()
fpr_my_survey_dist <- function(loc = 'us', sit = my_site){

  seg1 <- 'The stream was surveyed '

  if(identical(loc, 'us')){
    seg2 <- 'upstream '
    }else seg2 <- 'downstream '

  seg3 <- 'from the culvert for '

  seg4 <- fpr::fpr_my_priority_info(sit = sit, loc = loc, col_pull = 'length_surveyed')

  seg5 <- 'm '

  paste0(seg1, seg2, seg3, seg4, seg5)
}

#' Sentence to describe the dominant and subdominant cover types
#'
#' No hard stop at the end of the sentence so we can reference the habitat table.
#'
#' @param loc String in quotes which is either 'us' upstream or 'ds' downstream.  Defaults to 'us'
#' @param sit Integer. Corresponds to the PSCIS site id. Defaults to my_site defined in environment
#'
#' @return String sentence with the cover types described
#' @export
#'
#' @examples fpr_my_cover_sentence()
fpr_my_cover_sentence <- function(loc = 'us', sit = my_site){
  seg1 <- 'Total cover amount was rated as '

  seg2 <- fpr::fpr_my_habitat_info(sit = sit, loc = loc, col_pull = 'total_cover')

  seg3 <- ' with '

  seg4 <- fpr::fpr_my_habitat_cover(sit = sit, loc = loc, cover_type = 'dominant')

  seg5 <- ' dominant. Cover was also present as '

  seg6 <- fpr::fpr_my_habitat_cover(sit = sit, loc = loc, cover_type = 'sub-dominant')

  seg7 <- '.'

  paste0(seg1, seg2, seg3, seg4, seg5, seg6, seg7)
}

#' Sentence to explain cost to fix site
#'
#' @param dat Dataframe with pscis_crossing_id and cost_est_1000s columns
#' @param col_filter String (unqouted) name of column to filter. Defaults to pscis_crossing_id
#' @param site Integer. Corresponds to the PSCIS site id. Defaults to my_site defined in environment
#'
#' @return String. Formatted number
#' @export
#'
#' @examples
fpr_my_cost_estimate <- function(dat = tab_cost_est_phase2,
                                 col_filter = pscis_crossing_id,
                                 site = my_site,
                                 col_pull = cost_est
                                 ){
 dat %>%
    dplyr::mutate(cost_est = cost_est_1000s * 1000) %>%
    dplyr::filter({{ col_filter }} == site) %>%
    dplyr::distinct({{ col_filter }}, .keep_all = T) %>%
    dplyr::pull({{ col_pull }})
}


