# these are import scripts

#' Trim up an excel worksheet - usually during import
#'
#' @param dat Dataframe
#' @description Trim up a dataframe based on complete rows. Tidy header names
#' @return A dataframe
#'
#' @examples
fpr_sheet_trim <- function(dat) {
  dat %>%
    dplyr::select(1:ncol(dat)) %>% ##get rid of the extra columns because there are remnants way down low I believe
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}

#' Import the pscis template
#'
#' @param workbook_name string value for name of worksheet
#' @description Import the pscis template.  This is a helper for fpr_import_all
#' @return A dataframe
#' @export
#'
#' @examples
#' fpr_import_pscis()
fpr_import_pscis <- function(workbook_name = 'pscis_phase1.xlsm'){ ##new template.  could change file back to .xls
  sig_fig0 <- c('length_or_width_meters')
  sig_fig1 <- c('culvert_slope_percent', 'stream_width_ratio')
  sig_fig2 <- c('outlet_drop_meters')
  readxl::read_excel(path = paste0(getwd(),"/data/", workbook_name),
                     sheet = 'PSCIS Assessment Worksheet') %>%
    # purrr::set_names(janitor::make_clean_names(names(.))) %>%
    fpr_sheet_trim() %>% ##recently added function above and pulled the altools package as it was a week link
    tibble::rowid_to_column() %>%
    dplyr::rename(date = date_of_assessment_yyyy_mm_dd) %>%
    dplyr::filter(!is.na(date)) %>%
    readr::type_convert() %>%  ##guess the type!!
    dplyr:: mutate(date = janitor::excel_numeric_to_date(as.numeric(date)),
                   source = workbook_name,
                   dplyr::across(dplyr::all_of(sig_fig0), round, 0),
                   dplyr::across(dplyr::all_of(sig_fig1), round, 1),
                   dplyr::across(dplyr::all_of(sig_fig2), round, 2),
                   rowid = rowid + 4,
                   pscis_crossing_id = as.numeric(pscis_crossing_id),
                   my_crossing_reference = as.numeric(my_crossing_reference),
                   aggregated_crossings_id = dplyr::case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                                                              my_crossing_reference > 200000000 ~ my_crossing_reference,  ##date based id's are greater than this number
                                                              T ~ my_crossing_reference + 1000000000),
                   time_start = stringr::str_squish(stringr::str_extract(assessment_comment, "[^.|^?]*$")), #assumes the time is positioned after last "."
                   date_time_start = lubridate::ymd_hm(paste0(date, ' ', time_start)),
                   camera_id = stringr::word(crew_members, 1), ##identify who had the camera as this is the first initials in the crew_members
                   site_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                                       T ~ my_crossing_reference)
    )
}

#' Get Names of PSCIS Files
#'
#' @description Reads filenames in your data file for files with the 'pscis' string in their name. Ignores temp files that are there when
#' @return String of pull path name of file by that pattern
#' @export
#'
#' @examples
fpr_pscis_wkb_paths <- function(){
  list.files(path = 'data', pattern = "pscis", all.files = F) %>%
    grep(pattern = '~', invert = T, value = T)
}

#' Import All PSCIS Sheets at Once
#'
#' @return list of tibbles
#' @export
#'
#' @examples
fpr_import_pscis_all <- function(){
  wkbs_paths <- fpr_pscis_wkb_paths()

  pscis_list <- wkbs_paths %>%
    map(fpr_import_pscis) %>%
    purrr::set_names(nm = tools::file_path_sans_ext(wkbs_paths))
}

#' Import habitat priorities csv and assign aggregated crossing id
#'
#' @param input csv to input which has site as a column
#' @param filter_ef_s2 Boolean - Do we filter out electrofishing sites, feature_record_only ie.falls, LWD jams, and sites with multiple cards for different sections surveyed?
#'
#' @return dataframe of the priorities with an aggregated_crossings_id added
#' @export
#'
#' @examples
fpr_import_hab_prior <- function(input = 'data/habitat_confirmations_priorities.csv',
                                 filter_ef_s2 = TRUE){
  hab_con <- readr::read_csv(input,
                             col_types = cols(date = 'D', time_start = 'c')) %>%
    dplyr::mutate(aggregated_crossings_id =
                    dplyr::case_when(site > 200000000 ~ site,
                                     site >= 200000 & site < 200000000 ~ site + 1000000000,
                                     T ~ site),
                  # date = janitor::excel_numeric_to_date(as.numeric(date)),
                  date_time_start = lubridate::ymd_hm(paste0(date, ' ', time_start)),
                  camera_id = stringr::word(crew_members, 1) ##identify who had the camera as this is the first initials in the crew_members)
    )
  if(filter_ef_s2){hab_con <- hab_con %>%
    dplyr::filter(!alias_local_name %ilike% '_ef' &
                    !alias_local_name %ilike% 's2' &
                    !comments %ilike% 'feature_record_only')}
  hab_con
}

#' Import provincial fish data submission template
#'
#' Includes habitat data and fish sampling information
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
fpr_import_hab_con <- function(path = "./data/habitat_confirmations.xls"){
  readxl::excel_sheets(path = path) %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel,
               path = path,
               .name_repair = janitor::make_clean_names) %>%
    purrr::set_names(janitor::make_clean_names(names(.))) %>%
    purrr::map(fpr_sheet_trim) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
    purrr::map(plyr::colwise(type.convert))

}
