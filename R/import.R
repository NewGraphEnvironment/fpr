# these are import scripts

#' Trim up an excel worksheet - usually during import
#'
#' @param col_filter_na Logical whether to remove rows if `NA` is present in a specified column. Defaults to false.
#' @param col_filter_na_num Integer specifying column to filter if `NA`s are present. Defaults to 2 because
#' we use that in the fish submission form since `gazetted_name` is required and because first column
#' of location sheet (`step_1_ref_and_loc_info') is populated to row 1500 out of box. Note that this !is.na for col 2 results in some
#' trimming of a couple events on the admin_only_pick_lists.csv
#' @param dat Dataframe
#'
#' @description Select column names based on first row with inputs all across.
#' Trim up the dataframe based on whether there are events in the first column.
#' This is a `date` in the pscis inputs and `reference_number` in the fish submission template
#' @return A dataframe
#' @export
#'
#' @examples
fpr_sheet_trim <- function(dat,
                           col_filter_na = FALSE,
                           col_filter_na_num = 2) {
  dat2 <- dat %>%
    dplyr::select(1:ncol(dat)) %>% ##get rid of the extra columns because there are remnants way down low I believe
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows") %>%
    # remove rows that don't have a value in the first column
    dplyr::filter(!is.na(.[[1]]))

  if(col_filter_na){
    dat2 <- dat2 %>%
      dplyr::filter(!is.na(.[[col_filter_na_num]]))
  }
  dat2
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
#' Includes habitat data and fish sampling information.  Can back up all sheets to csvs. Suggest updateing git index for submission
#' template to to assume-unchanged so that a version of the spreadsheet is stored in the repo but can be updated when it is actually necessary.
#'
#' @param path String location of submission template file populated from
#' https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/fish-and-fish-habitat-data-information/fish-data-submission/submit-fish-data#submitfish
#' @param backup Logical whether to backup all sheets as csvs or not. Defaults to true
#' @param path_backup String indicating directory to create and backup to.  Defaults to 'data/backup/'
#' @param ... Not used.  For passing true or false to col_filter_na and col_filter_na_num from \link{fpr_sheet_trim}
#' @seealso \link{fpr_sheet_trim}
#'
#' @return
#' @export
#'
#' @examples
fpr_import_hab_con <- function(path = "data/habitat_confirmations.xls",
                               backup = TRUE,
                               path_backup = 'data/backup/',
                               ...){
  hab_con <- readxl::excel_sheets(path = path) %>%
    stringr::str_subset("INSTRUCTIONS", negate = T) %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel,
               path = path,
               .name_repair = janitor::make_clean_names) %>%
    purrr::set_names(janitor::make_clean_names(names(.))) %>%
    purrr::map(fpr_sheet_trim, ...) %>%
    purrr::map(plyr::colwise(type.convert))

  if(backup){
    dir.create(path_backup)
    sapply(names(hab_con),
           function (x) readr::write_csv(hab_con[[x]], file=paste0(path_backup, x, ".csv"), na = '' )   )
  }
  hab_con
}
