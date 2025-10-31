#' Sort in directories and rename photos base on field form column or photo tag.
#'
#' Sort in directories and rename photos loaded to Mergin as per the column they were taken for (ex. `photo_downstream`) or the "tag"
#' input for general "extra*" photos (ex. photo_extra1_tag). Renamed photos have strings appended to the end of the existing file name.
#'
#' @param dat Data frame to pull out the site names from. Defaults to a dataframe named `form_pscis`
#' @param col_directories The bare (unquoted) name of the column containing the parameter you want to use to build directories
#' for storing photos. Defaults to `site_id`
#' @param dir_from_stub String quoted. Path to the place where resized photos from mergin are stored.
#' @param dir_to_stub String quoted. Path to the place where directories will be burned (if not already present) and
#' renamed photos from mergin will be stored.
#' @param ... Unused.  For passing params to \link{fpr_photo_folders}
#' @param col_string_add Logical. Should the contents of one of the columns in `dat` be appended to the photo name
#' before the standard string additions. Defaults to `FALSE` for the `form_pscis` use case.  Likely will be `TRUE`
#' for `form_fiss_site`
#' @param col_string_append The bare (unquoted) name of the column containing the parameter you want to use to add to
#' the name of the photo before the appended string.  Defaults to `local_name`. Used for habitat confirmation sites when there are
#' multiple sites nested within the overall area (ex. small electrofishing sites)
#' @param return_df [logical] Optional. If TRUE return the dataframe used to rename the photos. For QA. Defaults to FALSE.
#'
#' @importFrom dplyr  if_all contains starts_with
#' @importFrom stringr str_detect
#' @importFrom janitor make_clean_names
#' @importFrom fs file_copy
#' @importFrom cli cli_alert_danger cli_alert_info
#'
#' @return Duplicates of existing photos renamed with directories specified by col_directories param
#' @export
#'
#' @examples


fpr_photo_rename <- function(dat = form_pscis,
                             col_directories = site_id,
                             dir_from_stub =NULL,
                             dir_to_stub = NULL,
                             col_string_add = FALSE,
                             col_string_append = location,
                             return_df = FALSE,
                             ...){
  # create new photo directories
  dat %>%
    dplyr::pull({{ col_directories }}) %>%
    purrr::map(fpr::fpr_photo_folders, path = dir_to_stub)

  # make a dataframe ready to rename photos with
  dat2 <- dat %>%
    tidyr::pivot_longer(
      # don't pivot the photo tag names though
      cols = dplyr::starts_with('photo_') & !dplyr::contains('tag'),
      values_to = 'photo_og',
      names_to = 'photo_renamed',
      cols_vary = 'slowest') %>%
    dplyr::filter(!is.na(photo_og)) %>%

    # ------below is for testing if no tag---------------
  # select(site_id, crew_members, mergin_user, contains('photo')) %>%
  #   add_row(site_id = 12345, photo_renamed = 'photo_extra1', photo_extra1_tag = NA_character_,  photo_og = '12345.jpg') %>%
  # ------above is for testing if no tag---------------

  dplyr::mutate(photo_renamed = dplyr::case_when(stringr::str_detect(photo_renamed,'photo_extra') &
                                                   dplyr::if_all(dplyr::contains('photo_extra'), is.na) ~
                                                   'untagged', T ~
                                                   photo_renamed),
#
# below needs to be generalized so we can have any number of "photo_extra#" columns and they are tagged accordingly.
photo_renamed = dplyr::case_when(stringr::str_detect(photo_renamed, 'photo_extra1') ~
                                   janitor::make_clean_names(photo_extra1_tag, allow_dupes = T, sep_out = ''),
                                 stringr::str_detect(photo_renamed, 'photo_extra2') ~
                                   janitor::make_clean_names(photo_extra2_tag, allow_dupes = T, sep_out = ''),
                                 stringr::str_detect(photo_renamed, 'photo_extra3') ~
                                   janitor::make_clean_names(photo_extra3_tag, allow_dupes = T, sep_out = ''),
                                 stringr::str_detect(photo_renamed, 'photo_extra4') ~
                                   janitor::make_clean_names(photo_extra4_tag, allow_dupes = T, sep_out = ''),
                                 stringr::str_detect(photo_renamed, 'photo_extra5') ~
                                   janitor::make_clean_names(photo_extra5_tag, allow_dupes = T, sep_out = ''),
                                 T ~ photo_renamed),
                photo_renamed = stringr::str_replace_all(photo_renamed, 'photo_', ''))
  # generalize above

  # if(identical(col_string_add, FALSE)){
  #   dat3 <- dat2 %>%
  #     dplyr::mutate(photo_renamed = paste0(dir_to_stub,
  #                                          {{ col_directories }},
  #                                          '/',
  #                                          tools::file_path_sans_ext(basename(photo_og)),
  #                                          '_',
  #                                          photo_renamed,
  #                                          # stringr::str_extract(photo_renamed, '_.*$'),
  #                                          '.JPG'),
  #                   photo_og = paste0(dir_from_stub, basename(photo_og))
  #     )
  # }else dat3 <- dat2 %>%
  #   dplyr::mutate(photo_renamed = paste0(dir_to_stub,
  #                                        {{ col_directories }},
  #                                        '/',
  #                                        tools::file_path_sans_ext(basename(photo_og)),
  #                                        '_',
  #                                        {{ col_string_append }},
  #                                        '_',
  #                                        photo_renamed,
  #                                        # stringr::str_extract(photo_renamed, '_.*$'),
  #                                        '.JPG'),
  #                 photo_og = paste0(dir_from_stub, basename(photo_og))
  #   )
  if(identical(col_string_add, FALSE)){
    dat3 <- dat2 |>
      dplyr::mutate(photo_renamed = fs::path(dir_to_stub,
                                           {{ col_directories }},
                                           paste0(tools::file_path_sans_ext(basename(photo_og)),
                                           '_',
                                           photo_renamed),
                                           ext = "JPG"),
                    photo_og = fs::path(dir_from_stub, basename(photo_og))
      )
  }else dat3 <- dat2 |>
    dplyr::mutate(photo_renamed = fs::path(dir_to_stub,
                                         {{ col_directories }},
                                         paste0(tools::file_path_sans_ext(basename(photo_og)),
                                         '_',
                                         {{ col_string_append }},
                                         '_',
                                         photo_renamed),
                                         ext = "JPG"),
                  photo_og = fs::path(dir_from_stub, basename(photo_og))
    )

  # fs::file_copy(
  #   path = dat3 |> dplyr::pull(photo_og),
  #   new_path = dat3 |> dplyr::pull(photo_renamed),
  #   overwrite = TRUE
  # )

  paths_from <- dat3 |> dplyr::pull(photo_og)
  paths_to   <- dat3 |> dplyr::pull(photo_renamed)

  for (i in seq_along(paths_from)) {
    tryCatch(
      fs::file_copy(paths_from[i], paths_to[i], overwrite = TRUE),
      error = function(e) {
        cli::cli_alert_danger(paste0("Failed to copy: ", paths_from[i]))
        cli::cli_alert_info(e$message)
      }
    )
  }

  if(return_df){
    dat3 |>
      dplyr::select({{ col_directories }}, contains('photo'))
  }
  # dat3 %>% select(site_id, crew_members, mergin_user, contains('photo')) %>% slice_tail(n=6)
}
