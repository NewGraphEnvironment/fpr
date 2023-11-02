# keeping all the photo related functions here


#' Pull names of a photo based on a string in the file name
#'
#' @param site Numeric indicating site id cooresponding to name of directory containing the images
#' @param str_to_pull String value to search for in name of photo. must be unique in the file. Defaults to barrel
#'
#' @return String representing the full path filename of photo that matches
#' @export
#'
#' @examples
fpr_photo_pull_by_str <- function(site = my_site, str_to_pull = 'barrel'){
  list.files(path = paste0(getwd(), '/data/photos/', site), full.names = T) %>%
    stringr::str_subset(., str_to_pull) %>%
    gsub(paste0(getwd(), '/'), '', .)
  # basename()
}

#' Test if a dataframe has rows
#'
#' Helper function for fpr::fpr_photo_qa()
#'
#' @param df Tibble or dataframe to test to see if it has no rows
#'
#' @return Bolean
#' @export
#'
#' @examples
fpr_dat_w_rows <- function(df){
  nrow(df) > 0
}

#' Resize images and convert JPG
#'
#'
#' @param photo String of full path name of photo
#' @param size  String image width and height of photo you want to resize to
#' @param path  String of path to the directory where you want to burn the converted img
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_resize_convert <- function(photo,
                                     size = "1036.8x777.6!",
                                     path = NULL){
  image <- magick::image_read(photo)
  image_scaled <- magick::image_scale(image, size)
  magick::image_write(image_scaled, path = paste0(path, '/', tools::file_path_sans_ext(basename(photo)), '.JPG'), format = 'jpg')
}

#' Resize batch of images and convert JPG
#'
#'
#' @param dir_source String full path name of directory where photos are at
#' @param dir_target  String full path name of directory where photos are to be copied to
#' @param ...  Not used. For passing alternative destination photo size as string to \link{fpr_photo_resize_convert}
#'
#' @return Photos resized with `.JPG` extension
#' @export
#'
#' @examples
fpr_photo_resize_batch <- function(dir_source = NULL,
                                    dir_target = NULL,
                                    ...){
  filestoconvert <- list.files(path = dir_source,
                               full.names = T)

  filestoconvert %>%
    purrr::map(fpr::fpr_photo_resize_convert,
               path = dir_target
    )
}

#' Build directories where the photos will be placed based on a list of site ids
#'
#' @param site String - number of site to build folder for
#' @param path String - path stub where photos directories are held
#'
#' @return Makes directories
#' @export
#'
#' @examples
fpr_photo_folders <- function(site, path = 'data/photos/'){
  dir.create(paste0(path, site))
}


#' Get the index of a time according to which interval it falls within
#'
#' For sorting photos into folders based on time they were taken cross referenced to field notes on site cards
#'
#' @param date_input POSIXct date time
#' @param intervs Formal class Interval
#' @export
#'
#' @return Number representing index
#'
#' @examples
fpr_photo_interval_idx <- function(date_input, intervs){
  which(date_input %within% intervs)
}


#' Get photo sorting specific metadata from the photos in the file
#'
#' @param input_file String full path to directory containing photos we need sorted by time
#' @importFrom magrittr %>%
#' @return A tibble
#' @export
#'
#' @examples
fpr_photo_sort_metadat <- function(input_file){
  exifr::read_exif(input_file,recursive=T) %>%
    purrr::set_names(., nm = tolower(names(.))) %>%
    dplyr::select(sourcefile, datetimeoriginal) %>%
    dplyr::mutate(datetimeoriginal = lubridate::ymd_hms(datetimeoriginal))
}


#' Make dataframe of all the data necessary to sort photos into site folders based on time.
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_time_prep <- function(){
  dplyr::bind_rows(
    fpr_import_pscis_all() %>%
      dplyr::bind_rows() %>% #this is pscis_all
      dplyr::distinct(aggregated_crossings_id, .keep_all = T) %>%   ##remove duplicates in the pscis files
      dplyr::select(rowid,
                    aggregated_crossings_id,
                    pscis_crossing_id,
                    my_crossing_reference,
                    date,
                    time_start,
                    date_time_start,
                    camera_id),

    fpr_import_hab_prior(filter_ef_s2 = FALSE) %>% #hab con files
      dplyr::select(aggregated_crossings_id,
                    site,
                    date,
                    time_start,
                    date_time_start,
                    camera_id)
  ) %>%
    # we need to have a column that identifies our initial folders correctly so we use "site"...
    dplyr::mutate(site = case_when(!is.na(site) ~ site,
                                   is.na(site) & !is.na(my_crossing_reference) ~ my_crossing_reference,
                                   T ~ pscis_crossing_id)
    )
}

#' Make plan for which folder to put each photo in based on surveyor and time
#'
#' Time is input into PSCIS submission template in comments after last period
#' Time is input into habitat_confirmation_priorities.csv template within a time_start column
#' This function assumes you deine the folders you want to
#'
#' @param surveyor String with the initials of the surveyor input first into spreadsheet because they had the camera. This must
#' also be the name of the folder in which the raw photos are stored before transfer.
#'
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_sort_plan <- function(surveyor){

  photo_time_prep <- fpr_photo_time_prep()

  time_surveyor_prep <- photo_time_prep %>%
    dplyr::filter(camera_id == surveyor) %>%
    dplyr::arrange(date_time_start) %>%
    tibble::rowid_to_column(var = 'time_idx_start') %>%
    mutate(time_idx_end = time_idx_start + 1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(date_time_start))


  time_surveyor <- left_join(
    time_surveyor_prep,
    select(time_surveyor_prep, time_idx_start, date_end = date, time_end = time_start),
    by = c('time_idx_end' = 'time_idx_start')
  ) %>%
    mutate(
      ##we have no value for the last date and time so need to insert -- #used 11pm.  Might mess with things!!!!!!!!!!!!!!!
      time_end = case_when(time_idx_end == max(time_idx_end) ~ '23:00', T ~ time_end),
      date_end = case_when(time_idx_end == max(time_idx_end) ~ date, T ~ date_end),
      date_time_end = lubridate::ymd_hm(paste0(date_end, ' ', time_end)),
      time_interv = interval(date_time_start, date_time_end))

  photo_meta_surveyor <- photo_meta %>%
    filter(camera_id == surveyor)

  intervs_surveyor <- time_surveyor %>%
    pull(time_interv)

  photo_folders_idx <- photo_meta_surveyor %>%
    pull(datetimeoriginal) %>% #this is "photo_time_lst"
    purrr::map(fpr::fpr_photo_interval_idx, intervs = intervs_surveyor) %>%
    purrr::set_names(nm = photo_meta_surveyor$rowid) %>%
    tibble::as_tibble_col(column_name = "site_idx")

  photo_meta_tojoin <- bind_cols(
    photo_meta_surveyor,
    photo_folders_idx
  ) %>%
    #filter out .mov
    filter(!sourcefile %ilike% '.mov') %>%
    mutate(site_idx = as.numeric(site_idx))  ##this will only work when we only have one result per photo! bound to break

  ##join on the name of the folder we paste to
  photo_folder_targets <- left_join(
    photo_meta_tojoin,
    time_surveyor %>% select(time_idx_start, site),
    by = c('site_idx' = 'time_idx_start')  ##rememberthat rowid is for the input excel files!!
  ) %>%
    #make a column to hold all the ids.  there are no duplicates in this dataset
    dplyr::mutate(
      photo_basename = basename(sourcefile),
      photo_fullname = paste0(camera_id, '_', photo_basename))
}

#' Prep for fpr_photo_qa which will QA photos
#'
#' Ensure that there are 6 named photos and no duplicates named
#'
#' @param site_id Numeric value of site corresponding to folder name
#'
#' @return
#'
#' @examples
fpr_photo_qa_prep <- function(site_id){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = F) %>%
    stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet') %>%
    as_tibble() %>%
    mutate(x = case_when(
      value %ilike% 'road' ~ 'road',
      value %ilike% 'inlet' ~ 'inlet',
      value %ilike% 'upstream' ~ 'upstream',
      value %ilike% 'barrel' ~ 'barrel',
      value %ilike% 'outlet' ~ 'outlet',
      value %ilike% 'downstream' ~ 'downstream'
    )) %>%
    tidyr::pivot_wider(names_from = x) %>%
    dplyr::mutate(site = site_id)
}

#' QA Photos
#'
#'Ensure that there are 6 named photos and no duplicates named.
#'
#'
#'#' find sites with 0 Rows.
#' fpr::fpr_photo_qa()[fpr::fpr_photo_qa() %>%
#' map(fpr::fpr_dat_w_rows) %>%
#' grep(pattern = F) ] %>%
#' names(.) %>%
#' unique(.)
#'
#'
#' See and fix duplicates with
#' fpr::fpr_photo_qa() %>%
#' data.table::rbindlist(fill = T)
#'
#' After dups are fixed then Query for missing values with
#' dplyr::bind_rows() %>%
#' dplyr::filter(dplyr::if_any(dplr::everything(), is.na))
#'
#' @param dat Dataframe to pull site IDs from that coincide with photo folders. Defaults to pscis_phase1
#' @param col Column to pull to get site IDs. Defaults to my_crossing_reference
#'
#' @return List of tibbles for each sites.
#' @export
#'
#' @examples
fpr_photo_qa <- function(dat = pscis_all,
                         col = site_id){
  dat %>%
    dplyr::arrange({{col}}) %>%
    dplyr::pull({{col}}) %>%
    purrr::map(fpr_photo_qa_prep) %>%
    purrr::set_names(
      nm = dat %>%
        dplyr::arrange({{col}}) %>%
        dplyr::pull({{col}})
    )
}


#' Amalgamate 6 photos into 1
#'
#' Here we stack up and down then side to side for reporting.  There must be all 6 photos
#' present, only 1 of each and within the file names of those photos must be the strings
#' containing upstream, downstream, inlet, outlet, barrel, road
#'
#' @param  site_id Numeric value of site corresponding to the directory name that holds the photos
#' which include 'road', 'inlet', 'upstream', 'downstream', 'outlet', barrel' in the filenames.
#' @param size String. Dimensions of individual photos. Defaults to "560x420!" to give oveall photo size of 1120 x 1260.
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_amalg_cv <- function(site_id, size = "560x420!"){
  photos_images1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'upstream|road|inlet') %>%
    as_tibble() %>%
    mutate(sort = case_when(
      value %ilike% 'road' ~ 1,
      value %ilike% 'inlet' ~ 2,
      value %ilike% 'upstream' ~ 3,
      # value %ilike% 'barrel' ~ 4,
      # value %ilike% 'outlet' ~ 5,
      # value %ilike% 'downstream' ~ 6,
    )) %>%
    arrange(sort) %>%
    pull(value) %>%
    image_read()
  photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'barrel|outlet|downstream') %>%
    as_tibble() %>%
    mutate(sort = case_when(
      # value %ilike% 'road' ~ 1,
      # value %ilike% 'inlet' ~ 2,
      # value %ilike% 'upstream' ~ 3,
      value %ilike% 'barrel' ~ 4,
      value %ilike% 'outlet' ~ 5,
      value %ilike% 'downstream' ~ 6,
    )) %>%
    arrange(sort) %>%
    pull(value) %>%
    image_read()
  photos_stack1 <-image_append(image_scale(photos_images1, size), stack = T) ##1/3 the width 373.33 and half the original height
  photos_stack2 <- image_append(image_scale(photos_images2, size), stack = T)
  photos_stack <- c(photos_stack1, photos_stack2)
  photos_stacked <- image_append(image_scale(photos_stack), stack = F)
  image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')
}

#' Flip an image
#'
#' @param site_id Numeric indicating site id cooresponding to name of directory containing the images
#' @param rotate Numeric value of the amount of rotation in degrees. Defaults to 180
#' @param str_to_pull Sting value in photo name.  Must be unique in file
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_flip <- function(site_id = my_site, rotate = 180, str_to_pull = 'barrel'){
  photo <- fpr_photo_pull_by_str(site = site_id, str_to_pull = str_to_pull)
  magick::image_read(paste0('data/photos/', site_id, '/', photo)) %>%
    magick::image_rotate(rotate) %>%
    magick::image_write(paste0('data/photos/', site_id, '/rotated_', photo))

}

##back photos to another place.  Going to split into two functions
fpr_photos_backup <- function(filename = 'al'){
  ##get teh name of the folder we are in
  project_name <- basename(dirname(dirname(getwd())))
  ##here we back everything up to the D drive
  dir_backup_prj = paste0("D:/New_Graph/backups/photos/", project_name, "/")
  dir.create(dir_backup_prj)

  dir_backup_photos = paste0("D:/New_Graph/backups/photos/", project_name, "/", filename)
  dir.create(dir_backup_photos)


  ##path to the photos
  path_photos <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", project_name, '/data/photos/', filename)

  filestocopy <- list.files(path = path_photos,
                            full.names = T)

  #copy over the photos in the al folder -- this is done already
  file.copy(from=filestocopy, to=dir_backup_photos,
            overwrite = F, recursive = FALSE,
            copy.mode = TRUE)
}

#' Find photos that should be converted to another file extension
#'
#' @param target String that is path to the directory where the photos are that may need renameing.
#'
#' @return String identifying full path of photos to be renamed.
#' @export
#'
#' @examples
fpr_photo_ext_to_change <- function(target){
  list.files(path = target,
             pattern = '.*\\.(jpg|png|jpeg)$',
             recursive = TRUE,
             ignore.case = F, ##note this is false!!
             full.names = T,
             include.dirs = T)
}

#' Rename file extensions to what they will be after the change
#'
#' @param filenames_to_change String full path names of photos we want to change
#'
#' @return String of changed file name
#' @export
#'
#' @examples
fpr_photo_change_ext <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = '(.jpg|.png|.jpeg)', replacement = '.JPG') #.*\\.(jpg|png|jpeg)$
}

#' Rename photos
#'
#' @param filescopy String full path names of photos we want to change
#' @param filespaste String of changed file name
#'
#' @return Action renameing files
#' @export
#'
#' @examples
fpr_photo_rename_ext <- function(filescopy, filespaste){
  file.rename(from=filescopy, to=filespaste)
}


#' Identify photos that should be copied over into file to upload to PSCIS
#'
#' @param path_to_photo_dir Full path to photo directories that will be copied over
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_paths_to_copy <- function(path_to_photo_dir){
  bind_rows(
    list.files(path = path_to_photo_dir,
               pattern = ".JPG$",
               recursive = TRUE,
               ignore.case = T,
               full.names = T,
               include.dirs = T) %>%
      stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet') %>%
      as_tibble(),
    ##this section will grab up to 4 more photos that have been add with the 'keep' tag (_k_) as they are in the reporting
    list.files(path = path_to_photo_dir,
               pattern = ".JPG$",
               recursive = TRUE,
               ignore.case = T,
               full.names = T,
               include.dirs = T) %>%
      stringr::str_subset(., '_k_') %>%
      as_tibble() %>%
      slice(1:4) ##we needed a way to grab only the first 4 photos that have a _k_ in them or else we get too many photos.
  ) %>%
    pull(value)
}

#' Untested!! Identify photos that should be copied over into a file to upload to the phase 2 PSCIS interface
#' This is every photo not moved by fpr_photo_paths_to_copy
#' adapted from https://stackoverflow.com/questions/56160445/how-to-pass-multiple-necessary-patterns-to-str-subset
#'
#' @param path_to_photo_dir Full path to photo directories that will be copied over
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_paths_to_copy_phase2 <- function(path_to_photo_dir){
  all_photos <- list.files(path = path_to_photo_dir,
                           pattern = ".JPG$",
                           recursive = TRUE,
                           ignore.case = T,
                           full.names = T,
                           include.dirs = T)
  negate_these <- all_photos %>%
    stringr::str_subset(., '_k_') %>%
    as_tibble() %>%
    slice(1:4) ##we needed a way to grab only the first 4 photos that have a _k_ in them or else we get too many photos.
  keep_these <- all_photos %>%
    stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet', negate = T) %>%
    purrr::map(negate_these, stringr::str_subset, string = ., negate = T) %>%
    purrr::reduce(generics::intersect) |>
    as_tibble() %>%
    pull(value)
}


## we have seen an issue with very long photo names getting rejected from PSCIS submisson
## find the long names and truncate them
## hold a record of the orignal and shortened named so we can repeat?
## Maybe not necessary if we run this each time but necessary on this job since we hand bombed earlier (doh)...
#' Document all the photos that are in the directories to csv as a bay to track changes after scripted organization
#'
#' @param path_to_photo_dir Full path to photo directories to be
#' @param full_names whether we should use full names when listing photos
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_document_all <- function(path_to_photo_dir, full_names = T){
  list.files(path = path_to_photo_dir,
             pattern =  '.*\\.(jpg|png|jpeg)$', #".JPG$"
             recursive = TRUE,
             ignore.case = T,
             full.names = full_names, ##this is false
             include.dirs = T) %>%
    as_tibble()
}

#' Sort in directories and rename photos loaded to Mergin as per the column they were taken for (ex. `photo_downstream`)or the "tag"
#' input for general "extra1" photos (ex. photo_extra1_tag). Renamed photos have strings appended after to the end of the existing file.
#'
#' @param dat Data frame to pull out the site names from. Defaults to a dataframe named `form_pscis`
#' @param col_directories The bare (unquoted) name of the column containing the parameter you want to use to build directories
#' for storing photos. Defaults to `site_id`
#' @param dir_from_stub String quoted. Path to the place where resized photos from mergin are stored.
#' @param dir_to_stub String quoted. Path to the place where directories will be burned (if not already present) and
#' renamed photos from mergin will be stored.
#' @param ... Unused.  For passing params to [fpr_photo_folders()]
#' @param col_string_add Logical. Should the contents of one of the columns in `dat` be appended to the photo name
#' before the standard string additions. Defaults to `FALSE` for the `form_pscis` use case.  Likely will be `TRUE`
#' for `form_fiss_site`
#' @param col_string_append The bare (unquoted) name of the column containing the parameter you want to use to add to
#' the name of the photo before the appended string.  Defaults to `local_name`. Used for habitat confirmation sites when there are
#' multiple sites nested within the overall area (ex. small electrofishing sites)
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
                             ...){
  # create new photo directories
  dat %>%
    dplyr::pull({{ col_directories }}) %>%
    purrr::map(fpr::fpr_photo_folders, path = dir_to_stub)

  # make a dataframe ready to rename photos with
  dat2 <- dat %>%
    tidyr::pivot_longer(
      # don't pivot the photo tag names though
      cols = starts_with('photo_') & !contains('tag'),
      values_to = 'photo_og',
      names_to = 'photo_renamed',
      cols_vary = 'slowest') %>%
    dplyr::filter(!is.na(photo_og)) %>%

    # ------below is for testing if no tag---------------
  # select(site_id, crew_members, mergin_user, contains('photo')) %>%
  #   add_row(site_id = 12345, photo_renamed = 'photo_extra1', photo_extra1_tag = NA_character_,  photo_og = '12345.jpg') %>%
  # ------above is for testing if no tag---------------

  dplyr::mutate(photo_renamed = dplyr::case_when(stringr::str_detect(photo_renamed,'photo_extra') &
                                                   if_all(contains('photo_extra'), is.na) ~
                                                   'untagged', T ~
                                                   photo_renamed),

                # below needs to be generalized so we can have any number of "photo_extra#" columns and they are tagged accordingly.
                photo_renamed = dplyr::case_when(stringr::str_detect(photo_renamed, 'photo_extra1') ~
                                                   janitor::make_clean_names(photo_extra1_tag, allow_dupes = T, sep_out = ''),
                                                 stringr::str_detect(photo_renamed, 'photo_extra2') ~
                                                   janitor::make_clean_names(photo_extra2_tag, allow_dupes = T, sep_out = ''),
                                                 T ~ photo_renamed),
                photo_renamed = stringr::str_replace_all(photo_renamed, 'photo_', ''))
  # generalize above

  if(identical(col_string_add, FALSE)){
    dat3 <- dat2 %>%
      dplyr::mutate(photo_renamed = paste0(dir_to_stub,
                                           {{ col_directories }},
                                           '/',
                                           tools::file_path_sans_ext(basename(photo_og)),
                                           '_',
                                           photo_renamed,
                                           # stringr::str_extract(photo_renamed, '_.*$'),
                                           '.JPG'),
                    photo_og = paste0(dir_from_stub, basename(photo_og))
      )
  }else dat3 <- dat2 %>%
  dplyr::mutate(photo_renamed = paste0(dir_to_stub,
                                       {{ col_directories }},
                                       '/',
                                       tools::file_path_sans_ext(basename(photo_og)),
                                       '_',
                                       {{ col_string_append }},
                                       '_',
                                       photo_renamed,
                                       # stringr::str_extract(photo_renamed, '_.*$'),
                                       '.JPG'),
                photo_og = paste0(dir_from_stub, basename(photo_og))
  )

  mapply(file.copy,
         from =  dat3 %>% pull(photo_og),
         to = dat3 %>% pull(photo_renamed),
         overwrite = T,
         copy.mode = TRUE)
  # dat3 %>% select(site_id, crew_members, mergin_user, contains('photo')) %>% slice_tail(n=6)
}

#' Remove duplicated photos before or after photo name appended to end of photo file name by [fpr_photo_rename()]
#'
#' @param dir_target String. Full path name of directory to be scanned for duplicate photos with different names.
#' @param dry_run Logical. Should photos selected by `remove_renamed` be removed or should a dataframe be
#' produced to show the identical photos? Defaults to TRUE.
#' @param min_replicates Integer.  Minimum number of identical photos (based on identical `col_time`, `col_model` and `col_iso` values
#' in photo metadata) that should be identified.  Defaults to 2.  Setting this to higher than 2 is intended for `dry_run` = TRUE
#' as it is likely to remove photos used for more than one thing vs removing renamed photos.
#' @param col_time The bare (unquoted) name of the column containing the exif parameter stamping when photo was created.
#' Defaults to `date_time_original`.
#' @param col_photo_name The bare (unquoted) name of the column containing the exif parameter with full path photo name.
#' Defaults to `source_file`.
#' @param col_model The bare (unquoted) name of the column containing the exif parameter stamping camera type that took the photo.
#' Defaults to `model`.
#' @param col_iso he bare (unquoted) name of the column containing the exif parameter stamping photo iso.
#' Defaults to `iso`.
#' @param remove_renamed Logical. Should renamed photos be removed (based on length of photo name)? Defaults to FALSE so that
#' duplicate photos that have not been renamed are removed.
#' @param ... Not used.
#'
#' @return A dataframe if `dry_run` is TRUE (default) or removal of photos specified through `remove_renamed` param.
#' @export
#'
#' @examples t <- fpr::fpr_photo_remove_dupes(dir_target = 'data/photos/sorted/', min_replicates = 3)
fpr_photo_remove_dupes <- function(dir_target = NULL,
                                   dry_run = TRUE,
                                   min_replicates  = 2,
                                   col_time = date_time_original,
                                   col_photo_name = source_file,
                                   col_model = model,
                                   col_iso = iso,
                                   remove_renamed = FALSE,
                                   ...){
  dat1 <- exifr::read_exif(path = dir_target,recursive=T)  %>%
    janitor::clean_names()  %>%
    dplyr::group_by( {{ col_model }}, {{ col_iso }}, {{ col_time }}) %>%
    dplyr::filter(dplyr::n()>min_replicates-1) %>%
    dplyr::mutate(l = stringr::str_length( {{ col_photo_name }} ))

  if(remove_renamed){
    dat2 <- dat1 %>%
      dplyr::arrange( {{ col_time }}, {{ col_model }}, {{ col_iso }}, desc(l))
  }else if(identical(remove_renamed, FALSE))
    dat2 <- dat1 %>%
      dplyr::arrange( {{ col_time }},{{ col_model }}, {{ col_iso }}, l)

  if(dry_run){
    dat2 %>%
      dplyr::ungroup() %>%
      dplyr::select({{ col_photo_name }},{{ col_time }},{{ col_model }})
  }else if(identical(dry_run, FALSE))
    dat3 <- dat2 %>%
      dplyr::mutate(photo_directory = dirname( {{ col_photo_name }} )) %>%
      dplyr::select(photo_directory,{{ col_photo_name }},{{ col_time }},{{ col_model }}, {{ col_iso }}) %>%
      dplyr::group_split() %>%
      purrr::map(~.x %>% dplyr::slice(1)) %>%
      dplyr::bind_rows() %>%
      dplyr::pull( {{ col_photo_name }}) %>%
      purrr::map(file.remove)

}


