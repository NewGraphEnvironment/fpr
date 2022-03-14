


##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {
  fils <- list.files(path = where, pattern = in_files, recursive = recursive)
  found <- FALSE
  file_cmd <- Sys.which("file")
  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    contents <- readLines(fil)
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    if (length(res) > 0) {
      found <-  TRUE
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
    }
  }
  if (!found) message("(No results found)")
}




fit_to_page <- function(ft, pgwidth = 6.75){

  ft_out <- ft %>% flextable::autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

fit_to_page_landscape <- function(ft, pgwidth = 12){

  ft_out <- ft %>% flextable::autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}


# my_flextable <- function(df,  ...){ ##left_just_col = 2 was an option
#   flextable::autofit(flextable::flextable(
#     df,
#     defaults = list(fontname = 'tahoma'))) %>%
#     flextable::theme_booktabs(fontsize = 8) %>% ##changed from flextable::my_theme_booktabs(fontsize = 9) %>%
#     fit_to_page()
# }



#' Trim up an excel worksheet - usually during import
#'
#' @param dat Dataframe
#' @description Trim up a dataframe based on complete rows and col. Tidy header names
#' @return A dataframe
#' @import
#' magrittr
#' chk
#' janitor
#' dplyr
#' @export
#'
#' @examples
#' fpr_import_pscis()
##function to trim up sheet and get names (was previously source from altools package)
fpr_sheet_trim <- function(dat) {
  dat %>%
    dplyr::select(1:ncol(dat)) %>% ##get rid of the extra columns.
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}



####------my_kable-------------------------------
my_kable_scroll <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font) %>%
    kableExtra::scroll_box(width = "100%", height = "500px")
}

my_tab_overview <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"), full_width = T, font_size = font) %>%
    kableExtra::column_spec(column = c(9), width_min = '1.5in') %>%
    kableExtra::column_spec(column = c(5), width_min = '1.0in', width_max = '1.0in')
}

my_tab_overview_scroll <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"),
                              full_width = T,
                              font_size = font) %>%
    kableExtra::column_spec(column = c(9), width_min = '1.5in') %>%
    kableExtra::column_spec(column = c(5), width_max = '1in') %>%
    kableExtra::scroll_box(width = "100%", height = "500px")
}


my_kable_scroll_no_height <- function(dat, caption_text = ''){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::scroll_box(width = "100%")
}

my_kable <- function(dat, caption_text = '', font = font_set, ...){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font)
    # kableExtra::scroll_box(width = "100%", height = "500px")
}

my_kable <- function(dat,
                     caption_text = '',
                     font = font_set,
                     footnote_text = NULL){
  dat2 <- dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font)
  if(!is.null(footnote_text)){
    dat2 <- dat2 %>%
      kableExtra::footnote(symbol = footnote_text)
  }
  dat2
}

get_img <- function(site = my_site, photo = my_photo){
  jpeg::readJPEG(paste0('data/photos/', site, '/', photo))
}

get_img_path_abs <- function(site = my_site, photo = my_photo){
  stub <- 'https://github.com/NewGraphEnvironment/fish_passage_elk_2020_reporting/blob/master/'
  paste0(stub, 'data/photos/', site, '/', photo)
}

get_img_path <- function(site = my_site, photo = my_photo){
  paste0('data/photos/', site, '/', photo)
}


print_tab_cost_mult <- function(dat = tab_cost_rd_mult_report, ...){
  tab_cost_rd_mult_report %>%
  my_kable()
}

##here is a shot at a function to pull a photo based on a string subset
pull_photo_by_str <- function(site_id = my_site, str_to_pull = 'barrel', ...){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., str_to_pull) %>%
    basename()
}

fpr_appendix_title <- function(site = my_site){
  paste0('# Appendix - ', site, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}


##when we have 2 crosings
fpr_appendix_title2 <- function(site = my_site, site2 = my_site2){
  paste0('# Appendix - ', site, ' & ', site2, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}

##when we have 3 crosings
appendix_title3 <- function(site = my_site, site2 = my_site2, site3 = my_site3){
  paste0('# Appendix - ', site, ' & ', site2, ' & ', site3, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}


appendix_subtitle <- function(){
  paste0('**', my_overview_info() %>% pull(road_name), ' - ', my_overview_info() %>% pull(stream_name), '**')
}


##############this is for making kmls
make_kml_col <- function(df){
  df %>%
    mutate(`PSCIS ID` = as.integer(`PSCIS ID`),
           `Modelled ID` = as.integer(`Modelled ID`),
           color = case_when(Priority == 'high' ~ 'red',
                             Priority == 'no fix' ~ 'green',
                             Priority == 'moderate' ~ 'yellow',
                             T ~ 'grey'),
           # shape = case_when(Priority == 'high' ~ 'http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png',
           #                   Priority == 'no fix' ~ 'http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png',
           #                   Priority == 'moderate' ~ 'http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png',
           #                   T ~ 'http://maps.google.com/mapfiles/kml/pushpin/wht-pushpin.png'),
           shape = case_when(Priority == 'high' ~ 'http://maps.google.com/mapfiles/kml/paddle/red-blank.png',
                             Priority == 'no fix' ~ 'http://maps.google.com/mapfiles/kml/paddle/grn-blank.png',
                             Priority == 'moderate' ~ 'http://maps.google.com/mapfiles/kml/paddle/ylw-blank.png',
                             T ~ 'http://maps.google.com/mapfiles/kml/paddle/wht-blank.png'),
           color = plotKML::col2kml(color),
           site_id = case_when(!is.na(`PSCIS ID`) ~ paste('PSCIS ', `PSCIS ID`),
                               is.na(`PSCIS ID`) ~ paste0('Modelled ', `Modelled ID`)),
           label = paste0(site_id, '-', Priority),
           `Image link` = case_when(!is.na(`Image link`) ~ cell_spec('crossing', "html", link = `Image link`),
                                    T ~ `Image link`)) %>%
    select(site_id, Priority, label, color, shape, everything())
  # mutate(across(where(is.numeric), round(.,2)))

}

## add a line to the function to make the comments column wide enough
make_html_tbl <- function(df) {
  # df2 <- df %>%
  #   dplyr::mutate(`Image link` = cell_spec('crossing', "html", link = `Image link`))
  df2 <- select(df, -shape, -color, -label) %>% janitor::remove_empty()
  df %>%
    mutate(html_tbl = knitr::kable(df2, 'html', escape = F) %>%
             kableExtra::row_spec(0:nrow(df2), extra_css = "border: 1px solid black;") %>% # All cells get a border
             kableExtra::row_spec(0, background = "yellow") %>%
             kableExtra::column_spec(column = ncol(df2) - 1, width_min = '0.5in') %>%
             kableExtra::column_spec(column = ncol(df2), width_min = '4in')
    )
}

fpr_make_html_tbl <- function(df) {
  # df2 <- df %>%
  #   dplyr::mutate(`Image link` = cell_spec('crossing', "html", link = `Image link`))
  df2 <- select(df, -shape, -label) %>% janitor::remove_empty() #removed color
  df %>%
    mutate(html_tbl = knitr::kable(df2, 'html', escape = F) %>%
             kableExtra::row_spec(0:nrow(df2), extra_css = "border: 1px solid black;") %>% # All cells get a border
             kableExtra::row_spec(0, background = "yellow") %>%
             kableExtra::column_spec(column = ncol(df2) - 1, width_min = '0.5in') %>%
             kableExtra::column_spec(column = ncol(df2), width_min = '4in')
    )
}


openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))

#' Import the pscis template
#'
#' @param workbook_name string value for name of worksheet
#' @description Import the pscis template.  This is a helper for fpr_import_all
#' @return A dataframe
#' @import chk
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
    dplyr::rename(date = date_of_assessment_yyyy_mm_dd) %>%
    dplyr:: mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
    dplyr::filter(!is.na(date)) %>%
    readr::type_convert() %>%  ##guess the type!!
    dplyr::mutate(source = workbook_name) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(sig_fig0), round, 0)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(sig_fig1), round, 1)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(sig_fig2), round, 2)) %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(rowid = rowid + 4,
                  pscis_crossing_id = as.numeric(pscis_crossing_id),
                  my_crossing_reference = as.numeric(my_crossing_reference)
    ) %>%
    dplyr::mutate(
      aggregated_crossings_id = dplyr::case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                                                 my_crossing_reference > 200000000 ~ my_crossing_reference,  ##date based id's are greater than this number
                                                 T ~ my_crossing_reference + 1000000000)
    )
}


# import_pscis_all <- function(){
#   dat1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
#   # filter(!my_crossing_reference %in% dups)
#   dat2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm')
#   dat3 <- import_pscis(workbook_name = 'pscis_reassessments.xlsm')
#   pscis <- bind_rows(
#     dat1,
#     dat2,
#     dat3
#   )
#   all <- list(dat1, dat2, dat3, pscis) %>%
#     purrr::set_names(c('pscis_phase1', 'pscis_phase2', 'pscis_reassessments', 'pscis_all'))
#   return(all)
# }



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

## we  want to convert our png to jpeg in case we want them for something
fpr_img_resize_convert <- function(img){
  image <- image_read(img)
  image_scaled <- image_scale(image,"1440x1080!")
  image_write(image_scaled, path = paste0(path, '/', tools::file_path_sans_ext(basename(img)), '.JPG'), format = 'jpg')
}

##function that builds the folders
fpr_make_photo_folders <- function(xing){
  dir.create(paste0(getwd(), '/data/photos/', xing))
}


fpr_time_interval_idx <- function(date_input, intervs){
  which(date_input %within% intervs)
}


##get the photo sorting specific metadata from the photos in the file
fpr_photo_sort_metadat <- function(input_file){
  exifr::read_exif(input_file,recursive=T) %>%
    purrr::set_names(., nm = tolower(names(.))) %>%
    select(sourcefile, datetimeoriginal) %>%
    mutate(datetimeoriginal = lubridate::ymd_hms(datetimeoriginal))
}

##get the names of your pscis files
fpr_pscis_wkb_paths <- function(){
  list.files(path = 'data', pattern = "pscis", all.files = F) %>%
    grep(pattern = '~', invert = T, value = T)
}

fpr_import_pscis_all <- function(){
  wkbs_paths <- fpr_pscis_wkb_paths()

  pscis_list <- wkbs_paths %>%
    map(fpr_import_pscis) %>%
    purrr::set_names(nm = tools::file_path_sans_ext(wkbs_paths))
}

fpr_photo_qa <- function(site_id){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
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
    select(-value)
}

##here we stack up and down then side to side for reporting - this works!
fpr_photo_amalg_cv <- function(site_id){
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
  photos_stack1 <-image_append(image_scale(photos_images1, "x420"), stack = T) ##1/3 the width 373.33 and half the original height
  photos_stack2 <- image_append(image_scale(photos_images2, "x420"), stack = T)
  photos_stack <- c(photos_stack1, photos_stack2)
  photos_stacked <- image_append(image_scale(photos_stack), stack = F)
  image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')
}


fpr_import_hab_con <- function(path = "./data/habitat_confirmations.xls"){
  readxl::excel_sheets(path = path) %>%
    purrr::set_names() %>%
    purrr::map(read_excel,
               path = path,
               .name_repair = janitor::make_clean_names) %>%
    purrr::set_names(janitor::make_clean_names(names(.))) %>%
    purrr::map(fpr_sheet_trim) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
    purrr::map(plyr::colwise(type.convert))

}

####---------------make the report table-----
##grab a df with the names of the left hand side of the table
fpr_make_tab_summary <- function(df){
  tab_results_left <- xref_names %>%
    filter(id_side == 1)
  ##get the data
  tab_pull_left <- df %>%
    select(pull(tab_results_left,spdsht)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_names, by = c('rowname' = 'spdsht'))

  tab_results_right <- xref_names %>%
    filter(id_side == 2)

  ##get the data
  tab_pull_right<- df %>%
    select(pull(tab_results_right,spdsht)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_names, by = c('rowname' = 'spdsht'))

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
fpr_make_tab_summary_comments <- function(df){
  df %>%
    # sf::st_drop_geometry() %>%
    select(assessment_comment) %>%
    # slice(1) %>%
    set_names('Comment')
}

####--------------phase1 summary tables--------------------------
fpr_print_tab_summary_all <- function(tab_sum, comments, photos){
  kable(tab_sum, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
                                            '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            photos[[1]]), notation = 'none')
  # kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
}

####--------------phase1 summary tables pdf--------------------------
fpr_print_tab_summary_all_pdf <- function(tab_sum, comments, photos){
  kable(tab_sum, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
                                            '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            photos[[1]]), notation = 'none') %>%
    kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
}

my_overview_info <- function(dat = pscis_phase2, site = my_site){
  dat %>% filter(pscis_crossing_id == site)
}

##transpose the data so you can get ranges and filter
my_habitat_info <- function(dat = hab_site, sit = my_site){
  left_join(
    hab_site %>%
      filter(site == sit & location == 'us') %>%
      select(site, everything()) %>%
      t() %>%
      as.data.frame() %>%  # as_tibble() %>%
      tibble::rownames_to_column() %>%
      rename(us = V1),

    hab_site %>%
      filter(site == sit & location == 'ds') %>%
      select(site, everything()) %>%
      t() %>%
      as.data.frame() %>%  # as_tibble() %>%
      tibble::rownames_to_column() %>%
      rename(ds = V1),
    by = 'rowname'
  ) %>%
    mutate(rowname = stringr::str_replace_all(rowname, '_', ' '))
}

##transpose the data so you can get ranges and filter
my_habitat_info2 <- function(dat = hab_site, sit = my_site,
                             loc = 'us'){
  dat %>%
    filter(site == sit & location == loc) %>%
    select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%  # as_tibble() %>%
    tibble::rownames_to_column() %>%
    rename(v = V1) %>%
    mutate(rowname = stringr::str_replace_all(rowname, '_', ' '))
  # filter(column == row) %>%
  # pull(v)
}

##transpose the data so you can get ranges and filter
my_habitat_info3 <- function(dat = hab_site, sit = my_site,
                             loc = 'us', row = 'site'){
  dat %>%
    filter(site == sit & location == loc) %>%
    select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%  # as_tibble() %>%
    tibble::rownames_to_column() %>%
    # rename(v = V1) %>%
    mutate(rowname = stringr::str_replace_all(rowname, '_', ' ')) %>%
    filter(rowname == row) %>%
    pull(V1)
}

my_pscis_info <- function(dat = pscis_phase2, site = my_site){
  dat %>%
    filter(pscis_crossing_id == site) %>%
    mutate(stream_name = stringr::str_replace_all(stream_name, 'Tributary', 'tributary'))
}


my_bcfishpass <- function(dat = bcfishpass_phase2, site = my_site, round_dig = 0){
  dat %>%
    mutate(across(where(is.numeric), round, round_dig)) %>%
    filter(stream_crossing_id == site) %>%
    distinct(stream_crossing_id, .keep_all = T)
}

# my_bcfishpass <- function(dat = bcfishpass_phase2, site = my_site){
#   dat %>%
#     mutate(across(where(is.numeric), round, 0)) %>%
#     filter(pscis_crossing_id == site) %>%
#     distinct(pscis_crossing_id, .keep_all = T)
# }

my_watershed_area <- function(dat = wsheds, site = my_site){
  dat %>%
    filter(pscis_crossing_id == my_site) %>%
    pull(area_km)
}

##we needed to back off this b/c maps not ready
my_mapsheet <- function(wshd = 'elk'){
  paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/', wshd, '/FishPassage_', my_bcfishpass() %>%
           pull(dbm_mof_50k_grid), '.pdf')
}

# my_mapsheet <- function(){
#   paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/elk/confirmations')
# }

my_priority_info <- function(dat = habitat_confirmations_priorities, sit = my_site, loc = 'us'){
  dat %>%
    filter(site == sit & location == loc)
}




my_cost_estimate <- function(dat = tab_cost_est_phase2, site = my_site){
  dat %>%
    filter(pscis_crossing_id == site) %>%
    distinct(pscis_crossing_id, .keep_all = T)
}

##this will pull out fish species names from our fish species codes
# my_fish_sp <- function(sit = my_site, col_to_pull = quo(observedspp_upstr), df = bcfishpass_phase2){
#   str_to_pull <- stringr::str_replace_all((my_bcfishpass(dat = df, site = sit) %>% pull(!!col_to_pull)), c("\\{" = "","\\}" = "")) %>%
#     strsplit(., ",") %>% unlist()
#   fishbc::freshwaterfish %>%
#     filter(Code %in% str_to_pull &
#              !Code %in% c('SST','TR')) %>%
#     pull(CommonName) %>%
#     stringr::str_to_lower() %>%
#     knitr::combine_words()
# }


##this will pull out fish species names from our fish species codes
# updated with rlang
my_fish_sp <- function(sit = my_site,
                       col_to_pull = 'observedspp_upstr',
                       df = bcfishpass_phase2){
  col_to_pull <-  sym(col_to_pull)
  str_to_pull <- stringr::str_replace_all((my_bcfishpass(dat = df, site = sit) %>% pull(!!col_to_pull)), c("\\{" = "","\\}" = "")) %>%
    strsplit(., ",") %>% unlist()
  fishbc::freshwaterfish %>%
    filter(Code %in% str_to_pull &
             !Code %in% c('SST','TR')) %>%
    pull(CommonName) %>%
    stringr::str_to_lower() %>%
    knitr::combine_words()
}

####-------culvert details summary---------------
fpr_make_tab_cv <- function(dat = pscis){
  tab_culvert_prep <- dat %>%
    select(pscis_crossing_id, continuous_embeddedment_yes_no,
           outlet_drop_meters, diameter_or_span_meters,
           stream_width_ratio, culvert_slope_percent,
           length_or_width_meters,
           final_score, barrier_result)

  names_report <- left_join(
    as_tibble(names(tab_culvert_prep)),
    select(xref_names, spdsht, report),
    by = c('value' = 'spdsht')
  ) %>%
    pull(report)

  tab_culvert <- tab_culvert_prep %>%
    purrr::set_names(nm = names_report)
}

######modelling summary table
####---------------make the report table-----
##grab a df with the names of the left hand side of the table
make_tab_summary_bcfp <- function(dat = bcfishpass,
                                  xref_table = xref_bcfishpass_names,
                                  site = my_site,
                                  col = 'stream_crossing_id',
                                  ...
                                  ){
  col = sym(col)
  df <- dat %>%
    mutate(across(where(is.numeric), round, 1)) %>%
    filter(!!col == site) %>%
    distinct(!!col, .keep_all = T)
  tab_results_left <- xref_table %>%
    filter(id_side == 1) %>%
    arrange(id_join)
  ##get the data
  tab_pull_left <- df %>%
    select(pull(tab_results_left,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_results_right <- xref_table %>%
    filter(id_side == 2)

  ##get the data
  tab_pull_right<- df %>%
    select(pull(tab_results_right,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_joined <- left_join(
    select(left, report, V1, id_join),
    select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Habitat', 'Potential', 'remove', 'Remediation Gain')) %>%
    mutate(Potential = as.numeric(Potential),
           `Remediation Gain` = as.numeric(`Remediation Gain`)) %>%
    mutate(`Remediation Gain (%)` = round(`Remediation Gain`/Potential * 100,0),
           Habitat = stringr::str_replace_all(Habitat, 'Ha', '(ha)'),
           Habitat = stringr::str_replace_all(Habitat, 'Km', '(km)'),
           Habitat = stringr::str_replace_all(Habitat, 'Lakereservoir', 'Lake and Reservoir'),
           Habitat = stringr::str_replace_all(Habitat, 'Spawningrearing ', 'Spawning and Rearing ')) %>%
    select(-remove)
  return(tab_joined)
}



# make_tab_summary_bcfp <- function(dat = bcfishpass,
#                                   xref_table = xref_bcfishpass_names,
#                                   site = my_site
# ){
#   df <- dat %>%
#     mutate(across(where(is.numeric), round, 1)) %>%
#     filter(stream_crossing_id == site) %>%
#     distinct(stream_crossing_id, .keep_all = T)
#   tab_results_left <- xref_table %>%
#     filter(id_side == 1) %>%
#     arrange(id_join)
#   ##get the data
#   tab_pull_left <- df %>%
#     select(pull(tab_results_left,bcfishpass)) %>%
#     # slice(1) %>%
#     t() %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column()
#
#   left <- left_join(tab_pull_left, xref_table, by = c('rowname' = 'bcfishpass'))
#
#   tab_results_right <- xref_table %>%
#     filter(id_side == 2)
#
#   ##get the data
#   tab_pull_right<- df %>%
#     select(pull(tab_results_right,bcfishpass)) %>%
#     # slice(1) %>%
#     t() %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column()
#
#   right <- left_join(tab_pull_right, xref_table, by = c('rowname' = 'bcfishpass'))
#
#   tab_joined <- left_join(
#     select(left, report, V1, id_join),
#     select(right, report, V1, id_join),
#     by = 'id_join'
#   ) %>%
#     select(-id_join) %>%
#     purrr::set_names(c('Habitat', 'Potential', 'remove', 'Remediation Gain')) %>%
#     mutate(Potential = as.numeric(Potential),
#            `Remediation Gain` = as.numeric(`Remediation Gain`)) %>%
#     mutate(`Remediation Gain (%)` = round(`Remediation Gain`/Potential * 100,0),
#            Habitat = stringr::str_replace_all(Habitat, 'Ha', '(ha)'),
#            Habitat = stringr::str_replace_all(Habitat, 'Km', '(km)'),
#            Habitat = stringr::str_replace_all(Habitat, 'Lakereservoir', 'Lake and Reservoir'),
#            Habitat = stringr::str_replace_all(Habitat, 'Spawningrearing ', 'Spawning and Rearing ')) %>%
#     select(-remove)
#   return(tab_joined)
# }


##this is in two places and should not be - see 0355-tables-reporting-html
print_tab_summary_bcfp <- function(site = my_site, font = 11, ...){
  make_tab_summary_bcfp(site = site) %>%
    kable(caption = paste0('Summary of fish habitat modelling for PSCIS crossing ', site, '.'), booktabs = T) %>%    #
    kableExtra::add_footnote(c('Model data is preliminary and subject to adjustments.',
                               'Modelled rearing habitat estimates do not currently include linear lengths of centrelines within lakes and wetlands.'), notation = 'symbol') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = font)
}

##summary table of just the culvert info
print_tab_summary <- function(dat = pscis_phase2, site = my_site, site_photo_id = my_site, font = 11){
  fpr_make_tab_summary(df = dat %>% filter(pscis_crossing_id == site)) %>%
    kable(caption = paste0('Summary of fish passage assessment for PSCIS crossing ', site, '.'), booktabs = T) %>%    #
    kableExtra::add_footnote(label = paste0('Comments: ', dat %>% filter(pscis_crossing_id == site) %>%
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            paste0('![](data/photos/', site_photo_id, '/crossing_all.JPG)')), notation = 'none') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = font)
  # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}

text_ref_tab_summary_bcfp <-  function(site = my_site){
  paste0('presents preliminary fish passage modelling data for crossing ', site,
         ' with linear length of spawning and rearing habitat estimated for westslope cutthrout trout at ',
         my_bcfishpass(site = site, round_dig = 1) %>% pull(wct_spawning_belowupstrbarriers_km), 'km and ',
         my_bcfishpass(site = site, round_dig = 1) %>% pull(wct_rearing_belowupstrbarriers_km), 'km respectively.')
}

##make a function to retrieve the watersheds
fpr_get_watershed <- function(dat){
  mapply(fwapgr::fwa_watershed_at_measure,
         blue_line_key = dat$blue_line_key,
         downstream_route_measure = dat$downstream_route_measure,
         SIMPLIFY = F) %>%
    purrr::set_names(nm = dat$stream_crossing_id) %>%
    discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
    data.table::rbindlist(idcol="stream_crossing_id") %>%
    distinct(stream_crossing_id, .keep_all = T) %>% ##in case there are duplicates we should get rid of
    st_as_sf()
}

# workaround function for watershed size
fpr_wshd_par <- function(site = my_site, col = 'area_km'){
  col = sym(col)
  wshds %>%
    filter(stream_crossing_id == site) %>%
    pull((!!col))
}


fpr_elev_stats <- function(){
  wshds %>%
    pull(stream_crossing_id) %>%
    map(
      function(site_ids){
        wshd <- wshds %>%
          filter(stream_crossing_id == site_ids)

        nh_elmat <- wshd %>%
          elevatr::get_elev_raster(., 14) %>%
          raster::crop(., wshd) %>%
          rayshader::raster_to_matrix()

        nh_elmat[nh_elmat < 100] = NA #ditch values <100m bc must be errors -something is wrong at the mine

        wshd %>%
          mutate(
            elev_min = min(nh_elmat, na.rm = T),
            elev_max = max(nh_elmat, na.rm = T),
            elev_mean = mean(nh_elmat, na.rm = T),
            elev_median = median(nh_elmat, na.rm = T),
            elev_p60 = quantile(nh_elmat, probs = .4, na.rm = T)) #60% of points are greater than this
      }
    ) %>%
    bind_rows()
}

# summary stats for each watershed
fpr_tab_wshd_sum <- function(site_id = NULL){
  wshds_prep <- wshds %>%
    st_drop_geometry() %>%
    select(site = stream_crossing_id, area_km, contains('elev')) %>%
    mutate(elev_min = case_when(site == 62181 |
                                  site == 62182 ~ NA_real_,
                                T ~ elev_min)) %>%
    purrr::set_names(nm = names(.) %>%
                       janitor::make_clean_names(case = 'title') %>%
                       stringr::str_to_title())
  if(!is.null(site_id)){
    wshds_prep <- wshds_prep %>%
      filter(Site == site_id)}
  wshds_prep
}

# flip an image
fpr_flip_img <- function(site = my_site, rotate = 180, ...){
  photo <- pull_photo_by_str(...)
  image_read(paste0('data/photos/', site, '/', photo)) %>%
    image_rotate(rotate) %>%
    image_write(paste0('data/photos/', site, '/rotated_', photo))

}

# make the html tables that we link to in the interactive map.
# we should be storing these outside of the repo.. Oh well
fpr_print_tab_bcfp_html <- function(sites, ...){
  make_tab_summary_bcfp(site = sites) %>%
    kable(caption = paste0('Summary of fish habitat modelling for PSCIS crossing ', sites, '.'), booktabs = T) %>%    #
    kableExtra::add_footnote('Model data is preliminary and subject to adjustments including incorperation of area based estimates.', notation = 'symbol') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 18) %>%
    readr::write_file(., file = paste0("docs/sum/bcfp/", sites, ".html"))
}

fpr_print_tab_cv_html <- function(site){
  fpr_make_tab_summary(df = pscis_all %>% filter(pscis_crossing_id == site)) %>%
    kable(booktabs = T) %>%    #
    kableExtra::add_footnote(label = paste0('Comments: ', pscis_all %>% filter(pscis_crossing_id == site) %>%
                                              distinct(pscis_crossing_id, .keep_all = T) %>% ##might be my_crossing_refe
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 18, html_font = 'helvetica') %>%
    readr::write_file(., file = paste0("docs/sum/cv/", site, ".html"))
}




