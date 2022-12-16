


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

# my_kable <- function(dat, caption_text = '', font = font_set, ...){
#   dat %>%
#     kable(caption = caption_text, booktabs = T) %>%
#     kableExtra::kable_styling(c("condensed", "responsive"),
#                               full_width = T,
#                               font_size = font)
#     # kableExtra::scroll_box(width = "100%", height = "500px")
# }

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




print_tab_cost_mult <- function(dat = tab_cost_rd_mult_report, ...){
  tab_cost_rd_mult_report %>%
  my_kable()
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


####---------------make the report table-----

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



my_pscis_info <- function(dat = pscis_phase2, site = my_site){
  dat %>%
    filter(pscis_crossing_id == site) %>%
    mutate(stream_name = stringr::str_replace_all(stream_name, 'Tributary', 'tributary'))
}





my_watershed_area <- function(dat = wsheds, site = my_site){
  dat %>%
    filter(pscis_crossing_id == my_site) %>%
    pull(area_km)
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



print_tab_summary_bcfp <- function(site = my_site, font = 11, ...){
  make_tab_summary_bcfp(site = site) %>%
    kable(caption = paste0('Summary of fish habitat modelling for PSCIS crossing ', site, '.'), booktabs = T) %>%    #
    kableExtra::add_footnote(c('Model data is preliminary and subject to adjustments.',
                               'Modelled rearing habitat estimates do not currently include linear lengths of centrelines within lakes and wetlands.'), notation = 'symbol') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = font)
}



text_ref_tab_summary_bcfp <-  function(site = my_site){
  paste0('presents preliminary fish passage modelling data for crossing ', site,
         ' with linear length of spawning and rearing habitat estimated for westslope cutthrout trout at ',
         my_bcfishpass(site = site, round_dig = 1) %>% pull(wct_spawning_belowupstrbarriers_km), 'km and ',
         my_bcfishpass(site = site, round_dig = 1) %>% pull(wct_rearing_belowupstrbarriers_km), 'km respectively.')
}











#' Color font in html resulting from rmarkdown inline text
#'
#' @param x String of text to colorize.
#' @param color String in english
#'
#' @return
#' @export
#'
#' @examples fpr_colorize('color this red', 'red')
fpr_colorize <- function(x, color) {
  sprintf("<span style='color: %s;'>%s</span>", color,
          x)
}



#' Make point geopackage layer from dataframe
#'
#' @param dat Dataframe with coordinates in
#' @param gpkg_name String name of gpkg
#' @param utm_zone Numeric value for utm zone
#' @param easting Numeric value for
#' @param northing Numeric value for
#' @param directory String value for
#'
#' @return
#' @export
#'
#' @examples
fpr_gpkg_p <- function(dat,
                            gpkg_name = 'mapping',
                            utm_zone = 10,
                            easting = "utm_easting",
                            northing = "utm_northing",
                            directory = "data/mapping/"){
  dir.create(directory)
  nm <-deparse(substitute(dat))
  dat %>%
    sf::st_as_sf(coords = c(easting, northing), crs = 26900 + utm_zone, remove = F) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_write(paste0(directory, gpkg_name, ".gpkg"), nm, delete_layer = TRUE)
}
