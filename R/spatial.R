# mapping functions


#' Make html table from dataframe
#'
#' Usually just a one row tibble is transformed as this is helper function for
#' making kml files from PSCIS spreadsheets.  Other functions for the kml are in the 0110-make-kml.R file
#' in the fish_passage_skeena_2021_reporting repo for now.
#'
#' @param dat Dataframe or tibble which usually only has one row
#' @family spatial
#'
#' @return html object
#' @export
#'
#' @examples
fpr_make_html_tbl <- function(dat) {
  # df2 <- df %>%
  #   dplyr::mutate(`Image link` = cell_spec('crossing', "html", link = `Image link`))
  dat2 <- select(dat, -shape, -color, -label) %>% janitor::remove_empty()
  dat %>%
    mutate(html_tbl = knitr::kable(dat2, 'html', escape = F) %>%
             kableExtra::row_spec(0:nrow(dat2), extra_css = "border: 1px solid black;") %>% # All cells get a border
             kableExtra::row_spec(0, background = "yellow") %>%
             kableExtra::column_spec(column = ncol(dat2) - 1, width_min = '0.5in') %>%
             kableExtra::column_spec(column = ncol(dat2), width_min = '4in')
    )
}


#' Retrieve watershed geometry using fwapgr
#'
#' @param dat Dataframe with a stream_crossing_id, blue_line_key and downstream_route_measure defined. Ususally pulled from bcfishpass
#'
#' @return sf object representing watershed areas
#' @export
#'
#' @examples
fpr_sp_watershed <- function(dat){
  mapply(fwapgr::fwa_watershed_at_measure,
         blue_line_key = dat$blue_line_key,
         downstream_route_measure = dat$downstream_route_measure,
         SIMPLIFY = F) %>%
    purrr::set_names(nm = dat$stream_crossing_id) %>%
    discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
    data.table::rbindlist(idcol="stream_crossing_id") %>%
    dplyr::distinct(stream_crossing_id, .keep_all = T) %>% ##in case there are duplicates we should get rid of
    sf::st_as_sf()
}




