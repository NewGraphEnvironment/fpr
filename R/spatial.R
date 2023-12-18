# mapping functions


#' Make html table from dataframe
#'
#' Usually just a one row tibble is transformed as this is helper function for
#' making kml files from PSCIS spreadsheets.  Other functions for the kml are in the 0110-make-kml.R file
#' in the fish_passage_skeena_2021_reporting repo for now.
#'
#' @param dat Dataframe or tibble which usually only has one row
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


#' Retrieve watershed geometry using fwapg
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

#' Get elevation statistics
#'
#' @param dat sf object of polygons to gather elevation statistics for.  Must contain `stream_crossing_id` column.
#'
#' @return sf object of polygons with elevation details (ie. p60, median, min, max) and median aspect information added.
#' @export
#'
#' @examples
fpr_sp_wshd_stats <- function(dat){
  directions <- tibble::tibble( degree_centre = seq(0, 360 , by=22.5),
                                cardinal = c("N","NNE","NE", "ENE", "E", "ESE", "SE", "SSE",
                                             "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")) %>%
    dplyr::mutate(offset = 11.25) %>%
    dplyr::mutate(degree_max = degree_centre - offset)

  dat %>%
    pull(stream_crossing_id) %>%
    purrr::map(
      function(site_ids){
        wshd <- dat %>%
          filter(stream_crossing_id == site_ids)

        nh_elmat <- wshd %>%
          elevatr::get_elev_raster(., 14) %>%
          raster::crop(., wshd)

        nh_elmat_elev <- nh_elmat %>%
          rayshader::raster_to_matrix()

        nh_elmat_asp <- nh_elmat %>%
          raster::terrain(opt = c('aspect'), unit = 'degrees') %>%
          rayshader::raster_to_matrix()

        # nh_elmat[nh_elmat < 100] = NA #ditch values <100m bc must be errors -something is wrong at the mine

        wshd %>%
          dplyr::mutate(
            elev_min = min(nh_elmat_elev, na.rm = T),
            elev_max = max(nh_elmat_elev, na.rm = T),
            # elev_mean = mean(nh_elmat_elev, na.rm = T),
            elev_median = stats::median(nh_elmat_elev, na.rm = T),
            #60% of points are greater than this
            elev_p60 = stats::quantile(nh_elmat_elev, probs = .4, na.rm = T),
            asp_median = stats::median(nh_elmat_asp, na.rm = T),
            aspect = cut(
              asp_median,
              breaks = c(directions$degree_max, 360),
              labels = c(directions$cardinal)
            )
          ) %>%
          select(-asp_median)
      }
    ) %>%
    dplyr::bind_rows() %>%
    mutate(area_km = round(area_ha/100, 1)) %>%
    mutate(across(contains('elev'), round, 0)) %>%
    arrange(stream_crossing_id)
}

#' Make geopackage in directory called
#'
#' Names the layer the same as the name of the dataframe that is burned
#'
#' @param dat Dataframe with coordinates to burn to geopackage
#' @param gpkg_name String Name of the geopackage
#' @param utm_zone Integer UTM zone
#' @param x String (quoted) representing column name where x coordinates are stored
#' @param y String (quoted) representing column name where y coordinates are stored
#' @param crs Integer destination CRS. Defaults to 4326
#' @param dir String (quoted) representing relative path to directory where geopackage will be burned
#'
#' @return geopackage layer
#' @export
#'
#' @examples
fpr_make_geopackage <- function(dat,
                                dir = "data/fishpass_mapping/",
                                gpkg_name = 'fishpass_mapping',
                                utm_zone = 9,
                                x = 'utm_easting',
                                y = 'utm_northing',
                                crs = 4326){

  dir.create(dir)
  nm <-deparse(substitute(dat))
  dat %>%
    sf::st_as_sf(coords = c(x, y), crs = 26900 + utm_zone, remove = F) %>%
    sf::st_transform(crs = crs) %>%
    sf::st_write(paste0(dir, gpkg_name, ".gpkg"), nm, delete_layer = TRUE)
}


