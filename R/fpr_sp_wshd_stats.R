#' Get elevation statistics.
#'
#' @param dat sf object of polygons to gather elevation statistics for. Must contain
#'   `stream_crossing_id` column.
#'
#' @return sf object of polygons with elevation details (ie. p60, median, min, max)
#'   and median aspect information added.
#' @family spatial
#' @export
#' @importFrom dplyr mutate pull filter bind_rows arrange across contains select
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom raster crop terrain
#' @importFrom rayshader raster_to_matrix
#' @importFrom elevatr get_elev_raster
#' @importFrom stats median quantile
#'
#' @examples \dontrun{wshd <- fpr_sp_wshd_stats(
#' fwapgr::fwa_watershed_at_measure(
#' blue_line_key = 360746095, downstream_route_measure = 100) |>
#' dplyr::mutate(stream_crossing_id = 1)
#' )}
fpr_sp_wshd_stats <- function(dat) {
  directions <- tibble::tibble(
    degree_centre = seq(0, 360, by = 22.5),
    cardinal = c(
      "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
      "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N"
    )
  ) %>%
    dplyr::mutate(
      offset = 11.25,
      degree_max = degree_centre - offset
    )

  dat %>%
    dplyr::pull(stream_crossing_id) %>%
    purrr::map(
      function(site_ids) {
        wshd <- dat %>%
          dplyr::filter(stream_crossing_id == site_ids)

        nh_elmat <- wshd %>%
          elevatr::get_elev_raster(., 14) %>%
          raster::crop(., wshd)

        nh_elmat_elev <- nh_elmat %>%
          rayshader::raster_to_matrix()

        nh_elmat_asp <- nh_elmat %>%
          raster::terrain(opt = "aspect", unit = "degrees") %>%
          rayshader::raster_to_matrix()

        wshd %>%
          dplyr::mutate(
            elev_min = min(nh_elmat_elev, na.rm = TRUE),
            elev_max = max(nh_elmat_elev, na.rm = TRUE),
            elev_median = stats::median(nh_elmat_elev, na.rm = TRUE),
            elev_p60 = stats::quantile(nh_elmat_elev, probs = 0.4, na.rm = TRUE),
            asp_median = stats::median(nh_elmat_asp, na.rm = TRUE),
            aspect = cut(
              asp_median,
              breaks = c(directions$degree_max, 360),
              labels = directions$cardinal
            )
          ) %>%
          dplyr::select(-asp_median)
      }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(area_km = round(area_ha / 100, 1)) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("elev"), round, 0)) %>%
    dplyr::arrange(stream_crossing_id)
}
