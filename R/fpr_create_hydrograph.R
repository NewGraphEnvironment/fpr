#' Creates hydrographs
#'
#' @param station String (quoted) number of station
#' @param pane_hydat Boolean TRUE if you want a pane layout of all hydrographs
#' @param single_hydat Boolean TRUE if you want a single hydrograph with mean flows
#' @param start_year Specific start year, if not specified, will use the first year of the data
#' @param end_year Specific end year, if not specified, will use the first year of the data
#' @param fig/hydrology_stats_ hydrology stats figure saved to the fig folder
#' @param fig/hydrograph_ hydrograph figure saved to the fig folder
#'
#' @importFrom lubridate year
#' @importFrom tidyhydat hy_daily_flows search_stn_number
#' @importFrom stringr str_to_title
#' @importFrom fasstr plot_data_screening
#' @importFrom ggdark dark_theme_bw
#' @importFrom dplyr mutate group_by summarise
#' @importFrom ggplot2 ggsave ggplot geom_ribbon scale_x_date labs geom_line scale_colour_manual
#' @importFrom cli cli_alert
#' @importFrom chk chk_string chk_flag chk_number
#' @importFrom poisutils ps_error
#'
#' @export
#'
#' @examples \dontrun{fpr_create_hydrograph('08EE004', pane_hydat = FALSE))}

fpr_create_hydrograph <- function(
    station = NULL,
    pane_hydat = TRUE,
    single_hydat = TRUE,
    start_year = NULL,
    end_year = NULL){

  if(is.null(station)){
    poisutils::ps_error('Please provide a station number, for example "08EE004"')
  }

  chk::chk_string(station)
  chk::chk_flag(pane_hydat)
  chk::chk_flag(single_hydat)

  flow_raw <- tidyhydat::hy_daily_flows(station)

  if(is.null(start_year)){
    start_year <- flow_raw$Date %>% min() %>% lubridate::year()
  }

  if(is.null(end_year)){
    end_year <- flow_raw$Date %>% max() %>% lubridate::year()
  }

  chk::chk_number(start_year)
  chk::chk_number(end_year)

  tidyhat_info <- tidyhydat::search_stn_number(station)



##### Hydrograph Pane #####

  ##build caption for the pane figure
  caption_info <- dplyr::mutate(tidyhat_info, title_stats = paste0(stringr::str_to_title(STATION_NAME),
                                                            " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                            " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", start_year,
                                                            # FIRST_YEAR, ##removed the default here
                                                            " to ",end_year, "."))

  hydrograph1_stats_caption <- caption_info$title_stats



  if (pane_hydat == TRUE){
    #Create pane of hydrographs with "Mean", "Minimum", "Maximum", and "Standard Deviation" flows
    hydrograph_stats_print <- fasstr::plot_data_screening(station_number = station, start_year = start_year,
                                                          include_stats = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
                                                          plot_availability = FALSE)[["Data_Screening"]] + ggdark::dark_theme_bw() ##first version is not dark
    hydrograph_stats_print

    #Save hydrograph pane
    ggplot2::ggsave(plot = hydrograph_stats_print, file=paste0("fig/hydrology_stats_", station, ".png"),
           h=3.4, w=5.11, units="in", dpi=300)

    cli::cli_alert(hydrograph1_stats_caption)
  }





##### Single Hydrograph  #####

  ##build caption for the single figure
  caption_info2 <- dplyr::mutate(tidyhat_info, title_stats2 = paste0(stringr::str_to_title(STATION_NAME),
                                                                   " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                                   " Lon ",round(LONGITUDE,6), "). Available mean daily discharge data from ", start_year,
                                                                   # FIRST_YEAR, ##removed the default here
                                                                   " to ",end_year, "."))

  hydrograph1_stats_caption2 <- caption_info2$title_stats2

  if (single_hydat == TRUE){
    # Create single hydrograph with mean flows from date range
    flow <- flow_raw %>%
      dplyr::mutate(day_of_year = yday(Date)) %>%
      dplyr::group_by(day_of_year) %>%
      dplyr::summarise(daily_ave = mean(Value, na.rm=TRUE),
                daily_sd = sd(Value, na.rm = TRUE),
                max = max(Value, na.rm = TRUE),
                min = min(Value, na.rm = TRUE)) %>%
      dplyr::mutate(Date = as.Date(day_of_year, origin = "2015-12-31"))

    plot <- ggplot2::ggplot()+
      ggplot2::geom_ribbon(data = flow, aes(x = Date, ymax = max,
                                   ymin = min),
                  alpha = 0.3, linetype = 1)+
      ggplot2::scale_x_date(date_labels = "%b", date_breaks = "2 month") +
      ggplot2::labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
      ggdark::dark_theme_bw() +
      ggplot2::geom_line(data = flow, aes(x = Date, y = daily_ave),
                linetype = 1, linewidth = 0.7) +
      ggplot2::scale_colour_manual(values = c("grey10", "red"))
    plot

    ggplot2::ggsave(plot = plot, file=paste0("fig/hydrograph_", station, ".png"),
           h=3.4, w=5.11, units="in", dpi=300)

    cli::cli_alert(hydrograph1_stats_caption2)
  }
}

