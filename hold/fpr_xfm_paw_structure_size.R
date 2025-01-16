#' Determine replacement structure type and size based on measured field metrics.
#' @param dat [data.frame] PSCIS data
#' @param fill_depth_max [numeric] Max amount of fill above the closed bottom structure - after which it is assumed that a large amount
#' of resloping of banks is likely required. Above this depth of fill there are increases in the span required
#' in replacement structures which is determined based on the `fill_depth_mult`.  Default is 3m.
#' @param span_standard [numeric] Standard minimum bridge span used when a clear span is required. Default is 15m based on past experience.
#' @param span_grace [numeric] Percentage (in decimal format) of width that a bridge span should be beyond the channel width.
#' Defaults to 0.5 (ex. 5m of grace consisting of 2.5m on either side for a 10m wide stream). This is a massively
#' simplified way of estimating some level of high flow event intended to just give a span ballpark for rough cost estimates.
#' @param fill_depth_min_ss_large [numeric] Minimum amount of fill depth above which a streambed simulation is preferred if channel width
#' is above the chn_wdth_max_ss_large and below or equal to the chn_wdth_max_ss_large. Default is 4.5m.
#' @param fill_depth_mult [numeric] Amount of span (m) to add for every 1m of fill deeper than `fill_depth_max`. Default is 3
#' because construction usually involves 1.5:1 slope on either side of the stream resulting in 3m more span required for each metre in depth.
#' @param chn_wdth_max_ss [numeric] Maximimum channel width where a streambed simulation is feasible because above this size the
#' substrate placed in the pipe has been known to blow out over time. Defaults to 4m so that a 6m pipe can be installed
#' (assuming span_grace is set to 0.5).
#' @param template [logical] Whether or not this function is used in conjunction with data read in from provincial
#' PSCIS template.  Default is FALSE.  If TRUE -column data populated by [fpr_import_pscis_all()] is used to generate filenames
#' for outputs that are burned to csv.
#'
#' @importFrom dplyr mutate filter select case_when
#' @importFrom plyr round_any
#' @importFrom readr write_csv
#' @importFrom chk chk_numeric chk_not_null chk_data
#' @importFrom fs dir_create
#'
#' @export
#'
#' #' @examples \dontrun{
#' fpr_structure_size_type(dat)
#' }
#'
fpr_xfm_paw_structure_size_type <- function(
    dat = NULL,
    fill_depth_max = 3,
    fill_dpth_min_ss = 5,
    span_standard = 15,
    span_grace = 0.2,
    fill_depth_mult = 3,
    chn_wdth_max_ss_small = 2,
    chn_wdth_max_ss_large = 4.5,

    template = FALSE) {

  chk::chk_not_null(dat)
  chk::chk_data(dat)
  chk::chk_numeric(fill_depth_max)
  chk::chk_numeric(span_standard)
  chk::chk_numeric(span_grace)
  chk::chk_numeric(fill_depth_mult)
  chk::chk_numeric(chn_wdth_max_ss)


  #above this stream width we need have a larger bridge than the standard
  chn_wdth_max <- span_standard - span_standard * span_grace

  dat_cbs_to_span <- dat |>
    dplyr::filter(
      crossing_type == "Closed Bottom Structure"
      & (barrier_result == 'Barrier' | barrier_result == 'Potential')
    )



  str_type <- dat |>
    dplyr::mutate(fill_depth_over = fill_depth_meters - fill_depth_max,
                  crossing_fix = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                  & downstream_channel_width_meters >= chn_wdth_max_ss ~ 'Replace with New Open Bottom Structure',
                                                  barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_character_
                                                  # ,
                                                  # T ~ 'Replace Structure with Streambed Simulation CBS'
                  ),
                  recommended_diameter_or_span_meters = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                                         & downstream_channel_width_meters >= chn_wdth_max_ss_large ~ span_standard,
                                                                         barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_real_,
                                                                         T ~ 3),
                  recommended_diameter_or_span_meters = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                                         & fill_depth_over > 0 & !stringr::str_like(crossing_fix, 'Simulation') ~
                                                                           (span_standard + fill_depth_mult * fill_depth_over),  ##1m more fill = 3 m more bridge
                                                                         T ~ recommended_diameter_or_span_meters),
                  recommended_diameter_or_span_meters = dplyr::case_when(recommended_diameter_or_span_meters < (downstream_channel_width_meters + span_grace)  ##span not need be extended if already 4m bigger than channel width
                                                                         & downstream_channel_width_meters > chn_wdth_max ~
                                                                           (downstream_channel_width_meters - chn_wdth_max) + recommended_diameter_or_span_meters,  ##for every m bigger than a 5 m channel add that much to each side in terms of span
                                                                         T ~ recommended_diameter_or_span_meters),
                  #
                  crossing_fix = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                  & downstream_channel_width_meters > chn_wdth_max_ss_small
                                                  & downstream_channel_width_meters <= chn_wdth_max_ss &
                                                    fill_depth_meters > fill_dpth_min_ss ~ 'Replace Structure with Streambed Simulation CBS',
                                                  T ~ crossing_fix),
                  recommended_diameter_or_span_meters = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                                         & downstream_channel_width_meters > chn_wdth_max_ss_small
                                                                         & downstream_channel_width_meters <= chn_wdth_max_ss &
                                                                           fill_depth_meters > fill_dpth_min_ss ~ chn_wdth_max_ss + chn_wdth_max_ss(chn_wdth_mult),
                                                                         T ~ recommended_diameter_or_span_meters),
                  recommended_diameter_or_span_meters = round(recommended_diameter_or_span_meters)
    )


  ## Extract the pscis phase so we can use it in the file name
  if(template){
    pscis_phase <- str_type |>
      dplyr::summarise(phase = dplyr::case_when(
        unique(source) == "pscis_phase1.xlsm" ~ "pscis1",
        unique(source) == "pscis_phase2.xlsm" ~ "pscis2",
        unique(source) == "pscis_reassessments.xlsm" ~ "pscis_reassessments")) |>
      dplyr::pull(phase)


    # Ensure output directory exists
    fs::dir_create("data/inputs_extracted")

    ## then burn to a csvs so we can copy and paste into spreadsheet
    str_type |>
      dplyr::select(rowid, aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, source, barrier_result,
                    downstream_channel_width_meters, fill_depth_meters) |>
      readr::write_csv(file = paste0('data/inputs_extracted/str_type_', pscis_phase, '.csv'),
                       na = '')
  }

  return(str_type)

}
