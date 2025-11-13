#' Transform PSCIS Assessment Worksheet Data by Calculating Replacement Structure Type and Size
#'
#' This function determines the type and size of the replacement structure for BC Provincial
#' Stream Crossing Inventory System (PSCIS) data based on the barrier result,
#' downstream channel width, and the amount of road fill above the crossing.
#'
#' @param dat [dataframe] A dataframe containing the PSCIS data.
#' @param col_barrier_result [character] A column name specifying the barrier result
#'   (Barrier, Potential, Passable, or Unknown), as a string or tidy-select syntax.
#'   Default is `barrier_result`.
#' @param col_downstream_channel_width_meters [character] A column name specifying
#'   the downstream channel width, as a string or tidy-select syntax.
#'   Default is `downstream_channel_width_meters`.
#' @param col_fill_depth_meters [character] A column name specifying the fill depth
#'   above the crossing, as a string or tidy-select syntax. Default is `fill_depth_meters`.
#' @param col_crossing_fix [character] A column name for the output recommended
#'   crossing fix, as a string or tidy-select syntax. Default is `crossing_fix`.
#' @param col_recommended_diameter_or_span_meters [character] A column name for the
#'   output recommended structure diameter or span (size), as a string or tidy-select
#'   syntax. Default is `recommended_diameter_or_span_meters`.
#' @param fill_dpth [numeric] Standard fill depth in meters above which additional
#'   fill starts to increase the required bridge span. Default is `3`.
#' @param brdg_wdth [numeric] Standard bridge width in meters used as the base span
#'   for Barrier/Potential crossings when a bridge is recommended. Default is `15`.
#' @param chn_wdth_max [numeric] Maximum channel width in meters at which the base
#'   bridge width is considered sufficient before span extensions are applied.
#'   Default is `5`.
#' @param fill_dpth_mult [numeric] Multiplier applied to additional fill depth above
#'   `fill_dpth`. For every 1m deeper than `fill_dpth`, `fill_dpth_mult` meters of
#'   additional bridge span are required. Default is `3`.
#' @param chan_wdth_min_brdg [numeric] Channel width in meters above which a bridge
#'   may be required for Barrier/Potential crossings. Default is `2`.
#' @param stream_wdth_max_ss [numeric] Maximum channel width in meters for which
#'   streambed simulation may be selected when fill depth exceeds `fill_dpth_max`.
#'   Default is `3.5`.
#' @param fill_dpth_max [numeric] Fill depth threshold in meters above which
#'   streambed simulation is selected when channel width is less than
#'   `stream_wdth_max_ss`. Default is `5`.
#' @param ss_span_min [numeric] Minimum span in meters used for streambed simulation
#'   structures when a bridge is not recommended. Default is `3`.
#' @param ss_span_max [numeric] Maximum span in meters used for streambed simulation
#'   structures. Default is `4.5`.
#' @param chan_wdth_to_span_ok [numeric] Amount in meters by which the structure
#'   span may exceed the channel width before further span extensions are applied.
#'   Default is `4`.
#'
#' @return [dataframe] A dataframe with the specified columns for crossing fix and
#'   recommended diameter/span added or updated.
#'
#'
#' @examples
#' dat <- data.frame(
#'   barrier_result = c("Barrier", "Potential", "Other"),
#'   downstream_channel_width_meters = c(3, 10, 3),
#'   fill_depth_meters = c(3, 7, 6)
#' )
#'
#' # Calculate replacement structure type and recommended span
#' fpr_xfm_paw_xing_fix_size(dat)
#'
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_like
#' @importFrom cli cli_abort
#' @importFrom chk chk_data chk_string chk_number
#' @importFrom rlang ensym
#' @importFrom plyr round_any
#' @family xfm paw
#' @export
fpr_xfm_paw_xing_fix_size <- function(
    dat,
    col_barrier_result = barrier_result,
    col_downstream_channel_width_meters = downstream_channel_width_meters,
    col_fill_depth_meters = fill_depth_meters,
    col_crossing_fix = crossing_fix,
    col_recommended_diameter_or_span_meters = recommended_diameter_or_span_meters,
    fill_dpth = 3,
    brdg_wdth = 15,
    chn_wdth_max = 5,
    fill_dpth_mult = 3,
    chan_wdth_min_brdg = 2,
    stream_wdth_max_ss = 3.5,
    fill_dpth_max = 5,
    ss_span_max = 4.5,
    chan_wdth_to_span_ok = 4,
    ss_span_min = 3
) {
  # Check input validity
  chk::chk_data(dat)
  chk::chk_string(deparse(substitute(col_barrier_result)))
  chk::chk_string(deparse(substitute(col_downstream_channel_width_meters)))
  chk::chk_string(deparse(substitute(col_fill_depth_meters)))
  chk::chk_string(deparse(substitute(col_crossing_fix)))
  chk::chk_string(deparse(substitute(col_recommended_diameter_or_span_meters)))
  chk::chk_number(fill_dpth)
  chk::chk_number(brdg_wdth)
  chk::chk_number(chn_wdth_max)
  chk::chk_number(fill_dpth_mult)
  chk::chk_number(chan_wdth_min_brdg)
  chk::chk_number(stream_wdth_max_ss)
  chk::chk_number(fill_dpth_max)
  chk::chk_number(ss_span_max)
  chk::chk_number(chan_wdth_to_span_ok)
  chk::chk_number(ss_span_min)

  # Evaluate tidy-select columns
  col_barrier_result <- rlang::ensym(col_barrier_result)
  col_downstream_channel_width_meters <- rlang::ensym(col_downstream_channel_width_meters)
  col_fill_depth_meters <- rlang::ensym(col_fill_depth_meters)
  col_crossing_fix <- rlang::ensym(col_crossing_fix)
  col_recommended_diameter_or_span_meters <- rlang::ensym(col_recommended_diameter_or_span_meters)

  # Check if required columns exist in the dataframe
  required_cols <- c(
    as.character(col_barrier_result),
    as.character(col_downstream_channel_width_meters),
    as.character(col_fill_depth_meters)
  )
  missing_cols <- setdiff(required_cols, colnames(dat))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "The following required columns are missing from the dataframe: {paste(missing_cols, collapse = ', ')}"
    )
  }

  # Additional fill depth above the standard threshold
  dat <- dat |>
    dplyr::mutate(
      fill_dpth_over = !!col_fill_depth_meters - fill_dpth
    )

  # Base crossing fix
  dat <- dat |>
    dplyr::mutate(
      !!col_crossing_fix := dplyr::case_when(
        (!!col_barrier_result == "Barrier" | !!col_barrier_result == "Potential") &
          !!col_downstream_channel_width_meters >= chan_wdth_min_brdg ~
          "Replace with New Open Bottom Structure",
        !!col_barrier_result == "Passable" | !!col_barrier_result == "Unknown" ~
          NA_character_,
        TRUE ~
          "Replace Structure with Streambed Simulation CBS"
      )
    )

  # Base recommended diameter/span
  dat <- dat |>
    dplyr::mutate(
      !!col_recommended_diameter_or_span_meters := dplyr::case_when(
        (!!col_barrier_result == "Barrier" | !!col_barrier_result == "Potential") &
          !!col_downstream_channel_width_meters >= chan_wdth_min_brdg ~
          brdg_wdth,
        !!col_barrier_result == "Passable" | !!col_barrier_result == "Unknown" ~
          NA_real_,
        TRUE ~
          ss_span_min
      )
    )

  # Adjust size for additional fill depth where a bridge is being used
  dat <- dat |>
    dplyr::mutate(
      !!col_recommended_diameter_or_span_meters := dplyr::case_when(
        (!!col_barrier_result == "Barrier" | !!col_barrier_result == "Potential") &
          fill_dpth_over > 0 &
          !stringr::str_like(!!col_crossing_fix, "Simulation") ~
          (brdg_wdth + fill_dpth_mult * fill_dpth_over),
        TRUE ~
          !!col_recommended_diameter_or_span_meters
      )
    )

  # Extend size if channel is very wide
  dat <- dat |>
    dplyr::mutate(
      !!col_recommended_diameter_or_span_meters := dplyr::case_when(
        !!col_recommended_diameter_or_span_meters <
          (!!col_downstream_channel_width_meters + chan_wdth_to_span_ok) &
          !!col_downstream_channel_width_meters > chn_wdth_max ~
          (!!col_downstream_channel_width_meters - chn_wdth_max) +
          !!col_recommended_diameter_or_span_meters,
        TRUE ~
          !!col_recommended_diameter_or_span_meters
      )
    )

  # Special case: deep fill and relatively narrow channel → streambed simulation
  dat <- dat |>
    dplyr::mutate(
      !!col_crossing_fix := dplyr::case_when(
        (!!col_barrier_result == "Barrier" | !!col_barrier_result == "Potential") &
          !!col_downstream_channel_width_meters > chan_wdth_min_brdg &
          !!col_downstream_channel_width_meters <= stream_wdth_max_ss &
          !!col_fill_depth_meters > fill_dpth_max ~
          "Replace Structure with Streambed Simulation CBS",
        TRUE ~
          !!col_crossing_fix
      ),
      !!col_recommended_diameter_or_span_meters := dplyr::case_when(
        (!!col_barrier_result == "Barrier" | !!col_barrier_result == "Potential") &
          !!col_downstream_channel_width_meters > chan_wdth_min_brdg &
          !!col_downstream_channel_width_meters <= stream_wdth_max_ss &
          !!col_fill_depth_meters > fill_dpth_max ~
          ss_span_max,
        TRUE ~
          !!col_recommended_diameter_or_span_meters
      )
    )

  # Round recommended diameter/span to nearest 0.5m
  dat <- dat |>
    dplyr::mutate(
      !!col_recommended_diameter_or_span_meters :=
        plyr::round_any(!!col_recommended_diameter_or_span_meters, 0.5)
    )

  return(dat)
}
