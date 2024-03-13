#' Clips a point layer by a polygon layer, and returns the clipped point layer containing user specified columns.
#'
#' @param schtab_pnt String (quoted) name of point layer schema.table.
#' @param schtab_pol String (quoted) name of polygon layer schema.table.
#' @param join_column String (quoted) name of column to join tables on from polygon. See column names of any table with \link{fpr_dbq_lscols}.
#' @param schtab_pol_filter String (quoted) or quoted comma seperated vector that defines how the polygon table is filtered, example c("FRAN", "NECR").
#' @param cols_schtab_pnt String (quoted) or list of column names to select from point layer, default is all columns (*). Must include the geom column if user wants an sf object.
#'
#' @importFrom glue glue
#' @importFrom chk chk_string
#' @family database postgres
#' @return Sf of clipped point layer
#' @export
#'
#' @examples \dontrun{fpr_db_query(query = fpr_dbq_clip('whse_environmental_monitoring.envcan_hydrometric_stn_sp',
#''whse_basemapping.fwa_watershed_groups_poly', 'watershed_group_code', c("FRAN", "NECR")))}


fpr_dbq_clip <- function(
    schtab_pnt = NULL,
    schtab_pol = NULL,
    join_column = NULL,
    schtab_pol_filter = NULL,
    cols_schtab_pnt = '*') {

  chk::chk_string(schtab_pnt)
  chk::chk_string(schtab_pol)
  chk::chk_string(join_column)

  join_on_str <- paste0("'", schtab_pol_filter, "'", collapse = ", ")
  cols_schtab_pnt_str <- paste0("point.", cols_schtab_pnt, collapse = ", ")

  glue::glue("SELECT {cols_schtab_pnt_str}, poly.{join_column}
   FROM {schtab_pnt} point
   INNER JOIN {schtab_pol} poly
   ON ST_Intersects(poly.geom, point.geom)
   WHERE poly.{join_column} IN ({join_on_str});")

}

