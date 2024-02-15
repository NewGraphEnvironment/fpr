#' Clips a point layer by a polygon layer.
#'
#' @param schema_table_point String (quoted) name of point layer schema.table
#' @param schema_table_polygon String (quoted) name of polygon layer schema.table
#' @param join_column String (quoted) name of column to join tables on from polygon. See column names of any table with \link{fpr_dbq_lscols}
#' @param join_on String (quoted) or vector of specific terms to join on.
#'
#' @importFrom glue glue
#' @family database postgres
#' @return Sf of clipped point layer
#' @export
#'
#' @examples \dontrun{fpr_dbq_clip(c("FRAN", "NECR"))}


fpr_dbq_clip <- function(
    schema_table_point,
    schema_table_polygon,
    join_column,
    join_on) {

  join_on_string <- paste0("'", join_on, "'", collapse = ", ")

  fpr_db_query(query = glue::glue("SELECT point.*, poly.{join_column}
   FROM {schema_table_point} point
   INNER JOIN {schema_table_polygon} poly
   ON ST_Intersects(poly.geom, point.geom)
   WHERE poly.{join_column} IN ({join_on_string});"))

}

fpr_dbq_clip('whse_environmental_monitoring.envcan_hydrometric_stn_sp',
                        'whse_basemapping.fwa_watershed_groups_poly', 'watershed_group_code',
                        c("FRAN", "NECR"))

fpr_dbq_clip('bcfishpass.barriers_bt',
             'whse_basemapping.fwa_watershed_groups_poly', 'watershed_group_code',
             c("FRAN", "NECR"))

