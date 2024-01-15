#' List all tables and their sizes
#'
#' @inheritParams fpr_dbq_lscols
#' @param col_order Single quoted string with columns to order results on. To order by size descending
#' use 'size desc'.
#'
#' @family database postgres
#' @return
#' @export
#'
#' @examples \dontrun{fpr_dbq_lstables(col_order = 'size desc')}
fpr_dbq_lstables <-  function(
    col_order = 'table_schema, table_name'){
  glue::glue(
    "SELECT table_schema,
    table_name,
    pg_size_pretty(pg_relation_size(table_schema||'.'||table_name)),
    pg_relation_size(table_schema||'.'||table_name) as size
             FROM information_schema.tables
             WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
             ORDER BY {col_order};"
  )
}
