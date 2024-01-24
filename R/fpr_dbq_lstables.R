#' List all tables and their sizes
#'
#' @param col_order A single quoted character string with columns to order results on. Default is 'table_schema,
#' table_name'. To order by size descending use 'size desc'.
#'
#' @importFrom glue glue
#' @family database postgres
#' @return tibble with schema name, table name and sizes of all tables in database.
#' @export
#'
#' @examples \dontrun{
#' fpr_db_query(fpr_dbq_lstables(col_order = 'size desc'))
#' }
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
