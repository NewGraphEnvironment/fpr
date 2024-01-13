#' List all tables and their sizes
#'
#' @param col_order Single quoted string with columns to order results on. To order by size
#' use 'pg_relation_size desc'.
#'
#' @family database postgres
#' @return
#' @export
#'
#' @examples
fpr_dbq_lstable <-  function(col_order = 'table_schema, table_name'){
  glue::glue(
    "SELECT table_schema,
    table_name,
    pg_size_pretty(pg_relation_size(table_schema||'.'||table_name)),
    pg_relation_size(table_schema||'.'||table_name)
             FROM information_schema.tables
             WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
             ORDER BY ",
    col_order,
    ";")

}
