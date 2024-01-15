#' List names of all columns in a postgres table
#'
#'
#' @param schema Character string (quoted) name of target postgres db schema.  See names of schemas with \link{fpr_dbq_lstables}
#' @param table Character string (quoted) name of target postgres db table  See names of schemas with \link{fpr_dbq_lstables}
#'
#' @family database postgres
#' @return
#' @export
#'
#' @examples \dontrun{fpr_db_query(query = fpr_dbq_lscols())}
fpr_dbq_lscols <- function(
    schema = 'bcfishpass',
    table = 'crossings_vw',
    ...){
  glue::glue(
    "SELECT attname AS column_name, format_type(atttypid, atttypmod) AS data_type
FROM   pg_attribute
WHERE  attrelid = '{schema}.{table}'::regclass
ORDER  BY attnum;
"
  )
}
