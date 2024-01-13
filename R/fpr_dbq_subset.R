#' Build query to read in subset table from postgres database.
#'
#'
#' @param col_order String (quoted) name of column to order partitioned table subsets before sliced.
#' See column names of any table with fpr_db_query('SELECT * FROM <schema>.<table> LIMIT 0;')
#' @param schema String (quoted) name of target postgres db schema.  See names of schemas with \link{}
#' @param table String (quoted) name of target postgres db table. See names of tables with \link{}
#' @param num_rows Number of rows to slice (if available) from each partitioned section of table.
#' @param col_partition String (quoted) name of column to partition table on. See column names of any table with \link{}
#' @param col_select String (quoted) name of columns to select from the table.  Must include the `col_order` and `col_partition`.
#'
#' @return Text string to insert into \link{fpr_db_conn} call
#' @family database postgres
#' @export
#'
#' @examples \dontrun{fpr_db_query(query = fpr_db_q_crossings_utm())}
fpr_dbq_subset <- function(
    col_partition = 'utm_zone',
    col_order = 'aggregated_crossings_id',
    col_select = '*',
    schema = 'bcfishpass',
    table = 'crossings_vw',
    num_rows = 100
) {
  glue::glue(
    "
    WITH RankedRows AS (
        SELECT
            {col_select},
            ROW_NUMBER() OVER (PARTITION BY {col_partition} ORDER BY {col_order}) AS row_num
        FROM {schema}.{table}
    )
    SELECT
        {col_select}
    FROM RankedRows
    WHERE
        row_num <= {num_rows};
    "
  )
}
