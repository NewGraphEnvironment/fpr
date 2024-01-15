#' Build query to read in subset table from postgres database.
#'
#' @inheritParams fpr_dbq_lscols
#' @param col_order String (quoted) name of column to order partitioned table subsets before sliced.
#' See column names of any table with \link{fpr_dbq_lscols}
#' @param schema
#' @param table
#' @param randomize Logical (TRUE or FALSE) of whether to choose rows within partitioned subsets optionally.
#' If FALSE `col_order` needs to be specified
#' @param num_rows Number of rows to slice (if available) from each partitioned section of table.
#' @param col_partition String (quoted) name of column to partition table on. See column names of any table with \link{fpr_dbq_lscols}
#' @param col_select String (quoted) name of columns to select from the table.  Must include the `col_order` and `col_partition`.
#'
#' @return Text string to insert into \link{fpr_db_conn} call
#' @family database postgres
#' @export
#'
#' @examples \dontrun{fpr_db_query(query = fpr_dbq_subset())}
fpr_dbq_subset <- function(
    col_partition = 'utm_zone',
    col_select = '*',
    schema = 'bcfishpass',
    table = 'crossings',
    num_rows = 100,
    randomize = TRUE,
    col_order = 'aggregated_crossings_id',
    ...){
  if(randomize){
    glue::glue(
      "WITH RandomizedRows AS (
  SELECT
  {col_select},
    ROW_NUMBER() OVER (PARTITION BY {col_partition} ORDER BY random()) AS row_num
  FROM {schema}.{table}
)
SELECT
    {col_select}
FROM RandomizedRows
WHERE row_num <= {num_rows};"
    )
  }else if(identical(randomize,FALSE)){
    glue::glue(
      "WITH RankedRows AS (
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
}
