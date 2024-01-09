#' Query crossings by utm zone
#'
#' Grabs 100 crossings from each of the utm zones for QA
#'
#' @return Text string to insert int0 \link{fpr_db_conn} call
#' @export
#'
#' @examples \dontrun{fpr_db_query(query = fpr_db_q_crossings_utm())}
fpr_db_q_crossings_utm <- function(){
  a <- "WITH RankedRows AS (\
    SELECT *,\
        ROW_NUMBER() OVER (PARTITION BY utm_zone ORDER BY aggregated_crossings_id) AS row_num\
    FROM\
        bcfishpass.crossings\
    WHERE\
        utm_zone BETWEEN 8 AND 12\
)\
SELECT *\
FROM\
    RankedRows\
WHERE\
    row_num < 100;"
  a
}
