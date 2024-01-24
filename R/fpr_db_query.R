#' Query a postgres database
#'
#' Provides connection from \link{fpr_db_conn} as well as a subsequent disconnection
#'
#' @param query Quoted string query written in `sql`.
#' @param ... Not used.  Facilitates pass through of quoted strings to set postgres database
#' params to link\{fpr_db_conn}
#'
#' @family database postgres
#'
#' @return Object pulled from database and message of how long the query took to complete.
#' @export
#'
#' @examples \dontrun{
#' fpr_db_query(query = fpr_dbq_subset())
#' }
fpr_db_query <- function(
    query = "SELECT * FROM bcfishpass.crossings limit 100;",
    ...){

  start_time <- Sys.time()

  conn <- fpr_db_conn(...)

  dat <- sf::st_read(conn, query = query)

  DBI::dbDisconnect(conn)

  end_time <- Sys.time()
  time_taken <- round(difftime(end_time, start_time, units="secs"),1)

  message(paste0("Query time = ", time_taken, " seconds!"))

  dat

}
