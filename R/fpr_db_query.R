#' Query a postgres database
#'
#' Provides connection from \link{tfpr_db_conn} as well as a subsequent disconnection
#'
#' @param query Quoted string query written in `sql`.  Defaults to  "SELECT * FROM bcfishpass.crossings limit 100000;"
#' @param ... Not used.  Facilitates pass through of quoted strings to set postgres database params to link\{fpr_db_conn}
#'
#' @return Object pulled from database.
#' @export
#'
#' @examples \dontrun{fpr_db_query(query = fpr_db_q_crossings_utm())}
fpr_db_query <- function(
    query = "SELECT * FROM bcfishpass.crossings limit 100000;",
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
