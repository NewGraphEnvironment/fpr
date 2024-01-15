#' Connect to postgres database
#'
#' @param db_var Quoted character string value of database name.
#' @param host_var Quoted character string value of host name.
#' @param port_var Quoted character string value of port name.
#' @param user_var Quoted character string value of user name.
#' @param password_var Quoted character string value of password.
#'
#' @family database postgres
#'
#' @return
#' @export
#'
#' @examples \dontrun{conn <- fpr_db_conn()}
fpr_db_conn <- function(
    db_var = Sys.getenv('PG_DB_SHARE'),
    host_var = Sys.getenv('PG_HOST_SHARE'),
    port_var = Sys.getenv('PG_PORT_SHARE'),
    user_var = Sys.getenv('PG_USER_SHARE'),
    password_var = Sys.getenv('PG_PASS_SHARE')
) {

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = db_var,
    host = host_var,
    port = port_var,
    user = user_var,
    password = password_var
  )
}
