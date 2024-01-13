#' Connect to postgres database
#'
#' Connect to pg database using environmental variables.  Ass
#'
#' @param db_var Quoted string value of database name.  Defaults to `Sys.getenv('PG_DB_SHARE')`
#' @param host_var Quoted string value of host name.  Defaults to `Sys.getenv('PG_HOST_SHARE')`
#' @param port_var Quoted string value of port name.  Defaults to `Sys.getenv('PG_PORT_SHARE')`
#' @param user_var Quoted string value of user name.  Defaults to `Sys.getenv('PG_USER_SHARE')`
#' @param password_var QQuoted string value of password.  Defaults to `Sys.getenv('PG_PASS_SHARE')`
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
