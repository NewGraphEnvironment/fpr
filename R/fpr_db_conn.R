#' Connect to postgres database
#'
#' Connect to pg database using environmental variables
#'
#' @param db_var Quoted string value of environmental variable with database name
#' @param host_var Quoted string value of environmental variable with host name
#' @param port_var Quoted string value of environmental variable with port name
#' @param user_var Quoted string value of environmental variable with user name
#' @param password_var Quoted string value of environmental variable with password
#'
#' @return
#' @export
#'
#' @examples \dontrun{fpr_db_conn()}
fpr_db_conn <- function(
    db_var = 'PG_DB_SHARE',
    host_var = 'PG_HOST_SHARE',
    port_var = 'PG_PORT_SHARE',
    user_var = 'PG_USER_SHARE',
    password_var = 'PG_PASS_SHARE'
) {

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv(db_var),
    host = Sys.getenv(host_var),
    port = Sys.getenv(port_var),
    user = Sys.getenv(user_var),
    password = Sys.getenv(password_var)
  )
}
