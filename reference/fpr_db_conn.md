# Connect to postgres database

Connect to postgres database

## Usage

``` r
fpr_db_conn(
  db_var = Sys.getenv("PG_DB_SHARE"),
  host_var = Sys.getenv("PG_HOST_SHARE"),
  port_var = Sys.getenv("PG_PORT_SHARE"),
  user_var = Sys.getenv("PG_USER_SHARE"),
  password_var = Sys.getenv("PG_PASS_SHARE")
)
```

## Arguments

- db_var:

  Quoted character string value of database name.

- host_var:

  Quoted character string value of host name.

- port_var:

  Quoted character string value of port name.

- user_var:

  Quoted character string value of user name.

- password_var:

  Quoted character string value of password.

## Value

DBI connection

## See also

Other database postgres:
[`fpr_db_query()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_query.md),
[`fpr_dbq_lscols()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lscols.md),
[`fpr_dbq_lstables()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md),
[`fpr_dbq_subset()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_subset.md)

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- fpr_db_conn()
} # }
```
