# Query a postgres database

Provides connection from
[fpr_db_conn](http://www.newgraphenvironment.com/fpr/reference/fpr_db_conn.md)
as well as a subsequent disconnection

## Usage

``` r
fpr_db_query(query = "SELECT * FROM bcfishpass.crossings limit 100;", ...)
```

## Arguments

- query:

  Quoted string query written in \`sql\`.

## Value

Object pulled from database and message of how long the query took to
complete.

## See also

Other database postgres:
[`fpr_db_conn()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_conn.md),
[`fpr_dbq_lscols()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lscols.md),
[`fpr_dbq_lstables()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md),
[`fpr_dbq_subset()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_subset.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fpr_db_query(query = fpr_dbq_subset())
} # }
```
