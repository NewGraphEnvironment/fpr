# List all tables and their sizes

List all tables and their sizes

## Usage

``` r
fpr_dbq_lstables(col_order = "table_schema, table_name")
```

## Arguments

- col_order:

  A single quoted character string with columns to order results on.
  Default is 'table_schema, table_name'. To order by size descending use
  'size desc'.

## Value

tibble with schema name, table name and sizes of all tables in database.

## See also

Other database postgres:
[`fpr_db_conn()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_conn.md),
[`fpr_db_query()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_query.md),
[`fpr_dbq_lscols()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lscols.md),
[`fpr_dbq_subset()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_subset.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fpr_db_query(fpr_dbq_lstables(col_order = 'size desc'))
} # }
```
