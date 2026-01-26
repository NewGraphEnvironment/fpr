# List names of all columns in a postgres table

List names of all columns in a postgres table

## Usage

``` r
fpr_dbq_lscols(schema = "bcfishpass", table = "crossings_vw", ...)
```

## Arguments

- schema:

  Character string (quoted) name of target postgres db schema. See names
  of schemas with
  [fpr_dbq_lstables](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md)

- table:

  Character string (quoted) name of target postgres db table See names
  of schemas with
  [fpr_dbq_lstables](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md)

## Value

tibble of columns names and associated type for specified table.

## See also

Other database postgres:
[`fpr_db_conn()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_conn.md),
[`fpr_db_query()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_query.md),
[`fpr_dbq_lstables()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md),
[`fpr_dbq_subset()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_subset.md)

## Examples

``` r
if (FALSE) fpr_db_query(query = fpr_dbq_lscols()) # \dontrun{}
```
