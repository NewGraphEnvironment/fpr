# Build query to read in subset table from postgres database.

Build query to read in subset table from postgres database.

## Usage

``` r
fpr_dbq_subset(
  col_partition = "utm_zone",
  col_select = "*",
  schema = "bcfishpass",
  table = "crossings",
  num_rows = 100,
  randomize = TRUE,
  col_order = "aggregated_crossings_id",
  ...
)
```

## Arguments

- col_partition:

  String (quoted) name of column to partition table on. See column names
  of any table with
  [fpr_dbq_lscols](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lscols.md)

- col_select:

  String (quoted) name of columns to select from the table. Must include
  the \`col_order\` and \`col_partition\`.

- schema:

  Character string (quoted) name of target postgres db schema. See names
  of schemas with
  [fpr_dbq_lstables](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md)

- table:

  Character string (quoted) name of target postgres db table See names
  of schemas with
  [fpr_dbq_lstables](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md)

- num_rows:

  Number of rows to slice (if available) from each partitioned section
  of table.

- randomize:

  Logical (TRUE or FALSE) of whether to choose rows within partitioned
  subsets optionally. If FALSE \`col_order\` needs to be specified

- col_order:

  String (quoted) name of column to order partitioned table subsets
  before sliced. See column names of any table with
  [fpr_dbq_lscols](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lscols.md)

## Value

Text string to insert into
[fpr_db_conn](http://www.newgraphenvironment.com/fpr/reference/fpr_db_conn.md)
call

## See also

Other database postgres:
[`fpr_db_conn()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_conn.md),
[`fpr_db_query()`](http://www.newgraphenvironment.com/fpr/reference/fpr_db_query.md),
[`fpr_dbq_lscols()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lscols.md),
[`fpr_dbq_lstables()`](http://www.newgraphenvironment.com/fpr/reference/fpr_dbq_lstables.md)

## Examples

``` r
if (FALSE) fpr_db_query(query = fpr_dbq_subset()) # \dontrun{}
```
