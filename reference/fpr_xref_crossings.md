# Table used to crosswalk bcfishpass column names and comments to report table column names

In past reports this object was called xref_bcfishpass_names. Usually a
hand curated tribble located in \`fish_passage_region_reporting\`. This
is a crosswalk for the column names in the \`bcfishpass.crossings_vw\`
to the cleaned column names for reporting and used to order the columns
in the reports. It also contains a description of columns described in
the \`bcfishpass“ documentation. Used in methods sections of reports as
well as in individual site memos to summarize the bcfishpass model
results.

In order to update this table for individual reports the output of
\`fpr_xref_crossings\` can be copied and pasted from a \`knitr::kable\`
to a new tribble in the \`fish_passage_region_reporting\`
\`scripts/tables.R\` file. From there the id_join and id_side columns
can be updated to call and order the columns presented in the report.

Updateing procedures are documented in the \`data-raw\` directory.

## Usage

``` r
fpr_xref_crossings
```

## Format

\## \`fpr_xref_crossings\` A data frame with 225 rows and 5columns:

- bcfishpass:

  raw column name from bcfishpass

- report:

  column name cleaned up for asthetics and clarity to be included in the
  report

- id_join:

  integer lookup used to order the rows in the tables of the report.
  This is the row the column will be placed in the tabel in the report

- id_side:

  integer lookup used to join together the columns in the tables of the
  report. 1 = left and 2 = right

- column_comment:

  description of the column pulled from the bcfishpass documentation

## Source

\<https://github.com/smnorris/bcfishpass\>
