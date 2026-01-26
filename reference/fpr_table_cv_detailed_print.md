# Print out a list of fpr_table_cv_detailed tables showing PSCIS data in untidy table in either gitbook or pdf versions.

Print out a list of fpr_table_cv_detailed tables showing PSCIS data in
untidy table in either gitbook or pdf versions.

## Usage

``` r
fpr_table_cv_detailed_print(
  tab_sum,
  comments,
  photos,
  gitbook_switch = gitbook_on
)
```

## Arguments

- tab_sum:

  list of dataframes formatted by fpr_table_cv_detailed.

- comments:

  list of dataframes formatted by fpr_table_cv_detailed_comments

- photos:

  list of dataframes with just the url of the photo for each site

- gitbook:

  logical based on whether out put is gitbook or pagedown pdf. Defaults
  to gitbook_on which is defined in the index file as FALSE when the
  pagedown output is on. This is when we need the spaces.

## Value

html tables with photo as footnotes
