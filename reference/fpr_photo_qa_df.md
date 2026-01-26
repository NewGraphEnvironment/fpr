# QA photos

See if all required PSCIS photos are named and look for duplicates of
those names.

## Usage

``` r
fpr_photo_qa_df(dat = NULL, ...)
```

## Arguments

- dat:

  Dataframe that contains a column with the numeric site ids. Defaults
  to \`pscis_all\` created using
  [fpr_import_pscis_all](http://www.newgraphenvironment.com/fpr/reference/fpr_import_pscis_all.md)
  @family photo

- ...:

  Not used. Use to pass string path defined as \`dir_photos =
  'quoted_filepath'\` to run QA outside of defualt filepath of root of
  helper function
  [fpr_photo_qa](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_qa.md).
  Incorperates outputs of
  [fpr_photo_qa_missing_all](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_qa_missing_all.md)

## Value

Tibble with 7 columns showing missing phots.
