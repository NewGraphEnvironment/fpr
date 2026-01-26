# Find sites missing all photos

Find sites that exist in the PSCIS input spreadsheets in the \`data\`
directory but do not have a directory or do not have any of the required
PSCIS tagged photo names. Fed into
[fpr_photo_qa_df](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_qa_df.md)
and relies on
[fpr_photo_qa](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_qa.md).
Site directories must be named as numbers.

## Usage

``` r
fpr_photo_qa_missing_all(...)
```

## Arguments

- ...:

  Not used. Used to pass \`dir_photos = 'filepath'\` to
  [fpr_photo_qa](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_qa.md)

## Value

Tibble with 7 columns.

## See also

Other photo:
[`fpr_photo_amalg_cv()`](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_amalg_cv.md),
[`fpr_photo_folders()`](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_folders.md)
