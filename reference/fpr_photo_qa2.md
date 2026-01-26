# Helper function to QA Photos to ensure reserved photo strings are used only once per site.

Ensure that there are 6 named photos and no duplicates named.
\`fpr_photo_qa\` is almost identical but looks for only 1 occurance of
\`12345_barrel.JPG\` and allows \`6789_barrel2.JPG\`. This updated
function will create a Used inside of
[fpr_photo_qa_df](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_qa_df.md).

## Usage

``` r
fpr_photo_qa2(dat = pscis_all, col = site_id, dir_photos = "data/photos/")
```

## Arguments

- dat:

  Dataframe that contains a column with the numeric site ids. Defaults
  to \`pscis_all\`

- col:

  Column to pull to get site IDs. Defaults to site_id

- dir_photos:

  Directory that contains the photos. Defaults to data/photos/

## Value

List of dataframes with photo names for 6 PSCIS photos. Will return NAs
when photo not there and list of names when there is more than one that
matches the pattern.
