# Sort in directories and rename photos base on field form column or photo tag.

Sort in directories and rename photos loaded to Mergin as per the column
they were taken for (ex. \`photo_downstream\`) or the "tag" input for
general "extra\*" photos (ex. photo_extra1_tag). Renamed photos have
strings appended to the end of the existing file name.

## Usage

``` r
fpr_photo_rename(
  dat = form_pscis,
  col_directories = site_id,
  dir_from_stub = NULL,
  dir_to_stub = NULL,
  col_string_add = FALSE,
  col_string_append = location,
  return_df = FALSE,
  ...
)
```

## Arguments

- dat:

  Data frame to pull out the site names from. Defaults to a dataframe
  named \`form_pscis\`

- col_directories:

  The bare (unquoted) name of the column containing the parameter you
  want to use to build directories for storing photos. Defaults to
  \`site_id\`

- dir_from_stub:

  String quoted. Path to the place where resized photos from mergin are
  stored.

- dir_to_stub:

  String quoted. Path to the place where directories will be burned (if
  not already present) and renamed photos from mergin will be stored.

- col_string_add:

  Logical. Should the contents of one of the columns in \`dat\` be
  appended to the photo name before the standard string additions.
  Defaults to \`FALSE\` for the \`form_pscis\` use case. Likely will be
  \`TRUE\` for \`form_fiss_site\`

- col_string_append:

  The bare (unquoted) name of the column containing the parameter you
  want to use to add to the name of the photo before the appended
  string. Defaults to \`local_name\`. Used for habitat confirmation
  sites when there are multiple sites nested within the overall area
  (ex. small electrofishing sites)

- return_df:

  \[logical\] Optional. If TRUE return the dataframe used to rename the
  photos. For QA. Defaults to FALSE.

- ...:

  Unused. For passing params to
  [fpr_photo_folders](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_folders.md)

## Value

Duplicates of existing photos renamed with directories specified by
col_directories param
