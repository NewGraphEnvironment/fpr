# Remove duplicated photos before or after photo name appended to end of photo file name by [fpr_photo_rename](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_rename.md)

Remove duplicated photos before or after photo name appended to end of
photo file name by
[fpr_photo_rename](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_rename.md)

## Usage

``` r
fpr_photo_remove_dupes(
  dir_target = NULL,
  dry_run = TRUE,
  min_replicates = 2,
  col_time = date_time_original,
  col_photo_name = source_file,
  col_model = model,
  col_iso = iso,
  remove_renamed = FALSE,
  ...
)
```

## Arguments

- dir_target:

  String. Full path name of directory to be scanned for duplicate photos
  with different names.

- dry_run:

  Logical. Should photos selected by \`remove_renamed\` be removed or
  should a dataframe be produced to show the identical photos? Defaults
  to TRUE.

- min_replicates:

  Integer. Minimum number of identical photos (based on identical
  \`col_time\`, \`col_model\` and \`col_iso\` values in photo metadata)
  that should be identified. Defaults to 2. Setting this to higher than
  2 is intended for \`dry_run\` = TRUE as it is likely to remove photos
  used for more than one thing vs removing renamed photos.

- col_time:

  The bare (unquoted) name of the column containing the exif parameter
  stamping when photo was created. Defaults to \`date_time_original\`.

- col_photo_name:

  The bare (unquoted) name of the column containing the exif parameter
  with full path photo name. Defaults to \`source_file\`.

- col_model:

  The bare (unquoted) name of the column containing the exif parameter
  stamping camera type that took the photo. Defaults to \`model\`.

- col_iso:

  he bare (unquoted) name of the column containing the exif parameter
  stamping photo iso. Defaults to \`iso\`.

- remove_renamed:

  Logical. Should renamed photos be removed (based on length of photo
  name)? Defaults to FALSE so that duplicate photos that have not been
  renamed are removed.

- ...:

  Not used.

## Value

A dataframe if \`dry_run\` is TRUE (default) or removal of photos
specified through \`remove_renamed\` param.
