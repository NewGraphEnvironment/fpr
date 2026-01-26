# Identify photos that should be copied over into file to upload to PSCIS. See fpr_xref_photo_order to reference the sort order for some of the common photo tag names. The first six photos are for upstream, downstream, inlet, outlet and barrel as those are required by PSCIS (all except road anyway).

Identify photos that should be copied over into file to upload to PSCIS.
See fpr_xref_photo_order to reference the sort order for some of the
common photo tag names. The first six photos are for upstream,
downstream, inlet, outlet and barrel as those are required by PSCIS (all
except road anyway).

## Usage

``` r
fpr_photo_paths_to_copy(
  path_dir = "data/photos",
  slice_start = 1,
  slice_end = 10,
  tag_add = NULL,
  tag_add_location = 6.5
)
```

## Arguments

- path_dir:

  String. Path to directory where photo directories are found

- slice_start:

  Integer. Place in the list of defined photos fpr_xref_photo_order
  where you choose to start the selection for submission. Defaults to 1.

- slice_end:

  Integer. Place in the list of defined photos fpr_xref_photo_order
  where you choose to end the selection for submission. Defaults to 10.

- tag_add:

  String. Quoted (ex. '\_p2\_'). If is not NULL photos will be searched
  for match of this string in addition to the strings matched in
  fpr_xref_photo_order. Defaults to NULL.

- tag_add_location:

  Numeric. If \`tag_add\` is not NULL this will be used to indicate what
  location in fpr_xref_photo_order the string \`tag_add\` will land as
  to control which photos names will be included in the slice of
  matchingnames. Defaults to 6.5 as to place \`tag_add\` after the first
  6 mandatory PSCIS submission photos and before any other tag.

## Value

Vector of full path photo names.
