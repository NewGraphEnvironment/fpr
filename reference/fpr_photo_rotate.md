# Rotate an image

Rotate an image

## Usage

``` r
fpr_photo_rotate(
  site_id = my_site,
  dir_photos = "data/photos/",
  rotate = 180,
  str_to_pull = "barrel",
  str_prepend = "rotated_",
  ...
)
```

## Arguments

- site_id:

  Numeric indicating site id cooresponding to name of directory
  containing the images

- dir_photos:

  String quoted for directory where photo directories are kept. Trailing
  forward slash required. Defaults to 'data/photos/'.

- rotate:

  Numeric value of the amount of rotation in degrees. Defaults to 180

- str_to_pull:

  Sting value in photo name. Must be unique in file

- ...:

  Not used. Used to pass \`dir_photos\` to
  [fpr_photo_pull_by_str](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_pull_by_str.md)

## Value

Rotated photo burned into directory of targeted original.
