# Pull names of a photo based on a string in the file name

Pull names of a photo based on a string in the file name

## Usage

``` r
fpr_photo_pull_by_str(
  site = my_site,
  dir_photos = "data/photos/",
  str_to_pull = "barrel"
)
```

## Arguments

- site:

  Numeric indicating site id cooresponding to name of directory
  containing the images

- dir_photos:

  String quoted for directory where photo directories are kept. Trailing
  forward slash required. Defaults to 'data/photos/'.

- str_to_pull:

  String (quoted) value to search for in name of photo. must be unique
  in the file. Defaults to barrel

## Value

String representing the full path filename of photo that matches
