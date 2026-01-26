# Amalgamate 6 PSCIS photos into 1

Here we stack up and down then side to side for reporting. There must be
all 6 photos present, only 1 of each and within the file names of those
photos must be the strings containing upstream, downstream, inlet,
outlet, barrel, road. Before running QA with

## Usage

``` r
fpr_photo_amalg_cv(site_id, dir_photos = "data/photos/", size = "560x420!")
```

## Arguments

- site_id:

  Numeric value of site corresponding to the directory name that holds
  the photos which include 'road', 'inlet', 'upstream', 'downstream',
  'outlet', barrel' in the filenames.

- dir_photos:

  String quoted for directory where photo directories are kept. Trailing
  forward slash required. Defaults to 'data/photos/'.

- size:

  String. Dimensions of individual photos. Defaults to "560x420!" to
  give oveall photo size of 1120 x 1260.

## Value

Burned amalgamated crossing_all.JPG which includes 6 PSCIS photos
together as one.

## See also

Other photo:
[`fpr_photo_folders()`](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_folders.md),
[`fpr_photo_qa_missing_all()`](http://www.newgraphenvironment.com/fpr/reference/fpr_photo_qa_missing_all.md)
