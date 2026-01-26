# Get elevation statistics.

Get elevation statistics.

## Usage

``` r
fpr_sp_wshd_stats(dat)
```

## Arguments

- dat:

  sf object of polygons to gather elevation statistics for. Must contain
  \`stream_crossing_id\` column.

## Value

sf object of polygons with elevation details (ie. p60, median, min, max)
and median aspect information added.

## See also

Other spatial:
[`fpr_make_html_tbl()`](http://www.newgraphenvironment.com/fpr/reference/fpr_make_html_tbl.md)

## Examples

``` r
if (FALSE) wshd <- fpr_sp_wshd_stats(
fwapgr::fwa_watershed_at_measure(
blue_line_key = 360746095, downstream_route_measure = 100) |>
dplyr::mutate(stream_crossing_id = 1)
) # \dontrun{}
```
