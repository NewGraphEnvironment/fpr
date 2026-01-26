# Transform PSCIS Assessment Worksheet Data by Calculating Stream Width Ratio

This function calculates the stream width ratio (SWR) for PSCIS data
based on the ratio of two specified columns: one for downstream channel
width and one for diameter or span.

## Usage

``` r
fpr_xfm_paw_swr(
  dat,
  col_downstream_channel_width_meters = downstream_channel_width_meters,
  col_diameter_or_span_meters = diameter_or_span_meters,
  col_stream_width_ratio = stream_width_ratio,
  digits = 2
)
```

## Arguments

- dat:

  \[data.frame\] A dataframe containing the PSCIS data.

- col_downstream_channel_width_meters:

  \[character\] Column name for downstream channel width, as a string or
  tidy-select syntax. Default is \`downstream_channel_width_meters\`.

- col_diameter_or_span_meters:

  \[character\] Column name for diameter or span, as a string or
  tidy-select syntax. Default is \`diameter_or_span_meters\`.

- col_stream_width_ratio:

  \[character\] Column name for the output stream width ratio, as a
  string. Default is \`stream_width_ratio\`.

- digits:

  \[numeric\] Passed to \`\[round()\]\` function to specify the number
  of decimal places in the output. The default value is \`2\`.

## Value

\[data.frame\] A dataframe with the specified \`col_stream_width_ratio\`
column added or updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
[`fpr_xfm_paw_score_cv_len()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_len.md),
[`fpr_xfm_paw_score_cv_slope()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_slope.md),
[`fpr_xfm_paw_score_embed()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_embed.md),
[`fpr_xfm_paw_score_final()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_final.md),
[`fpr_xfm_paw_score_outlet_drop()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_outlet_drop.md),
[`fpr_xfm_paw_score_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_swr.md),
[`fpr_xfm_paw_xing_fix_size()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_xing_fix_size.md)

## Examples

``` r
dat <- data.frame(
  downstream_channel_width_meters = c(5, NA, 10),
  diameter_or_span_meters = c(2, 3, NA)
)
fpr_xfm_paw_score_swr(
  dat,
  col_downstream_channel_width_meters = downstream_channel_width_meters,
  col_diameter_or_span_meters = diameter_or_span_meters
)
#> Error in fpr_xfm_paw_score_swr(dat, col_downstream_channel_width_meters = downstream_channel_width_meters,     col_diameter_or_span_meters = diameter_or_span_meters): unused arguments (col_downstream_channel_width_meters = downstream_channel_width_meters, col_diameter_or_span_meters = diameter_or_span_meters)
```
