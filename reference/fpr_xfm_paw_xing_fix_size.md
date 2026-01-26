# Transform PSCIS Assessment Worksheet Data by Calculating Replacement Structure Type and Size

This function determines the type and size of the replacement structure
for BC Provincial Stream Crossing Inventory System (PSCIS) data based on
the barrier result, downstream channel width, and the amount of road
fill above the crossing.

## Usage

``` r
fpr_xfm_paw_xing_fix_size(
  dat,
  col_barrier_result = barrier_result,
  col_downstream_channel_width_meters = downstream_channel_width_meters,
  col_fill_depth_meters = fill_depth_meters,
  col_crossing_fix = crossing_fix,
  col_recommended_diameter_or_span_meters = recommended_diameter_or_span_meters,
  fill_dpth = 3,
  brdg_wdth = 15,
  chn_wdth_max = 5,
  fill_dpth_mult = 3,
  chan_wdth_min_brdg = 2,
  stream_wdth_max_ss = 3.5,
  fill_dpth_max = 5,
  ss_span_max = 4.5,
  chan_wdth_to_span_ok = 4,
  ss_span_min = 3
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

- col_barrier_result:

  \[character\] A column name specifying the barrier result (Barrier,
  Potential, Passable, or Unknown), as a string or tidy-select syntax.
  Default is \`barrier_result\`.

- col_downstream_channel_width_meters:

  \[character\] A column name specifying the downstream channel width,
  as a string or tidy-select syntax. Default is
  \`downstream_channel_width_meters\`.

- col_fill_depth_meters:

  \[character\] A column name specifying the fill depth above the
  crossing, as a string or tidy-select syntax. Default is
  \`fill_depth_meters\`.

- col_crossing_fix:

  \[character\] A column name for the output recommended crossing fix,
  as a string or tidy-select syntax. Default is \`crossing_fix\`.

- col_recommended_diameter_or_span_meters:

  \[character\] A column name for the output recommended structure
  diameter or span (size), as a string or tidy-select syntax. Default is
  \`recommended_diameter_or_span_meters\`.

- fill_dpth:

  \[numeric\] Standard fill depth in meters above which additional fill
  starts to increase the required bridge span. Default is \`3\`.

- brdg_wdth:

  \[numeric\] Standard bridge width in meters used as the base span for
  Barrier/Potential crossings when a bridge is recommended. Default is
  \`15\`.

- chn_wdth_max:

  \[numeric\] Maximum channel width in meters at which the base bridge
  width is considered sufficient before span extensions are applied.
  Default is \`5\`.

- fill_dpth_mult:

  \[numeric\] Multiplier applied to additional fill depth above
  \`fill_dpth\`. For every 1m deeper than \`fill_dpth\`,
  \`fill_dpth_mult\` meters of additional bridge span are required.
  Default is \`3\`.

- chan_wdth_min_brdg:

  \[numeric\] Channel width in meters above which a bridge may be
  required for Barrier/Potential crossings. Default is \`2\`.

- stream_wdth_max_ss:

  \[numeric\] Maximum channel width in meters for which streambed
  simulation may be selected when fill depth exceeds \`fill_dpth_max\`.
  Default is \`3.5\`.

- fill_dpth_max:

  \[numeric\] Fill depth threshold in meters above which streambed
  simulation is selected when channel width is less than
  \`stream_wdth_max_ss\`. Default is \`5\`.

- ss_span_max:

  \[numeric\] Maximum span in meters used for streambed simulation
  structures. Default is \`4.5\`.

- chan_wdth_to_span_ok:

  \[numeric\] Amount in meters by which the structure span may exceed
  the channel width before further span extensions are applied. Default
  is \`4\`.

- ss_span_min:

  \[numeric\] Minimum span in meters used for streambed simulation
  structures when a bridge is not recommended. Default is \`3\`.

## Value

\[dataframe\] A dataframe with the specified columns for crossing fix
and recommended diameter/span added or updated.

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
[`fpr_xfm_paw_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_swr.md)

## Examples

``` r
dat <- data.frame(
  barrier_result = c("Barrier", "Potential", "Other"),
  downstream_channel_width_meters = c(3, 10, 3),
  fill_depth_meters = c(3, 7, 6)
)

# Calculate replacement structure type and recommended span
fpr_xfm_paw_xing_fix_size(dat)
#>   barrier_result downstream_channel_width_meters fill_depth_meters
#> 1        Barrier                               3                 3
#> 2      Potential                              10                 7
#> 3          Other                               3                 6
#>   fill_dpth_over                                    crossing_fix
#> 1              0          Replace with New Open Bottom Structure
#> 2              4          Replace with New Open Bottom Structure
#> 3              3 Replace Structure with Streambed Simulation CBS
#>   recommended_diameter_or_span_meters
#> 1                                  15
#> 2                                  27
#> 3                                   3

```
