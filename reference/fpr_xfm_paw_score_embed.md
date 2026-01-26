# Transform PSCIS Assessment Worksheet Data by Calculating Embedment Score

This function calculates the embedment score for BC Provincial Stream
Crossing Inventory System (PSCIS) data based on conditions involving
continuous embedment, pipe diameter, and average depth of embedment.

## Usage

``` r
fpr_xfm_paw_score_embed(
  dat,
  col_continuous_embeddedment_yes_no = continuous_embeddedment_yes_no,
  col_diameter_or_span_meters = diameter_or_span_meters,
  col_average_depth_embededdment_meters = average_depth_embededdment_meters,
  col_embed_score = embed_score,
  risk_high = 10,
  risk_mod = 5,
  risk_low = 0
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

- col_continuous_embeddedment_yes_no:

  \[character\] A column name specifying continuous embedment ("Yes" or
  "No"), as a string or tidy-select syntax. Default is
  \`continuous_embeddedment_yes_no\`.

- col_diameter_or_span_meters:

  \[character\] A column name specifying the diameter or span, as a
  string or tidy-select syntax. Default is \`diameter_or_span_meters\`.

- col_average_depth_embededdment_meters:

  \[character\] A column name specifying the average depth of embedment,
  as a string or tidy-select syntax. Default is
  \`average_depth_embededdment_meters\`.

- col_embed_score:

  \[character\] A column name for the output embedment score, as a
  string or tidy-select syntax. Default is \`embed_score\`.

- risk_high:

  \[numeric\] A numeric value representing the risk score for
  non-continuous embedment. Default is \`10\`, aligning with the Pisces
  assessment worksheet.

- risk_mod:

  \[numeric\] A numeric value representing the risk score for moderate
  embedment conditions. Default is \`5\`, aligning with the Pisces
  assessment worksheet.

- risk_low:

  \[numeric\] A numeric value representing the risk score for low-risk
  embedment conditions. Default is \`0\`, aligning with the Pisces
  assessment worksheet.

## Value

\[dataframe\] A dataframe with the specified column for embedment score
added or updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
[`fpr_xfm_paw_score_cv_len()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_len.md),
[`fpr_xfm_paw_score_cv_slope()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_slope.md),
[`fpr_xfm_paw_score_final()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_final.md),
[`fpr_xfm_paw_score_outlet_drop()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_outlet_drop.md),
[`fpr_xfm_paw_score_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_swr.md),
[`fpr_xfm_paw_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_swr.md),
[`fpr_xfm_paw_xing_fix_size()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_xing_fix_size.md)

## Examples

``` r
dat <- data.frame(
  continuous_embeddedment_yes_no = c("No", "Yes", "Yes"),
  diameter_or_span_meters = c(2, NA, 3),
  average_depth_embededdment_meters = c(0.4, 0.1, NA)
)
fpr_xfm_paw_score_embed(
  dat
)
#>   continuous_embeddedment_yes_no diameter_or_span_meters
#> 1                             No                       2
#> 2                            Yes                      NA
#> 3                            Yes                       3
#>   average_depth_embededdment_meters embed_score
#> 1                               0.4          10
#> 2                               0.1           0
#> 3                                NA           0
```
