# Transform PSCIS Assessment Worksheet Data by Calculating Culvert Slope Score

This function calculates the culvert slope score for BC Provincial
Stream Crossing Inventory System (PSCIS) data based on conditions
involving culvert slope percent.

## Usage

``` r
fpr_xfm_paw_score_cv_slope(
  dat,
  col_culvert_slope_percent = culvert_slope_percent,
  col_culvert_slope_score = culvert_slope_score,
  slope_risk_low = 1,
  slope_risk_high = 3,
  risk_mod = 5,
  risk_high = 10,
  risk_low = 0
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

- col_culvert_slope_percent:

  \[character\] A column name specifying the culvert slope percent, as a
  string or tidy-select syntax. Default is \`culvert_slope_percent\`.

- col_culvert_slope_score:

  \[character\] A column name for the output culvert slope score, as a
  string or tidy-select syntax. Default is \`culvert_slope_score\`.

- slope_risk_low:

  \[numeric\] A numeric value representing the lower threshold for low
  culvert slope risk (in percent). Default is \`1\`.

- slope_risk_high:

  \[numeric\] A numeric value representing the threshold for high
  culvert slope risk (in percent). Default is \`3\`.

- risk_mod:

  \[numeric\] A numeric value representing the risk score for moderate
  culvert slope conditions. Default is \`5\`.

- risk_high:

  \[numeric\] A numeric value representing the risk score for high
  culvert slope conditions. Default is \`10\`.

- risk_low:

  \[numeric\] A numeric value representing the risk score for low
  culvert slope conditions. Default is \`0\`.

## Value

\[dataframe\] A dataframe with the specified column for culvert slope
score added or updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
[`fpr_xfm_paw_score_cv_len()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_len.md),
[`fpr_xfm_paw_score_embed()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_embed.md),
[`fpr_xfm_paw_score_final()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_final.md),
[`fpr_xfm_paw_score_outlet_drop()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_outlet_drop.md),
[`fpr_xfm_paw_score_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_swr.md),
[`fpr_xfm_paw_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_swr.md),
[`fpr_xfm_paw_xing_fix_size()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_xing_fix_size.md)

## Examples

``` r
dat <- data.frame(
  culvert_slope_percent = c(0.5, 1.5, 3.5, NA)
)
fpr_xfm_paw_score_cv_slope(
  dat
)
#>   culvert_slope_percent culvert_slope_score
#> 1                   0.5                   0
#> 2                   1.5                   5
#> 3                   3.5                  10
#> 4                    NA                   0
```
