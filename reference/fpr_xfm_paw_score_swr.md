# Transform PSCIS Assessment Worksheet Data by Calculating Stream Width Ratio Score

This function calculates the stream width ratio (SWR) score for BC
Provincial Stream Crossing Inventory System (PSCIS) data based on
specified thresholds.

## Usage

``` r
fpr_xfm_paw_score_swr(
  dat,
  col_stream_width_ratio = stream_width_ratio,
  col_stream_width_ratio_score = stream_width_ratio_score,
  risk_low_value = 1,
  risk_high_value = 1.3,
  risk_low_score = 0,
  risk_mod_score = 3,
  risk_high_score = 6
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

- col_stream_width_ratio:

  \[character\] A column name specifying the stream width ratio, as a
  string or tidy-select syntax. Default is \`stream_width_ratio\`.

- col_stream_width_ratio_score:

  \[character\] A column name for the output SWR score, as a string or
  tidy-select syntax. Default is \`stream_width_ratio_score\`.

- risk_low_value:

  \[numeric\] A numeric value representing the lower threshold for low
  stream width ratio risk. Default is \`1\`.

- risk_high_value:

  \[numeric\] A numeric value representing the threshold for high stream
  width ratio risk. Default is \`1.3\`.

- risk_low_score:

  \[numeric\] A numeric value representing the risk score for low SWR
  conditions. Default is \`0\`.

- risk_mod_score:

  \[numeric\] A numeric value representing the risk score for moderate
  SWR conditions. Default is \`3\`.

- risk_high_score:

  \[numeric\] A numeric value representing the risk score for high SWR
  conditions. Default is \`6\`.

## Value

\[dataframe\] A dataframe with the specified column for SWR score added
or updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
[`fpr_xfm_paw_score_cv_len()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_len.md),
[`fpr_xfm_paw_score_cv_slope()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_slope.md),
[`fpr_xfm_paw_score_embed()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_embed.md),
[`fpr_xfm_paw_score_final()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_final.md),
[`fpr_xfm_paw_score_outlet_drop()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_outlet_drop.md),
[`fpr_xfm_paw_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_swr.md),
[`fpr_xfm_paw_xing_fix_size()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_xing_fix_size.md)

## Examples

``` r
dat <- data.frame(
  stream_width_ratio = c(0.8, 1.2, 1.4, NA)
)
fpr_xfm_paw_score_swr(
  dat
)
#>   stream_width_ratio stream_width_ratio_score
#> 1                0.8                        0
#> 2                1.2                        3
#> 3                1.4                        6
#> 4                 NA                        0
```
