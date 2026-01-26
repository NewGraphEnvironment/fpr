# Transform PSCIS Assessment Worksheet Data by Calculating Outlet Drop Score

This function calculates the outlet drop score for PSCIS data based on
conditions involving outlet drop meters.

## Usage

``` r
fpr_xfm_paw_score_outlet_drop(
  dat,
  col_outlet_drop_meters = outlet_drop_meters,
  col_outlet_drop_score = outlet_drop_score,
  drop_risk_low = 0.15,
  drop_risk_high = 0.3,
  risk_mod = 5,
  risk_high = 10,
  risk_low = 0
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

- col_outlet_drop_meters:

  \[character\] A column name specifying the outlet drop meters, as a
  string or tidy-select syntax. Default is \`outlet_drop_meters\`.

- col_outlet_drop_score:

  \[character\] A column name for the output outlet drop score, as a
  string or tidy-select syntax. Default is \`outlet_drop_score\`.

- drop_risk_low:

  \[numeric\] A numeric value representing the lower threshold for low
  outlet drop risk. Default is \`0.15\`.

- drop_risk_high:

  \[numeric\] A numeric value representing the threshold for high outlet
  drop risk. Default is \`0.30\`.

- risk_mod:

  \[numeric\] A numeric value representing the risk score for moderate
  outlet drop conditions. Default is \`5\`.

- risk_high:

  \[numeric\] A numeric value representing the risk score for high
  outlet drop conditions. Default is \`10\`.

- risk_low:

  \[numeric\] A numeric value representing the risk score for low outlet
  drop conditions. Default is \`0\`.

## Value

\[dataframe\] A dataframe with the specified column for outlet drop
score added or updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
[`fpr_xfm_paw_score_cv_len()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_len.md),
[`fpr_xfm_paw_score_cv_slope()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_slope.md),
[`fpr_xfm_paw_score_embed()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_embed.md),
[`fpr_xfm_paw_score_final()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_final.md),
[`fpr_xfm_paw_score_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_swr.md),
[`fpr_xfm_paw_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_swr.md),
[`fpr_xfm_paw_xing_fix_size()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_xing_fix_size.md)

## Examples

``` r
dat <- data.frame(
  outlet_drop_meters = c(0.1, 0.2, 0.35, NA)
)
fpr_xfm_paw_score_outlet_drop(
  dat
)
#>   outlet_drop_meters outlet_drop_score
#> 1               0.10                 0
#> 2               0.20                 5
#> 3               0.35                10
#> 4                 NA                 0
```
