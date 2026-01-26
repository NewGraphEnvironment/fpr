# Transform PSCIS Assessment Worksheet Data by Calculating Culvert Length Score

This function calculates the culvert length score for BC Provincial
Stream Crossing Inventory System (PSCIS) data based on the given length
or width column.

## Usage

``` r
fpr_xfm_paw_score_cv_len(
  dat,
  col_length_or_width_meters = length_or_width_meters,
  col_culvert_length_score = culvert_length_score,
  risk_low_value = 15,
  risk_high_value = 30,
  risk_low_score = 0,
  risk_mod_score = 3,
  risk_high_score = 6
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

- col_length_or_width_meters:

  \[character\] Column name for length or width, as a string or
  tidy-select syntax. Default is \`length_or_width_meters\`.

- col_culvert_length_score:

  \[character\] Column name for the culvert length score, as a string.
  Default is \`culvert_length_score\`.

- risk_low_value:

  \[numeric\] A numeric value representing the lower threshold for
  culvert length risk. Default is \`15\`.

- risk_high_value:

  \[numeric\] A numeric value representing the threshold for culvert
  length risk. Default is \`30\`.

- risk_low_score:

  \[numeric\] A numeric value representing the low risk score. Default
  is \`0\`.

- risk_mod_score:

  \[numeric\] A numeric value representing the moderate risk score.
  Default is \`3\`.

- risk_high_score:

  \[numeric\] A numeric value representing the high risk score. Default
  is \`6\`.

## Value

\[dataframe\] A dataframe with \`culvert_length_score\` column added or
updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
[`fpr_xfm_paw_score_cv_slope()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_slope.md),
[`fpr_xfm_paw_score_embed()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_embed.md),
[`fpr_xfm_paw_score_final()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_final.md),
[`fpr_xfm_paw_score_outlet_drop()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_outlet_drop.md),
[`fpr_xfm_paw_score_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_swr.md),
[`fpr_xfm_paw_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_swr.md),
[`fpr_xfm_paw_xing_fix_size()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_xing_fix_size.md)

## Examples

``` r
dat <- data.frame(
  length_or_width_meters = c(10, 20, 35, NA)
)
fpr_xfm_culvert_length_score(
  dat,
  col_length_or_width_meters = length_or_width_meters
)
#> Error in fpr_xfm_culvert_length_score(dat, col_length_or_width_meters = length_or_width_meters): could not find function "fpr_xfm_culvert_length_score"
```
