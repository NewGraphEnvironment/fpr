# Transform PSCIS Assessment Worksheet Data by Calculating Barrier Result

This function calculates the barrier result for PSCIS data based on the
crossing type and final score.

## Usage

``` r
fpr_xfm_paw_barrier_result(
  dat,
  col_crossing_type = crossing_type,
  col_final_score = final_score,
  col_barrier_result = barrier_result,
  risk_low_value = 15,
  risk_high_value = 19
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

- col_crossing_type:

  \[character\] A column name specifying the crossing type. Default is
  \`crossing_type\`.

- col_final_score:

  \[character\] A column name specifying the final score. Default is
  \`final_score\`.

- col_barrier_result:

  \[character\] A column name for the output barrier result. Default is
  \`barrier_result\`.

- risk_low_value:

  \[numeric\] A numeric value representing the lower threshold for
  low-risk final scores. Default is \`15\`.

- risk_high_value:

  \[numeric\] A numeric value representing the upper threshold for
  moderate-risk final scores. Default is \`19\`.

## Value

\[dataframe\] A dataframe with the specified column for barrier result
added or updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_score_cv_len()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_len.md),
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
  crossing_type = c("Open Bottom Structure", "Other", NA, "", "Other"),
  final_score = c(10, 16, 25, NA, 18)
)
fpr_xfm_paw_barrier_result(dat)
#>           crossing_type final_score barrier_result
#> 1 Open Bottom Structure          10       Passable
#> 2                 Other          16        Unknown
#> 3                  <NA>          25               
#> 4                                NA               
#> 5                 Other          18        Unknown
```
