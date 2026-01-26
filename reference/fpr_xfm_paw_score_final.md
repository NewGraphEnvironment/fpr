# Transform PSCIS Assessment Worksheet Data by Calculating Final Score

This function calculates the final score for BC Provincial Stream
Crossing Inventory System (PSCIS) data by summing specified score
columns.

## Usage

``` r
fpr_xfm_paw_score_final(
  dat,
  col_final_score = final_score,
  col_culvert_length_score = culvert_length_score,
  col_embed_score = embed_score,
  col_outlet_drop_score = outlet_drop_score,
  col_culvert_slope_score = culvert_slope_score,
  col_stream_width_ratio_score = stream_width_ratio_score
)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the score data.

- col_final_score:

  \[character\] A column name for the final score output, as a string or
  tidy-select syntax. Default is \`final_score\`.

- col_culvert_length_score:

  \[character\] A column name for the culvert length score, as a string
  or tidy-select syntax. Default is \`culvert_length_score\`.

- col_embed_score:

  \[character\] A column name for the embed score, as a string or
  tidy-select syntax. Default is \`embed_score\`.

- col_outlet_drop_score:

  \[character\] A column name for the outlet drop score, as a string or
  tidy-select syntax. Default is \`outlet_drop_score\`.

- col_culvert_slope_score:

  \[character\] A column name for the culvert slope score, as a string
  or tidy-select syntax. Default is \`culvert_slope_score\`.

- col_stream_width_ratio_score:

  \[character\] A column name for the stream width ratio score, as a
  string or tidy-select syntax. Default is \`stream_width_ratio_score\`.

## Value

\[dataframe\] A dataframe with the specified column for the final score
added or updated.

## See also

Other xfm paw:
[`fpr_xfm_paw_all_scores_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_all_scores_result.md),
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
[`fpr_xfm_paw_score_cv_len()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_len.md),
[`fpr_xfm_paw_score_cv_slope()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_cv_slope.md),
[`fpr_xfm_paw_score_embed()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_embed.md),
[`fpr_xfm_paw_score_outlet_drop()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_outlet_drop.md),
[`fpr_xfm_paw_score_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_score_swr.md),
[`fpr_xfm_paw_swr()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_swr.md),
[`fpr_xfm_paw_xing_fix_size()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_xing_fix_size.md)

## Examples

``` r
dat <- data.frame(
  culvert_length_score = c(1, 2, 3, NA),
  embed_score = c(2, 3, NA, 1),
  outlet_drop_score = c(3, NA, 2, 1),
  culvert_slope_score = c(4, 3, 2, NA),
  stream_width_ratio_score = c(5, NA, 4, 3)
)
fpr_xfm_paw_score_final(
  dat
)
#> # A tibble: 4 × 6
#>   culvert_length_score embed_score outlet_drop_score culvert_slope_score
#>                  <dbl>       <dbl>             <dbl>               <dbl>
#> 1                    1           2                 3                   4
#> 2                    2           3                NA                   3
#> 3                    3          NA                 2                   2
#> 4                   NA           1                 1                  NA
#> # ℹ 2 more variables: stream_width_ratio_score <dbl>, final_score <dbl>
```
