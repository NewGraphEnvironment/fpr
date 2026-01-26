# Transform PSCIS Assessment Worksheet Data by Calculating All Metrics, Scores, and Results Necessary for Barrier Risk Assessment

This function calculates the fall metrics, scores, and results for BC
Provincial Stream Crossing Inventory System (PSCIS) data, sums score
columns, and assigns \`barrier_result\`.

## Usage

``` r
fpr_xfm_paw_all_scores_result(dat)
```

## Arguments

- dat:

  \[dataframe\] A dataframe containing the PSCIS data.

## Value

\[dataframe\] A dataframe with all calculated metrics, scores, and
results necessary for barrier risk assessment.

## See also

Other xfm paw:
[`fpr_xfm_paw_barrier_result()`](http://www.newgraphenvironment.com/fpr/reference/fpr_xfm_paw_barrier_result.md),
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
path <- system.file("extdata", "pscis_phase1.xlsm", package = "fpr")
dat <- fpr_import_pscis(dir_root = fs::path_dir(path))
result <- fpr_xfm_paw_all_scores_result(dat)
head(result[, grep("score|barrier", names(result), ignore.case = TRUE)])
#> # A tibble: 6 × 7
#>   culvert_length_score embed_score outlet_drop_score culvert_slope_score
#>                  <dbl>       <dbl>             <dbl>               <dbl>
#> 1                    3          10                 5                  10
#> 2                    0          10                 0                   5
#> 3                    3          10                 0                   5
#> 4                    6          10                 0                   5
#> 5                    3          10                10                  10
#> 6                    6          10                 5                   0
#> # ℹ 3 more variables: stream_width_ratio_score <dbl>, final_score <dbl>,
#> #   barrier_result <chr>
```
