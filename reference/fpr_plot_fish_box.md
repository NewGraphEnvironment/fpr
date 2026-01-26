# Generate boxplot for individual site showing densities of fish captured from electrofishing

Generate boxplot for individual site showing densities of fish captured
from electrofishing

## Usage

``` r
fpr_plot_fish_box(dat = fish_abund, sit = my_site, theme = ggplot2::theme_bw())
```

## Arguments

- dat:

  Dataframe. Defaults to fish_abund. Must contian the columns site,
  species_code, life_stage, density_100m2

- sit:

  Integer. ID for the site. Defaults to my_site.

- theme:

  Function. Theme used for the plot. Defaults to
  \`ggplot2::theme_bw()\`.

## Value

Boxplot
