# Summary table for fish densities within individual sites

Summary table for fish densities within individual sites

## Usage

``` r
fpr_table_fish_density(dat = fish_abund, sit = my_site, ...)
```

## Arguments

- dat:

  Dataframe. Defaults to fish_abund. Must have columns site, local_name,
  species_code, life_stage, catch, density_100m2, nfc_pass

- sit:

  Integer. Site ID. Defaults to my_site

- ...:

  Not used. Open for passing arguments to
  [fpr_kable](http://www.newgraphenvironment.com/fpr/reference/fpr_kable.md)

## See also

[fpr_kable](http://www.newgraphenvironment.com/fpr/reference/fpr_kable.md)
