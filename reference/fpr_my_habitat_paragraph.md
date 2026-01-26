# Paragraph to describe the cover, widths, gradient and substrates.

Randomly generated order from the inputs of cover, channel and substrate
sentences.

## Usage

``` r
fpr_my_habitat_paragraph(loc = "us", sit = my_site)
```

## Arguments

- loc:

  String in quotes which is usually either 'us' upstream or 'ds'
  downstream. Defaults to 'us'. Corresponds to the location column of
  input dataframes that comes from a split of the alias_local_name

- sit:

  Integer. Corresponds to the PSCIS site id. Defaults to my_site defined
  in environment

## Value

String paragraph with the habitat characteristics for a site summarized
