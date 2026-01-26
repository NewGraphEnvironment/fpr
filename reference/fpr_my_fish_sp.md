# Pull out fish species long form names from our fish species codes in bcfishpass export

Pull out fish species long form names from our fish species codes in
bcfishpass export

## Usage

``` r
fpr_my_fish_sp(
  ...,
  col_pull = observedspp_upstr,
  sp_ignore = c("SST", "TR", "SP", "CRS")
)
```

## Arguments

- ...:

  Parameters to pass to fpr_my_bcfishpass

- col_pull:

  Name of column to pull from bcfishpass. Defaults to observedspp_upstr.
  observedspp_dnstr is the other option

- sp_to_ignore:

  Vector of strings representing species codes to ignore in output.
  Defaults to SST and TR as these seem a bit ambigous.

## Value

Vector
