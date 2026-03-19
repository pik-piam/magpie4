# bioplasticDemand

returns demand for bioplastic or demand for substrate for bioplastic
production

## Usage

``` r
bioplasticDemand(
  gdx,
  type = "bioplastic",
  detail = FALSE,
  level = "regglo",
  file = NULL
)
```

## Arguments

- gdx:

  GDX file

- type:

  "bioplastic" for bioplastic demand, "substrate" for biomass demand as
  substrate for bioplastic production

- detail:

  only relevant for type = "substrate". If TRUE, substrate demand is
  disaggregated by crop type, if FALSE only the aggregated demand is
  reported.

- level:

  spatial aggregation to report bioplastic/substrate demand (only "reg"
  or "regglo")

- file:

  a file name the output should be written to using write.magpie

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- bioplasticDemand(gdx)
} # }
```
