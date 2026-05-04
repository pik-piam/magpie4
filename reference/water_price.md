# water_price

reads water prices from a MAgPIE gdx file

## Usage

``` r
water_price(
  gdx,
  file = NULL,
  level = "reg",
  weight = "value",
  index = FALSE,
  index_baseyear = 2005,
  digits = 4
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global) or any other
  aggregation level defined in superAggregate

- weight:

  For determining weights to use for generating water prices at levels
  beyond 'cellular'. Takes "value" and "quantity". "value" sums regional
  weights by value of water per cluster, "quantity" sums regional weight
  by qty of water per cluster

- index:

  FALSE (default) or TRUE

- index_baseyear:

  baseyear to use for index calculation (only used if index=TRUE)

- digits:

  integer. For rounding of the return values

## Value

A MAgPIE object containing the water shadow prices (US Dollar/cubic
metre).

## Author

Markus Bonsch, Vartika Singh, Miodrag Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- water_price(gdx)
  } # }
```
