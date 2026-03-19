# prices

calcluates prices based on a MAgPIE gdx file

## Usage

``` r
prices(
  gdx,
  file = NULL,
  level = "reg",
  products = "kall",
  product_aggr = FALSE,
  attributes = "dm",
  type = "consumer",
  glo_weight = "production"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- product_aggr:

  aggregate over products or not (boolean)

- attributes:

  USD17MER per ton X (dm,nr,p,k,wm) except gross energy (ge) where it is
  USD17MER per GJ

- type:

  "consumer" or "producer" prices. Producers' prices are calculated on
  the regional level as a sum of regional trade equation marginal values
  and respective global trade equation marginal values.For the non
  traded commodities, both global and regional producers prices are set
  to zero instead of NaN.

- glo_weight:

  Decides the calculation of global prices. Weighting schemes are
  applied for estimation of global producer price. If `"export"` prices
  are calculated as average of regional exporters' prices, weighted by
  the export volumes. If `"production"` (default), prices are calculated
  as average of regional prices weighted by regional production. If
  `"free_trade"`, the global prices are directly taken from the shadow
  prices of the global trade constraint, and no averaging is performed.

## Value

A MAgPIE object containing the consumer's or producers' prices (unit
depends on attributes)

## Author

Misko Stevanovic, Florian Humpenoeder, Jan Philipp Dietrich, Xiaoxi
Wang, Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- prices(gdx)
} # }
```
