# tradeValue

Calculates the value of traded goods based on a gdx file

## Usage

``` r
tradeValue(
  gdx,
  file = NULL,
  level = "reg",
  products = "k_trade",
  product_aggr = FALSE,
  type = "net-exports",
  glo_weight = "export",
  relative = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("reg", "glo", "regglo")

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- product_aggr:

  aggregate over products or not (boolean)

- type:

  exports-imports ("net-exports"), gross imports ("imports") or gross
  exports ("exports"); only valid if relative=FALSE

- glo_weight:

  Decides the calculation of global prices. Weighting schemes are
  applied for estimation of global producer price. If `"export"` prices
  are calculated as average of regional exporters' prices, weighted by
  the export volumes. If `"production"` (default), prices are calculated
  as average of regional prices weighted by regional production.
  Alternatively, if `"free_trade"`, the global prices are directly taken
  from the shadow prices of the global trade constraint, and no
  averaging is performed. Alternatively, if `"constant_prices_initial"`
  constant 1995 global prices for each commodity are used as weight.

- relative:

  if relative=TRUE, self sufficiencies are reported (the amount of
  production divided by domestic demand)

## Value

A MAgPIE object containing the value of trade flows in Million of US
dollars

## Author

Misko Stevanovic, Florian Humpenoeder, Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- tradeValue(gdx)
} # }
```
