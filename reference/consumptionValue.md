# consumptionValue

calculates consumption value of different types based on a MAgPIE gdx
file.

## Usage

``` r
consumptionValue(
  gdx,
  file = NULL,
  level = "reg",
  products = "kall",
  product_aggr = TRUE,
  type = NULL,
  type_aggr = TRUE
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

  aggregate over products or not (boolean, default TRUE)

- type:

  Consumption type(s): "food", "feed", "processed", "other_util",
  "bioenergy", "seed", "waste", "dom_balanceflow; NULL returns all types

- type_aggr:

  aggregate over demand types or not (boolean, default TRUE)

## Value

A MAgPIE object containing consumption value in million \$US.

## Author

Miodrag Stevanovic

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- consumptionValue(gdx)
  } # }
```
