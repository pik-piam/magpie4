# productionRevenue

calcluates production revenue based on a MAgPIE gdx file.

## Usage

``` r
productionRevenue(
  gdx,
  file = NULL,
  level = "reg",
  products = "kall",
  product_aggr = TRUE
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

  ggregate over products or not (boolean, default TRUE)

## Value

A MAgPIE object containing prodcution revenues.

## Author

Miodrag Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- productionRevenue(gdx)
  } # }
```
