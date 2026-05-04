# demand

Calculates MAgPIE demand out of a gdx file

## Usage

``` r
demand(
  gdx,
  file = NULL,
  level = "reg",
  products = "kall",
  product_aggr = FALSE,
  attributes = "dm",
  type = NULL,
  type_aggr = FALSE
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

  aggregate over products or not (boolean, default FALSE)

- attributes:

  dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt
  ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm").
  Can also be a vector.

- type:

  Demand type(s): "food", "feed", "processed", "other_util",
  "bioenergy", "seed", "waste", "dom_balanceflow; NULL returns all types

- type_aggr:

  aggregate over demand types or not (boolean, default FALSE)

## Value

demand as MAgPIE object (Unit depends on attributes)

## Details

Demand definitions are equivalent to FAO CBS categories

## Author

Benjamin Leon Bodirsky, Abhijeet Mishra, Miodrag Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- demand(level="regglo", products="kcr")
  } # }
```
