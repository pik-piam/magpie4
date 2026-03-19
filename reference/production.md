# production

reads production out of a MAgPIE gdx file

## Usage

``` r
production(
  gdx,
  file = NULL,
  level = "reg",
  products = "kall",
  product_aggr = FALSE,
  attributes = "dm",
  water_aggr = TRUE,
  cumulative = FALSE,
  baseyear = 1995
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
  in gdxAggregate

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- product_aggr:

  aggregate over products or not (boolean)

- attributes:

  dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt
  ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm").
  Can also be a vector.

- water_aggr:

  aggregate irrigated and non-irriagted production or not (boolean).

- cumulative:

  Logical; Determines if production is reported annually (FALSE,
  default) or cumulative (TRUE)

- baseyear:

  Baseyear used for cumulative production (default = 1995)

## Value

production as MAgPIE object (unit depends on attributes and cumulative)

## See also

[`reportProduction`](reportProduction.md), [`demand`](demand.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- production(gdx)
} # }
```
