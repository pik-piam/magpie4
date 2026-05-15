# yields

Calculates crop yields based on a MAgPIE gdx file

## Usage

``` r
yields(
  gdx,
  file = NULL,
  level = "reg",
  products = "kcr",
  product_aggr = FALSE,
  attributes = "dm",
  water_aggr = TRUE
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
  in superAggregateX

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr"), also including "pasture"

- product_aggr:

  aggregate over products or not (boolean)

- attributes:

  dry matter: Mt/ha ("dm"), gross energy: PJ/ha ("ge"), reactive
  nitrogen: Mt/ha ("nr"), phosphor: Mt/ha ("p"), potash: Mt/ha ("k"),
  wet matter: Mt/ha ("wm"). Can also be a vector.

- water_aggr:

  aggregate irrigated and non-irriagted production or not (boolean).

## Value

crop yield as MAgPIE object (unit depends on attributes)

## See also

[`reportYields`](reportYields.md)

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- yields(gdx)
  } # }
```
