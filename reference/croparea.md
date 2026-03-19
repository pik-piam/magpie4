# croparea

reads croparea out of a MAgPIE gdx file. Croparea excludes fallow land.

## Usage

``` r
croparea(
  gdx,
  file = NULL,
  level = "reg",
  products = "kcr",
  product_aggr = TRUE,
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
  in gdxAggregate

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- product_aggr:

  aggregate over products or not (boolean)

- water_aggr:

  aggregate irrigated and non-irriagted production or not (boolean).

## Value

production as MAgPIE object (unit depends on attributes)

## See also

[`reportCroparea`](reportCroparea.md)

## Author

Jan Philipp Dietrich, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- croparea(gdx)
} # }
```
