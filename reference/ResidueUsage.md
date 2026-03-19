# ResidueUsage

reads Crop Residue Usage out of a MAgPIE gdx file

## Usage

``` r
ResidueUsage(
  gdx,
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

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in gdxAggregate

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- product_aggr:

  aggregate over products or not. Usually boolean, but here also the
  value "kres" is allowed, which provides kcr aggregated to kres

- attributes:

  dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt
  ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm").
  Can also be a vector.

- water_aggr:

  aggregate irrigated and non-irriagted production or not (boolean).

## Value

production as MAgPIE object (unit depends on attributes)

## See also

[`ResidueBiomass`](ResidueBiomass.md)

## Author

Kristine Karstens, Michael Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
x <- ResidueUsage(gdx)
} # }
```
