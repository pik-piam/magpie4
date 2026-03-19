# gdxAggregate

aggregates and disaggregates on spatial scales using mappings from the
gdx files.

## Usage

``` r
gdxAggregate(gdx, x, weight = NULL, to, absolute = TRUE, ...)
```

## Arguments

- gdx:

  gdx file

- x:

  object to be aggrgeagted or disaggregated

- weight:

  weight can be either an object or a functionname in "", where the
  function provides the weight

- to:

  either a fixed target aggregation level (grid, cell, iso, reg, glo,
  regglo) or the name of a mapping based on regions

- absolute:

  is it a absolute or a relative value (absolute: tons, relative: tons
  per hectare)

- ...:

  further parameters handed on to weight function.

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

Other Spatial: [`addGeometry()`](addGeometry.md),
[`clusterOutputToTerraVector()`](clusterOutputToTerraVector.md),
[`mappingToLongFormat()`](mappingToLongFormat.md),
[`superAggregateX()`](superAggregateX.md)

## Author

Benjamin Leon Bodirsky, Edna J. Molina Bacca, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
gdp_pc <- income(gdx, level = "reg")
is.function(population)
gdp_pc_iso <- gdxAggregate(gdx = gdx, x = gdp_pc, weight = "population", to = "iso",
                           absolute = FALSE)
gdp_pc_glo <- gdxAggregate(gdx = gdx, x = gdp_pc, weight = "population", to = "glo",
                           absolute = FALSE)
gdp <- income(gdx, level = "reg", per_capita = FALSE)
gdp_iso <- gdxAggregate(gdx = gdx, x = gdp, weight = "population", to = "iso", absolute = TRUE)
gdp_glo <- gdxAggregate(gdx = gdx, x = gdp, weight = "population", to = "glo", absolute = TRUE)
} # }
```
