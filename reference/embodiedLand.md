# embodiedLand

Calculates production-based and consumption-based (embodied) land
footprint accounting using bilateral trade flows. Land use is allocated
to traded products based on production ratios and bilateral trade
patterns.

## Usage

``` r
embodiedLand(
  gdx,
  file = NULL,
  level = "reg",
  type = "all",
  landType = "all",
  bilateral = FALSE
)
```

## Source

tradSecondaryToPrimary.R

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate. Only used when bilateral=FALSE.

- type:

  Type of accounting: "production" (production-based), "consumption"
  (consumption-based), "trade" (export, import, and net-trade), "all"
  (all five), or "flows" (bilateral flows, requires bilateral=TRUE)

- landType:

  Type of land to report: "crop" (cropland), "past" (pasture), "all"
  (total agricultural land), or a vector of specific land types

- bilateral:

  Logical; if TRUE, returns bilateral flows with dimensions
  (exporter.importer, year, product) instead of regional totals (default
  FALSE)

## Value

Embodied land use as MAgPIE object. When bilateral=FALSE: dimensions are
(region, year, accounting.product). When bilateral=TRUE: dimensions are
(exporter.importer, year, product).

## See also

[`land`](land.md), [`croparea`](croparea.md), [`trade`](trade.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- embodiedLand(gdx, type = "all", landType = "all")
  # Bilateral flows
  xBilat <- embodiedLand(gdx, type = "flows", bilateral = TRUE)
} # }
```
