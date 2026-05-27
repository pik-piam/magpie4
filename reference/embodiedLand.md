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
  bilateral = FALSE,
  disaggLivestock = FALSE
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

- disaggLivestock:

  Logical; if TRUE, the feed pathway retains the livestock product
  dimension, so land is attributed per animal product × feed crop
  combination. Passes `disaggLivestock` to `tradedPrimariesBilateral`.
  Use `dimSums(x[kli_items], dim=3.1)` to collapse to feed crops, or
  `dimSums(x[kli_items], dim=3.2)` to collapse to animal products.
  Default is FALSE (current behaviour: feed attributed to crops).

## Value

Embodied land use as MAgPIE object. When bilateral=FALSE and
disaggLivestock=FALSE: dim 3 = accounting.product (2 subdims). When
bilateral=FALSE and disaggLivestock=TRUE: dim 3 =
accounting.prim,secd,kli\_\*.product (3 subdims); production/consumption
have prim = crop+pasture land and kli\_\* = feed chain land per animal
product (secd=0 in production); trade types retain the full secd
pathway. Note: prim and kli\_\* items overlap (feed crops appear in
both), so they should not be summed — use one or the other for
attribution. When bilateral=TRUE: dim 3 = prim,secd,kli\_\*.product
(pathway.product).

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
