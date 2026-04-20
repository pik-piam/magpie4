# embodiedWater

Calculates production-based and consumption-based (embodied) water
footprint accounting using bilateral trade flows. Water use is allocated
to traded products based on production ratios and bilateral trade
patterns.

## Usage

``` r
embodiedWater(
  gdx,
  file = NULL,
  level = "reg",
  type = "all",
  waterType = "consumption",
  bilateral = FALSE
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
  in superAggregate. Only used when bilateral=FALSE.

- type:

  Type of accounting: "production" (production-based), "consumption"
  (consumption-based), "net-trade" (consumption minus production), "all"
  (all three), or "flows" (bilateral flows, requires bilateral=TRUE)

- waterType:

  Type of water to report: "withdrawal" (water withdrawal),
  "consumption" (consumptive water use)

- bilateral:

  Logical; if TRUE, returns bilateral flows with dimensions
  (exporter.importer, year, product) instead of regional totals (default
  FALSE)

## Value

Embodied water use as MAgPIE object. When bilateral=FALSE: dimensions
are (region, year, accounting.product). When bilateral=TRUE: dimensions
are (exporter.importer, year, product).

## See also

[`water_usage`](water_usage.md), [`trade`](trade.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- embodiedWater(gdx, type = "all", waterType = "consumption")
  # Bilateral flows
  xBilat <- embodiedWater(gdx, type = "flows", bilateral = TRUE)
} # }
```
