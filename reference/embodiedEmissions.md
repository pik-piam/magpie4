# embodiedEmissions

Calculates production-based and consumption-based (embodied) emissions
accounting using bilateral trade flows. For livestock products,
emissions are attributed where livestock is produced (enteric
fermentation, AWMS). For crop products, primary equivalents can be used.
This uses the Kastner bilateral trade adjustment method.

## Usage

``` r
embodiedEmissions(
  gdx,
  file = NULL,
  level = "reg",
  type = "all",
  unit = "GWP100AR6",
  pollutants = "all",
  aggregation = "pollutant",
  kastner = TRUE,
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
  in superAggregate

- type:

  Type of accounting: "production" (production-based), "consumption"
  (consumption-based), "trade" (export, import, and net-trade), "all"
  (all five), or "flows" (bilateral flows, requires bilateral=TRUE)

- unit:

  GWP metric: "GWP100AR5", "GWP100AR6", "GWP\*AR5", "GWP\*AR6", "gas",
  or "element"

- pollutants:

  Selection of pollutants: "co2", "ch4", "n2o", "nh3", "no2", "no3" or
  "all"

- aggregation:

  Aggregate over products ("product"), pollutants ("pollutant"), both
  ("both"), or none (FALSE)

- kastner:

  Logical; apply Kastner bilateral trade adjustment (default TRUE)

- bilateral:

  Logical; if TRUE, returns bilateral flows with dimensions
  (exporter.importer, year, product) instead of regional totals (default
  FALSE)

## Value

Embodied emissions as MAgPIE object (unit depends on `unit`). When
bilateral=FALSE: dimensions are (region, year, accounting.product). When
bilateral=TRUE: dimensions are (exporter.importer, year, product).

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- embodiedEmissions(gdx, type = "all", unit = "GWP100AR6")
  # Bilateral flows
  xBilat <- embodiedEmissions(gdx, type = "flows", bilateral = TRUE)
} # }
```
