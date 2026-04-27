# embodiedLabor

Calculates production-based and consumption-based (embodied) labor
footprint accounting using bilateral trade flows. Employment (number of
people) is allocated to traded products based on production ratios and
bilateral trade patterns.

## Usage

``` r
embodiedLabor(gdx, file = NULL, level = "reg", type = "all", bilateral = FALSE)
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
  (consumption-based), "trade" (export, import, and net-trade), "all"
  (all five), or "flows" (bilateral flows, requires bilateral=TRUE)

- bilateral:

  Logical; if TRUE, returns bilateral flows with dimensions
  (exporter.importer, year, product) instead of regional totals (default
  FALSE)

## Value

Embodied employment as MAgPIE object (number of people). When
bilateral=FALSE: dimensions are (region, year, accounting.product). When
bilateral=TRUE: dimensions are (exporter.importer, year, product).

## See also

[`agEmployment`](agEmployment.md), [`trade`](trade.md),
[`embodiedLand`](embodiedLand.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- embodiedLabor(gdx, type = "all")
  # Bilateral flows
  xBilat <- embodiedLabor(gdx, type = "flows", bilateral = TRUE)
} # }
```
