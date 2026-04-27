# embodiedBiodiversity

Calculates production-based and consumption-based (embodied)
biodiversity impact accounting using bilateral trade flows. Biodiversity
values (BII-weighted area) are allocated to traded products based on
cropland and pasture area shares, similar to how CO2 LUC emissions are
allocated.

## Usage

``` r
embodiedBiodiversity(
  gdx,
  file = NULL,
  level = "reg",
  type = "all",
  indicator = "bv",
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
  (consumption-based), "trade" (export, import, and net-trade), "all"
  (all five), or "flows" (bilateral flows, requires bilateral=TRUE)

- indicator:

  Which biodiversity indicator to use: "bv" (biodiversity value,
  BII-weighted area in Mha), "bii_loss" (1-BII, representing
  biodiversity loss)

- bilateral:

  Logical; if TRUE, returns bilateral flows with dimensions
  (exporter.importer, year, product) instead of regional totals (default
  FALSE)

## Value

Embodied biodiversity impact as MAgPIE object. When bilateral=FALSE:
dimensions are (region, year, accounting.product). When bilateral=TRUE:
dimensions are (exporter.importer, year, product).

## Details

Biodiversity is measured via the Biodiversity Intactness Index (BII)
which ranges from 0 to 1. BII is calculated at the land cover class
level (crop_ann, crop_per, manpast, rangeland, etc.) and not directly
per product. This function allocates the biodiversity impact from
cropland to individual crop products based on their area shares.

The indicator "bv" returns the BII-weighted area (higher = more
biodiversity preserved), allocated to each crop by its share of total
cropland area.

The indicator "bii_loss" returns the biodiversity loss ((1-BII) \* area)
for aggregate cropland/pasture, then allocates to individual products by
area share. This represents "share of biodiversity loss attributable to
this crop based on its area share."

## See also

[`BII`](BII.md), [`land`](land.md), [`croparea`](croparea.md),
[`trade`](trade.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- embodiedBiodiversity(gdx, type = "all")
  # Bilateral flows
  xBilat <- embodiedBiodiversity(gdx, type = "flows", bilateral = TRUE)
} # }
```
