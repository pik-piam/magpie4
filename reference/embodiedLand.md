# embodiedLand

Consumption-based (embodied) land footprint using the column-normalised
Kastner allocation in
[`embodiedResourceKastner`](embodiedResourceKastner.md). Unlike
`embodiedLand` (which uses production + net-trade and can produce local
negatives), this distributes each region's actual cropland and pasture
to consumers, so the consumption footprint is non-negative and closes
globally to total agricultural land.

## Usage

``` r
embodiedLand(
  gdx,
  file = NULL,
  level = "reg",
  type = "all",
  landType = "all",
  bilateral = FALSE,
  secdToFeed = TRUE,
  reassignLivestock = TRUE
)
```

## Arguments

- gdx:

  GDX file

- file:

  optional file name to write the result with `write.magpie`

- level:

  regional aggregation level (only "reg" supported)

- type:

  "production", "consumption", "trade", or "all" (default)

- landType:

  "all" (crop + pasture), "crop", or "past"

- bilateral:

  logical; if TRUE return bilateral (exporter.importer) flows

- secdToFeed:

  logical; if TRUE (default) move the processed-then-fed share (e.g.
  soybean -\> oilcake -\> feed) from the secd pathway to the feed
  pathway, so the Livestock pathway captures all crop products that end
  up as feed. See
  [`embodiedResourceKastner`](embodiedResourceKastner.md).

- reassignLivestock:

  logical; if TRUE (default) move every livestock product's whole
  footprint into the feed (Livestock) pathway. See
  [`embodiedResourceKastner`](embodiedResourceKastner.md). A no-op for
  land (no kli land).

## Value

MAgPIE object in Mha. When bilateral=FALSE: (region, year,
accounting.pathway.product). When bilateral=TRUE: (exporter.importer,
year, pathway.product).

## See also

[`embodiedResourceKastner`](embodiedResourceKastner.md), `embodiedLand`

## Author

David M Chen
