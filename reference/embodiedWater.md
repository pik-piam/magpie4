# embodiedWater

Consumption-based (embodied) water footprint using the column-normalised
Kastner allocation in
[`embodiedResourceKastner`](embodiedResourceKastner.md). Non-negative
and closes globally to total water use. Crop water is allocated through
primary-equivalent (feed-traced) trade; livestock water through direct
trade.

## Usage

``` r
embodiedWater(
  gdx,
  file = NULL,
  level = "reg",
  type = "all",
  waterType = "consumption",
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

- waterType:

  "consumption" (default) or "withdrawal"

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
  [`embodiedResourceKastner`](embodiedResourceKastner.md).

## Value

MAgPIE object (region, year, accounting.pathway.product), or bilateral.

## See also

[`embodiedResourceKastner`](embodiedResourceKastner.md), `embodiedWater`

## Author

David M Chen
