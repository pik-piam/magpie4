# embodiedEmissions

Consumption-based (embodied) emissions footprint using the
column-normalised Kastner allocation in
[`embodiedResourceKastner`](embodiedResourceKastner.md). Non-negative
and closes globally to total emissions. Pollutants are aggregated to a
single CO2-equivalent value per product (via `unit`); crop emissions are
allocated through primary-equivalent trade, livestock emissions through
direct trade.

## Usage

``` r
embodiedEmissions(
  gdx,
  file = NULL,
  level = "reg",
  type = "all",
  unit = "GWP100AR6",
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

- unit:

  GWP metric passed to `productEmissions` (default "GWP100AR6")

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

MAgPIE object (region, year, accounting.pathway.product) in Mt CO2eq.

## See also

[`embodiedResourceKastner`](embodiedResourceKastner.md),
`embodiedEmissions`

## Author

David M Chen
