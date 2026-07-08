# embodiedResourceKastner

Generic consumption-based (embodied) resource footprint using a
column-normalised Kastner (2011) allocation. Each producing region's
resource total (e.g. cropland, water, emissions, labour) is distributed
to consuming regions using the bilateral commodity consumption shares
from the Kastner \\(I-A)^{-1}\\ solution, normalised so that each
origin's column sums exactly to its actual resource. This guarantees, by
construction:

- non-negativity of the consumption footprint (if the resource is
  non-negative);

- production\[r\] == resource\[r\];

- additivity: production - exports + imports == consumption;

- global closure: sum(consumption) == sum(resource).

Crop/pasture products are allocated through primary-equivalent
(feed-traced) trade; livestock products (`kli`) through direct bilateral
trade. The prim/secd/feed pathway split is derived from demand-category
shares (processed -\> secd, feed -\> feed, remainder -\> prim), as in
[`embodiedLand`](embodiedLand.md)/[`embodiedWater`](embodiedWater.md).

## Usage

``` r
embodiedResourceKastner(
  gdx,
  resource,
  file = NULL,
  level = "reg",
  type = "all",
  bilateral = FALSE,
  secdToFeed = TRUE,
  reassignLivestock = TRUE
)
```

## Arguments

- gdx:

  GDX file

- resource:

  MAgPIE object (region, year, product) of the resource total per
  product (production basis). Products may include crop/pasture and
  livestock (kli) products; the function routes them to the correct
  trade matrix automatically.

- file:

  optional file name to write the result with `write.magpie`

- level:

  regional aggregation level (only "reg" supported for now)

- type:

  "production", "consumption", "trade", or "all" (default); ignored when
  `bilateral = TRUE`

- bilateral:

  logical; if TRUE, returns the bilateral consumption allocation with
  dimensions (exporter.importer, year, pathway.product) where exporter =
  production origin and importer = consumer. Default FALSE.

- secdToFeed:

  logical; if TRUE, the share of each primary product that is processed
  and then fed to livestock (e.g. soybean -\> oilcake -\> feed) is moved
  from the secd pathway to the feed pathway. The processed-then-fed
  share is sSecd \* phi, where phi\[primary\] is the feed fraction of
  the secondary products derived from that primary (mass allocation via
  `primaryPerSecondary`). Default TRUE (processed soybean etc. is moved
  to feed, keyed on its immediate demand category).

- reassignLivestock:

  logical; if TRUE (default) every livestock (`kli`) product's whole
  footprint (all pathways) is moved into the `feed` (Livestock) pathway
  via [`reassignLivestockPathway`](reassignLivestockPathway.md), so the
  feed pathway carries the FULL livestock footprint (feed crops PLUS the
  livestock products' own enteric/manure/labour/water footprint) rather
  than only the feed crops. Orthogonal to `secdToFeed` (which re-routes
  a crop's processed-then-fed share); a no-op for resources without
  `kli` products (e.g. land). Per-product totals are conserved either
  way.

## Value

MAgPIE object. When `bilateral = FALSE`: (region, year,
accounting.pathway.product) with accounting in {production, consumption,
export, import, net-trade} and pathway in {prim, secd, feed}. When
`bilateral = TRUE`: (exporter.importer, year, pathway.product).

## Author

David M Chen
