# footprints

Consumption-based (embodied) resource footprints as ready-to-use MAgPIE
objects, on top of the column-normalised Kastner allocation in
[`embodiedResourceKastner`](embodiedResourceKastner.md). A single entry
point that (a) chooses the resource (`land`, `water`, `emissions` or
`labor`), (b) derives regional production / consumption / import /
export / net-trade by prim/secd/feed pathway and product from the
bilateral Kastner allocation, and (c) returns the footprint as an
absolute total, per capita, or per tonne of the final product consumed
in each pathway.

This is the reusable computation extracted from the bilateral footprint
plotting scripts, so any MAgPIE run can obtain the same numbers without
the plotting layer. Regional accounting is derived from the bilateral
object by aggregation: production = sum over importer; consumption = sum
over exporter; domestic = the self-trade diagonal; imports =
consumption - domestic; exports = production - domestic. The pathway
split of every accounting component is therefore keyed on the CONSUMER's
demand mix (consistent bilateral basis).

## Usage

``` r
footprints(
  gdx,
  resource = "land",
  type = "total",
  level = "reg",
  file = NULL,
  reassignLivestock = TRUE,
  secdToFeed = TRUE,
  bil = NULL,
  dem = NULL,
  ...
)
```

## Arguments

- gdx:

  GDX file (must contain bilateral trade, i.e. a bilateral MAgPIE run).

- resource:

  which resource footprint: "land", "water", "emissions" or "labor"
  (alias "labour").

- type:

  the footprint metric:

  - `"total"` (default): absolute embodied footprint (region, year,
    accounting.pathway.product), in the resource's own unit (Mha, km3,
    Mt CO2eq, mio people);

  - `"perCapita"`: total divided by population (region, year,
    accounting.pathway.product); unit is resource-per-person (e.g. Mha /
    mio people = ha / capita);

  - `"perTonne"`: footprint per tonne of the FINAL product consumed in
    each pathway (region, year, accounting.pathway), products summed
    within each pathway. The denominator is primary demand (prim),
    secondary (ksd) demand consumed as food (secd) and livestock (kli)
    demand (feed); see `secdToFeed`. Per-tonne is a pathway-level
    quantity, so the product dimension is collapsed.

- level:

  regional aggregation level (only "reg" supported by the underlying
  embodied\* functions).

- file:

  optional file name to write the result with `write.magpie`.

- reassignLivestock:

  logical; if TRUE (default) move every livestock product's whole
  footprint into the feed (Livestock) pathway, so the feed pathway
  carries the full livestock footprint. See
  [`reassignLivestockPathway`](reassignLivestockPathway.md).

- secdToFeed:

  logical; if TRUE (default) route each crop's processed-then-fed share
  from secd to feed, and (for `type = "perTonne"`) drop the fed share of
  secondary products from the secd denominator so it reads per tonne of
  secondary eaten as food. See
  [`embodiedResourceKastner`](embodiedResourceKastner.md).

- bil:

  optional precomputed RAW bilateral embodied object (exporter.importer,
  year, pathway.product) from
  `embodied*Kastner(bilateral = TRUE, reassignLivestock = FALSE)`. When
  supplied the expensive Kastner computation is skipped and this object
  is used directly (reassignment, if requested, is applied here).
  Enables reuse of a cached bilateral allocation.

- dem:

  optional precomputed demand object (region, year, demand.product), as
  returned by `demand(gdx, level)` WITHOUT the dom_balanceflow category;
  only used for `type = "perTonne"`. Read from `gdx` if not supplied.

- ...:

  further resource-specific arguments passed to the chosen
  `embodied*Kastner` wrapper (e.g. `landType`, `waterType`, `unit`);
  ignored when `bil` is supplied.

## Value

MAgPIE object; layout depends on `type` (see above).

## See also

[`embodiedResourceKastner`](embodiedResourceKastner.md),
[`reportFootprints`](reportFootprints.md),
[`footprintDemand`](footprintDemand.md),
[`reassignLivestockPathway`](reassignLivestockPathway.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  land   <- footprints(gdx, "land", type = "total")       # Mha, by product & pathway
  emisPC <- footprints(gdx, "emissions", type = "perCapita")
  waterT <- footprints(gdx, "water", type = "perTonne")   # by pathway
} # }
```
