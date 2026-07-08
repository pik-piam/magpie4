# footprintDemand

Per-tonne denominator for consumption-based (embodied) resource
footprints: the tonnes of the FINAL product consumed in each
prim/secd/feed pathway. Used by [`footprints`](footprints.md)
(`type = "perTonne"`) and [`reportFootprints`](reportFootprints.md) to
turn an absolute footprint into a per-tonne intensity that is comparable
across resources.

- `prim` = primary products eaten directly (all demand categories except
  the processed and feed categories);

- `secd` = tonnes of SECONDARY (ksd) products consumed (the processing
  OUTPUT, not the primary input); with `secdToFeed = TRUE` the
  fed-secondary share is excluded so it reads per tonne of secondary
  eaten as food/non-feed;

- `feed` = tonnes of LIVESTOCK (kli) products consumed.

## Usage

``` r
footprintDemand(dem, prods, kli, ksd, secdToFeed = TRUE)
```

## Arguments

- dem:

  demand object (region, year, demand.product) as returned by
  `demand(gdx, level)`, with the `dom_balanceflow` category already
  removed.

- prods:

  products present in the footprint (used to restrict the primary set);
  typically `getItems(<footprint>, dim = "product")`.

- kli:

  livestock product set (`readGDX(gdx, "kli")`).

- ksd:

  secondary (processed) product set (`readGDX(gdx, "ksd")`).

- secdToFeed:

  logical; if TRUE (default) drop the fed share of secondary products
  from the secd denominator so it reads per tonne of secondary eaten as
  food. Should match the `secdToFeed` used for the numerator. See
  [`embodiedResourceKastner`](embodiedResourceKastner.md).

## Value

MAgPIE object (region, year, pathway) with pathway in {prim, secd,
feed}, in tonnes (Mt DM) of the final product consumed.

## See also

[`footprints`](footprints.md), [`reportFootprints`](reportFootprints.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  dem   <- demand(gdx, level = "reg")[, , "dom_balanceflow", invert = TRUE]
  denom <- footprintDemand(dem, getItems(dem, dim = 3.2),
                           kli = readGDX(gdx, "kli"), ksd = readGDX(gdx, "ksd"))
} # }
```
