# reportFootprints

Reports the consumption-based (embodied) resource footprints of MAgPIE
results in the standard reporting-variable hierarchy for shiny/mip.
Built on [`footprints`](footprints.md) /
[`embodiedResourceKastner`](embodiedResourceKastner.md) (the
column-normalised Kastner allocation), so each variable is the amount of
the resource embodied in the products a region CONSUMES (production
footprint of the goods consumed there, including via imports). Three
metrics are reported under a common tree,
`Footprints|<metric>|<Resource>|...`:

- **Total** — absolute footprint (Mha, km3, Mt CO2eq, mio people);

- **Per-Capita** — footprint divided by population;

- **Per-Tonne** — footprint per tonne of the final product consumed in
  each pathway.

For *Total* and *Per-Capita* the product tree (Crops -\> Cereals/Oil
crops/..., Livestock products, Pasture, Bioenergy crops, Forage) is
reported with `+` summation markers and the prim/secd/feed pathway split
(Primary/Secondary/Livestock end use) with `++` markers; both partitions
sum to the resource total. Per-Capita stays additive because every
product and pathway shares the same population denominator (and the
global value is the global footprint divided by global population, not
the sum of regional per-capita values). *Per-Tonne* is NOT additive —
each pathway has its own tonnes denominator (primary demand / secondary
(ksd) demand / livestock (kli) demand) — so it is reported only at the
pathway level as flat variables with NO summation markers and no grand
total.

NB requires a BILATERAL MAgPIE run (bilateral trade in the GDX). It is
called from [`getReport`](getReport.md) once per resource (so each is a
right-sized worker in the parallel report pool rather than one worker
holding all four). On standard runs the bilateral trade matrix
(`ov21_trade` with an exporter-importer dimension) is absent, so the
function emits a message and returns `NULL`; the reporting wrapper then
simply omits the footprint variables.

## Usage

``` r
reportFootprints(
  gdx,
  level = "regglo",
  resources = c("land", "emissions", "water", "labor"),
  reassignLivestock = TRUE,
  secdToFeed = TRUE
)
```

## Arguments

- gdx:

  GDX file (bilateral MAgPIE run).

- level:

  spatial aggregation: "reg", "glo" or "regglo" (default). The footprint
  is computed at "reg"; for the global total the absolute footprint and
  each denominator are summed over regions and only then divided, so the
  per-capita/per-tonne globals are correct intensive quantities.

- resources:

  character vector of resources to report; any subset of
  `c("land", "emissions", "water", "labor")`.

- reassignLivestock:

  logical; if TRUE (default) the Livestock pathway carries the full
  livestock footprint (kli products' own footprint moved into feed). See
  [`reassignLivestockPathway`](reassignLivestockPathway.md).

- secdToFeed:

  logical; if TRUE (default) route each crop's processed-then-fed share
  from secd to feed (and drop the fed-secondary share from the per-tonne
  secd denominator). See
  [`embodiedResourceKastner`](embodiedResourceKastner.md).

## Value

consumption footprints as a MAgPIE object with reporting names, or
`NULL` (with a message) if the GDX is not a bilateral trade run.

## Footprint variables (Land shown; analogous for Emissions/Water/Labor)

|                                        |             |                                                          |
|----------------------------------------|-------------|----------------------------------------------------------|
| Name                                   | Unit        | Meta                                                     |
| Footprints\|Total\|Land                | million ha  | Total consumption-based land footprint                   |
| Footprints\|Total\|Land\|+\|Crops      | million ha  | Land embodied in crops consumed                          |
| Footprints\|Total\|Land\|++\|Livestock | million ha  | Footprint consumed via livestock (feed + kli own)        |
| Footprints\|Per-Capita\|Land           | ha / capita | Land footprint per capita (additive)                     |
| Footprints\|Per-Tonne\|Land\|Primary   | ha / t      | Land per tonne of primary product eaten directly         |
| Footprints\|Per-Tonne\|Land\|Secondary | ha / t      | Land per tonne of secondary (processed) product consumed |
| Footprints\|Per-Tonne\|Land\|Livestock | ha / t      | Land per tonne of livestock product consumed             |

## See also

[`footprints`](footprints.md),
[`embodiedResourceKastner`](embodiedResourceKastner.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- reportFootprints(gdx)
} # }
```
