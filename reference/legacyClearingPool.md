# legacyClearingPool

Memoised core of [`legacyEmissions`](legacyEmissions.md): the expensive
shared computation (cell-level emisCO2 clearing read + Köppen
first-order-decay convolution) returning the annual slash/deadwood pool
at `level` in Mt C (net/storage/release/stock). Both
[`reportEmissions`](reportEmissions.md) (fluxes) and
[`reportCarbonstock`](reportCarbonstock.md) (stock) reach it through
`legacyEmissions`, so memoising it (like [`land`](land.md)) means
repeated calls in one process share the result; a
[`getReport`](getReport.md) pre-warm additionally lets the parallel
report workers inherit it copy-on-write. Cleared by
[`clearCacheMagpie4`](clearCacheMagpie4.md). Unit/cumulative transforms
live in the `legacyEmissions` wrapper.

## Usage

``` r
legacyClearingPool(
  gdx,
  level = "regglo",
  priming = "hist",
  primingStart = 1850,
  harvestSlashFrac = 0,
  a0Biome = c(A = 0.25, B = 0.25, C = 0.2, D = 0.15, E = 0.15),
  halfLifeBiome = c(A = 4, B = 8, C = 10, D = 20, E = 25)
)
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level (see legacyEmissions)

- priming, primingStart, harvestSlashFrac, a0Biome, halfLifeBiome:

  see [`legacyEmissions`](legacyEmissions.md)

## Value

MAgPIE object (region x annual year x name), Mt C, with names
legacy_net, legacy_storage, legacy_release and legacy_stock

## Author

Florian Humpenoeder
