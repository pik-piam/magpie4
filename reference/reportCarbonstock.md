# reportCarbonstock

Reports the carbon stocks for future MAgPIE projections

## Usage

``` r
reportCarbonstock(gdx, level = "regglo", legacyEmis = TRUE)
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

- legacyEmis:

  Logical (default TRUE). If TRUE, add the legacy-clearing
  slash/deadwood pool ([`legacyEmissions`](legacyEmissions.md)) as an
  additive `+` child of `Resources|Carbon`, so the reported total carbon
  includes the carbon still held as slash/deadwood under the reporting
  reframe. This mirrors the `+|Legacy clearing` child on the emission
  side in [`reportEmissions`](reportEmissions.md) and keeps emissions
  and stocks consistent (the reframed Land-use Change flux equals minus
  the change in this pool). The soil/litter/ vegetation sub-pools stay
  model-native; only the aggregate changes (~0.5 percent).
  legacyEmis=FALSE =\> `Resources|Carbon` is the model's
  soil+litter+vegetation (backward compatible).

## Carbon stock variables

|                                            |      |                                                                                                |
|--------------------------------------------|------|------------------------------------------------------------------------------------------------|
| Name                                       | Unit | Meta                                                                                           |
| Resources\|Carbon                          | Mt C | Total terrestrial carbon stocks (incl. the legacy-clearing pool when legacyEmis=TRUE)          |
| Resources\|Carbon\|+\|Soil                 | Mt C | Soil carbon stocks                                                                             |
| Resources\|Carbon\|+\|Litter               | Mt C | Litter carbon stocks                                                                           |
| Resources\|Carbon\|+\|Vegetation           | Mt C | Vegetation carbon stocks (above and below ground biomass)                                      |
| Resources\|Carbon\|+\|Legacy clearing pool | Mt C | Slash/deadwood pool of the legacy-clearing reframe (additive child; only when legacyEmis=TRUE) |

## Author

Kristine Karstens, Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportSOM(gdx)
  } # }
```
