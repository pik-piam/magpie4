# reportEmissions

reports GHG emissions

## Usage

``` r
reportEmissions(gdx, level = "regglo", storageWood = TRUE, legacyEmis = TRUE)
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

- storageWood:

  Accounting for long term carbon storage in wood products. Default is
  TRUE.

- legacyEmis:

  Logical (default TRUE). If TRUE, fold a legacy-clearing correction
  into Land-use Change: the instantaneous aboveground clearing pulse
  (deforestation + other-land conversion) is re-spread over a
  first-order-decay slash/deadwood pool via
  [`legacyEmissions`](legacyEmissions.md), mirroring the HWP
  convolution. "Legacy" here is the Houghton-style delayed release of
  carbon from land already cleared (a slash/deadwood decay tail) - it is
  NOT the front-loaded "committed deforestation emissions" of the
  trade-footprint literature (which assigns the whole future tail to the
  clearing year). legacyEmis=FALSE =\> output is bit-identical to the
  raw instantaneous accounting. This is a REPORTING reframe only (the
  model still emits instantaneously and the carbon price is unaffected)
  and is NOT mass-conserving (the pre-1995 priming credits a ~+6 to +21
  percent legacy tail); the kernel and priming are literature-anchored
  and MUST NOT be tuned to a comparison cloud. See
  [`legacyEmissions`](legacyEmissions.md) for details and caveats.

## Value

GHG emissions as MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr, and Mt
CH4/yr, for cumulative emissions Gt CO2)

## Details

Comparability with historical LUC-CO2 sources has two axes (documented
in mrvalidation `calcValidGlobalCarbonBudget`): (1) PEAT -
`Emissions|CO2|Land|+|Land-use Change` includes peat (separable via its
`+|Peatland` child); the bookkeeping sources (BLUE/OSCAR/H&C/GCB/Gasser)
are ex-peat, so compare them to Land-use Change net of the Peatland
child. (2) INDIRECT (Grassi) - `+|Land-use Change` is a
direct/bookkeeping flux, whereas net `Emissions|CO2|Land` adds the
managed-land environmental sink (`+|Indirect`) and is the quantity
comparable to NGHGI/inventory sources (FAO/PRIMAP/EDGAR). Note the GCB
net-Land / Indirect series in mrvalidation is all-land S_LAND, NOT this
managed-land quantity, so do not benchmark net Land against it.

## CO2 land-use change emissions (yearly)

|                                                                                 |           |                                                                                                                                                                                                                                                                                                                                                                    |
|---------------------------------------------------------------------------------|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Name                                                                            | Unit      | Meta                                                                                                                                                                                                                                                                                                                                                               |
| Emissions\|CO2\|Land                                                            | Mt CO2/yr | Net CO2 flux from land use and land management, including environmental effects on managed land                                                                                                                                                                                                                                                                    |
| Emissions\|CO2\|Land\|+\|Indirect                                               | Mt CO2/yr | Carbon sink on managed land from environmental change (CO2 fertilization, climate, N deposition); managed land proxy following Grassi et al. 2021                                                                                                                                                                                                                  |
| Emissions\|CO2\|Land\|+\|Land-use Change                                        | Mt CO2/yr | Net CO2 flux from land-use change, harvest and regrowth                                                                                                                                                                                                                                                                                                            |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Deforestation                         | Mt CO2/yr | CO2 emissions from permanent deforestation                                                                                                                                                                                                                                                                                                                         |
| Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Primary forests        | Mt CO2/yr | CO2 emissions from deforestation of primary forests                                                                                                                                                                                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Secondary forests      | Mt CO2/yr | CO2 emissions from deforestation of secondary forests                                                                                                                                                                                                                                                                                                              |
| Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Forestry plantations   | Mt CO2/yr | CO2 emissions from deforestation of forestry plantations                                                                                                                                                                                                                                                                                                           |
| Emissions\|CO2\|Land\|Land-use Change\|Deforestation\|+\|Cropland Tree Cover    | Mt CO2/yr | CO2 emissions from removal of trees on cropland                                                                                                                                                                                                                                                                                                                    |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Forest degradation                    | Mt CO2/yr | CO2 emissions from forest degradation and shifting cultivation                                                                                                                                                                                                                                                                                                     |
| Emissions\|CO2\|Land\|Land-use Change\|Forest degradation\|+\|Primary forests   | Mt CO2/yr | CO2 emissions from degradation of primary forests                                                                                                                                                                                                                                                                                                                  |
| Emissions\|CO2\|Land\|Land-use Change\|Forest degradation\|+\|Secondary forests | Mt CO2/yr | CO2 emissions from degradation of secondary forests                                                                                                                                                                                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Other land conversion                 | Mt CO2/yr | CO2 emissions from conversion of other natural land                                                                                                                                                                                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Regrowth                              | Mt CO2/yr | CO2 removals from forest regrowth (negative values)                                                                                                                                                                                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|CO2-price AR                | Mt CO2/yr | CO2 removals from afforestation/reforestation driven by CO2 price                                                                                                                                                                                                                                                                                                  |
| Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|NPI_NDC AR                  | Mt CO2/yr | CO2 removals from afforestation/reforestation under national policies                                                                                                                                                                                                                                                                                              |
| Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Timber Plantations          | Mt CO2/yr | CO2 removals from timber plantation regrowth                                                                                                                                                                                                                                                                                                                       |
| Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Secondary Forest            | Mt CO2/yr | CO2 removals from secondary forest regrowth                                                                                                                                                                                                                                                                                                                        |
| Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Cropland Tree Cover         | Mt CO2/yr | CO2 removals from trees on cropland                                                                                                                                                                                                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|Regrowth\|+\|Other Land                  | Mt CO2/yr | CO2 removals from regrowth on other land                                                                                                                                                                                                                                                                                                                           |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Peatland                              | Mt CO2/yr | Net CO2 flux from managed peatlands                                                                                                                                                                                                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|Peatland\|+\|Positive                    | Mt CO2/yr | CO2 emissions from drained peatlands                                                                                                                                                                                                                                                                                                                               |
| Emissions\|CO2\|Land\|Land-use Change\|Peatland\|+\|Negative                    | Mt CO2/yr | CO2 removals from rewetted peatlands                                                                                                                                                                                                                                                                                                                               |
| Emissions\|CO2\|Land\|Land-use Change\|Excl Peatland                            | Mt CO2/yr | Net CO2 flux from land-use change excluding peat (non-additive memo; like-for-like with ex-peat bookkeeping sources)                                                                                                                                                                                                                                               |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Soil                                  | Mt CO2/yr | Net CO2 flux from soil organic matter changes                                                                                                                                                                                                                                                                                                                      |
| Emissions\|CO2\|Land\|Land-use Change\|Soil\|++\|Emissions                      | Mt CO2/yr | CO2 emissions from soil carbon loss                                                                                                                                                                                                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|Soil\|++\|Withdrawals                    | Mt CO2/yr | CO2 removals from soil carbon accumulation                                                                                                                                                                                                                                                                                                                         |
| Emissions\|CO2\|Land\|Land-use Change\|Soil\|+\|Land Conversion                 | Mt CO2/yr | Net soil CO2 flux from land-use conversion                                                                                                                                                                                                                                                                                                                         |
| Emissions\|CO2\|Land\|Land-use Change\|Soil\|+\|Cropland management             | Mt CO2/yr | Net soil CO2 flux from cropland management changes                                                                                                                                                                                                                                                                                                                 |
| Emissions\|CO2\|Land\|Land-use Change\|Soil\|+\|Soil Carbon Management          | Mt CO2/yr | Net soil CO2 flux from explicit soil carbon management                                                                                                                                                                                                                                                                                                             |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Wood Harvest                          | Mt CO2/yr | CO2 emissions from wood harvest                                                                                                                                                                                                                                                                                                                                    |
| Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Timber Plantations      | Mt CO2/yr | CO2 emissions from harvest in timber plantations                                                                                                                                                                                                                                                                                                                   |
| Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Primary Forest          | Mt CO2/yr | CO2 emissions from harvest in primary forests                                                                                                                                                                                                                                                                                                                      |
| Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Secondary Forest        | Mt CO2/yr | CO2 emissions from harvest in secondary forests                                                                                                                                                                                                                                                                                                                    |
| Emissions\|CO2\|Land\|Land-use Change\|Wood Harvest\|+\|Other Land              | Mt CO2/yr | CO2 emissions from harvest on other land                                                                                                                                                                                                                                                                                                                           |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Timber                                | Mt CO2/yr | Net CO2 flux from harvested wood products (storage minus release)                                                                                                                                                                                                                                                                                                  |
| Emissions\|CO2\|Land\|Land-use Change\|Timber\|+\|Storage in HWP                | Mt CO2/yr | CO2 stored in harvested wood products (negative values)                                                                                                                                                                                                                                                                                                            |
| Emissions\|CO2\|Land\|Land-use Change\|Timber\|+\|Release from HWP              | Mt CO2/yr | CO2 released from decay of harvested wood products                                                                                                                                                                                                                                                                                                                 |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Legacy clearing                       | Mt CO2/yr | REPORTING REFRAME: net correction spreading the instantaneous aboveground clearing pulse (deforestation + other-land conversion) over a slash/deadwood decay tail, mirroring the HWP convolution. Added on top of the raw Deforestation/Other-land-conversion lines, which stay at 100 percent instantaneous. Always reported; exactly zero when legacyEmis=FALSE. |
| Emissions\|CO2\|Land\|Land-use Change\|Legacy clearing\|+\|Storage              | Mt CO2/yr | Clearing carbon deferred into the slash/deadwood pool this year (strictly \<= 0, a withdrawal from the instantaneous flux). Always reported; zero when legacyEmis=FALSE.                                                                                                                                                                                           |
| Emissions\|CO2\|Land\|Land-use Change\|Legacy clearing\|+\|Release              | Mt CO2/yr | Clearing carbon released from decay of the slash/deadwood pool (strictly \>= 0). Always reported; zero when legacyEmis=FALSE.                                                                                                                                                                                                                                      |
| Emissions\|CO2\|Land\|Land-use Change\|Excl Legacy clearing                     | Mt CO2/yr | Net CO2 flux from land-use change excluding the legacy-clearing reframe: the raw instantaneous accounting, equal to the legacyEmis=FALSE value of +\|Land-use Change (non-additive memo; only when legacyEmis=TRUE)                                                                                                                                                |
| Emissions\|CO2\|Land\|Land-use Change\|Gross Positive                           | Mt CO2/yr | Gross source of the LUC-CO2 flux: all positive process contributions summed (non-additive memo; net = Gross Positive + Gross Negative). Mirrors ScenarioMIP Gross Emissions (Land part).                                                                                                                                                                           |
| Emissions\|CO2\|Land\|Land-use Change\|Gross Negative                           | Mt CO2/yr | Gross sink of the LUC-CO2 flux: all negative process contributions summed (non-additive memo). Mirrors ScenarioMIP Gross Removals (Land part).                                                                                                                                                                                                                     |
| Emissions\|CO2\|Land\|Land-use Change\|+\|Residual                              | Mt CO2/yr | Residual CO2 flux not captured in other categories                                                                                                                                                                                                                                                                                                                 |
| Emissions\|CO2\|Land\|Land-use Change\|Residual\|+\|Positive                    | Mt CO2/yr | Positive residual CO2 flux                                                                                                                                                                                                                                                                                                                                         |
| Emissions\|CO2\|Land\|Land-use Change\|Residual\|+\|Negative                    | Mt CO2/yr | Negative residual CO2 flux                                                                                                                                                                                                                                                                                                                                         |
| Emissions\|CO2\|Land\|++\|Above Ground Carbon                                   | Mt CO2/yr | CO2 flux from above ground carbon pools                                                                                                                                                                                                                                                                                                                            |
| Emissions\|CO2\|Land\|++\|Below Ground Carbon                                   | Mt CO2/yr | CO2 flux from below ground carbon pools                                                                                                                                                                                                                                                                                                                            |

## CO2 land carbon sink (yearly)

|                                                                        |           |                                                             |
|------------------------------------------------------------------------|-----------|-------------------------------------------------------------|
| Name                                                                   | Unit      | Meta                                                        |
| Emissions\|CO2\|Land Carbon Sink\|Grassi\|Managed Land\|Managed Forest | Mt CO2/yr | Carbon sink in managed forests following Grassi et al. 2021 |
| Emissions\|CO2\|Land Carbon Sink\|LPJmL                                | Mt CO2/yr | Total carbon sink from LPJmL vegetation model               |
| Emissions\|CO2\|Land Carbon Sink\|LPJmL\|+\|Managed Land               | Mt CO2/yr | Carbon sink on managed land from LPJmL                      |
| Emissions\|CO2\|Land Carbon Sink\|LPJmL\|+\|Unmanaged Land             | Mt CO2/yr | Carbon sink on unmanaged land from LPJmL                    |

## CO2 land-use change emissions (cumulative)

|                                                                             |        |                                                                                                                          |
|-----------------------------------------------------------------------------|--------|--------------------------------------------------------------------------------------------------------------------------|
| Name                                                                        | Unit   | Meta                                                                                                                     |
| Emissions\|CO2\|Land\|Cumulative                                            | Gt CO2 | Cumulative net CO2 flux from land use and land management, including environmental effects on managed land               |
| Emissions\|CO2\|Land\|Cumulative\|+\|Indirect                               | Gt CO2 | Cumulative carbon sink on managed land from environmental change                                                         |
| Emissions\|CO2\|Land\|Cumulative\|+\|Land-use Change                        | Gt CO2 | Cumulative net CO2 flux from land-use change, harvest and regrowth                                                       |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Deforestation         | Gt CO2 | Cumulative CO2 emissions from deforestation and degradation                                                              |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Regrowth              | Gt CO2 | Cumulative CO2 removals from regrowth                                                                                    |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Other land conversion | Gt CO2 | Cumulative CO2 emissions from other land conversion                                                                      |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Peatland              | Gt CO2 | Cumulative net CO2 flux from peatland                                                                                    |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|Excl Peatland            | Gt CO2 | Cumulative net CO2 flux from land-use change excluding peat (non-additive memo)                                          |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Soil                  | Gt CO2 | Cumulative net CO2 flux from soil                                                                                        |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Wood Harvest          | Gt CO2 | Cumulative CO2 emissions from wood harvest                                                                               |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Timber                | Gt CO2 | Cumulative net CO2 flux from harvested wood products                                                                     |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Legacy clearing       | Gt CO2 | Cumulative legacy clearing correction. Always reported; zero when legacyEmis=FALSE.                                      |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|Excl Legacy clearing     | Gt CO2 | Cumulative net CO2 flux from land-use change excluding the legacy reframe (non-additive memo; only when legacyEmis=TRUE) |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|+\|Residual              | Gt CO2 | Cumulative residual CO2 flux                                                                                             |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|Gross Positive           | Gt CO2 | Cumulative gross source of the LUC-CO2 flux (non-additive memo; net = Gross Positive + Gross Negative)                   |
| Emissions\|CO2\|Land\|Cumulative\|Land-use Change\|Gross Negative           | Gt CO2 | Cumulative gross sink of the LUC-CO2 flux (non-additive memo)                                                            |

## N2O emissions variables

|                                                                                           |           |                                                                                     |
|-------------------------------------------------------------------------------------------|-----------|-------------------------------------------------------------------------------------|
| Name                                                                                      | Unit      | Meta                                                                                |
| Emissions\|N2O\|Land                                                                      | Mt N2O/yr | Total N2O emissions from agriculture, forestry and other land use (IPCC category 3) |
| Emissions\|N2O\|Land\|+\|Agriculture                                                      | Mt N2O/yr | N2O emissions from the agriculture sector                                           |
| Emissions\|N2O\|Land\|Agriculture\|+\|Animal Waste Management                             | Mt N2O/yr | N2O emissions from animal waste management systems                                  |
| Emissions\|N2O\|Land\|Agriculture\|+\|Agricultural Soils                                  | Mt N2O/yr | N2O emissions from agricultural soils                                               |
| Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Inorganic Fertilizers           | Mt N2O/yr | N2O emissions from inorganic fertilizer application                                 |
| Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|Inorganic Fertilizers\|+\|Cropland | Mt N2O/yr | N2O emissions from fertilizer on cropland                                           |
| Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|Inorganic Fertilizers\|+\|Pasture  | Mt N2O/yr | N2O emissions from fertilizer on pasture                                            |
| Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Manure applied to Croplands     | Mt N2O/yr | N2O emissions from manure applied to croplands                                      |
| Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Decay of Crop Residues          | Mt N2O/yr | N2O emissions from decay of crop residues                                           |
| Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Soil Organic Matter Loss        | Mt N2O/yr | N2O emissions from soil organic matter loss                                         |
| Emissions\|N2O\|Land\|Agriculture\|Agricultural Soils\|+\|Pasture                         | Mt N2O/yr | N2O emissions from pasture soils (manure deposited by grazing animals)              |
| Emissions\|N2O\|Land\|+\|Peatland                                                         | Mt N2O/yr | N2O emissions from managed peatlands                                                |
| Emissions\|N2O\|Land\|Peatland\|+\|Managed                                                | Mt N2O/yr | N2O emissions from managed peatlands (excluding intact)                             |
| Emissions\|N2O\|Land\|+\|Biomass Burning                                                  | Mt N2O/yr | N2O emissions from biomass burning                                                  |
| Emissions\|N2O\|Land\|Biomass Burning\|+\|Burning of Crop Residues                        | Mt N2O/yr | N2O emissions from burning of crop residues                                         |

## CH4 emissions variables

|                                                                    |           |                                                                   |
|--------------------------------------------------------------------|-----------|-------------------------------------------------------------------|
| Name                                                               | Unit      | Meta                                                              |
| Emissions\|CH4\|Land                                               | Mt CH4/yr | Total CH4 emissions from agriculture, forestry and other land use |
| Emissions\|CH4\|Land\|+\|Agriculture                               | Mt CH4/yr | CH4 emissions from agriculture sector                             |
| Emissions\|CH4\|Land\|Agriculture\|+\|Rice                         | Mt CH4/yr | CH4 emissions from flooded rice cultivation                       |
| Emissions\|CH4\|Land\|Agriculture\|+\|Animal waste management      | Mt CH4/yr | CH4 emissions from animal waste management systems                |
| Emissions\|CH4\|Land\|Agriculture\|+\|Enteric fermentation         | Mt CH4/yr | CH4 emissions from enteric fermentation of livestock              |
| Emissions\|CH4\|Land\|+\|Peatland                                  | Mt CH4/yr | CH4 emissions from managed peatlands                              |
| Emissions\|CH4\|Land\|Peatland\|+\|Managed                         | Mt CH4/yr | CH4 emissions from managed peatlands (excluding intact)           |
| Emissions\|CH4\|Land\|+\|Biomass Burning                           | Mt CH4/yr | CH4 emissions from biomass burning                                |
| Emissions\|CH4\|Land\|Biomass Burning\|+\|Burning of Crop Residues | Mt CH4/yr | CH4 emissions from burning of crop residues                       |

## GWP emissions variables

|                                        |            |                                                                       |
|----------------------------------------|------------|-----------------------------------------------------------------------|
| Name                                   | Unit       | Meta                                                                  |
| Emissions\|GWP100AR6\|Land             | Gt CO2e/yr | Total GHG emissions from land use in CO2-equivalents using AR6 GWP100 |
| Emissions\|GWP100AR6\|Land\|Cumulative | Gt CO2e    | Cumulative total GHG emissions from land use                          |
| Emissions\|CH4_GWP100AR6\|Land         | Mt CO2e/yr | CH4 emissions in CO2-equivalents using AR6 GWP100 (factor 27)         |
| Emissions\|N2O_GWP100AR6\|Land         | Mt CO2e/yr | N2O emissions in CO2-equivalents using AR6 GWP100 (factor 273)        |

## Author

Florian Humpenoeder, Benjamin Leon Bodirsky, Michael Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportEmissions(gdx)
} # }
```
