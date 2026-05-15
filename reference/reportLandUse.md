# reportLandUse

reports land-use

## Usage

``` r
reportLandUse(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  The aggregation level to be used ("regglo" by default)

## Value

land-use as MAgPIE object (million ha)

## Total land cover variables

|                                          |            |                                                   |
|------------------------------------------|------------|---------------------------------------------------|
| Name                                     | Unit       | Meta                                              |
| Resources\|Land Cover                    | million ha | Total land cover                                  |
| Resources\|Land Cover\|Agricultural land | million ha | Agricultural land including cropland and pastures |

## Cropland variables

|                                                |            |                                                                                                                                     |
|------------------------------------------------|------------|-------------------------------------------------------------------------------------------------------------------------------------|
| Name                                           | Unit       | Meta                                                                                                                                |
| Resources\|Land Cover\|+\|Cropland             | million ha | Arable land, i.e. land in bioenergy crop, food, and feed/fodder crops, permanent crops as well as other arable land (physical area) |
| Resources\|Land Cover\|Cropland\|+\|Croparea   | million ha | Physical cropland area used for crop production                                                                                     |
| Resources\|Land Cover\|Cropland\|+\|Fallow     | million ha | Fallow cropland                                                                                                                     |
| Resources\|Land Cover\|Cropland\|+\|Tree Cover | million ha | Trees on cropland for agroforestry                                                                                                  |

## Pasture and urban variables

|                                                   |            |                                                                                  |
|---------------------------------------------------|------------|----------------------------------------------------------------------------------|
| Name                                              | Unit       | Meta                                                                             |
| Resources\|Land Cover\|+\|Pastures and Rangelands | million ha | Pasture and range land based on FAO definition of permanent meadows and pastures |
| Resources\|Land Cover\|+\|Urban Area              | million ha | Built-up land associated with human settlements                                  |

## Other natural land variables

|                                              |            |                                                                                                                                       |
|----------------------------------------------|------------|---------------------------------------------------------------------------------------------------------------------------------------|
| Name                                         | Unit       | Meta                                                                                                                                  |
| Resources\|Land Cover\|+\|Other Land         | million ha | Non-forest natural land including primary non-forest, restored and recovered natural land                                             |
| Resources\|Land Cover\|Other Land\|Initial   | million ha | Primary non-forest natural land without clearly visible indications of human activities                                               |
| Resources\|Land Cover\|Other Land\|Recovered | million ha | Recovered natural land due to the abandonment of agricultural or forestry land without intention for nature/biodiversity conservation |
| Resources\|Land Cover\|Other Land\|Restored  | million ha | Intentionally restored natural land for the purpose of nature and/or biodiversity conservation                                        |

## Forest variables

|                                                                             |            |                                                                                                                                         |
|-----------------------------------------------------------------------------|------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| Name                                                                        | Unit       | Meta                                                                                                                                    |
| Resources\|Land Cover\|+\|Forest                                            | million ha | Sum of primary, secondary and planted forest (FAO definition)                                                                           |
| Resources\|Land Cover\|Forest\|+\|Natural Forest                            | million ha | Naturally regenerated forest including primary and secondary forest                                                                     |
| Resources\|Land Cover\|Forest\|Natural Forest\|+\|Primary Forest            | million ha | Naturally regenerated forest of native tree species where there are no clearly visible indications of human activities (FAO definition) |
| Resources\|Land Cover\|Forest\|Natural Forest\|+\|Secondary Forest          | million ha | Forest predominantly composed of trees established through natural regeneration excluding primary forest (based on FAO definition)      |
| Resources\|Land Cover\|Forest\|Natural Forest\|Secondary Forest\|Young      | million ha | Young secondary forest                                                                                                                  |
| Resources\|Land Cover\|Forest\|Natural Forest\|Secondary Forest\|Mature     | million ha | Mature secondary forest                                                                                                                 |
| Resources\|Land Cover\|Forest\|+\|Planted Forest                            | million ha | Forest predominantly composed of trees established through planting and/or deliberate seeding (FAO definition)                          |
| Resources\|Land Cover\|Forest\|Planted Forest\|+\|Plantations               | million ha | Intensively managed planted forests with one or two species, even age class, and regular spacing (FAO definition)                       |
| Resources\|Land Cover\|Forest\|Planted Forest\|Plantations\|+\|Timber       | million ha | Plantations for timber production                                                                                                       |
| Resources\|Land Cover\|Forest\|Planted Forest\|Plantations\|+\|CO2-price AR | million ha | Reforestation and/or afforestation for carbon sequestration with non-native species and/or as monoculture plantation                    |
| Resources\|Land Cover\|Forest\|Planted Forest\|+\|Natural                   | million ha | Planted forest not classified as plantation forest                                                                                      |
| Resources\|Land Cover\|Forest\|Planted Forest\|Natural\|+\|CO2-price AR     | million ha | Reforestation and/or afforestation for carbon sequestration with native tree species resembling natural vegetation                      |
| Resources\|Land Cover\|Forest\|Planted Forest\|Natural\|+\|NPI_NDC AR       | million ha | Afforestation/reforestation under national policies and NDC commitments                                                                 |

## Author

Florian Humpenoeder, Kristine Karstens, Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportLandUse(gdx)
  } # }
```
