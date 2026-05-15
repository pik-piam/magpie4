# reportLandTransitionMatrix

reports land transition matrix with gross land-use change flows between
all land types

## Usage

``` r
reportLandTransitionMatrix(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  The aggregation level to be used ("regglo" by default)

## Value

land transition matrix as MAgPIE object (Mha/yr)

## Total land transition variables

|                             |        |                                                                   |
|-----------------------------|--------|-------------------------------------------------------------------|
| Name                        | Unit   | Meta                                                              |
| Resources\|Land Transitions | Mha/yr | Total gross land-use change (sum of all off-diagonal transitions) |

## From Cropland variables

|                                                                               |        |                                                                                                 |
|-------------------------------------------------------------------------------|--------|-------------------------------------------------------------------------------------------------|
| Name                                                                          | Unit   | Meta                                                                                            |
| Resources\|Land Transitions\|+\|From Cropland                                 | Mha/yr | Total annual gross land-use change from cropland to other land types                            |
| Resources\|Land Transitions\|From Cropland\|+\|To Pastures and Rangelands     | Mha/yr | Annual gross transition from cropland to pastures and rangelands                                |
| Resources\|Land Transitions\|From Cropland\|+\|To Urban Area                  | Mha/yr | Annual gross transition from cropland to urban area                                             |
| Resources\|Land Transitions\|From Cropland\|+\|To Other Land                  | Mha/yr | Annual gross transition from cropland to other land                                             |
| Resources\|Land Transitions\|From Cropland\|+\|To Forest                      | Mha/yr | Annual gross transition from cropland to forest (sum of planted, primary, and secondary forest) |
| Resources\|Land Transitions\|From Cropland\|To Forest\|+\|To Planted Forest   | Mha/yr | Annual gross transition from cropland to planted forest                                         |
| Resources\|Land Transitions\|From Cropland\|To Forest\|+\|To Primary Forest   | Mha/yr | Annual gross transition from cropland to primary forest (structurally zero)                     |
| Resources\|Land Transitions\|From Cropland\|To Forest\|+\|To Secondary Forest | Mha/yr | Annual gross transition from cropland to secondary forest                                       |

## From Pastures and Rangelands variables

|                                                                                              |        |                                                                                                                |
|----------------------------------------------------------------------------------------------|--------|----------------------------------------------------------------------------------------------------------------|
| Name                                                                                         | Unit   | Meta                                                                                                           |
| Resources\|Land Transitions\|+\|From Pastures and Rangelands                                 | Mha/yr | Total annual gross land-use change from pastures and rangelands to other land types                            |
| Resources\|Land Transitions\|From Pastures and Rangelands\|+\|To Cropland                    | Mha/yr | Annual gross transition from pastures and rangelands to cropland                                               |
| Resources\|Land Transitions\|From Pastures and Rangelands\|+\|To Urban Area                  | Mha/yr | Annual gross transition from pastures and rangelands to urban area                                             |
| Resources\|Land Transitions\|From Pastures and Rangelands\|+\|To Other Land                  | Mha/yr | Annual gross transition from pastures and rangelands to other land                                             |
| Resources\|Land Transitions\|From Pastures and Rangelands\|+\|To Forest                      | Mha/yr | Annual gross transition from pastures and rangelands to forest (sum of planted, primary, and secondary forest) |
| Resources\|Land Transitions\|From Pastures and Rangelands\|To Forest\|+\|To Planted Forest   | Mha/yr | Annual gross transition from pastures and rangelands to planted forest                                         |
| Resources\|Land Transitions\|From Pastures and Rangelands\|To Forest\|+\|To Primary Forest   | Mha/yr | Annual gross transition from pastures and rangelands to primary forest (structurally zero)                     |
| Resources\|Land Transitions\|From Pastures and Rangelands\|To Forest\|+\|To Secondary Forest | Mha/yr | Annual gross transition from pastures and rangelands to secondary forest                                       |

## From Planted Forest variables

|                                                                                     |        |                                                                                                         |
|-------------------------------------------------------------------------------------|--------|---------------------------------------------------------------------------------------------------------|
| Name                                                                                | Unit   | Meta                                                                                                    |
| Resources\|Land Transitions\|+\|From Planted Forest                                 | Mha/yr | Total annual gross land-use change from planted forest to other land types                              |
| Resources\|Land Transitions\|From Planted Forest\|+\|To Cropland                    | Mha/yr | Annual gross transition from planted forest to cropland                                                 |
| Resources\|Land Transitions\|From Planted Forest\|+\|To Pastures and Rangelands     | Mha/yr | Annual gross transition from planted forest to pastures and rangelands                                  |
| Resources\|Land Transitions\|From Planted Forest\|+\|To Urban Area                  | Mha/yr | Annual gross transition from planted forest to urban area                                               |
| Resources\|Land Transitions\|From Planted Forest\|+\|To Other Land                  | Mha/yr | Annual gross transition from planted forest to other land                                               |
| Resources\|Land Transitions\|From Planted Forest\|+\|To Forest                      | Mha/yr | Annual gross transition from planted forest to other forest types (sum of primary and secondary forest) |
| Resources\|Land Transitions\|From Planted Forest\|To Forest\|+\|To Primary Forest   | Mha/yr | Annual gross transition from planted forest to primary forest (structurally zero)                       |
| Resources\|Land Transitions\|From Planted Forest\|To Forest\|+\|To Secondary Forest | Mha/yr | Annual gross transition from planted forest to secondary forest                                         |

## From Primary Forest variables

|                                                                                     |        |                                                                                                         |
|-------------------------------------------------------------------------------------|--------|---------------------------------------------------------------------------------------------------------|
| Name                                                                                | Unit   | Meta                                                                                                    |
| Resources\|Land Transitions\|+\|From Primary Forest                                 | Mha/yr | Total annual gross land-use change from primary forest to other land types                              |
| Resources\|Land Transitions\|From Primary Forest\|+\|To Cropland                    | Mha/yr | Annual gross transition from primary forest to cropland                                                 |
| Resources\|Land Transitions\|From Primary Forest\|+\|To Pastures and Rangelands     | Mha/yr | Annual gross transition from primary forest to pastures and rangelands                                  |
| Resources\|Land Transitions\|From Primary Forest\|+\|To Urban Area                  | Mha/yr | Annual gross transition from primary forest to urban area                                               |
| Resources\|Land Transitions\|From Primary Forest\|+\|To Other Land                  | Mha/yr | Annual gross transition from primary forest to other land                                               |
| Resources\|Land Transitions\|From Primary Forest\|+\|To Forest                      | Mha/yr | Annual gross transition from primary forest to other forest types (sum of planted and secondary forest) |
| Resources\|Land Transitions\|From Primary Forest\|To Forest\|+\|To Planted Forest   | Mha/yr | Annual gross transition from primary forest to planted forest                                           |
| Resources\|Land Transitions\|From Primary Forest\|To Forest\|+\|To Secondary Forest | Mha/yr | Annual gross transition from primary forest to secondary forest                                         |

## From Secondary Forest variables

|                                                                                     |        |                                                                                                         |
|-------------------------------------------------------------------------------------|--------|---------------------------------------------------------------------------------------------------------|
| Name                                                                                | Unit   | Meta                                                                                                    |
| Resources\|Land Transitions\|+\|From Secondary Forest                               | Mha/yr | Total annual gross land-use change from secondary forest to other land types                            |
| Resources\|Land Transitions\|From Secondary Forest\|+\|To Cropland                  | Mha/yr | Annual gross transition from secondary forest to cropland                                               |
| Resources\|Land Transitions\|From Secondary Forest\|+\|To Pastures and Rangelands   | Mha/yr | Annual gross transition from secondary forest to pastures and rangelands                                |
| Resources\|Land Transitions\|From Secondary Forest\|+\|To Urban Area                | Mha/yr | Annual gross transition from secondary forest to urban area                                             |
| Resources\|Land Transitions\|From Secondary Forest\|+\|To Other Land                | Mha/yr | Annual gross transition from secondary forest to other land                                             |
| Resources\|Land Transitions\|From Secondary Forest\|+\|To Forest                    | Mha/yr | Annual gross transition from secondary forest to other forest types (sum of planted and primary forest) |
| Resources\|Land Transitions\|From Secondary Forest\|To Forest\|+\|To Planted Forest | Mha/yr | Annual gross transition from secondary forest to planted forest                                         |
| Resources\|Land Transitions\|From Secondary Forest\|To Forest\|+\|To Primary Forest | Mha/yr | Annual gross transition from secondary forest to primary forest (structurally zero)                     |

## From Urban Area variables

|                                                                                 |        |                                                                                                   |
|---------------------------------------------------------------------------------|--------|---------------------------------------------------------------------------------------------------|
| Name                                                                            | Unit   | Meta                                                                                              |
| Resources\|Land Transitions\|+\|From Urban Area                                 | Mha/yr | Total annual gross land-use change from urban area to other land types                            |
| Resources\|Land Transitions\|From Urban Area\|+\|To Cropland                    | Mha/yr | Annual gross transition from urban area to cropland                                               |
| Resources\|Land Transitions\|From Urban Area\|+\|To Pastures and Rangelands     | Mha/yr | Annual gross transition from urban area to pastures and rangelands                                |
| Resources\|Land Transitions\|From Urban Area\|+\|To Other Land                  | Mha/yr | Annual gross transition from urban area to other land                                             |
| Resources\|Land Transitions\|From Urban Area\|+\|To Forest                      | Mha/yr | Annual gross transition from urban area to forest (sum of planted, primary, and secondary forest) |
| Resources\|Land Transitions\|From Urban Area\|To Forest\|+\|To Planted Forest   | Mha/yr | Annual gross transition from urban area to planted forest                                         |
| Resources\|Land Transitions\|From Urban Area\|To Forest\|+\|To Primary Forest   | Mha/yr | Annual gross transition from urban area to primary forest (structurally zero)                     |
| Resources\|Land Transitions\|From Urban Area\|To Forest\|+\|To Secondary Forest | Mha/yr | Annual gross transition from urban area to secondary forest                                       |

## From Other Land variables

|                                                                                 |        |                                                                                                   |
|---------------------------------------------------------------------------------|--------|---------------------------------------------------------------------------------------------------|
| Name                                                                            | Unit   | Meta                                                                                              |
| Resources\|Land Transitions\|+\|From Other Land                                 | Mha/yr | Total annual gross land-use change from other land to other land types                            |
| Resources\|Land Transitions\|From Other Land\|+\|To Cropland                    | Mha/yr | Annual gross transition from other land to cropland                                               |
| Resources\|Land Transitions\|From Other Land\|+\|To Pastures and Rangelands     | Mha/yr | Annual gross transition from other land to pastures and rangelands                                |
| Resources\|Land Transitions\|From Other Land\|+\|To Urban Area                  | Mha/yr | Annual gross transition from other land to urban area                                             |
| Resources\|Land Transitions\|From Other Land\|+\|To Forest                      | Mha/yr | Annual gross transition from other land to forest (sum of planted, primary, and secondary forest) |
| Resources\|Land Transitions\|From Other Land\|To Forest\|+\|To Planted Forest   | Mha/yr | Annual gross transition from other land to planted forest                                         |
| Resources\|Land Transitions\|From Other Land\|To Forest\|+\|To Primary Forest   | Mha/yr | Annual gross transition from other land to primary forest (structurally zero)                     |
| Resources\|Land Transitions\|From Other Land\|To Forest\|+\|To Secondary Forest | Mha/yr | Annual gross transition from other land to secondary forest                                       |

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportLandTransitionMatrix(gdx)
  } # }
```
