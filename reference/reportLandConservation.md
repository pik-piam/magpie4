# reportLandConservation

reports land conservation areas

## Usage

``` r
reportLandConservation(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

land conservation area in Mha

## Total conserved land variables

|                                                                           |            |                                                                |
|---------------------------------------------------------------------------|------------|----------------------------------------------------------------|
| Name                                                                      | Unit       | Meta                                                           |
| Resources\|Land Cover Conserved\|Cropland                                 | million ha | Total conserved cropland area (protected + restored)           |
| Resources\|Land Cover Conserved\|Pastures and Rangelands                  | million ha | Total conserved pasture area (protected + restored)            |
| Resources\|Land Cover Conserved\|Forest\|Planted Forest                   | million ha | Total conserved planted forest area (protected + restored)     |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest   | million ha | Total conserved primary forest area (protected + restored)     |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest | million ha | Total conserved secondary forest area (protected + restored)   |
| Resources\|Land Cover Conserved\|Urban Area                               | million ha | Total conserved urban area (protected + restored)              |
| Resources\|Land Cover Conserved\|Other Land                               | million ha | Total conserved other natural land area (protected + restored) |

## Protected land variables

|                                                                                         |            |                                   |
|-----------------------------------------------------------------------------------------|------------|-----------------------------------|
| Name                                                                                    | Unit       | Meta                              |
| Resources\|Land Cover Conserved\|Cropland\|+\|Protected                                 | million ha | Protected cropland area           |
| Resources\|Land Cover Conserved\|Pastures and Rangelands\|+\|Protected                  | million ha | Protected pasture area            |
| Resources\|Land Cover Conserved\|Forest\|Planted Forest\|+\|Protected                   | million ha | Protected planted forest area     |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|+\|Protected   | million ha | Protected primary forest area     |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|+\|Protected | million ha | Protected secondary forest area   |
| Resources\|Land Cover Conserved\|Urban Area\|+\|Protected                               | million ha | Protected urban area              |
| Resources\|Land Cover Conserved\|Other Land\|+\|Protected                               | million ha | Protected other natural land area |

## Restored land variables

|                                                                                        |            |                                  |
|----------------------------------------------------------------------------------------|------------|----------------------------------|
| Name                                                                                   | Unit       | Meta                             |
| Resources\|Land Cover Conserved\|Cropland\|+\|Restored                                 | million ha | Restored cropland area           |
| Resources\|Land Cover Conserved\|Pastures and Rangelands\|+\|Restored                  | million ha | Restored pasture area            |
| Resources\|Land Cover Conserved\|Forest\|Planted Forest\|+\|Restored                   | million ha | Restored planted forest area     |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|+\|Restored   | million ha | Restored primary forest area     |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|+\|Restored | million ha | Restored secondary forest area   |
| Resources\|Land Cover Conserved\|Urban Area\|+\|Restored                               | million ha | Restored urban area              |
| Resources\|Land Cover Conserved\|Other Land\|+\|Restored                               | million ha | Restored other natural land area |

## Annual restoration variables

|                                                                                              |               |                                          |
|----------------------------------------------------------------------------------------------|---------------|------------------------------------------|
| Name                                                                                         | Unit          | Meta                                     |
| Resources\|Land Cover Conserved\|Cropland\|Restored annually                                 | million ha/yr | Annual cropland restoration rate         |
| Resources\|Land Cover Conserved\|Pastures and Rangelands\|Restored annually                  | million ha/yr | Annual pasture restoration rate          |
| Resources\|Land Cover Conserved\|Forest\|Planted Forest\|Restored annually                   | million ha/yr | Annual planted forest restoration rate   |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|Restored annually   | million ha/yr | Annual primary forest restoration rate   |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|Restored annually | million ha/yr | Annual secondary forest restoration rate |
| Resources\|Land Cover Conserved\|Urban Area\|Restored annually                               | million ha/yr | Annual urban area restoration rate       |
| Resources\|Land Cover Conserved\|Other Land\|Restored annually                               | million ha/yr | Annual other land restoration rate       |

## Cumulative restoration variables

|                                                                                                  |                       |                                                    |
|--------------------------------------------------------------------------------------------------|-----------------------|----------------------------------------------------|
| Name                                                                                             | Unit                  | Meta                                               |
| Resources\|Land Cover Conserved\|Cropland\|Restored cumulatively                                 | million ha since 2025 | Cumulative cropland restoration since 2025         |
| Resources\|Land Cover Conserved\|Pastures and Rangelands\|Restored cumulatively                  | million ha since 2025 | Cumulative pasture restoration since 2025          |
| Resources\|Land Cover Conserved\|Forest\|Planted Forest\|Restored cumulatively                   | million ha since 2025 | Cumulative planted forest restoration since 2025   |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|Restored cumulatively   | million ha since 2025 | Cumulative primary forest restoration since 2025   |
| Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|Restored cumulatively | million ha since 2025 | Cumulative secondary forest restoration since 2025 |
| Resources\|Land Cover Conserved\|Urban Area\|Restored cumulatively                               | million ha since 2025 | Cumulative urban area restoration since 2025       |
| Resources\|Land Cover Conserved\|Other Land\|Restored cumulatively                               | million ha since 2025 | Cumulative other land restoration since 2025       |

## Author

Patrick v. Jeetze, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportLandConservation(gdx)
} # }
```
