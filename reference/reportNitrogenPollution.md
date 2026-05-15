# reportNitrogenPollution

Reports total Nitrogen Pollution as the sum of surplus from cropland,
pasture, awms, consumption and non-agricutlural land

## Usage

``` r
reportNitrogenPollution(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Nitrogen pollution variables

|                                                                     |          |                                                      |
|---------------------------------------------------------------------|----------|------------------------------------------------------|
| Name                                                                | Unit     | Meta                                                 |
| Resources\|Nitrogen\|Pollution\|Surplus                             | Mt Nr/yr | Total nitrogen pollution surplus from all sources    |
| Resources\|Nitrogen\|Pollution\|Surplus\|+\|Cropland                | Mt Nr/yr | Nitrogen surplus from cropland                       |
| Resources\|Nitrogen\|Pollution\|Surplus\|+\|Pasture                 | Mt Nr/yr | Nitrogen surplus from pasture                        |
| Resources\|Nitrogen\|Pollution\|Surplus\|+\|Animal waste management | Mt Nr/yr | Nitrogen losses from animal waste management systems |
| Resources\|Nitrogen\|Pollution\|Surplus\|+\|Non-agricultural land   | Mt Nr/yr | Nitrogen surplus from non-agricultural land          |
| Resources\|Nitrogen\|Pollution\|Surplus\|+\|End-of-life losses      | Mt Nr/yr | Nitrogen losses from food consumption and waste      |

## Nitrogen aggregate variables

|                                                                                    |          |                                                      |
|------------------------------------------------------------------------------------|----------|------------------------------------------------------|
| Name                                                                               | Unit     | Meta                                                 |
| Resources\|Nitrogen\|Nutrient surplus from agricultural land                       | Mt Nr/yr | Nitrogen surplus from cropland and pasture           |
| Resources\|Nitrogen\|Nutrient surplus from agricultural land and manure management | Mt Nr/yr | Nitrogen surplus from cropland, pasture, and AWMS    |
| Resources\|Nitrogen\|Nutrient surplus from all land and manure management          | Mt Nr/yr | Nitrogen surplus from all land and manure management |

## See also

[`NitrogenBudget`](NitrogenBudget.md)

## Author

Benjamin Leon Bodirsky, Michael Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportNitrogenPollution(gdx)
} # }
```
