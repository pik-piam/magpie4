# reportLivestockDemStructure

reports the share of different livestock products (excluding fish) in
total livestock calorie food supply

## Usage

``` r
reportLivestockDemStructure(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

livestock demand structure as MAgPIE object (kcal/kcal)

## Livestock demand structure variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Ruminant meat | kcal/kcal | Share of ruminant meat in livestock calorie supply |
| Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Poultry meat and eggs | kcal/kcal | Share of poultry and eggs in livestock calorie supply |
| Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Dairy | kcal/kcal | Share of dairy in livestock calorie supply |
| Nutrition\|Dietary Composition\|Livestock Demand Structure\|+\|Monogastric meat | kcal/kcal | Share of monogastric meat in livestock calorie supply |

## Author

Isabelle Weindl

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportLivestockDemStructure(gdx)
  } # }
```
