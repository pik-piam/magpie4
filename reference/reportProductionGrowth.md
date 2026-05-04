# reportProductionGrowth

reports production growth rate

## Usage

``` r
reportProductionGrowth(gdx, detail = FALSE)
```

## Arguments

- gdx:

  GDX file

- detail:

  if true, provides results for all commodities, otherwhise aggregates
  some groups

## Value

Production growth rates (index)

## Production growth variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Production\|Production Growth Rate | Index | Production volume index relative to base year |
| Production\|Production Growth Rate\|+\|Crop products | Index | Crop production growth index |
| Production\|Production Growth Rate\|+\|Livestock products | Index | Livestock production growth index |

## Author

Xiaoxi Wang

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportProductionGrowth(gdx="fulldata.gdx",detail=TRUE)
  } # }
```
