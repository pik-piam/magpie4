# reportTradeGrowth

reports trade growth rate

## Usage

``` r
reportTradeGrowth(gdx, detail = FALSE)
```

## Arguments

- gdx:

  GDX file

- detail:

  if true, provides results for all commodities, otherwhise aggregates
  some groups

## Value

Trade growth rates (index)

## Trade growth variables

|                                                 |       |                                          |
|-------------------------------------------------|-------|------------------------------------------|
| Name                                            | Unit  | Meta                                     |
| Trade\|Trade Growth Rate                        | Index | Trade volume index relative to base year |
| Trade\|Trade Growth Rate\|+\|Crop products      | Index | Crop trade growth index                  |
| Trade\|Trade Growth Rate\|+\|Livestock products | Index | Livestock trade growth index             |

## Author

Xiaoxi Wang

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportTradeGrowth(gdx="fulldata.gdx",detail=TRUE)
  } # }

```
