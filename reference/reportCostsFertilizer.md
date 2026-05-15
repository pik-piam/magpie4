# reportCostsFertilizer

reports MAgPIE nitrogen fertilizer costs disaggregated to crop
categories

## Usage

``` r
reportCostsFertilizer(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

magpie object with fertilizer costs

## Nitrogen fertilizer cost variables

|                                 |                     |                                 |
|---------------------------------|---------------------|---------------------------------|
| Name                            | Unit                | Meta                            |
| Costs\|N Fertilizer             | million US\$2017/yr | Total nitrogen fertilizer costs |
| Costs\|N Fertilizer\|+\|Crops   | million US\$2017/yr | N fertilizer costs for crops    |
| Costs\|N Fertilizer\|+\|Pasture | million US\$2017/yr | N fertilizer costs for pasture  |

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportCostsFertilizer(gdx)
  } # }
```
