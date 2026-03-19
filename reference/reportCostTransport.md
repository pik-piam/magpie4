# reportCostTransport

reports MAgPIE costs

## Usage

``` r
reportCostTransport(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

consumption value as MAgPIE object Unit: see names

## Transport cost variables

|                                         |                     |                                                    |
|-----------------------------------------|---------------------|----------------------------------------------------|
| Name                                    | Unit                | Meta                                               |
| Costs\|Transport                        | million US\$2017/yr | Total transport costs for agricultural commodities |
| Costs\|Transport\|+\|Crops              | million US\$2017/yr | Transport costs for crop products                  |
| Costs\|Transport\|+\|Livestock products | million US\$2017/yr | Transport costs for livestock products             |

## Author

David Chen

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportCostTransport(gdx)
  } # }
```
