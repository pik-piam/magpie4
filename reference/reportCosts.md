# reportCosts

reports MAgPIE costs

## Usage

``` r
reportCosts(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via gdxAggregate.

## Value

consumption value as MAgPIE object Unit: see names

## Cost variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Costs | million US\$2017/yr | Total production costs |
| Costs\|+\|Input Factors | million US\$2017/yr | Costs for input factors (labor, capital, materials) |
| Costs\|+\|Land Conversion | million US\$2017/yr | Costs for land conversion |
| Costs\|+\|Transport | million US\$2017/yr | Transport costs |
| Costs\|+\|TC | million US\$2017/yr | Technology costs (research and development) |
| Costs\|+\|GHG Emissions | million US\$2017/yr | Costs from GHG emission pricing |
| Costs\|MainSolve w/o GHG Emissions | million US\$2017/yr | Total costs excluding GHG emission costs |

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportCosts(gdx)
  } # }
```
