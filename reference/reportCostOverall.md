# reportCostOverall

reports MAgPIE costs

## Usage

``` r
reportCostOverall(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

Magpie object associated with overall costs and value of production

## Overall cost variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Costs\|Gross value of production | million US\$2017/yr | Total gross value of agricultural production |

## Author

Edna J. Molina Bacca

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportCostOverall(gdx)
  } # }
```
