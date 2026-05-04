# reportCostCapitalStocks

reports MAgPIE capital stocks

## Usage

``` r
reportCostCapitalStocks(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

Magpie object associated with overall costs and value of production

## Capital stock variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Capital Stocks\|Arable farm capital | million US\$2017 | Capital stocks used in cropland (sticky cost implementation) |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCostCapitalStocks(gdx)
} # }
```
