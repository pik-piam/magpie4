# reportCostCapitalInvestment

reports MAgPIE capital investments

## Usage

``` r
reportCostCapitalInvestment(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

Magpie object associated with overall costs and value of production

## Capital investment cost variables

|                            |                  |                                                  |
|----------------------------|------------------|--------------------------------------------------|
| Name                       | Unit             | Meta                                             |
| Costs\|Capital Investments | million US\$2017 | Capital investments (sticky cost implementation) |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCostCapitalInvestment(gdx)
} # }
```
