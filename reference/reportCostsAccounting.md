# reportCostsAccounting

reports MAgPIE costs including total investments

## Usage

``` r
reportCostsAccounting(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

Costs accounting including total investments

## Cost accounting variables

|                                      |                     |                                             |
|--------------------------------------|---------------------|---------------------------------------------|
| Name                                 | Unit                | Meta                                        |
| Costs Accounting                     | million US\$2017/yr | Total cost accounting including investments |
| Costs Accounting\|+\|Land Conversion | million US\$2017/yr | Investment costs for land conversion        |
| Costs Accounting\|+\|Transport       | million US\$2017/yr | Transport cost investments                  |
| Costs Accounting\|+\|TC              | million US\$2017/yr | Technological change investment costs       |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCostsAccounting(gdx)
} # }
```
