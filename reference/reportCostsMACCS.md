# reportCostsMACCS

reports MAgPIE mitigation costs disaggregated into labor and capital

## Usage

``` r
reportCostsMACCS(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

magpie object with mitigation costs

## MACC cost variables

|                                |                     |                                       |
|--------------------------------|---------------------|---------------------------------------|
| Name                           | Unit                | Meta                                  |
| Costs\|MACCS\|+\|Labor costs   | million US\$2017/yr | Labor costs for MACC implementation   |
| Costs\|MACCS\|+\|Capital costs | million US\$2017/yr | Capital costs for MACC implementation |

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportCostsMACCS(gdx)
  } # }
```
