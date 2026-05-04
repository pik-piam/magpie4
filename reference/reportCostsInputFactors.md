# reportFactorCosts

reports MAgPIE factor costs (split into labor and capital for sticky
realization)

## Usage

``` r
reportCostsInputFactors(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

magpie object with factor costs

## Input factor cost variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Costs\|Input Factors | million US\$2017/yr | Total input factor costs (labor + capital) |
| Costs\|Input Factors\|+\|Crops | million US\$2017/yr | Factor costs for crop production |
| Costs\|Input Factors\|+\|Livestock | million US\$2017/yr | Factor costs for livestock production |
| Costs\|Input Factors\|+\|Fish | million US\$2017/yr | Factor costs for fish production |
| Costs\|Input Factors\|+\|Pasture | million US\$2017/yr | Factor costs for pasture management |
| Costs\|Input Factors\|+\|Residues | million US\$2017/yr | Factor costs for residue handling |
| Costs\|Input Factors\|++\|Labor costs | million US\$2017/yr | Total labor costs |
| Costs\|Input Factors\|++\|Capital costs | million US\$2017/yr | Total capital costs |

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCostsInputFactors(gdx)
} # }

```
