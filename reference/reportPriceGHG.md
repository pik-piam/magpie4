# reportPriceGHG

reports GHG emission prices

## Usage

``` r
reportPriceGHG(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

GHG emission prices as MAgPIE object

## GHG price variables

|                           |               |                               |
|---------------------------|---------------|-------------------------------|
| Name                      | Unit          | Meta                          |
| Prices\|GHG Emission\|CO2 | US\$2017/tCO2 | Carbon dioxide emission price |
| Prices\|GHG Emission\|N2O | US\$2017/tN2O | Nitrous oxide emission price  |
| Prices\|GHG Emission\|CH4 | US\$2017/tCH4 | Methane emission price        |

## Author

Florian Humpenoeder, Amsalu W. Yalew

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportPriceGHG(gdx)
  } # }

```
