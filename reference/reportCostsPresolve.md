# reportCostsPresolve

reports MAgPIE costs

## Usage

``` r
reportCostsPresolve(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

consumption value as MAgPIE object Unit: see names

## Presolve cost variables

|                        |                  |                                      |
|------------------------|------------------|--------------------------------------|
| Name                   | Unit             | Meta                                 |
| Costs\|PreSolve\|Total | million US\$2017 | Cumulative costs from presolve phase |

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportCostsPresolve(gdx)
  } # }
```
