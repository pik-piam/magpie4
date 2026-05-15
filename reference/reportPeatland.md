# reportPeatland

reports peatland area

## Usage

``` r
reportPeatland(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

peatland area as magclass object (million ha)

## Peatland variables

|                                  |            |                            |
|----------------------------------|------------|----------------------------|
| Name                             | Unit       | Meta                       |
| Resources\|Peatland              | million ha | Area of peatlands          |
| Resources\|Peatland\|+\|Intact   | million ha | Area of intact peatlands   |
| Resources\|Peatland\|+\|Degraded | million ha | Area of drained peatlands  |
| Resources\|Peatland\|+\|Rewetted | million ha | Area of rewetted peatlands |

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportPeatland(gdx)
  } # }
```
