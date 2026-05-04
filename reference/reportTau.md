# reportTau

reports Tau

## Usage

``` r
reportTau(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

tau values as MAgPIE object (Index)

## Tau variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Productivity\|Landuse Intensity Indicator Tau | Index | Agricultural land-use intensity indicator for crops |

## Author

Florian Humpenoeder, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportTau(gdx)
} # }

```
