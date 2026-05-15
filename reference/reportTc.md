# reportTc

reports Tc

## Usage

``` r
reportTc(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  Aggregation level of the returned Tc report

## Value

tc values as MAgPIE object (%/yr)

## Technological change variables

|                                                           |      |                                                                |
|-----------------------------------------------------------|------|----------------------------------------------------------------|
| Name                                                      | Unit | Meta                                                           |
| Productivity\|Yield-increasing technological change crops | %/yr | Annual rate of yield-increasing technological change for crops |

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportTc(gdx)
  } # }

```
