# reportSDG12

reports all SDG indicators relevant for SD12 - Sustainable Production
and Consumption

## Usage

``` r
reportSDG12(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

MAgPIE object

## SDG12 Sustainable consumption variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| SDG\|SDG12\|Material footprint | tDM/capita/yr | Per-capita crop demand (material footprint proxy) |
| SDG\|SDG12\|Food waste | kcal/cap/day | Per-capita daily food waste (caloric availability minus intake) |
| SDG\|SDG12\|Food waste total | Mt DM/yr | Total food waste in dry matter |
| SDG\|SDG12\|Food loss | Mt DM/yr | Food losses in supply chain (pre-consumer waste) |

## Author

Benjamin Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportSDG12(gdx)
  } # }

```
