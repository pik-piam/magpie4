# reportForestYield

reports MAgPIE harvested area for timber.

## Usage

``` r
reportForestYield(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Yield from Forests for timber production

## Forest yield variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Timber Yields\|Harvest\|Forestry | m3 per ha | Timber yield from plantation forestry |
| Timber Yields\|Harvest\|Primary forest | m3 per ha | Timber yield from primary forest harvesting |
| Timber Yields\|Harvest\|Secondary forest | m3 per ha | Timber yield from secondary forest harvesting |

## Author

Abhijeet Mishra, Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportForestYield(gdx)
  } # }

```
