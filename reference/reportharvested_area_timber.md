# reportharvested_area_timber

reports MAgPIE harvested area for timber.

## Usage

``` r
reportharvested_area_timber(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Area harvested for timber production

## Timber harvested area variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Timber operations\|Harvested area for timber\|Forestry | Mha per yr | Area harvested from managed forests |
| Resources\|Timber operations\|Harvested area for timber\|Primary forest | Mha per yr | Area harvested from primary forests |
| Resources\|Timber operations\|Harvested area for timber\|Secondary forest | Mha per yr | Area harvested from secondary forests |

## Author

Abhijeet Mishra

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportharvested_area_timber(gdx)
  } # }

```
