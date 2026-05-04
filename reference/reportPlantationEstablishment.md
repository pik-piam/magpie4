# reportPlantationEstablishment

reports MAgPIE harvested area for timber.

## Usage

``` r
reportPlantationEstablishment(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Area harvested for timber production

## Plantation establishment variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Timber operations\|Area Newly Established\|Forestry | Mha per yr | Annual area of new timber plantations established |

## Author

Abhijeet Mishra

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportPlantationEstablishment(gdx)
  } # }

```
