# reportWaterAvailability

reports water availability

## Usage

``` r
reportWaterAvailability(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

water availability as MAgPIE object Unit: see names

## Water availability variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Water\|Availability\|Agriculture | km3/yr | Water available for agricultural use |

## Author

Felicitas Beier

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportWaterAvailability(gdx)
  } # }
```
