# reportPriceWater

reports water prices

## Usage

``` r
reportPriceWater(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

water usage as MAgPIE object Unit: see names

## Water price variables

|                            |                |                                |
|----------------------------|----------------|--------------------------------|
| Name                       | Unit           | Meta                           |
| Prices\|Water\|Agriculture | Index 2005=100 | Agricultural water price index |

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportPriceWater(gdx)
} # }
```
