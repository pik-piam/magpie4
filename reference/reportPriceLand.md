# reportPriceLand

reports land prices (land rent)

## Usage

``` r
reportPriceLand(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

land prices as MAgPIE object Unit: see names

## Land price variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Prices\|Land\|Cropland | US\$2017/ha | Land rent (shadow price of cropland constraint) |

## Author

Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportPriceLand(gdx)
  } # }
```
