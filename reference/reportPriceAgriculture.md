# reportPriceAgriculture

reports food commodity prices

## Usage

``` r
reportPriceAgriculture(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

agricultural commodity prices as MAgPIE object (USD)

## Agricultural price variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Prices\|Agriculture\|Crops | US\$2017/tDM | Prices for crop products |
| Prices\|Agriculture\|Livestock products | US\$2017/tDM | Prices for livestock products |

## Author

Mishko Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportPriceAgriculture(gdx)
  } # }
```
