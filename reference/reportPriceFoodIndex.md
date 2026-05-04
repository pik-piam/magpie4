# reportPriceFoodIndex

reports food price index

## Usage

``` r
reportPriceFoodIndex(gdx, baseyear = "y2020", level = "regglo")
```

## Arguments

- gdx:

  GDX file

- baseyear:

  baseyear of the price index

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Food price index as MAgPIE object Unit: see names

## Food price index variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Prices\|Index\|Agriculture\|Food products | 1 | Food price index relative to baseyear |
| Prices\|Index\|Agriculture\|Food products\|Plant-based | 1 | Plant-based food price index |
| Prices\|Index\|Agriculture\|Food products\|Plant-based\|Maize | 1 | Maize price index |
| Prices\|Index\|Agriculture\|Food products\|Plant-based\|Rice | 1 | Rice price index |
| Prices\|Index\|Agriculture\|Food products\|Plant-based\|Soybean | 1 | Soybean price index |
| Prices\|Index\|Agriculture\|Food products\|Plant-based\|Temperate cereals | 1 | Wheat/temperate cereals price index |
| Prices\|Index\|Agriculture\|Food products\|Livestock | 1 | Livestock food price index |

## Author

Florian Humpenoeder, Felicitas Beier

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportPriceFoodIndex(gdx)
  } # }

```
