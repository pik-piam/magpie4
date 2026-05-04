# reportCarbonstock

Reports the carbon stocks for future MAgPIE projections

## Usage

``` r
reportCarbonstock(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Carbon stock variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Carbon | Mt C | Total terrestrial carbon stocks |
| Resources\|Carbon\|+\|Soil | Mt C | Soil carbon stocks |
| Resources\|Carbon\|+\|Litter | Mt C | Litter carbon stocks |
| Resources\|Carbon\|+\|Vegetation | Mt C | Vegetation carbon stocks (above and below ground biomass) |

## Author

Kristine Karstens

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportSOM(gdx)
  } # }
```
